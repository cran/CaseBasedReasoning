test_that("Terminal Nodes", {
  set.seed(1234)
  rf <- ranger::ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
  tn1 <- terminal_nodes(iris[, -5], rf)
  tn2 <- predict(rf, iris[, -5], type = "terminalNodes")
  expect_equal(tn1, tn2$predictions)
})


test_that("Number of Edges between Terminal Nodes", {
  set.seed(42)
  df <- data.frame(
    class = as.factor(c(rep(0, 100), rep(1, 100), rep(2, 100))),
    x1 = c(rnorm(100, 0, .1), rnorm(100, 10, .1), rnorm(100, 20, .1)),
    x2 = c(rnorm(100, 10, .1), rnorm(100, 0, .1), rnorm(100, 20, .1))
  )

  set.seed(1234)
  rf_fit <- ranger::ranger(class ~ ., data = df, num.trees = 1, mtry = 2, min.node.size = 0)

  n_edges <- edges_between_terminal_nodes(rf_fit)

  # Result is a data.frame with expected columns
  expect_s3_class(n_edges, "data.frame")
  expect_true(all(c("x", "y", "tree_1") %in% names(n_edges)))

  # Get actual terminal node IDs from the forest
  tn <- predict(rf_fit, df[, -1], type = "terminalNodes")$predictions
  terminal_ids <- sort(unique(as.vector(tn)))
  n_terminal <- length(terminal_ids)

  # Number of rows = n_terminal_nodes choose 2
  expect_equal(nrow(n_edges), n_terminal * (n_terminal - 1) / 2)

  # Pairs are ordered: x < y
  expect_true(all(n_edges$x < n_edges$y))

  # Terminal node IDs in x and y match actual terminal nodes
  edge_ids <- sort(unique(c(n_edges$x, n_edges$y)))
  expect_equal(edge_ids, terminal_ids)

  # Edge counts are positive integers
  expect_true(all(n_edges$tree_1 > 0))
  expect_equal(n_edges$tree_1, as.integer(n_edges$tree_1))
})


test_that("Perfect Separation Test - Proximity", {
  set.seed(42)
  df <- data.frame(
    class = as.factor(c(rep(0, 100), rep(1, 100), rep(2, 100))),
    x1 = c(rnorm(100, 0, .1), rnorm(100, 10, .1), rnorm(100, 20, .1)),
    x2 = c(rnorm(100, 10, .1), rnorm(100, 0, .1), rnorm(100, 20, .1))
  )

  set.seed(1234)
  rf_fit <- ranger::ranger(class ~ ., data = df, num.trees = 1, mtry = 2, min.node.size = 0)

  # Proximity
  d <- distance_random_forest(x = df[, -1], rfObject = rf_fit)
  expect_s3_class(d, "dist")
  expect_equal(sum(diag(table(cutree(hclust(d), k = 3), df$class))), 300, info = 'Proximity Distance')

  # Depth returns a valid dist object
  d <- distance_random_forest(x = df[, -1], rfObject = rf_fit, method = 'Depth')
  expect_s3_class(d, "dist")
  expect_true(all(d >= 0))
})
