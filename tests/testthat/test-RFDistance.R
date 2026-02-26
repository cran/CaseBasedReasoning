test_that("Terminal Nodes", {
  set.seed(1234)
  rf <- ranger::ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
  tn1 <- terminal_nodes(iris[, -5], rf)
  tn2 <- predict(rf, iris[, -5], type = "terminalNodes")
  expect_equal(tn1, tn2$predictions)
})


test_that("Number of Edges between Terminal Nodes", {
  df <- data.frame(
    class = as.factor(c(rep(0, 100), rep(1, 100), rep(2, 100))),
    x1 = c(rnorm(100, 0, .1), rnorm(100, 10, .1), rnorm(100, 20, .1)),
    x2 = c(rnorm(100, 10, .1), rnorm(100, 0, .1), rnorm(100, 20, .1))
  )

  set.seed(1234)
  rf_fit <- ranger::ranger(class ~ ., data = df, num.trees = 1, mtry = 2, min.node.size = 0)
  m_forest <- ranger_forests_to_matrix(rf_fit)

  n_edges_by_hand <- data.frame(
    x = c(2, 2, 4),
    y = c(4, 5, 5),
    tree_1 = c(3, 3, 2)
  )

  n_edges_calculated <- edges_between_terminal_nodes(rf_fit)
  expect_equal(n_edges_by_hand, n_edges_calculated)
})


test_that("Perfect Separation Test - Proximity", {
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
