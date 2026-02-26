test_that("Vector ordering", {
  set.seed(1234)
  x <- sample(1:20, size = 20)
  xOrder <- CaseBasedReasoning:::cpp_orderVector(x, sortDirection = 0) |>
    as.numeric()
  yOrder <- order(x)
  expect_equal(xOrder, yOrder)
})

test_that("Matrix ordering", {
  set.seed(1234)
  x <- matrix(rnorm(100), 10)
  xOrder <- CaseBasedReasoning:::cpp_orderMatrix(x, sortDirection = 0, k = 10)
  yOrder <- do.call(cbind, lapply(1:nrow(x), function(col) order(x[, col])))
  expect_equal(xOrder, yOrder)
})
