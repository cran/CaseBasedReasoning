testthat::context("Order")

testthat::test_that("Vector ordering", {
  set.seed(1234)
  x <- sample(1:20, size = 20)
  xOrder <- cpp_orderVector(x)
  yOrder <- order(x)
  testthat::expect_equal(xOrder, yOrder)
})

testthat::test_that("Matrix ordering", {
  set.seed((1234))
  x <- matrix(rnorm(100), 10)
  xOrder <- cpp_orderMatrix(x)
  yOrder <- do.call(cbind, lapply(1:nrow(x), function(col) {order(x[, col])}))
  testthat::expect_equal(xOrder, yOrder)
})