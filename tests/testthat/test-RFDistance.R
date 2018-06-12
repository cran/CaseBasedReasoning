testthat::context("Ranger Proximity")

testthat::test_that("Terminal Node IDs", {
  set.seed((1234))
  rf <- ranger::ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
  tn1 <- terminalNodeIDs(iris[, -5], rf)
  tn2 <- predict(rf, iris[, -5], type = "terminalNodes")
  testthat::expect_equal(tn1, tn2$predictions)
})