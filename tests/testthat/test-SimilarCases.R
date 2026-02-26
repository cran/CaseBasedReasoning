test_that("Classification Random Forest", {
  set.seed(1234)
  df <- data.frame(
    class = as.factor(c(rep(0, 100), rep(1, 100), rep(2, 100))),
    x1 = c(rnorm(100, 0, .1), rnorm(100, 10, .1), rnorm(100, 20, .1)),
    x2 = c(rnorm(100, 10, .1), rnorm(100, 0, .1), rnorm(100, 20, .1))
  )

  rf_model <- RFModel$new(class ~ ., data = df, num.trees = 1, mtry = 2, min.node.size = 0)
  rf_model$fit()
  expect_s3_class(rf_model$model_fit, 'ranger')

  rf_model$set_distance_method('Depth')
  d <- rf_model$calc_distance_matrix()
  expect_s3_class(d, "dist")

  rf_model$set_distance_method('Proximity')
  d <- rf_model$calc_distance_matrix()
  expect_s3_class(d, "dist")
  expect_equal(sum(diag(table(cutree(hclust(d), k = 3), df$class))), 300, info = 'Proximity Distance')
})

test_that("Cox-Proportional-Hazard", {
  ovarian <- survival::ovarian
  ovarian$resid.ds <- factor(ovarian$resid.ds)
  ovarian$rx <- factor(ovarian$rx)
  ovarian$ecog.ps <- factor(ovarian$ecog.ps)

  # initialize R6 object
  cph_model <- CoxModel$new(survival::Surv(futime, fustat) ~ age + resid.ds + rx + ecog.ps, ovarian)

  cph_model$fit()
  expect_s3_class(cph_model$model_fit, 'cph')

  dist_mat <- cph_model$calc_distance_matrix()

  matchedData <- cph_model$get_similar_cases(query = ovarian, k = 3)
  expect_true(nrow(matchedData) > 0)
})

test_that("Validation: formula terms must exist in data", {
  expect_error(
    RFModel$new(nonexistent ~ x1 + x2, data = data.frame(x1 = 1:5, x2 = 1:5)),
    "Variables not found in data"
  )
})

test_that("Validation: k must be valid", {
  df <- data.frame(
    class = as.factor(c(rep(0, 50), rep(1, 50))),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  rf_model <- RFModel$new(class ~ ., data = df)
  rf_model$fit()

  expect_error(rf_model$get_similar_cases(query = df[1:5, ], k = "a"), "'k' must be numeric")
  expect_error(rf_model$get_similar_cases(query = df[1:5, ], k = 0), "'k' must be >= 1")
})

test_that("Validation: query is required in get_similar_cases", {
  df <- data.frame(
    class = as.factor(c(rep(0, 50), rep(1, 50))),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  rf_model <- RFModel$new(class ~ ., data = df)
  rf_model$fit()

  expect_error(rf_model$get_similar_cases(), "'query' is required")
})

test_that("Validation: ranger validator works", {
  expect_error(CaseBasedReasoning:::validate_ranger("not_ranger"), "Expected a 'ranger' object")
  expect_error(terminal_nodes(iris[, -5], "not_ranger"), "Expected a 'ranger' object")
})

test_that("Validation: RFModel requires trained model for distance", {
  df <- data.frame(
    class = as.factor(c(rep(0, 50), rep(1, 50))),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  rf_model <- RFModel$new(class ~ ., data = df)
  expect_error(rf_model$calc_distance_matrix(), "Expected a 'ranger' object")
})

test_that("Validation: RegressionModel requires trained model for distance", {
  ovarian <- survival::ovarian
  ovarian$resid.ds <- factor(ovarian$resid.ds)
  ovarian$rx <- factor(ovarian$rx)
  ovarian$ecog.ps <- factor(ovarian$ecog.ps)
  cph_model <- CoxModel$new(survival::Surv(futime, fustat) ~ age + resid.ds + rx + ecog.ps, ovarian)
  expect_error(cph_model$calc_distance_matrix(), "Model not trained")
})

test_that("Deprecated function wrappers emit warnings", {
  set.seed(1234)
  rf <- ranger::ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)

  expect_warning(terminalNodes(iris[, -5], rf), "deprecated")
  expect_warning(asDistObject(1:3, 3, "test"), "deprecated")
  expect_warning(weightedDistance(as.matrix(iris[1:5, 1:4])), "deprecated")
  expect_warning(distanceRandomForest(x = iris[, -5], rfObject = rf), "deprecated")
})

test_that("Deprecated R6 field bindings emit warnings", {
  ovarian <- survival::ovarian
  ovarian$resid.ds <- factor(ovarian$resid.ds)
  ovarian$rx <- factor(ovarian$rx)
  ovarian$ecog.ps <- factor(ovarian$ecog.ps)
  cph_model <- CoxModel$new(survival::Surv(futime, fustat) ~ age + resid.ds + rx + ecog.ps, ovarian)

  expect_warning(cph_model$endPoint, "deprecated")
  expect_warning(cph_model$distMat, "deprecated")
  expect_warning(cph_model$orderMat, "deprecated")
})

test_that("S3 predict method dispatches for RFModel", {
  df <- data.frame(
    class = as.factor(c(rep(0, 50), rep(1, 50))),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  rf_model <- RFModel$new(class ~ ., data = df)
  rf_model$fit()

  result <- predict(rf_model, newdata = df[1:5, ], k = 2)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5 * 2)
})

test_that("S3 summary method works for RFModel", {
  df <- data.frame(
    class = as.factor(c(rep(0, 50), rep(1, 50))),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  rf_model <- RFModel$new(class ~ ., data = df)
  rf_model$fit()

  expect_output(summary(rf_model), "RFModel")
})

test_that("S3 summary method works for CoxModel", {
  ovarian <- survival::ovarian
  ovarian$resid.ds <- factor(ovarian$resid.ds)
  ovarian$rx <- factor(ovarian$rx)
  ovarian$ecog.ps <- factor(ovarian$ecog.ps)
  cph_model <- CoxModel$new(survival::Surv(futime, fustat) ~ age + resid.ds + rx + ecog.ps, ovarian)
  cph_model$fit()

  expect_output(summary(cph_model), "CoxModel")
})
