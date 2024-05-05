ols_test_data <- function(n, p = 1, sd.error = 1){
  eps <- rnorm(n, sd = sd.error)
  X <- matrix(rnorm(n * p), ncol = 2)
  colnames(X) <- paste0("X", 1:p)
  beta <- rnorm(p, sd = 5)
  y <- as.matrix(X %*% beta + eps)
  data <- as.data.frame(cbind(y = y, X))
  colnames(data)[1] <- "y"
  return(data)
}

test_that("Results", {
  data <- readRDS(test_path("fixtures", "data_test.rds"))
  estim <- olsFunc(y ~ X, data = data, intercept = F)

  expect_equal(c(estim$estimates), 1.54976, tolerance = .01)
})


test_that("Output dim", {
  data <- readRDS(test_path("fixtures", "data_test.rds"))
  estim <- olsFunc(y ~ X, data = data, intercept = F)

  expect_length(c(estim$residuals), 100)
  expect_s3_class(estim, "newOLS")
})
