#' Function for OLS Estimators
#'
#' @param formula An object of class \link[stats:formula]{stats::formula}
#' @param data an optional data frame, list or environment containing the variables in the model.
#' @param intercept logical expression whether an intercept should be included in the linear model.
#'
#' @description
#' A new function to perform OLS estimation
#'
#' @details
#' \deqn{(X^\prime X)^{-1} X^\prime y}
#'
#' @return An object of class "newOLS" including the OLS estimates, standard devations, T-statistics and p-values.
#' @export
#'
#' @examples
#' n <- 100
#' X <- rnorm(n)
#' y <- 1.5*X+rnorm(n)
#' beta_OLS <- olsFunc(y~X)
#'
#' n <- 100
#' X1 <- rnorm(n)
#' X2 <- rnorm(n)
#' y <- X1 + 1.5*X2+rnorm(n)
#' beta_OLS2 <- olsFunc(y~X1+X2)
#'
#' df <- data.frame(y,X1, X2)
#' colnames(df) <- c("Y", "A", "B")
#' beta_OLS3 <- olsFunc(Y~A+B, data=df)
#'
#' @author Ties
#'
olsFunc <- function(formula, data=NULL, intercept=TRUE){
  var_names <- all.vars(formula)

  if(is.null(data)){
    # If present, remove the intercept
    formula <- stats::update(formula, . ~ . - 1)
    vars_x <- stats::model.matrix(formula)
    vars_y <- eval(formula[[2]])
    vars <- cbind(y = vars_y, vars_x)
    var_names <- colnames(vars)
  } else {
    if(var_names[2]=="."){
      var_names <- c(var_names[1], colnames(data)[colnames(data) != (var_names[1])])
    }
    vars <- data[, var_names]
  }

  y <- as.matrix(vars[,1])
  if(intercept){
    X <- as.matrix(cbind(1, vars[,-1]))
    colnames(X) <- c("intercept", colnames(vars)[-1])
  } else {
    X <- as.matrix(vars[,-1])
    colnames(X) <- c(var_names[-1])
  }

  # The Estimates:
  beta <- solve(t(X)%*%X)%*%t(X)%*%y

  # The Standard Deviation:
  n <- nrow(X)
  p <- ncol(X)
  resid <- y-X%*%beta
  sigma.sq.hat <- as.numeric(t(resid)%*%resid/(n-p))
  var.mat <- solve(t(X)%*%X)*sigma.sq.hat
  coef.var <- diag(var.mat)

  # The T-Statistic:
  T.stats <- sqrt(n)*(beta)/sqrt(coef.var)

  # The p-values
  p.vals <- 2 * stats::pt(abs(T.stats), df=(n-p), lower.tail = FALSE)


  results <- structure(list(estimates = beta,
                       covariance.matrix = var.mat,
                       T.stat = T.stats,
                       p.values = p.vals,
                       var_names = colnames(X),
                       residuals = resid,
                       formula = formula),
                       class= "newOLS")

  return(results)
}

#' Deprecated function for OLS
#'
#' @param y a vector of a dependent variable
#' @param X a matrix of numerical explanatory variables
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @return A vector of OLS estimates
#'
#' @export
really_old_olsFunc <- function(y, X){
  lifecycle::deprecate_stop(when = "0.1.0", what = "old_olsFunc()", with = "olsFunc()", env = asNamespace("newOLS"))
  qr.solve(y, X)
}




#' Summary function for newOLS objects
#'
#' @param obj summary method for class "newOLS"
#'
#' @return a dataframe with all main results of the newOLS object
#' @export
#'
#' @method summary newOLS
#' @examples
#' X1 <- rnorm(100)
#' X2 <- rnorm(100)
#' y <- 1*X1+rnorm(100)
#' ols <- olsFunc(y~X1+X2)
#' summary(ols)
summary.newOLS <- function(obj){
  std.dev <- diag(obj$covariance.matrix)
  results.df <- data.frame(obj$estimates, std.dev, obj$T.stat, obj$p.values)
  rownames(results.df) <- obj$var_names
  colnames(results.df) <- c("Estimate", "Standard Deviation",
                            "T-statistic ", "p-value")

  return(round(results.df, 6))
}

