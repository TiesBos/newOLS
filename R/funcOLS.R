#' Function for OLS Estimators
#'
#' @param formula An object of class \link[stats:formula]{stats::formula}
#' @param data an optional data frame, list or environment containing the variables in the model.
#' @param intercept logical expression whether an intercept should be included in the linear model.
#'
#' @return A numeric vector including the coefficient
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
olsFunc <- function(formula, data=NULL, intercept=TRUE){
  var_names <- all.vars(formula)

  if(is.null(data)){
    vars <- do.call(cbind,lapply(var_names, get))
  } else{
    vars <- data[, var_names]
  }

  y <- vars[,1]
  if(intercept){
    X <- cbind(1, vars[,-1])
    colnames(X) <- c("intercept", var_names[-1])
  } else {
    X <- vars[,-1]
    colnames(X) <- c(var_names[-1])
  }

  beta <- solve(t(X)%*%X)%*%t(X)%*%y
  rownames(beta) <- colnames(X)
  return(beta)
}
