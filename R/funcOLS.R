#' Function for OLS Estimators
#'
#' @param formula An object of class \link[stats:formula]{stats::formula}
#' @param data an optional data frame, list or environment containing the variables in the model.
#'
#' @return
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
#' x2 <- rnorm(n)
#' y <- 1.5*X1 + 1.5\*X2+rnorm(n)
#' df <- data.frame(y,X1, X2)
#' beta_OLS3 <- olsFunc(y~X1+X2, data=df)
olsFunc <- function(formula, data=NULL){

}
