
<!-- README.md is generated from README.Rmd. Please edit that file -->

# newOLS

<!-- badges: start -->
<!-- badges: end -->

The goal of newOLS is to estimate the OLS estimator based on a given
dependent variable and independent variables.

## Installation

You can install the development version of newOLS from
[GitHub](https://github.com/TiesBos/newOLS) with:

``` r
# install.packages("devtools")
devtools::install_github("TiesBos/newOLS")
```

## Example

``` r
n <- 100
X1 <- rnorm(n)
X2 <- rnorm(n)
y <- X1 + 1.5*X2+rnorm(n)
ols <- olsFunc(y~X1+X2)

summary(ols)
#>           Estimate Standard Deviation T-statistic  p-value
#> intercept 0.055076           0.009963     5.517721       0
#> X1        1.008352           0.010520    98.310518       0
#> X2        1.542702           0.013333   133.605740       0
```
