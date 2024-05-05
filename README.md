
<!-- README.md is generated from README.Rmd. Please edit that file -->

# newOLS

<!-- badges: start -->

[![R-CMD-check](https://github.com/TiesBos/newOLS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/TiesBos/newOLS/actions/workflows/R-CMD-check.yaml)
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
library(newOLS)
n <- 100
X1 <- rnorm(n)
X2 <- rnorm(n)
y <- X1 + 1.5*X2+rnorm(n)
ols <- olsFunc(y~X1+X2)

summary(ols)
#>            Estimate Standard Deviation T-statistic  p-value
#> intercept -0.229078           0.010181    -22.70311       0
#> X1         0.925036           0.008939     97.83707       0
#> X2         1.542448           0.009317    159.80145       0
```

Or alternatively

``` r
df <- data.frame(y,X1, X2)
colnames(df) <- c("Y", "A", "B")
ols2 <- olsFunc(Y~A+B, data=df)

summary(ols2)
#>            Estimate Standard Deviation T-statistic  p-value
#> intercept -0.229078           0.010181    -22.70311       0
#> A          0.925036           0.008939     97.83707       0
#> B          1.542448           0.009317    159.80145       0
```
