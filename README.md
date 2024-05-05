
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
#>            Estimate Standard Deviation T-statistic  p-value
#> intercept -0.057318           0.008869    -6.086303       0
#> X1         0.901492           0.010192    89.295341       0
#> X2         1.648958           0.008220   181.871847       0
```

Or alternatively

``` r
df <- data.frame(y,X1, X2)
colnames(df) <- c("Y", "A", "B")
ols2 <- olsFunc(Y~A+B, data=df)

summary(ols2)
#>            Estimate Standard Deviation T-statistic  p-value
#> intercept -0.057318           0.008869    -6.086303       0
#> A          0.901492           0.010192    89.295341       0
#> B          1.648958           0.008220   181.871847       0
```
