
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rpvetc: Reactor Pressure Vessel Embrittlement Trend Curve

<!-- badges: start -->
<!-- badges: end -->

The goal of **rpvetc** is to provide various models for Reactor Pressure
Vessel Embrittlement Trend Curve.

## Installation

You can install the released version of rpvetc from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rpvetc")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gglee4ai/rpvetc")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rpvetc)
E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)  # should be 31.743721
#> [1] 31.74372
```
