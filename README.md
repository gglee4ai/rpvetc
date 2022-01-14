
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rpvetc: Reactor Pressure Vessel Embrittlement Trend Curve

<!-- badges: start -->
<!-- badges: end -->

The goal of rpvetc is to …

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

## Quick demo

Binding two factors via `fbind()`:

``` r
library(rpvetc)
E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)  # should be 31.743721
#> [1] 31.74372
```

Simply catenating two factors leads to a result that most don’t expect.

The `fbind()` function glues two factors together and returns factor.

Often we want a table of frequencies for the levels of a factor. The
base `table()` function returns an object of class `table`, which can be
inconvenient for downstream work.

The `fcount()` function returns a frequency table as a tibble with a
column of factor levels and another of frequencies:
