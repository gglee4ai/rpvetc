
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
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

**NOTE: This is a toy package created for expository purposes, for the
second edition of [R Packages](https://r-pkgs.org). It is not meant to
actually be useful. If you want a package for factor handling, please
see [forcats](https://forcats.tidyverse.org).**

# foofactors

<!-- badges: start -->
<!-- badges: end -->

Factors are a very useful type of variable in R, but they can also be
very aggravating. This package provides some helper functions for the
care and feeding of factors.

## Installation

You can install foofactors like so:

``` r
devtools::install_github("jennybc/foofactors")
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
