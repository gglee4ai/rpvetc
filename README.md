
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rpvetc: Reactor Pressure Vessel Embrittlement Trend Curve

<!-- badges: start -->
<!-- badges: end -->

## Overview

The **rpvetc** package provides models for predicting Reactor Pressure
Vessel (RPV) embrittlement trend curves based on multiple regulatory and
research models. This package is designed for engineers and researchers
working in the field of nuclear materials, allowing them to assess the
effects of neutron irradiation on RPV materials.

The package includes various embrittlement models such as: -
**Regulatory Guide 1.99 Rev. 2 (RG199R2)** - **NUREG/CR-3391
(CR3391)** - **ASTM E900-15e2 (E900_15)** - **ASTM E900-15e2 with
MRP-462 Flux Effects (E900_flux)** - **Odette’s Model in EPRI NP-3319
(NP3319)**

## Installation

To install the development version from
[GitHub](https://github.com/gglee4ai/rpvetc), run:

``` r
# install.packages("devtools")
devtools::install_github("gglee4ai/rpvetc")
```

## Usage

Once installed, you can use `rpvetc` to estimate embrittlement
properties such as **TTS (Temperature Transition Shift)**, **CF
(Chemistry Factor)**, **FF (Fluence Factor)**, and **Standard Deviation
(SD)**.

### Example: Computing TTS using ASTM E900-15e2

This example demonstrates how to use the `E900_15` function to estimate
the **TTS (TTS1 + TTS2)** for a plate material:

``` r
library(rpvetc)

# Compute TTS for a plate material under neutron irradiation
E900_15(product_form = "P", Cu = 0.2, Ni = 0.18, Mn = 1.36, 
        P = 0.012, temperature = 290, fluence = 2.56894e18) 
#> [1] 31.74387
# Expected output: 31.74387
```

### Example: Considering Flux Effects in TTS Calculation

The following example uses `E900_flux` to compute TTS while
incorporating flux effects:

``` r
E900_flux(product_form = "P", Cu = 0.2, Ni = 0.18, Mn = 1.36, 
          P = 0.012, temperature = 290, fluence = 2.56894e18, flux = 1e13)
#> [1] 31.81079
# Expected output: 31.81079
```

### Example: Using Regulatory Guide 1.99 Rev. 2

To compute **Chemistry Factor (CF)** and **Fluence Factor (FF)** based
on **RG199R2**, use:

``` r
# Compute CF from tabulated values
RG199R2(product_form = "B", Cu = 0.2, Ni = 0.2, fluence = 1e19, output = "CF")
#> [1] 56.66667

# Compute FF based on neutron fluence
RG199R2(fluence = 1e19, output = "FF")
#> [1] 1
```

## Model References

- **Regulatory Guide 1.99 Rev. 2 (1988)**
- **NUREG/CR-3391 (1983)**
- **ASTM E900-15e2 (2015)**
- **MRP-462 (2021)**
- **EPRI NP-3319 (1984)**

For more details on each model, refer to the corresponding
documentation.

## License

This package is licensed under the MIT License.

------------------------------------------------------------------------
