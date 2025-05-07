
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggEDA <a href="https://CCICB.github.io/ggEDA/"><img src="man/figures/logo.png" alt="ggEDA website" align="right" height="138"/></a>

<!-- badges: start -->

[![CRAN
version](https://img.shields.io/cran/v/ggEDA.svg)](https://CRAN.R-project.org/package=ggEDA)
[![R-CMD-check](https://github.com/CCICB/ggEDA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CCICB/ggEDA/actions/workflows/R-CMD-check.yaml)
[![lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov branch
coverage](https://codecov.io/gh/CCICB/ggEDA/branch/main/graph/badge.svg)](https://app.codecov.io/gh/CCICB/ggEDA?branch=main)
[![Issues](https://img.shields.io/github/issues/CCICB/ggEDA)](https://github.com/CCICB/ggEDA/issues)
[![Code
size](https://img.shields.io/github/languages/code-size/CCICB/ggEDA)](https://github.com/CCICB/ggEDA)
[![Last
commit](https://img.shields.io/github/last-commit/CCICB/ggEDA)](https://github.com/CCICB/ggEDA/commits/main)
[![r-universe](https://ccicb.r-universe.dev/badges/ggEDA)](https://ccicb.r-universe.dev)
<!-- badges: end -->

**ggEDA** streamlines exploratory data analysis by providing turnkey
approaches to visualising n-dimensional data which can graphically
reveal correlative or associative relationships between two or more
features:

- **ggstack**: tiled one-dimensional visualisations that more
  effectively show missingness and complex categorical relationships in
  smaller datasets.
- **ggparallel**: parallel coordinate plots (PCPs) for examining large
  datasets with mostly quantitative features.

## Installation

``` r
install.packages("ggEDA")
```

### Development Version

You can install the development version of ggEDA from
[GitHub](https://github.com/CCICB/ggEDA) with:

``` r
if (!require("remotes"))
    install.packages("remotes")

remotes::install_github("CCICB/ggEDA")
```

Or from R-universe with:

``` r
install.packages("ggEDA", repos = "https://ropensci.r-universe.dev")
```

## Quick Start

For examples of interactive EDA plots see the [ggEDA
gallery](https://CCICB.github.io/ggEDA/articles/gallery.html)

``` r
# Load library
library(ggEDA)

# Plot data, sort by Glasses
ggstack(
  baseballfans,
  col_id = "ID",
  col_sort = "Glasses",
  interactive = FALSE,
  verbose = FALSE,
  options = ggstack_options(legend_nrow = 2)
)
```

<img src="man/figures/README-example-1.png" width="100%" />

## Customise Colours

Customise colours by supplying a named list to the `palettes` argument

``` r
ggstack(
  baseballfans,
  col_id = "ID",
  col_sort = "Glasses",
  palettes = list("EyeColour" = c(
    Brown = "rosybrown4",
    Blue = "steelblue",
    Green = "seagreen"
  )),
  interactive = FALSE,
  verbose = FALSE,
  options = ggstack_options(legend_nrow = 2)
)
```

<img src="man/figures/README-customise_colours-1.png" width="100%" />

## Parallel Coordinate Plots

For datasets with many observations and mostly numeric features,
parallel coordinate plots may be more appropriate.

``` r
ggparallel(
 data = minibeans,
 col_colour = "Class",
 order_columns_by = "auto",
 interactive = FALSE
)
#> ℹ Ordering columns based on mutual information with [Class]
```

<img src="man/figures/README-minibeans_class-1.png" width="100%" />

``` r
 ggparallel(
   data = minibeans,
   col_colour = "Class",
   highlight = "DERMASON",
   order_columns_by = "auto",
   interactive = FALSE
 )
#> ℹ Ordering columns based on how well they differentiate 1 group from the rest [DERMASON] (based on mutual information)
```

<img src="man/figures/README-minibeans_highlight-1.png" width="100%" />

``` r
 ggparallel(
   data = minibeans,
   order_columns_by = "auto",
   interactive = FALSE
 )
#> ℹ To add colour to plot set `col_colour` to one of: Class
#> ℹ Ordering columns to minimise crossings
#> ℹ Choosing axis order via repetitive nearest neighbour with two-opt refinement
```

<img src="man/figures/README-minibeans_noclass-1.png" width="100%" />

## Community Contributions

All types of contributions are encouraged and valued. See our [guide to
community
contributions](https://CCICB.github.io/ggEDA/CONTRIBUTING.html) for
different ways to help.
