
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wisselstroom

<!-- badges: start -->
<!-- badges: end -->

Each Higher Educational Institute (HEI) in the Netherlands can request
its institute-specific files named “Bekostigingsbestanden”. The
functions in this package extract information from these files about
Switching Studies. It facilitates the HEIs gain insight in the
prevalence of Switching Studies.

## Installation

You can install the development version of wisselstroom from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ed2c/wisselstroom")
```

## Example

Read in your VLPBEK file with `read_vlpbek()`:

``` r
path_to_file <- here::here("extdata", "VLPBEK_2025_20240115_99XX.csv")
read_vlpbek(path_to_file)
```
