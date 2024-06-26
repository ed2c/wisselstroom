
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wisselstroom <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
<!-- badges: end -->

Each Higher Educational Institute (HEI) in the Netherlands can request
its institute-specific files named “Bekostigingsbestanden”. The
functions in this package extract information from these files related
to switching studies. It facilitates the HEIs gain insight in the
prevalence of switching studies. Flows can be visualised by a Sankey
diagram.

## Installation

You can install the development version of wisselstroom from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ed2c/wisselstroom")
```

## Example

Read in your VLPBEK file with `read_vlpbek_data()`:

``` r
path_to_file <- here::here("extdata", "VLPBEK_2025_20240115_99XX.csv")
my_vlpbek_data <- read_vlpbek_data(path_to_file)
```

This results in one large data set. Hidden in that data set are
different sub data sets, which contain information about enrolment and
degrees. To ease the access to this information, make a `vlpbek` object
with the function `vlpbek()`:

``` r
my_vlpbek <- vlpbek(my_vlpbek_data)
```

Gain insights by using the `compact_vlpbek` function. The resulting
object contains summary information related to switching HEIs and/or
programs, which can be plotted using for instance `plot_brinflows()`:

``` r
my_vlpbek_compact <- compact_vlpbek(my_vlpbek)
plot_brinflows(my_vlpbekcompact, display_labels = TRUE)
```

## Credits

The code for the Sankey diagram is adapted from code found end of may
2024 at <https://github.com/ssp3nc3r/ggSankeyGrad>.
