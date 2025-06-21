# tidysummary

<!-- badges: start -->

<!-- badges: end -->

The goal of tidysummary is to streamlines the analysis of clinical data by automatically selecting appropriate statistical descriptions and inference methods based on variable types. See [the vignette](https://htqqdd.github.io/tidysummary/) for more details.

## Installation

You can install the development version of tidysummary like so:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("htqqdd/tidysummary")
```

## Usage

A quick example which shows you how to start:

``` r
library(tidysummary)
result <- iris %>%
  add_var(var = c("Sepal.Length", "Sepal.Width"), group = "Species") %>%
  add_summary(add_overall = T) %>%
  add_p()
```
