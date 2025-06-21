
# tidysummary

<!-- badges: start -->
<!-- badges: end -->

The goal of tidysummary is to streamlines the analysis of clinical data by automatically selecting appropriate statistical descriptions and inference methods based on variable types.

## Installation

You can install the development version of tidysummary like so:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("htqqdd/tidysummary")
```

## Example

A quick example which shows you how to start:

``` r
library(tidysummary)
result <- iris %>%
  add_var(var = c("Sepal.Length", "Sepal.Width"), group = "Species") %>%
  add_summary(add_overall = T) %>%
  add_p()
```

A real-world example with your dataset:

``` r
library(tidysummary)
result <- your_dataset %>%
  add_var(var = c("age", "sex", "BMI"), group = "group", norm = "auto") %>%
  add_summary(add_overall = TRUE,
              continuous_format = "{mean} ± {SD}",
              categorical_format = "{n} ({pct})",
              digit = 2) %>%
  add_p(digit = 3, asterisk = F)
```


