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

### A quick example

``` r
library(tidysummary)
result <- iris %>%
  add_var() %>%
  add_summary() %>%
  add_p()
```

### Another example

``` r
#Here is an prepared dataset
iris <- iris %>%
  mutate(group = factor(rep(1:3, each = 50),
                        labels = c("group1", "group2", "group3")))

#Now use tidysummary
library(tidysummary)
result <- iris %>%
  add_var() %>%
  add_summary(binary_show = "all") %>%
  add_p()
```

#### Following options

-   Use as DataFrame

``` r
View(result)
```

-   Display as HTML (use `kableExtra` or others your prefer)

``` r
library(kableExtra)
result[is.na(result)] <- ""
result %>%
  kbl(caption = "Table 1. Summary of Iris Dataset",
      row.names = F,
      align = "c") %>%
  kable_classic(
    full_width = FALSE,
    html_font = "Cambria")
```

![](man/figures/example.png)

-   Save as Excel (.xlsx)

``` r
result %>%
  writexl::write_xlsx("./test.xlsx")
```
