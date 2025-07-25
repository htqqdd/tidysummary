% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_p.R
\name{add_p}
\alias{add_p}
\title{Add statistical test results to summary data}
\usage{
add_p(
  summary,
  digit = 3,
  asterisk = FALSE,
  add_method = FALSE,
  add_statistic_name = FALSE,
  add_statistic_value = FALSE
)
}
\arguments{
\item{summary}{A data frame that has been processed by \code{add_summary()}.}

\item{digit}{A numeric determine decimal. Accepts:
\itemize{
\item \code{3}:convert to 3 decimal, default
\item \code{4}:convert to 4 decimal
}}

\item{asterisk}{Logical indicating whether to show asterisk significance markers.}

\item{add_method}{Control parameter for display of statistical methods. Accepts:
\itemize{
\item \code{'code'}: Show method as codes according to order of appearance
\item \code{TRUE}/\code{'true'}: Show method text
\item \code{FALSE}/\code{'false'}: Not show method text
}}

\item{add_statistic_name}{Logical indicating whether to include test statistic names.}

\item{add_statistic_value}{Logical indicating whether to include test statistic values.}
}
\value{
A data frame merged with statistical test results, containing:
- Variable names
- Summary
- Formatted p-values
- Optional method names/codes
- Optional statistic names/values
}
\description{
Calculates and appends p-values with optional statistical details to a summary table based on variable types and group comparisons. Handles both continuous and categorical variables with appropriate statistical tests.
}
\examples{
# `summary` is a data frame processed by `add_var()` and `add_summary()`:
data <- add_var(iris, var = c("Sepal.Length", "Sepal.Width"), group = "Species")
summary <- add_summary(data)

# Add statistical test results
result <- add_p(summary)

}
