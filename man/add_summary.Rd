% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_summary.R
\name{add_summary}
\alias{add_summary}
\title{Add summary statistics to a add_var object}
\usage{
add_summary(
  data,
  add_overall = TRUE,
  continuous_format = NULL,
  norm_continuous_format = "{mean} ± {SD}",
  unnorm_continuous_format = "{median} ({Q1}, {Q3})",
  categorical_format = "{n} ({pct})",
  binary_show = "last",
  digit = 2
)
}
\arguments{
\item{data}{A data frame that has been processed by \code{add_var()}.}

\item{add_overall}{Logical indicating whether to include an "Overall" summary column. \code{TRUE}, by default.}

\item{continuous_format}{Format string to override both normal/abnormal continuous formats. Accepted placeholders are \code{{mean}}, \code{{SD}}, \code{{median}}, \code{{Q1}}, \code{{Q3}}.}

\item{norm_continuous_format}{Format string for normally distributed continuous variables. Default is \code{"{mean} ± {SD}"}. Accepted placeholders same as \code{continuous_format}.}

\item{unnorm_continuous_format}{Format string for non-normal continuous variables. Default is \code{"{median} ({Q1}, {Q3})"}. Accepted placeholders same as \code{continuous_format}.}

\item{categorical_format}{Format string for categorical variables. Default is \code{"{n} ({pct})"}. Accepted placeholders are \code{{n}} and \code{{pct}}.}

\item{binary_show}{Display option for binary variables:
\itemize{
\item \code{"first"}: show only first level
\item \code{"last"}: show only last level, default
\item \code{"all"}: show all levels
}}

\item{digit}{digit A numeric determine decimal.}
}
\value{
A data frame containing summary statistics with the following columns:
\itemize{
\item \code{variable}: Variable name
\item \code{Overall (n=X)}: Summary statistics for all data, if \code{add_overall=TRUE}
\item Group-specific columns named \verb{[group] (n=X)} with summary statistics
}
}
\description{
This function generates summary statistics for variables from a data frame that has been processed by \code{add_var()}, with options to format outputs.
}
\examples{
# `data` is a data frame processed by `add_var()`:
data <- add_var(iris, var = c("Sepal.Length", "Sepal.Width"), group = "Species")
# Add summary statistics
result <- add_summary(data, add_overall = TRUE)
result <- add_summary(data, continuous_format = "{mean}, ({SD})")

}
