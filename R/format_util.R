#' Format numeric values to specified decimal places
#'
#' Converts numeric values to character strings with fixed decimal formatting using `sprintf()`.
#'
#' @param x A numeric vector to format.
#' @param digit Number of decimal places to display, 0 by default.
#'
#' @return A character vector of formatted values.
#'
#' @examples
#' my_round(3.1415, 2)
#' my_round(3, 2)
#'
#' @noRd
my_round <- function(x, digit = 0) {
  if (!is.numeric(digit) || length(digit) != 1 || digit < 0 || digit != floor(digit)) {
    cli_alert_danger("'digit' must be a numeric value greater than or equal to 0")
    stop()
  }
  sprintf(paste0("%.", digit, "f"), x)
}


#' Format p-values with significance markers
#'
#' Formats p-values as strings with specified precision and optional significance asterisks.
#'
#' @param p A numeric p-value between 0 and 1.
#' @param digit A numeric determine decimal. Accepts:
#'   - `3`:convert to 3 decimal, default
#'   - `4`:convert to 4 decimal
#' @param asterisk Logical indicating whether to return significance asterisks.
#'
#' @return Character of formatted p-value or asterisks.
#'
#' @examples
#' format_p(0.00009, 4)
#' format_p(0.03, 3)
#' format_p(0.02, asterisk = TRUE)
#'
#' @export
format_p <- function(p, digit = 3, asterisk = FALSE){

  if (is.na(p)){
    return("\u2014")
  }

  if (!is.numeric(p) || any(p < 0 | p > 1)) {
    cli_alert_danger("'p' must be a numeric vector with values between 0 and 1")
    stop()
  }

  if (!digit %in% c(3, 4)) {
    cli_alert_warning("'digit' must be either 3 or 4, using 3 instead")
    digit <- 3
  }

  case_when(
    digit == 3 ~ case_when(
      p < 0.001 ~ ifelse(asterisk, "***", "<0.001"),
      p < 0.01  ~ ifelse(asterisk, "**",  my_round(p, digit)),
      p < 0.05  ~ ifelse(asterisk, "*",   my_round(p, digit)),
      TRUE      ~ ifelse(asterisk, "",    my_round(p, digit))),
    digit == 4 ~ case_when(
      p < 0.0001 ~ ifelse(asterisk, "****", "<0.0001"),
      p < 0.001  ~ ifelse(asterisk, "***",  my_round(p, digit)),
      p < 0.01   ~ ifelse(asterisk, "**",   my_round(p, digit)),
      p < 0.05   ~ ifelse(asterisk, "*",    my_round(p, digit)),
      TRUE       ~ ifelse(asterisk, "",     my_round(p, digit))
    ))
}
