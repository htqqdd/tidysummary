#' Convert Specific Values in a Data Frame
#'
#' Replaces all occurrences of a specified value with another value within a data frame.
#'
#' @param data A data frame where value replacement will be performed.
#' @param from Original value to be replaced.
#' @param to New value that will replace the original value.
#'
#' @return A modified data frame with specified value replacements.
#'
#' @examples
#' df <- data.frame(a = c(1, 2, 3), b = c("", "y", "z"))
#' convert2(df, from = "", to = NA)
#'
#' @export
convert2 <- function(data, from = "", to = "") {
  #检查data参数
  if (!is.data.frame(data)) {
    cli_alert_danger("'data' must be a data frame")
    stop()
  }

  data[data == from] <- to
}

#' Convert binary variables to factors
#'
#' This function automatically detects and converts binary variables (with exactly two unique non-NA values) in a data frame to factors.
#' Currently only supports conversion from binary variables.
#'
#' @param data A data frame containing variables to be converted.
#' @param from Type of variables to convert. Currently only supports "binary" (default).
#'
#' @return The original data frame with binary variables converted to factors.
#'         The function preserves existing factor variables and skips non-binary columns.
#'
#' @examples
#' df <- data.frame(a = c(0,1,0), b = c("Y","N","Y"))
#' converted_df <- convert2factor(df)
#'
#' @export
convert2factor <- function(data, from = "binary") {
  #检查data参数
  if (!is.data.frame(data)) {
    cli_alert_danger("'data' must be a data frame")
    stop()
  }
  #检查from参数
  if (!is.character(from) || length(from) != 1 || from != "binary") {
    cli_alert_danger("'from' only support 'binary' currently")
    stop()
  }

  binary <- names(data)[sapply(data[names(data)], function(x) !is.factor(x) && length(unique(x, na.rm = T)) == 2)]

  if (length(binary) > 0) {
    for (b in binary) {
        data[[b]] <- factor(data[[b]])
        cli_alert_info(paste0(b, " has been converted to a factor: ",
                              paste(levels(data[[b]]), collapse = ", ")))
    }
  } else {
    cli_alert_info("No binary variables need to be converted to factor")
  }

  return(data)
}
