#' Check Sample Size Adequacy for Chi-Squared Test
#'
#' This function determines if a contingency table meets the expected frequency assumptions for a valid chi-squared test. It categorizes the data into "not_small", "small", or "very_small" based on sample size and expected frequencies.
#'
#' @param data A data frame containing the variables to be tested.
#' @param var A character string specifying the factor variable in `data` to test.
#' @param group A character string specifying the grouping variable in `data`.
#'
#' @return A character string with one of three values:
#'   - `"not_small"`: Sample size more than or euqal to 40 and all expected frequencies more than or euqal to 5
#'   - `"small"`: Sample size more than or euqal to 40, all expected frequencies more than or euqal to 1 and at least one <5, only for 2*2 contingency tables
#'   - `"very_small"`: Other conditions, including sample size <40 or any expected frequency <1
#'
#' @examples
#' df <- data.frame(
#'   category = factor(c("A", "B", "A", "B")),
#'   group    = factor(c("X", "X", "Y", "Y"))
#' )
#' small_test(data = df, var = "category", group = "group")
#'
#' @export
small_test <- function(data, var, group){

  #检查data参数
  if (!is.data.frame(data)) {
    cli_alert_danger("'data' must be a data frame")
    stop()
  }

  #检查group参数
  if (!is.character(group) || length(group) != 1 || !group %in% names(data)){
    cli_alert_danger("'group' must be a character within 'data' colnames")
    stop()
  }
  if (!is.factor(data[[group]])){
    data[[group]] <- factor(data[[group]])
    cli_alert_info(paste0(group, " has been converted to a factor: ",
                               paste(levels(data[[group]]), collapse = ", ")))
  }

  #检查var参数
  if (!is.character(var) || length(var) != 1 || !var %in% names(data) || var == group) {
    cli_alert_danger(paste0(var, ": must be a character within 'data' colnames and different to 'group'"))
    stop()
  }
  if (!is.factor(data[[var]])){
    data[[var]] <- factor(data[[var]])
    cli_alert_info(paste0(var, " has been converted to a factor: ",
                               paste(levels(data[[var]]), collapse = ", ")))
  }

  # 创建列联表
  tbl <- table(data[[var]], data[[group]])
  # 检查期望频数
  expected <- tryCatch({
    suppressWarnings(chisq.test(tbl)$expected)
  }, error = function(e) {
    cli_alert_warning("Error in checking expected frequencies, assuming small sample sizes")
    return(matrix(0, nrow=nrow(tbl), ncol=ncol(tbl)))  # 错误时返回零矩阵
  })

  if (sum(tbl) >= 40 && all(expected >= 5)) {
    return("not_small")
  } else if (sum(tbl) >= 40 && all(expected >= 1) && any(expected < 5) && nrow(tbl) == 2 && ncol(tbl) == 2) {
    return("small")
  } else {
    return("very_small")
  }
}
