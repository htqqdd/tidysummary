#' Test for Equality of Variances
#'
#' Performs Levene's test to assess equality of variances between groups.
#'
#' @param data A data frame containing the variables to be tested.
#' @param var A character string specifying the numeric variable in `data` to test.
#' @param group A character string specifying the grouping variable in `data`.
#'
#' @return Logical value:
#'   - `TRUE`: Variances are equal, p-value ≥ 0.05
#'   - `FALSE`: Variances are unequal or an error occurred during testing
#'
#' @examples
#' \dontrun{
#'   equal_test(mtcars, "mpg", "cyl")
#'   }
#' @export
equal_test <- function(data, var, group){

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

  #检查var参数
  if (!is.character(var) || length(var) != 1 || !var %in% names(data) || var == group) {
    cli_alert_danger("'var' must be a character within 'data' colnames and different to 'group'")
    stop()
  }
  if (!is.numeric(data[[var]])){
    cli_alert_danger("'var' data must be numeric.")
    stop()
  }

  result <- tryCatch(
    leveneTest(data[[var]], data[[group]], center = median)$`Pr(>F)`[1],
    error = function(e) {
      cli_alert_warning("Error in leveneTest, assuming unequal variances")
      return(0)  #若报错默认有差异
    }
  )
  return(result >= 0.05)
}
