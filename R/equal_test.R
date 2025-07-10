#' Test for Equality of Variances
#'
#' Performs Levene's test to assess equality of variances between groups.
#'
#' @param data A data frame containing the variables to be tested.
#' @param var A character string specifying the numeric variable in `data` to test.
#' @param group A character string specifying the grouping variable in `data`.
#' @param center A character string specifying the `center` to use in Levene's test. Default is `'median'`, which is more robust than the mean.
#'
#' @return Logical value:
#'   - `TRUE`: Variances are equal, p-value more than 0.05
#'   - `FALSE`: Variances are unequal or an error occurred during testing
#'
#' @section Methodology for Equality of Variances:
#'   Levene's test is the default method adopted in SPSS, the original Levene's test select center = mean, but here select center = median for a more robust test
#'
#' @examples
#' equal_test(iris, "Sepal.Length", "Species")
#'
#' @export
equal_test <- function(data, var, group, center = "median"){

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
    cli_alert_danger(paste0(var, ": must be a character within 'data' colnames and different to 'group'"))
    stop()
  }
  if (!is.numeric(data[[var]])){
    cli_alert_danger(paste0(var,": must be numeric"))
    stop()
  }

  result <- tryCatch(
    #SPSS 默认采用leveneTest，但是center = mean，R推荐median
    leveneTest(data[[var]], data[[group]], center = center)$`Pr(>F)`[1],
    error = function(e) {
      cli_alert_warning(paste0(var, ": error in leveneTest, assuming unequal variances"))
      return(0)  #若报错默认有差异
    }
  )
  return(result >= 0.05)
}
