#' Perform normality test on a variable
#'
#' Conducts normality tests for a specified variable, optionally by group. Supports automatic testing and interactive visualization.
#'
#' @param data A data frame containing the variables to be tested.
#' @param var A character string specifying the numeric variable in `data` to test.
#' @param group A character string specifying the grouping variable in `data`. If `NULL`, treated as one group.
#' @param norm Control parameter for test behavior. Accepts:
#'   - `'auto'`:Automatically decide based on p-values, default
#'   - `'ask'`:Plots QQ plots and prompts for decision, if any p-value < 0.05
#'   - `TRUE`/`'true'`: always returns `TRUE`
#'   - `FALSE`/`'false'`: always returns `FALSE`
#'
#' @return A logical value:
#'   - `TRUE`: data are normally distributed
#'   - `FALSE`: data are not normally distributed
#'
#' @section Testing Methodology:
#'   Automatically selects test based on sample size per group:
#'   - n < 3: Too small, assuming non-normal
#'   - 3 ≤ n ≤ 50: Shapiro-Wilk test
#'   - n > 50: D'Agostino skewness test
#'
#' @examples
#' \dontrun{
#' # Automatic mode
#' normal_test(iris, "Sepal.Length", "Species", norm = "auto")
#'
#' # Interactive mode
#' normal_test(mtcars, "mpg", "cyl", norm = "ask")
#' }
#' @export
normal_test <- function(data = NULL, var = NULL, group = NULL, norm = "auto"){
  . <- value <- NULL #避免R CMD check警告

  #检查data参数
  if (!is.data.frame(data)) {
    cli_alert_danger("'data' must be a data frame")
    stop()
  }

  #检查group参数
  if (is.null(group)) {
    group_var <- "..group.." #添加一个虚拟分组，用于处理Overall
  } else if (!is.character(group) || length(group) != 1 || !group %in% names(data) || group == "..group.."){
    cli_alert_danger("'group' must be a character within 'data' colnames")
    stop()
  } else {
    group_var <- group
  }

  #检查var参数
  if (!is.character(var) || length(var) != 1 || !var %in% names(data) || var == group_var) {
    cli_alert_danger("'var' must be a character within 'data' colnames and different to 'group'")
    stop()
  }
  if (!is.numeric(data[[var]])){
    cli_alert_danger("'var' data must be numeric.")
    stop()
  }

  #检查norm参数
  if (!(is.logical(norm) || (is.character(norm) && length(norm) == 1 && tolower(norm) %in% c("true", "false", "auto", "ask")))) {
    cli_alert_danger("'norm' must be a logical or 'true' or 'false' or 'auto' or 'ask'.")
    stop()
  }


  if ((is.logical(norm) && norm) || (is.character(norm) && tolower(norm) == "true")){
    return(T)
  }

  if ((is.logical(norm) && !norm) || (is.character(norm) && tolower(norm) == "false")){
    return(F)
  }

  result <- data %>%
    {if (is.null(group)) mutate(., ..group.. = "Overall") else .} %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      p.value = {
        n = n()
        if (n < 3) NA
        else if (n <= 50) tryCatch(stats::shapiro.test(.data[[var]])$p.value, error = function(e) NA)
        else if (n <= 46340) tryCatch(fBasics::dagoTest(.data[[var]])@test$p.value[1], error = function(e) NA)
        else tryCatch(nortest::ad.test(.data[[var]])$p.value, error = function(e) NA)
      },
      .groups = "drop"
    ) %>%
    deframe()

  if (any(is.na(result))){
    cli_alert_warning("Error in normality test, assuming non-normal")
    return(F)
  }

  if (is.character(norm) && tolower(norm) == "auto"){
    return(all(result >= 0.05))
  }

  if (is.character(norm) && tolower(norm) == "ask"){
    if (any(result < 0.05)){
      temp_data <- data[, c(var, group_var)]
      colnames(temp_data) <- c("value", "group")
      p <- ggplot(temp_data, aes(sample = value, color = group)) +
        geom_qq() +
        geom_qq_line() +
        facet_wrap(~group) +
        ggtitle(var) +
        theme_bw()
      print(p)
      cli_alert_info(paste("Is", var , "normal? (Y/n): "))
      input <- readline()
      return(input != "n")
    } else return(T)

  }
}
