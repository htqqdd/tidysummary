#' Perform normality test on a variable
#'
#' Conducts normality tests for a specified variable, optionally by group. Supports automatic testing and interactive visualization.
#'
#' @param data A data frame containing the variables to be tested.
#' @param var A character string specifying the numeric variable in `data` to test.
#' @param group A character string specifying the grouping variable in `data`. If `NULL`, treated as one group.
#' @param norm Control parameter for test behavior. Accepts:
#'   - `'auto'`: Automatically decide based on p-values, but the same as `'ask'` when n > 1000, default
#'   - `'ask'`: Show p-values, plots QQ plots and prompts for decision
#'   - `TRUE`/`'true'`: Always returns `TRUE`
#'   - `FALSE`/`'false'`: Always returns `FALSE`
#'
#' @return A logical value:
#'   - `TRUE`: data are normally distributed
#'   - `FALSE`: data are not normally distributed
#'
#' @section Methodology for p-values:
#'   Automatically selects test based on sample size per group:
#'   - n < 3: Too small, assuming non-normal
#'   - 3 ≤ n ≤ 50: Shapiro-Wilk test
#'   - 50 < n<= 1000: D'Agostino Chi2 test, instead of Kolmogorov-Smirnov test
#'   - n > 1000: Show p-values, plots QQ plots and prompts for decision
#'   references: 1.https://www.graphpad.com/guides/prism/latest/statistics/stat_choosing_a_normality_test.htm
#'   2.D’Agostino, R.B. (1971) An Omnibus Test of Normality for Moderate and Large Size Samples. Biometrika, 58, 341-348.
#'
#' @examples
#' normal_test(iris, "Sepal.Length", "Species", norm = "auto")
#' normal_test(iris, "Sepal.Length", "Species", norm = TRUE)
#'
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
    cli_alert_danger(paste0(var, ": must be a character within 'data' colnames and different to 'group'"))
    stop()
  }
  if (!is.numeric(data[[var]])){
    cli_alert_danger(paste0(var,": must be numeric"))
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
        n_val = n()
        case_when(
          n_val < 3 ~ NA,
          n_val <= 50 ~ tryCatch(stats::shapiro.test(.data[[var]])$p.value, error = function(e) NA),
          n_val <= 1000 ~ tryCatch(fBasics::dagoTest(.data[[var]])@test$p.value[1], error = function(e) NA),
          TRUE ~ 2)},
      .groups = "drop") %>%
    deframe()

  if (any(is.na(result))){
    cli_alert_warning(paste0(var, ": error in normality test, assuming non-normal"))
    return(F)
  }

  if (is.character(norm) && tolower(norm) == "auto"){
    if (any(result == 2)) { #样本量>1000, 不推荐使用统计检验
      norm = "ask"
    } else {
      return(all(result >= 0.05))
    }
  }


  if (is.character(norm) && tolower(norm) == "ask"){
    temp_data <- data[, c(var, group_var)]
    colnames(temp_data) <- c("value", "group")
    temp_data$group <- as.factor(temp_data$group)
    tidyplot(temp_data, sample = value, color = group) |>
      tidyplots::add(stat_qq_band(alpha = 0.1)) |>
      tidyplots::add(stat_qq_line()) |>
      tidyplots::add(stat_qq_point()) |>
      tidyplots::adjust_size(width = NA, height = NA) |>
      print()
    cli_alert_info(ifelse(all(result >= 0.05),
                          paste0(var, ": automatic test normally distributed, please check QQ plot"),
                          paste0(var, ": automatic test not normally distributed, please check QQ plot")))
    cli_alert_info("Is it normal (Y/n): ")
    input <- readline()
    cli_alert_info(ifelse(input != "n",
                          paste(var, ": is regarded as normally distributed"),
                          paste(var, ":is regarded as not normally distributed")))
    return(input != "n")
  }
}
