#' Add statistical test results to summary data
#'
#' Calculates and appends p-values with optional statistical details to a summary table based on variable types and group comparisons. Handles both continuous and categorical variables with appropriate statistical tests.
#'
#' @param summary A data frame that has been processed by `add_summary()`.
#' @param digit A numeric determine decimal. Accepts:
#'   - `3`:convert to 3 decimal, default
#'   - `4`:convert to 4 decimal
#' @param asterisk Logical indicating whether to show asterisk significance markers.
#' @param add_method Control parameter for display of statistical methods. Accepts:
#'   - `'code'`: Show method as codes according to order of appearance
#'   - `TRUE`/`'true'`: Show method text
#'   - `FALSE`/`'false'`: Not show method text
#' @param add_statistic_name Logical indicating whether to include test statistic names.
#' @param add_statistic_value Logical indicating whether to include test statistic values.
#'
#' @return A data frame merged with statistical test results, containing:
#'         - Variable names
#'         - Summary
#'         - Formatted p-values
#'         - Optional method names/codes
#'         - Optional statistic names/values
#'
#' @examples
#' # `summary` is a data frame processed by `add_var()` and `add_summary()`:
#' data <- add_var(iris, var = c("Sepal.Length", "Species"), group = "Species")
#' summary <- add_summary(data)
#'
#' # Add statistical test results
#' result <- add_p(summary)
#'
#' @export
add_p <- function(summary, digit = 3, asterisk = FALSE,
                  add_method = FALSE, add_statistic_name = FALSE, add_statistic_value = FALSE){
  #检查add_summary属性
  if (!"add_summary" %in% names(attributes(summary))) {
    cli_alert_danger("must use add_summary() for 'summary' before using add_p()")
    stop()
  }

  #检查digit参数
  if (!digit %in% c(3, 4)) {
    cli_alert_warning("'digit' for p must be either 3 or 4, using 3 instead")
    digit <- 3
  }

  #检查asterisk参数
  if (!is.logical(asterisk) || length(asterisk) != 1) {
    cli_alert_danger("'asterisk' must be a logical value")
    stop()
  }

  #检查add_method参数
  if (!(is.logical(add_method) || (is.character(add_method) && length(add_method) == 1 && tolower(add_method) %in% c("true", "false", "code")))) {
    cli_alert_danger("'add_method' must be a logical or 'true' or 'false' or 'code'")
    stop()
  }

  #检查add_statistic_name参数
  if (!is.logical(add_statistic_name) || length(add_statistic_name) != 1) {
    cli_alert_danger("'add_statistic_name' must be a logical value")
    stop()
  }

  #检查add_statistic_value参数
  if (!is.logical(add_statistic_value) || length(add_statistic_value) != 1) {
    cli_alert_danger("'add_statistic_value' must be a logical value")
    stop()
  }

  #获取add_var属性,注意此时输入的dataframe为summary
  datalist = attr(summary, "add_var", exact = TRUE)
  data = attr(summary, "add_summary", exact = TRUE)[["input_data"]]

  group = datalist$group
  valid_var = datalist$var$valid
  group_nlevels = datalist$group_nlevels

  norm_equal_var = datalist$var$continuous$norm_equal
  norm_unequal_var = datalist$var$continuous$norm_unequal
  unnorm_var = datalist$var$continuous$unnorm

  rank_var = datalist$var$categorical$rank
  not_small_var = datalist$var$categorical$not_small
  small_var = datalist$var$categorical$small
  very_small_var = datalist$var$categorical$very_small

  cli_progress_step("Geting p-values", spinner = TRUE)
  method = list()
  p = list()
  statistic_name = list()
  statistic_value = list()
  for (v in valid_var){
    cli_progress_update()
    if (group_nlevels > 2) {
      # 多组比较
      if (v %in% norm_equal_var) {
        method[[v]] <- "One-way ANOVA"
        p_result <- oneway.test(data[[v]] ~ data[[group]], var.equal = TRUE)
        p[[v]] <- format_p(p_result$p.value, digit, asterisk)
        statistic_name[[v]] <- names(p_result$statistic)
        statistic_value[[v]] <- my_round(p_result$statistic, digit)
      }
      if (v %in% norm_unequal_var) {
        method[[v]] <- "Welch's ANOVA"
        p_result <- oneway.test(data[[v]] ~ data[[group]], var.equal = FALSE)
        p[[v]] <- format_p(p_result$p.value, digit, asterisk)
        statistic_name[[v]] <- names(p_result$statistic)
        statistic_value[[v]] <- my_round(p_result$statistic, digit)
      }
      if (v %in% unnorm_var) {
        method[[v]] = "Kruskal-Wallis H test"
        p_result <- kruskal.test(data[[v]] ~ data[[group]])
        p[[v]] <- format_p(p_result$p.value, digit, asterisk)
        statistic_name[[v]] <- "H"
        statistic_value[[v]] <- my_round(p_result$statistic, digit)
      }
      if (v %in% rank_var) {
        method[[v]] = "Kruskal-Wallis H test"
        p_result <- kruskal.test(data[[v]] ~ data[[group]])
        p[[v]] <- format_p(p_result$p.value, digit, asterisk)
        statistic_name[[v]] <- "H"
        statistic_value[[v]] <- my_round(p_result$statistic, digit)
      }
      if (v %in% not_small_var) {
        method[[v]] = "Pearson's \u03c7\u00b2 test"
        p_result <- suppressWarnings(chisq.test(data[[v]], data[[group]], correct = FALSE))
        p[[v]] <- format_p(p_result$p.value, digit, asterisk)
        statistic_name[[v]] <- "\u03c7\u00b2"
        statistic_value[[v]] <- my_round(p_result$statistic, digit)
      }
      if (v %in% small_var) {
        method[[v]] = "Yates' \u03c7\u00b2 test"
        p_result <- suppressWarnings(chisq.test(data[[v]], data[[group]], correct = TRUE))
        p[[v]] <- format_p(p_result$p.value, digit, asterisk)
        statistic_name[[v]] <- "\u03c7\u00b2"
        statistic_value[[v]] <- my_round(p_result$statistic, digit)
      }
      if (v %in% very_small_var) {
        method[[v]] = "Fisher's exact test"
        p_result <- suppressWarnings(fisher.test(data[[v]], data[[group]]))
        p[[v]] <- format_p(p_result$p.value, digit, asterisk)
        statistic_name[[v]] <- "\u2014"
        statistic_value[[v]] <- "\u2014"
      }
    } else {
      # 两组比较
      if (v %in% norm_equal_var) {
        method[[v]] <- "Student's t-test"
        p_result <- t.test(data[[v]] ~ data[[group]], var.equal = TRUE)
        p[[v]] <- format_p(p_result$p.value, digit, asterisk)
        statistic_name[[v]] <- names(p_result$statistic)
        statistic_value[[v]] <- my_round(p_result$statistic, digit)
      }
      if (v %in% norm_unequal_var) {
        method[[v]] <- "Welch's t-test"
        p_result <- t.test(data[[v]] ~ data[[group]], var.equal = FALSE)
        p[[v]] <- format_p(p_result$p.value, digit, asterisk)
        statistic_name[[v]] <- names(p_result$statistic)
        statistic_value[[v]] <- my_round(p_result$statistic, digit)
      }
      if (v %in% unnorm_var) {
        method[[v]] = "Mann-Whitney U test"
        p_result <- suppressWarnings(wilcox.test(data[[v]] ~ data[[group]]))
        p[[v]] <- format_p(p_result$p.value, digit, asterisk)
        statistic_name[[v]] <- names(p_result$statistic)
        statistic_value[[v]] <- my_round(p_result$statistic, digit)
      }
      if (v %in% rank_var) {
        method[[v]] = "Mann-Whitney U test"
        p_result <- suppressWarnings(wilcox.test(data[[v]] ~ data[[group]]))
        p[[v]] <- format_p(p_result$p.value, digit, asterisk)
        statistic_name[[v]] <- names(p_result$statistic)
        statistic_value[[v]] <- my_round(p_result$statistic, digit)
      }
      if (v %in% not_small_var) {
        method[[v]] = "Pearson's \u03c7\u00b2 test"
        p_result <- suppressWarnings(chisq.test(data[[v]], data[[group]], correct = FALSE))
        p[[v]] <- format_p(p_result$p.value, digit, asterisk)
        statistic_name[[v]] <- "\u03c7\u00b2"
        statistic_value[[v]] <- my_round(p_result$statistic, digit)
      }
      if (v %in% small_var) {
        method[[v]] = "Yates' \u03c7\u00b2 test"
        p_result <- suppressWarnings(chisq.test(data[[v]], data[[group]], correct = TRUE))
        p[[v]] <- format_p(p_result$p.value, digit, asterisk)
        statistic_name[[v]] <- "\u03c7\u00b2"
        statistic_value[[v]] <- my_round(p_result$statistic, digit)
      }
      if (v %in% very_small_var) {
        method[[v]] = "Fisher's exact test"
        p_result <- suppressWarnings(fisher.test(data[[v]], data[[group]]))
        p[[v]] <- format_p(p_result$p.value, digit, asterisk)
        statistic_name[[v]] <- "\u2014"
        statistic_value[[v]] <- "\u2014"
      }
    }
  }

  # 创建结果数据框
  p_result <- data.frame(
    variable = valid_var,
    p = unlist(p))
  if ((is.logical(add_method) && add_method) || (is.character(add_method) && tolower(add_method) == "true")) {
    p_result$method <- unlist(method)
  }
  if (is.character(add_method) && tolower(add_method) == "code"){
    p_result$method <- match(unlist(method), unique(unlist(method)))
  }
  if (add_statistic_name) {
    p_result$statistic_name <- unlist(statistic_name)
  }
  if (add_statistic_value) {
    p_result$statistic_value <- unlist(statistic_value)
  }

  summary_with_p <- left_join(summary, p_result, by = "variable")
  rownames(summary_with_p) <- rownames(summary)
  if (!is.logical(add_method) && add_method == "code") {
    unique_methods <- unique(unlist(method))
    method_labels <- setNames(unique_methods, seq_along(unique_methods))
    summary_with_p[1, "method_code"] <- paste(names(method_labels),
                                              method_labels,
                                              sep = " = ",
                                              collapse = "; ")
  }

  return(summary_with_p)

}
