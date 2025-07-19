#' Generate summary statistics for continuous or categorical variables
#'
#' This function computes formatted summary statistics for specified variables. Handles both continuous and categorical variables with custom formatting.
#'
#' @param data A data frame containing the variables to summarize.
#' @param v A character of variable names to summarize.
#' @param group A character of grouping variable name. If `NULL`, creates "Overall" summary.
#' @param summary_format Glue-style format string for results:
#'   * For continuous: Uses `median`, `Q1`, `Q3`, `mean`, `SD` stats
#'   * For categorical: Uses `n` and `pct`
#' @param type Analysis type: `"continuous"` or `"categorical"`.
#' @param binary_show Control for binary categorical variables:
#'   * `"first"`: Show first level only
#'   * `"last"`: Show last level only
#'   * `"all"`: Show all levels, with a blank row header,
#'   * `NULL`: Treated as `"all"`, as this is a non-binary variable.
#' @param digit Rounding digits for statistics using `my_round`.
#'
#' @return A data frame with:
#'   * `variable` column containing original variable names
#'   * Group-specific summary columns with formatted statistics
#' @noRd
create_summary <- function(data, v = NULL, group = NULL, summary_format = NULL, type = c("continuous", "categorical"), binary_show = NULL, digit = NULL){
  . <- pct <- value <- variable <- NULL #避免R CMD check警告

  #连续性变量
  if (type == "continuous") {
    group_var <- if (!is.null(group)) group else "..group.." #添加一个虚拟分组，用于处理Overall
    summary <- data %>%
      {if (is.null(group)) mutate(., ..group.. = "Overall") else .} %>% #添加一个虚拟分组，用于处理Overall
      group_by(!!sym(group_var)) %>%
      summarise(across(
        all_of(v),
        function (x) {
          stats <- list(
            median = my_round(median(x, na.rm = TRUE), digit),
            Q1     = my_round(quantile(x, 0.25, na.rm = TRUE), digit),
            Q3     = my_round(quantile(x, 0.75, na.rm = TRUE), digit),
            mean   = my_round(mean(x, na.rm = TRUE), digit),
            SD     = my_round(sd(x, na.rm = TRUE), digit))
          glue_data(stats, summary_format)})) %>%
      column_to_rownames(group_var) %>%
      t() %>%
      as.data.frame() %>%
      mutate(variable = v) %>%
      relocate(variable, .before = everything())
    return(summary)
  }

  #分类变量
  if (type == "categorical"){
    group_var <- if (!is.null(group)) group else "..group.." #添加一个虚拟分组，用于处理Overall
    summary <- data %>%
      {if (is.null(group)) mutate(., ..group.. = "Overall") else .} %>% #添加一个虚拟分组，用于处理Overall
      count(!!sym(group_var), !!sym(v), name = "n", .drop = FALSE) %>%
      group_by(!!sym(group_var)) %>%
      mutate(pct = my_round(n / sum(n) * 100, digit = 2),
             value = glue(summary_format)) %>%
      ungroup() %>%
      na.omit() %>% #去除NA level
      select(-n, -pct) %>%
      pivot_wider(names_from = !!sym(v), values_from = value, names_glue = paste0(v, "({ ", v, "})")) %>%
      column_to_rownames(group_var) %>%
      t() %>%
      as.data.frame() %>%
      mutate(variable = v) %>%
      relocate(variable, .before = everything())

    #二分类变量简化
    if (!is.null(binary_show)){
      summary <- switch(
        binary_show,
        "first" = slice_head(summary, n = 1),
        "last" = slice_tail(summary, n = 1),
        "all" = summary)
    }

    #多分类或二分类完整，添加空白行
    if (is.null(binary_show) || binary_show == "all"){ #若为多分类，添加一个空白行
      summary[["variable"]] <- paste0("  ",levels(data[[v]]))
      blank_row <- data.frame(row.names = v, variable = v)
      blank_row[colnames(summary)[-1]] <- NA
      summary <- rbind(blank_row, summary)
    }

    return(summary)
  }

}


#' Add summary statistics to a add_var object
#'
#' This function generates summary statistics for variables from a data frame that has been processed by `add_var()`, with options to format outputs.
#'
#' @param data A data frame that has been processed by `add_var()`.
#' @param add_overall Logical indicating whether to include an "Overall" summary column. `TRUE`, by default.
#' @param continuous_format Format string to override both normal/abnormal continuous formats. Accepted placeholders are `{mean}`, `{SD}`, `{median}`, `{Q1}`, `{Q3}`.
#' @param norm_continuous_format Format string for normally distributed continuous variables. Default is `"{mean} ± {SD}"`. Accepted placeholders same as `continuous_format`.
#' @param unnorm_continuous_format Format string for non-normal continuous variables. Default is `"{median} ({Q1}, {Q3})"`. Accepted placeholders same as `continuous_format`.
#' @param categorical_format Format string for categorical variables. Default is `"{n} ({pct})"`. Accepted placeholders are `{n}` and `{pct}`.
#' @param binary_show Display option for binary variables:
#'   - `"first"`: show only first level
#'   - `"last"`: show only last level, default
#'   - `"all"`: show all levels
#' @param digit digit A numeric determine decimal.
#' @return A data frame containing summary statistics with the following columns:
#'   \itemize{
#'     \item `variable`: Variable name
#'     \item `Overall (n=X)`: Summary statistics for all data, if `add_overall=TRUE`
#'     \item Group-specific columns named `[group] (n=X)` with summary statistics
#'   }
#'
#' @examples
#' # `data` is a data frame processed by `add_var()`:
#' data <- add_var(iris, var = c("Sepal.Length", "Sepal.Width"), group = "Species")
#' # Add summary statistics
#' result <- add_summary(data, add_overall = TRUE)
#' result <- add_summary(data, continuous_format = "{mean}, ({SD})")
#'
#' @export
add_summary <- function(data,
                        add_overall = TRUE,
                        continuous_format = NULL,
                        norm_continuous_format = "{mean} \u00b1 {SD}",
                        unnorm_continuous_format = "{median} ({Q1}, {Q3})",
                        categorical_format = "{n} ({pct})",
                        binary_show = "last",
                        digit = 2) {

  overall <- variable <- NULL #避免R CMD check警告

  #检查add_var属性
  if (!"add_var" %in% names(attributes(data))) {
    cli_alert_danger("must use add_var() for 'data' before using add_summary()")
    stop()
  }

  #检查add_overall参数
  if (!is.logical(add_overall) || length(add_overall) != 1) {
    cli_alert_danger("'add_overall' must be a logical value")
    stop()
  }

  #检查format参数
  valid_continuous_placeholders <- c("{mean}", "{SD}", "{median}", "{Q1}", "{Q3}")
  valid_categorical_placeholders <- c("{n}", "{pct}")
  if(!is.null(continuous_format)) {
    norm_continuous_format <- unnorm_continuous_format <- continuous_format
  }
  cal_index <- str_extract_all(norm_continuous_format, "\\{.*?\\}")[[1]]
  if (length(cal_index) == 0 || !all(cal_index %in% valid_continuous_placeholders)) {
    cli_alert_danger("norm_continuous_format must within {mean}, {SD}, {median}, {Q1} or {Q3}")
    stop()
  }
  cal_index <- str_extract_all(unnorm_continuous_format, "\\{.*?\\}")[[1]]
  if (length(cal_index) == 0 || !all(cal_index %in% valid_continuous_placeholders)) {
    cli_alert_danger("unnorm_continuous_format must within {mean}, {SD}, {median}, {Q1} or {Q3}")
    stop()
  }
  cal_index <- str_extract_all(categorical_format, "\\{.*?\\}")[[1]]
  if (length(cal_index) == 0 || !all(cal_index %in% valid_categorical_placeholders)) {
    cli_alert_danger("categorical_format must within {n} or {pct}")
    stop()
  }

  #检查binary_show参数
  if (!binary_show %in% c("first", "last", "all")) {
    cli_alert_danger("binary_show must be: 'first', 'last' or 'all'")
    stop()
  }

  #检查digit参数
  if (!is.numeric(digit) || length(digit) != 1 || digit < 0 || digit != floor(digit)) {
    cli_alert_danger("'digit' must be a numeric value greater than or equal to 0")
    stop()
  }

  #获取add_var属性
  datalist = attr(data, "add_var", exact = TRUE)
  group = datalist$group
  valid_var = datalist$var$valid
  continuous_var = datalist$var$continuous$all
  norm_var = datalist$var$continuous$norm
  unnorm_var = datalist$var$continuous$unnorm
  categorical_var = datalist$var$categorical$all
  binary_var = datalist$var$categorical$binary

  summary <- list()
  overall_summary <- list()
  cli_progress_step("Summarizing", spinner = TRUE)
  for (v in valid_var){
    cli_progress_update()
    if (v %in% norm_var) {
      summary[[v]] <- create_summary(data, v, group, norm_continuous_format, type = "continuous", digit = digit)
      overall_summary[[v]] <- create_summary(data, v, group = NULL, norm_continuous_format, type = "continuous", digit = digit)
    }
    if (v %in% unnorm_var) {
      summary[[v]] <- create_summary(data, v, group, unnorm_continuous_format, type = "continuous", digit = digit)
      overall_summary[[v]] <- create_summary(data, v, group = NULL, unnorm_continuous_format, type = "continuous", digit = digit)
    }
    if (v %in% categorical_var) {
      summary[[v]] <- create_summary(data, v, group, categorical_format, type = "categorical", binary_show, digit = digit)
      overall_summary[[v]] <- create_summary(data, v, group = NULL, categorical_format, type = "categorical", binary_show, digit = digit)
    }
  }
  summary = bind_rows(summary)
  colnames(summary) <- c("variable", paste0(datalist$group_levels, " (n=", datalist$group_n[datalist$group_levels], ")"))

  if(add_overall) {
    overall_summary = bind_rows(overall_summary)
    summary <- summary %>%
      mutate(overall = overall_summary$Overall) %>%
      relocate(overall, .after = variable)
    colnames(summary) <- c("variable",
                           paste0("Overall (n=", datalist$overall_n, ")"),
                           paste0(datalist$group_levels, " (n=", datalist$group_n[datalist$group_levels], ")"))
  }

  attr(summary, "add_var") <- datalist
  attr(summary, "add_summary") <- list(input_data = data)

  return(summary)
}
