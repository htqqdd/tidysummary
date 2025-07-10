#' Prepare variables for add_summary
#'
#' This function processes a dataset for statistical analysis by categorizing variables into continuous and categorical types. It automatically handles normality checks, equality of variances checks, and expected frequency assumptions checks.
#'
#' @param data A data frame containing the variables to analyze, with variables at columns and observations at rows.
#' @param var A character vector of variable names to include. If `NULL`, by default, all columns except the `group` column will be used.
#' @param group A character string specifying the grouping variable in `data`. If not specified, `'group'`, by default.
#' @param norm Control parameter for normality tests. Accepts:
#'   - `'auto'`: Automatically decide based on p-values, but the same as `'ask'` when n > 1000, default
#'   - `'ask'`: Show p-values, plots QQ plots and prompts for decision
#'   - `TRUE`/`'true'`: Always assuming data are normally distributed
#'   - `FALSE`/`'false'`: Always assuming data are non-normally distributed
#' @param center A character string specifying the `center` to use in Levene's test for equality of variances. Default is `'median'`, which is more robust than the mean.
#'
#' @return A modified data frame with an attribute `'add_var'` containing a list of categorized variables and their properties:
#' \itemize{
#'   \item \code{var}: List of categorized variables:
#'     \itemize{
#'       \item \code{valid}: All valid variable names after checks
#'       \item \code{continuous}: Sublist of continuous variables (further divided by normality/equal variance)
#'       \item \code{categorical}: Sublist of categorical variables (further divided by ordered/expected frequency)
#'     }
#'   \item \code{group}: Grouping variable name
#'   \item \code{overall_n}: Total number of observations
#'   \item \code{group_n}: Observation counts per group
#'   \item \code{group_nlevels}: Number of groups
#'   \item \code{group_levels}: Group level names
#'   \item \code{norm}: Normality check method used
#' }
#'
#' @examples
#' data <- add_var(iris, var = c("Sepal.Length", "Species"), group = "Species")
#'
#' @export
add_var <- function(data, var = NULL, group = "group", norm = "auto", center = "median"){

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
  if (!is.factor(data[[group]])) {
    data[[group]] <- factor(data[[group]])
    cli_alert_info(paste0(group, " has been converted to a factor: ",
                               paste(levels(data[[group]]), collapse = ", ")))
  }

  #检查var参数
  if (is.null(var)) {
    var <- setdiff(names(data), group)
    cli_alert_info(paste("No variables specified, using all except", group))
  }
  #检查变量是否存在于data
  valid_var1 <- var[var %in% names(data) & var != group]
  valid_var1 <- unique(valid_var1)
  if (length(valid_var1) < length(var)) {
    cli_alert_warning(paste("Invalid variables:",
                                 paste(setdiff(var, valid_var1), collapse = ", ")))
  }
  #检查分类变量的levels是否大于2
  valid_var2 <- valid_var1[sapply(data[valid_var1], function(x) !is.factor(x) || nlevels(x) > 1)]
  if (length(valid_var2) < length(valid_var1)) {
    cli_alert_warning(paste("Single-level variables dropped:",
                                 paste(setdiff(valid_var1, valid_var2), collapse = ", ")))
  }
  #检查数值变量每组不少于2个值
  valid_var3 <- valid_var2[sapply(data[valid_var2], function(x) {
    all(tapply(x, data[[group]], function(y) sum(!is.na(y))) > 2)
  })]
  if (length(valid_var3) < length(valid_var2)) {
    cli_alert_warning(paste("Variables with less than 3 values in each group dropped:",
                                 paste(setdiff(valid_var2, valid_var3), collapse = ", ")))
  }
  if (length(valid_var3) == 0) {
    cli_alert_danger("No valid variables")
    stop()
  }
  valid_var <- valid_var3
  #转换character为factor
  for (v in valid_var) {
    if (is.character(data[[v]]) || is.logical(data[[v]])) {
      data[[v]] <- factor(data[[v]])
      cli_alert_info(paste0(v, " has been converted to a factor: ",
                                 paste(levels(data[[v]]), collapse = ", ")))
    }
  }

  #检查norm参数
  if (!(is.logical(norm) || (is.character(norm) && length(norm) == 1 && tolower(norm) %in% c("true", "false", "auto", "ask")))) {
    cli_alert_danger("'norm' must be a logical or 'true' or 'false' or 'auto' or 'ask'.")
    stop()
  }

  #连续性变量
  continuous_var <- valid_var[sapply(valid_var, function(x) is.numeric(data[[x]]))]
  if (length(continuous_var) > 0) {
    norm_var <- continuous_var[sapply(continuous_var, function(x) normal_test(data, x, group, norm))]
    unnorm_var <- setdiff(continuous_var, norm_var)
    if (length(norm_var) > 0) {
      norm_equal_var <- norm_var[sapply(norm_var, function(x) equal_test(data, x, group, center))]
      norm_unequal_var <- setdiff(norm_var, norm_equal_var)
    } else {
      norm_equal_var <- norm_unequal_var <- c()
    }
  } else {
    norm_var <- norm_equal_var <- norm_unequal_var <- unnorm_var <- c()
  }


  #分类变量
  categorical_var <- valid_var[sapply(valid_var, function(x) is.factor(data[[x]]))]
  if (length(categorical_var) > 0) {
    rank_var <- categorical_var[sapply(categorical_var, function(x) is.ordered(data[[x]]))]
    unrank_var <- setdiff(categorical_var, rank_var)
    if (length(unrank_var) > 0) {
      binary_var <- unrank_var[sapply(unrank_var, function(x) nlevels(data[[x]]) == 2)]
      not_small_var <- unrank_var[sapply(unrank_var, function(x) small_test(data, x, group) == "not_small")]
      small_var <- unrank_var[sapply(unrank_var, function(x) small_test(data, x, group) == "small")]
      very_small_var <- unrank_var[sapply(unrank_var, function(x) small_test(data, x, group) == "very_small")]
    } else {
      binary_var <- not_small_var <- small_var <- very_small_var <- c()
    }
  } else {
    binary_var <- rank_var <- not_small_var <- small_var <- very_small_var <- c()
  }


  attr(data, "add_var") <- list(
    var = list(
      valid = valid_var,
      continuous = list(
        all = continuous_var,
        norm = norm_var,
        norm_equal = norm_equal_var,
        norm_unequal = norm_unequal_var,
        unnorm = unnorm_var),
      categorical = list(
        all = categorical_var,
        rank = rank_var,
        binary = binary_var,
        not_small = not_small_var,
        small = small_var,
        very_small = very_small_var)),
    group = group,
    overall_n = nrow(data),
    group_n = table(data[[group]]),
    group_nlevels = nlevels(data[[group]]),
    group_levels = levels(data[[group]]),
    norm = norm)


  return(data)
}
