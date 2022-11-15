#' @title Percentage by Group
#' @details
#' Summarize the percentage of rows that satisfies given condition(s) by group
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param group_by_cols Column names or ids for grouping the rows
#' @param conditions Conditions that a row should satisfy to be considered in percentage calculation
#' @return A summarized dataframe with all combinations of group_by_cols, and a new column called "percentage"
#' @export
#' @examples
#' apt_buildings %>% filter(year_built >= 1950) %>% percentage_by_group('year_built', sprinkler_system == "YES")
#' apt_buildings %>% filter(year_built >= 1950) %>% percentage_by_group(c('year_built', 'property_type'), no_of_storeys >= 5)
percentage_by_group <- function(.data, group_by_cols, conditions = TRUE) {
  stopifnot(is.data.frame(.data))
  if(is.numeric(group_by_cols)) {
    stopifnot(floor(group_by_cols) == group_by_cols)
  }
  .data %>%
    drop_na(all_of(group_by_cols)) %>%
    group_by(across(all_of(group_by_cols))) %>%
    summarise(percentage = round(sum({{conditions}}, na.rm = TRUE) / n(), digits=2))
}
