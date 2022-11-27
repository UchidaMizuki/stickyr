#' @importFrom dplyr distinct
#' @export
distinct.sticky_tbl_df <- function(.data, ..., .keep_all = FALSE) {
  distinct_sticky(NextMethod(), .data, ...)
}

#' @export
distinct.sticky_grouped_df <- function(.data, ..., .keep_all = FALSE) {
  distinct_sticky(NextMethod(), .data, ...)
}

#' @export
distinct.sticky_rowwise_df <- function(.data, ..., .keep_all = FALSE) {
  distinct_sticky(NextMethod(), .data, ...)
}

distinct_sticky <- function(data_distinct, data, ...) {
  data_summarised <- summarise(group_by(data, ...,
                                        .add = TRUE),
                               .groups = "drop")
  data_distinct <- dplyr::left_join(data_distinct, data_summarised,
                                    by = names(data_distinct))
  restore_sticky_attrs(data_distinct, data)
}
