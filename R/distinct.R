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
  data_summarised <- summarise(group_by(data, ...),
                               .groups = "drop")
  data_distinct <- dplyr::bind_cols(data_distinct, data_summarised[!names(data_summarised) %in% names(data_distinct)])
  restore_sticky_attrs(data_distinct, data)
}
