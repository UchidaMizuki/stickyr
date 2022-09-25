#' @importFrom dplyr filter
#' @export
filter.sticky_tbl_df <- function(.data, ..., .preserve = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}

#' @export
filter.sticky_grouped_df <- function(.data, ..., .preserve = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}

#' @export
filter.sticky_rowwise_df <- function(.data, ..., .preserve = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}
