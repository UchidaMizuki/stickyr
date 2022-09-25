#' @importFrom dplyr arrange
#' @export
arrange.sticky_tbl_df <- function(.data, ..., .by_group = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}

#' @export
arrange.sticky_grouped_df <- function(.data, ..., .by_group = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}

#' @export
arrange.sticky_rowwise_df <- function(.data, ..., .by_group = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}
