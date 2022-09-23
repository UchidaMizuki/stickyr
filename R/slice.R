#' @importFrom dplyr slice
#' @export
slice.sticky_tbl_df <- function(.data, ..., .preserve = FALSE) {
  out <- NextMethod()
  restore_sticky_attrs(out, .data)
}

#' @export
slice.sticky_grouped_df <- function(.data, ..., .preserve = FALSE) {
  out <- NextMethod()
  restore_sticky_attrs(out, .data)
}

#' @export
slice.sticky_rowwise_df <- function(.data, ..., .preserve = FALSE) {
  out <- NextMethod()
  restore_sticky_attrs(out, .data)
}
