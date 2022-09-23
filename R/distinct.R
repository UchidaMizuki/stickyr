#' @importFrom dplyr distinct
#' @export
distinct.sticky_tbl_df <- function(.data, ..., .keep_all = FALSE) {
  distinct_sticky(.data, ...,
                  .keep_all = .keep_all)
}

#' @export
distinct.sticky_grouped_df <- function(.data, ..., .keep_all = FALSE) {
  distinct_sticky(.data, ...,
                  .keep_all = .keep_all)
}

#' @export
distinct.sticky_rowwise_df <- function(.data, ..., .keep_all = FALSE) {
  distinct_sticky(.data, ...,
                  .keep_all = .keep_all)
}

distinct_sticky <- function(.data, ..., .keep_all) {
  sticky_cols <- attr(.data, "sticky_cols")
  class(.data) <- setdiff(class(.data), c("sticky_tbl_df", "sticky_grouped_df", "sticky_rowwise_df"))
  out <- distinct(.data, ..., dplyr::across(dplyr::all_of(row.names(sticky_cols))),
                  .keep_all = .keep_all)
  restore_sticky_attrs(out, .data)
}
