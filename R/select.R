#' @importFrom dplyr select
#' @export
select.sticky_tbl_df <- function(.data, ...) {
  out <- NextMethod()
  select_sticky(.data, out)
}

#' @export
select.sticky_grouped_df <- function(.data, ...) {
  out <- NextMethod()
  select_sticky(.data, out)
}

#' @export
select.sticky_rowwise_df <- function(.data, ...) {
  out <- NextMethod()
  select_sticky(.data, out)
}

select_sticky <- function(data, data_selected) {
  sticky_cols <- attr(data, "sticky_cols")
  data_selected <- tibble::add_column(data_selected, !!!tibble::as_tibble(data)[setdiff(row.names(sticky_cols), names(data_selected))])
  restore_sticky_attrs(data_selected, data)
}
