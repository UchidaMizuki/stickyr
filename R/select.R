#' @importFrom dplyr select
#' @export
select.sticky_tbl_df <- function(.data, ...) {
  select_sticky(NextMethod(), .data)
}

#' @export
select.sticky_grouped_df <- function(.data, ...) {
  select_sticky(NextMethod(), .data)
}

#' @export
select.sticky_rowwise_df <- function(.data, ...) {
  select_sticky(NextMethod(), .data)
}

select_sticky <- function(data_selected, data) {
  sticky_cols <- attr(data, "sticky_cols")
  data_selected <- dplyr::bind_cols(data_selected, tibble::as_tibble(data)[setdiff(row.names(sticky_cols), names(data_selected))])
  restore_sticky_attrs(data_selected, data)
}
