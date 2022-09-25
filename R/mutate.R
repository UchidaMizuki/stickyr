#' @importFrom dplyr mutate
#' @export
mutate.sticky_tbl_df <- function(.data, ...) {
  out <- NextMethod()
  mutate_sticky(.data, out)
}

#' @export
mutate.sticky_grouped_df <- function(.data, ...) {
  out <- NextMethod()
  mutate_sticky(.data, out)
}

#' @export
mutate.sticky_rowwise_df <- function(.data, ...) {
  out <- NextMethod()
  mutate_sticky(.data, out)
}

mutate_sticky <- function(data, data_mutated) {
  sticky_cols <- attr(data, "sticky_cols")
  data_mutated <- tibble::add_column(data_mutated, !!!tibble::as_tibble(data)[setdiff(row.names(sticky_cols), names(data_mutated))])
  restore_sticky_attrs(data_mutated, data)
}
