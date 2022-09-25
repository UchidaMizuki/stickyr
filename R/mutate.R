#' @importFrom dplyr mutate
#' @export
mutate.sticky_tbl_df <- function(.data, ...) {
  mutate_sticky(NextMethod(), .data)
}

#' @export
mutate.sticky_grouped_df <- function(.data, ...) {
  mutate_sticky(NextMethod(), .data)
}

#' @export
mutate.sticky_rowwise_df <- function(.data, ...) {
  mutate_sticky(NextMethod(), .data)
}

#' @importFrom dplyr transmute
#' @export
transmute.sticky_tbl_df <- function(.data, ...) {
  mutate_sticky(NextMethod(), .data)
}

#' @export
transmute.sticky_grouped_df <- function(.data, ...) {
  mutate_sticky(NextMethod(), .data)
}

#' @export
transmute.sticky_rowwise_df <- function(.data, ...) {
  mutate_sticky(NextMethod(), .data)
}

mutate_sticky <- function(data_mutated, data) {
  sticky_cols <- attr(data, "sticky_cols")
  data_mutated <- dplyr::bind_cols(data_mutated, tibble::as_tibble(data)[setdiff(row.names(sticky_cols), names(data_mutated))])
  restore_sticky_attrs(data_mutated, data)
}
