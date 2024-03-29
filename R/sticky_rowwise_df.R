#' @export
`[.sticky_rowwise_df` <- function(x, ...) {
  out <- NextMethod()
  sticky_cols <- attr(x, "sticky_cols")
  attr(out, "sticky_cols") <- vec_slice(sticky_cols, intersect(row.names(sticky_cols), names(out)))
  class(out) <- class(x)
  out
}

#' @export
as_sticky_tibble.sticky_rowwise_df <- function(x, ...) {
  out <- tibble::as_tibble(x)
  restore_sticky_attrs(out, x)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.sticky_rowwise_df <- function(x, ...) {
  check_dots_empty()
  as_sticky_tibble(x)
}

#' @export
format.sticky_rowwise_df <- function(x, ...) {
  x <- drop_hidden_cols(x)

  NextMethod()
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.sticky_rowwise_df <- function(x) {
  out <- NextMethod()
  sticky_cols <- attr(x, "sticky_cols")

  if (!vec_is_empty(sticky_cols)) {
    out <- c(out[1],
             Stickers = paste0(row.names(sticky_cols), collapse = ", "),
             out[2])
  }
  out
}

#' @export
vec_ptype_abbr.sticky_rowwise_df <- function(x, ...) {
  abbreviate("rowwise_df", 8L)
}
