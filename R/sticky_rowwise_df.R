#' @export
`[.sticky_rowwise_df` <- function(x, ...) {
  restore_sticky_cols(NextMethod(), x)
}

#' @export
as_sticky_tibble.sticky_rowwise_df <- function(x, ...) {
  restore_sticky_attrs(tibble::as_tibble(x), x)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.sticky_rowwise_df <- function(x, ...) {
  check_dots_empty()
  as_sticky_tibble(x)
}

#' @export
print.sticky_rowwise_df <- function(x, ...) {
  x <- drop_hidden_cols(x)
  NextMethod()
}

#' @export
format.sticky_rowwise_df <- function(x, ...) {
  x <- drop_hidden_cols(x)
  NextMethod()
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.sticky_rowwise_df <- function(x) {
  tbl_sum_sticky(NextMethod(), x)
}

#' @export
vec_ptype_abbr.sticky_rowwise_df <- function(x, ...) {
  abbreviate("rowwise_df", 8L)
}
