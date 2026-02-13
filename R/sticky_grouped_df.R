#' @export
`[.sticky_grouped_df` <- function(x, ...) {
  restore_sticky_cols(NextMethod(), x)
}

#' @export
as_sticky_tibble.sticky_grouped_df <- function(x, ...) {
  restore_sticky_attrs(tibble::as_tibble(x), x)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.sticky_grouped_df <- function(x, ...) {
  if (missing(...)) {
    as_sticky_tibble(x)
  } else {
    NextMethod()
  }
}

#' @export
format.sticky_grouped_df <- function(x, ...) {
  x <- drop_hidden_cols(x)

  NextMethod()
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.sticky_grouped_df <- function(x) {
  tbl_sum_sticky(NextMethod(), x)
}

#' @export
vec_ptype_abbr.sticky_grouped_df <- function(x, ...) {
  abbreviate("grouped_df", 8L)
}
