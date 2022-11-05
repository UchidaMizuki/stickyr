#' @export
`[.sticky_grouped_df` <- function(x, ...) {
  out <- NextMethod()
  sticky_cols <- attr(x, "sticky_cols")
  attr(out, "sticky_cols") <- vec_slice(sticky_cols, intersect(row.names(sticky_cols), names(out)))
  class(out) <- class(x)
  out
}

#' @export
as_sticky_tibble.sticky_grouped_df <- function(x, ...) {
  out <- tibble::as_tibble(x)
  restore_sticky_attrs(out, x)
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
vec_ptype_abbr.sticky_grouped_df <- function(x, ...) {
  abbreviate("grouped_df", 8L)
}
