#' @export
new_sticky_tibble <- function(x = list(),
                              cols = tidyselect::last_col(),
                              col_show = tidyselect::everything(),
                              col_summary = list(),
                              attrs = character(), ...,
                              class = character(),
                              class_grouped_df = character(),
                              class_rowwise_df = character()) {

  # sticky_cols
  cols <- names(tidyselect::eval_select(expr({{ cols }}), x))
  col_show <- tidyselect::eval_select(expr({{ col_show }}), set_names(cols))

  cols <- new_data_frame(df_list(show = FALSE,
                                 summary = list(vec_init),
                                 .size = vec_size(cols)),
                         row.names = cols)

  if (!vec_is_empty(col_show)) {
    vec_slice(cols, col_show)$show <- TRUE
  }

  if (!vec_is_empty(col_summary)) {
    vec_slice(cols, names(col_summary))$summary <- col_summary
  }

  tibble::new_tibble(x,
                     sticky_cols = cols,
                     sticky_attrs = attrs, ...,
                     class = c(class, "sticky_tbl_df"),
                     class_tbl_df = class,
                     class_grouped_df = class_grouped_df,
                     class_rowwise_df = class_rowwise_df)
}

#' @export
as_sticky_tibble <- function(x, ...) {
  UseMethod("as_sticky_tibble")
}

#' @export
as_sticky_tibble.sticky_tbl_df <- function(x, ...) {
  x
}

#' @export
`[.sticky_tbl_df` <- function(x, ...) {
  out <- NextMethod()
  sticky_cols <- attr(out, "sticky_cols")
  attr(out, "sticky_cols") <- vec_slice(sticky_cols, intersect(row.names(sticky_cols), names(out)))
  out
}

#' @export
`names<-.sticky_tbl_df` <- function(x, value) {
  out <- NextMethod()
  sticky_cols <- attr(out, "sticky_cols")
  loc <- vec_match(row.names(sticky_cols), names(x))
  row.names(sticky_cols) <- vec_slice(names(out), loc)
  attr(out, "sticky_cols") <- sticky_cols
  out
}

#' @importFrom dplyr select
#' @export
select.sticky_tbl_df <- function(.data, ...) {
  out <- NextMethod()
  sticky_cols <- attr(out, "sticky_cols")
  out <- vec_cbind(out, .data[setdiff(row.names(sticky_cols), names(out))])
  attr(out, "sticky_cols") <- sticky_cols
  out
}

#' @importFrom dplyr ungroup
#' @export
ungroup.sticky_tbl_df <- function(x, ...) {
  ellipsis::check_dots_empty()
  x
}

#' @export
format.sticky_tbl_df <- function(x, ...) {
  x <- drop_hidden_cols(x)

  NextMethod()
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.sticky_tbl_df <- function(x) {
  out <- NextMethod()
  sticky_cols <- attr(x, "sticky_cols")

  if (!vec_is_empty(sticky_cols)) {
    out <- c(out,
             Stickers = paste0(row.names(sticky_cols), collapse = ", "))
  }
  out
}
