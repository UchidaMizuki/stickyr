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
  vec_slice(cols, col_show)$show <- TRUE
  vec_slice(cols, names(col_summary))$summary <- col_summary

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
  attr(out, "sticky_cols") <- vec_slice(sticky_cols, intersect(names(out), row.names(sticky_cols)))
  out
}

# #' @export
# `names<-.sticky_tbl_df` <- function(x, value) {
#   col_names <- names(x)
#   value <- set_names(value, col_names)
#
#   attrs <- attributes(x)
#   sticky_cols <- attrs$sticky_cols
#   x <- update_sticky_cols(x, sticky_cols, attrs)
#
#   sticky_cols <- unname(value[sticky_cols])
#   sticky_col_summary <- set_names(attrs$sticky_col_summary, sticky_cols)
#   sticky_col_show <- set_names(attrs$sticky_col_show, sticky_cols)
#
#   attr(x, "sticky_cols") <- sticky_cols
#   attr(x, "sticky_col_summary") <- sticky_col_summary
#   attr(x, "sticky_col_show") <- sticky_col_show
#
#   NextMethod()
# }

drop_hidden_cols <- function(x) {
  sticky_cols <- attr(x, "sticky_cols")
  hidden_cols <- vec_slice(row.names(sticky_cols), sticky_cols$show)

  x[!names(x) %in% names(hidden_cols)]
}

#' @importFrom dplyr select
#' @export
select.sticky_tbl_df <- function(.data, ...) {
  out <- NextMethod()

  attrs <- attributes(.data)
  sticky_cols <- attrs$sticky_cols
  col_names <- names(out)

  for (sticky_col_name in sticky_cols) {
    if (!sticky_col_name %in% col_names) {
      out <- vec_cbind(out,
                       !!sticky_col_name := .data[[sticky_col_name]])
    }
  }

  update_sticky_cols(out, sticky_cols, attrs)
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
