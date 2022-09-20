#' @export
new_sticky_tbl_df <- function(x = list(),
                              cols = tidyselect::last_col(),
                              col_show = tidyselect::everything(),
                              col_summary = list(),
                              attr_names = character(), ...,
                              class = character(),
                              class_grouped_df = character(),
                              class_rowwise_df = character()) {
  # sticky_cols
  cols <- names(tidyselect::eval_select(expr({{ cols }}), x))

  # sticky_col_show
  sticky_col_show <- names(tidyselect::eval_select(expr({{ col_show }}), set_names(cols)))
  cols <- set_names(cols %in% sticky_col_show, cols)

  # col_summary
  col_names <- names(cols)
  col_summary <- set_names(col_summary[col_names], col_names)
  col_summary <- purrr::modify(col_summary,
                               function(x) {
                                 if (is_null(x)) {
                                   vec_init
                                 } else {
                                   if (!is_function(x)) {
                                     abort("`col_summary` must be a list of functions")
                                   }
                                   x
                                 }
                               })

  out <- tibble::new_tibble(x,
                            sticky_cols = cols,
                            sticky_col_summary = col_summary,
                            sticky_attr_names = attr_names, ...,
                            class = c(class, "sticky_tbl_df"),
                            class_tbl_df = class,
                            class_grouped_df = class_grouped_df,
                            class_rowwise_df = class_rowwise_df)

  if (!all(attr_names %in% names(attributes(out)))) {
    abort("`...` must provide attributes with `attr_names`.")
  }

  out
}

#' @export
as_sticky_tbl_df <- function(x, ...) {
  UseMethod("as_sticky_tbl_df")
}

#' @export
as_sticky_tbl_df.sticky_tbl_df <- function(x, ...) {
  x
}

#' @export
`[.sticky_tbl_df` <- function(x, ...) {
  out <- NextMethod()

  attrs <- attributes(x)

  # sticky_cols
  col_names <- names(out)
  sticky_cols <- attrs$sticky_cols
  sticky_cols <- sticky_cols[intersect(col_names, names(sticky_cols))]

  update_sticky_cols(out, sticky_cols, attrs)
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

update_sticky_cols <- function(x, sticky_cols, attrs) {
  attr(x, "sticky_cols") <- sticky_cols
  attr(x, "sticky_col_summary") <- attrs$sticky_col_summary[names(sticky_cols)]
  x
}

drop_hidden_cols <- function(x) {
  attrs <- attributes(x)
  sticky_cols <- attrs$sticky_cols
  hidden_cols <- sticky_cols[!sticky_cols]

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
             Stickers = paste0(names(sticky_cols), collapse = ", "))
  }

  out
}
