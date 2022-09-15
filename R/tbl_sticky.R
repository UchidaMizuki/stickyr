#' @export
new_tbl_sticky <- function(x = list(),
                           sticky_cols = NULL,
                           sticky_col_show = tidyselect::everything(),
                           sticky_col_summary = list(), ...,
                           sticky_attr_names = character(),
                           class = character(),
                           class_grouped = character(),
                           class_rowwise = character()) {
  # sticky_cols
  sticky_cols <- names(tidyselect::eval_select(expr({{ sticky_cols }}), x))

  # sticky_col_show
  sticky_col_show <- names(tidyselect::eval_select(expr({{ sticky_col_show }}), set_names(sticky_cols)))
  sticky_cols <- set_names(sticky_cols %in% sticky_col_show,
                           sticky_cols)

  sticky_col_names <- names(sticky_cols)
  if (!all(sticky_col_names %in% names(x))) {
    abort("`x` must have `sticky_cols` as column names.")
  }

  # sticky_col_summary
  sticky_col_summary <- vec_cast(sticky_col_summary, list())

  if (is_named(sticky_col_summary) && !all(names(sticky_col_summary) %in% sticky_col_names)) {
    abort("Names of `sticky_col_summary` must be included in `sticky_cols`.")
  }

  sticky_col_summary <- set_names(sticky_col_summary[sticky_col_names],
                                  sticky_col_names)
  sticky_col_summary <- purrr::modify(sticky_col_summary,
                                      function(x) {
                                        out <- x %||% vec_init

                                        if (!is_function(out)) {
                                          abort("`sticky_col_summary` must be a list of functions")
                                        }

                                        out
                                      })

  # sticky_attr_names
  sticky_attr_names <- vec_cast(sticky_attr_names, character())

  out <- tibble::new_tibble(x,
                            sticky_cols = sticky_cols,
                            sticky_col_summary = sticky_col_summary, ...,
                            sticky_attr_names = sticky_attr_names,
                            class = c(class, "tbl_sticky"),
                            class_tbl = class,
                            class_grouped = class_grouped,
                            class_rowwise = class_rowwise)

  if (!all(sticky_attr_names %in% names(attributes(out)))) {
    abort("`...` must provide attributes with `sticky_attr_names`.")
  }

  out
}

#' @export
as_tbl_sticky <- function(x, ...) {
  UseMethod("as_tbl_sticky")
}

#' @export
as_tbl_sticky.tbl_sticky <- function(x, ...) {
  x
}

#' @export
`[.tbl_sticky` <- function(x, ...) {
  out <- NextMethod()

  attrs <- attributes(x)

  # sticky_cols
  col_names <- names(out)
  sticky_cols <- attrs$sticky_cols
  sticky_cols <- sticky_cols[intersect(col_names, names(sticky_cols))]

  update_sticky_cols(out, sticky_cols, attrs)
}

# #' @export
# `names<-.tbl_sticky` <- function(x, value) {
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
select.tbl_sticky <- function(.data, ...) {
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
ungroup.tbl_sticky <- function(x, ...) {
  ellipsis::check_dots_empty()
  x
}

#' @export
format.tbl_sticky <- function(x, ...) {
  x <- drop_hidden_cols(x)
  NextMethod()
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.tbl_sticky <- function(x) {
  out <- NextMethod()

  sticky_cols <- attr(x, "sticky_cols")

  if (!vec_is_empty(sticky_cols)) {
    out <- c(out,
             Stickers = paste0(names(sticky_cols), collapse = ", "))
  }

  out
}
