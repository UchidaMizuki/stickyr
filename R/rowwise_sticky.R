#' @importFrom dplyr rowwise
#' @export
rowwise.sticky_tbl_df <- function(data, ...) {
  hidden_col_names <- hidden_col_names(attr(data, "sticky_col_show"))
  vars <- tidyselect::eval_select(expr(c(...)), data)
  vars <- setdiff(vars, hidden_col_names)

  group_data <- tibble::as_tibble(data)[vars]

  class_rowwise <- attr(data, "class_rowwise")
  dplyr::new_rowwise_df(data, group_data,
                        class = c(class_rowwise, "rowwise_sticky"),
                        class_tbl = attr(data, "class_tbl"),
                        class_grouped = attr(data, "class_grouped"),
                        class_rowwise = class_rowwise)
}

#' @export
rowwise.grouped_sticky <- function(data, ...) {
  data <- as_sticky_tbl_df(data)
  rowwise(data, ...)
}

#' @export
rowwise.rowwise_sticky <- function(data, ...) {
  data <- as_sticky_tbl_df(data)
  rowwise(data, ...)
}

#' @export
`[.rowwise_sticky` <- function(x, ...) {
  out <- NextMethod()
  class(out) <- class(x)

  attrs <- attributes(x)

  # sticky_cols
  col_names <- names(out)
  sticky_col_names <- attrs$sticky_col_names
  sticky_col_names <- col_names[col_names %in% sticky_col_names]

  out <- update_sticky_cols(out, sticky_col_names, attrs)

  out
}

#' @export
as_sticky_tbl_df.rowwise_sticky <- function(x, ...) {
  attr(x, "groups") <- NULL

  class_tbl <- attr(x, "class_tbl")
  class_rowwise <- attr(x, "class_rowwise")
  class(x) <- c(class_tbl, "sticky_tbl_df",
                setdiff(class(x), c(class_rowwise, "rowwise_sticky", "rowwise_df")))
  x
}

#' @importFrom dplyr ungroup
#' @export
ungroup.rowwise_sticky <- function(x, ...) {
  ellipsis::check_dots_empty()
  as_sticky_tbl_df(x)
}

#' @export
format.rowwise_sticky <- function(x, ...) {
  x <- drop_hidden_cols(x)
  NextMethod()
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.rowwise_sticky <- function(x) {
  out <- NextMethod()

  hidden_col_names <- hidden_col_names(attr(x, "sticky_col_show"))

  sticky_col_names <- attr(x, "sticky_col_names")
  sticky_col_names <- sticky_col_names[!sticky_col_names %in% hidden_col_names]

  if (!vec_is_empty(sticky_col_names)) {
    out <- c(out[1],
             `Stickers` = paste0(attr(x, "sticky_col_names"), collapse = ", "),
             out[2])
  }

  out
}
