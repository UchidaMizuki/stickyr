#' @importFrom dplyr group_by
#' @export
group_by.sticky_tbl_df <- function(.data, ...,
                                   .add = FALSE,
                                   .drop = dplyr::group_by_drop_default(.data)) {
  groups <- dplyr::group_by_prepare(.data, ...,
                                    .add = .add,
                                    caller_env = caller_env())
  data <- groups$data

  hidden_col_names <- hidden_col_names(attr(.data, "sticky_col_show"))
  vars <- setdiff(groups$group_names, hidden_col_names)

  out <- dplyr::grouped_df(data, vars, .drop)
  class_grouped <- attr(.data, "class_grouped")
  dplyr::new_grouped_df(data, dplyr::group_data(out),
                        class = c(class_grouped, "grouped_sticky"),
                        class_tbl = attr(.data, "class_tbl"),
                        class_grouped = class_grouped,
                        class_rowwise = attr(.data, "class_rowwise"))
}

#' @export
group_by.grouped_sticky <- function(.data, ...,
                                    .add = FALSE,
                                    .drop = dplyr::group_by_drop_default(.data)) {
  .data <- as_sticky_tbl_df(.data)
  group_by(.data, ...,
           .add = .add,
           .drop = .drop)
}

#' @export
group_by.rowwise_sticky <- function(.data, ...,
                                    .add = FALSE,
                                    .drop = dplyr::group_by_drop_default(.data)) {
  .data <- as_sticky_tbl_df(.data)
  group_by(.data, ...,
           .add = .add,
           .drop = .drop)
}

#' @export
`[.grouped_sticky` <- function(x, ...) {
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
as_sticky_tbl_df.grouped_sticky <- function(x, ...) {
  attr(x, "groups") <- NULL

  class_tbl <- attr(x, "class_tbl")
  class_grouped <- attr(x, "class_grouped")
  class(x) <- c(class_tbl, "sticky_tbl_df",
                setdiff(class(x), c(class_grouped, "grouped_sticky", "grouped_df")))
  x
}

#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_sticky <- function(x, ...) {
  if (missing(...)) {
    as_sticky_tbl_df(x)
  } else {
    NextMethod()
  }
}

#' @export
format.grouped_sticky <- function(x, ...) {
  x <- drop_hidden_cols(x)
  NextMethod()
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.grouped_sticky <- function(x) {
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
