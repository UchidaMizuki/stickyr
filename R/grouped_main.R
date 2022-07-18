#' @importFrom dplyr group_by
#' @export
group_by.tbl_main <- function(.data, ...,
                              .add = FALSE,
                              .drop = dplyr::group_by_drop_default(.data)) {
  groups <- dplyr::group_by_prepare(.data, ...,
                                    .add = .add,
                                    caller_env = caller_env())
  data <- groups$data
  vars <- groups$group_names

  hidden_col_names <- hidden_col_names(attr(.data, "main_col_show"))

  if (any(hidden_col_names %in% vars)) {
    abort("Group variables must not contain hidden columns.")
  }

  out <- dplyr::grouped_df(data, vars, .drop)
  class_grouped <- attr(.data, "class_grouped")
  dplyr::new_grouped_df(data, dplyr::group_data(out),
                        class = c(class_grouped, "grouped_main"),
                        class_tbl = attr(.data, "class_tbl"),
                        class_grouped = class_grouped,
                        class_rowwise = attr(.data, "class_rowwise"))
}

#' @export
`[.grouped_main` <- function(x, ...) {
  out <- NextMethod()
  class(out) <- class(x)

  attrs <- attributes(x)

  # main_cols
  col_names <- names(out)
  main_col_names <- attrs$main_col_names
  main_col_names <- col_names[col_names %in% main_col_names]

  out <- update_main_cols(out, main_col_names, attrs)

  out
}

#' @export
as_tbl_main.grouped_main <- function(x, ...) {
  attr(x, "groups") <- NULL

  class_tbl <- attr(x, "class_tbl")
  class_grouped <- attr(x, "class_grouped")
  class(x) <- c(class_tbl, "tbl_main",
                setdiff(class(x), c(class_grouped, "grouped_main", "grouped_df")))
  x
}

#' @export
format.grouped_main <- function(x, ...) {
  x <- drop_hidden_cols(x)
  NextMethod()
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.grouped_main <- function(x) {
  out <- NextMethod()

  hidden_col_names <- hidden_col_names(attr(x, "main_col_show"))

  main_col_names <- attr(x, "main_col_names")
  main_col_names <- main_col_names[!main_col_names %in% hidden_col_names]

  if (!vec_is_empty(main_col_names)) {
    out <- c(out[1],
             `Main columns` = paste0(attr(x, "main_col_names"), collapse = ", "),
             out[2])
  }

  out
}
