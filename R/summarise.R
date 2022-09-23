#' @importFrom dplyr summarise
#' @export
summarise.sticky_tbl_df <- function(.data, ..., .groups = NULL) {
  out <- NextMethod()

  summarise_sticky(.data, out)
}

#' @export
summarise.sticky_grouped_df <- function(.data, ..., .groups = NULL) {
  out <- NextMethod()

  summarise_sticky(.data, out)
}

#' @export
summarise.sticky_rowwise_df <- function(.data, ..., .groups = NULL) {
  out <- NextMethod()

  summarise_sticky(.data, out)
}

summarise_sticky <- function(data, data_summarised) {
  sticky_cols <- attr(data, "sticky_cols")

  class(data) <- setdiff(class(data), c("sticky_tbl_df", "sticky_grouped_df", "sticky_rowwise_df"))
  args <- vec_slice(sticky_cols, !row.names(sticky_cols) %in% names(data_summarised))

  if (!vec_is_empty(args)) {
    col_names <- row.names(args)
    args <- purrr::map2(col_names, args$summary,
                        function(.cols, .fns) {
                          expr(dplyr::across(!!.cols, !!.fns))
                        })
    data_summarised <- vec_cbind(data_summarised, dplyr::summarise(data, !!!args)[col_names])
  }

  restore_sticky_attrs(data_summarised, data)
}
