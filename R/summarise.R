#' @importFrom dplyr summarise
#' @export
summarise.sticky_tbl_df <- function(.data, ..., .groups = NULL) {
  summarise_sticky(.data, NextMethod())
}

#' @export
summarise.sticky_grouped_df <- function(.data, ..., .groups = NULL) {
  summarise_sticky(.data, NextMethod())
}

#' @export
summarise.sticky_rowwise_df <- function(.data, ..., .groups = NULL) {
  summarise_sticky(.data, NextMethod())
}

summarise_sticky <- function(data, data_summarised) {
  sticky_cols <- attr(data, "sticky_cols")
  summary <- sticky_summary(data, sticky_cols, exclude = names(data_summarised))

  if (!is.null(summary)) {
    data_summarised <- dplyr::bind_cols(data_summarised, summary)
  }

  restore_sticky_attrs(data_summarised, data)
}
