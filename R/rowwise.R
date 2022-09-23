#' @importFrom dplyr rowwise
#' @export
rowwise.sticky_tbl_df <- function(.data, ...,
                                  .add = FALSE,
                                  .drop = dplyr::rowwise_drop_default(.data)) {
  out <- NextMethod()
  class(out) <- c(attr(out, "class_rowwise_df"), "sticky_rowwise_df", class(out))
  out
}

#' @export
rowwise.sticky_grouped_df <- function(.data, ...,
                                      .add = FALSE,
                                      .drop = dplyr::rowwise_drop_default(.data)) {
  out <- NextMethod()
  class(out) <- c(attr(out, "class_rowwise_df"), "sticky_rowwise_df", class(out))
  out
}

#' @export
rowwise.sticky_rowwise_df <- function(.data, ...,
                                      .add = FALSE,
                                      .drop = dplyr::rowwise_drop_default(.data)) {
  out <- NextMethod()
  class(out) <- c(attr(out, "class_rowwise_df"), "sticky_rowwise_df", class(out))
  out
}
