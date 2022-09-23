#' @importFrom dplyr group_by
#' @export
group_by.sticky_tbl_df <- function(.data, ...,
                                   .add = FALSE,
                                   .drop = dplyr::group_by_drop_default(.data)) {
  out <- NextMethod()
  class(out) <- c(attr(out, "class_grouped_df"), "sticky_grouped_df", class(out))
  out
}

#' @export
group_by.sticky_grouped_df <- function(.data, ...,
                                       .add = FALSE,
                                       .drop = dplyr::group_by_drop_default(.data)) {
  out <- NextMethod()
  class(out) <- c(attr(out, "class_grouped_df"), "sticky_grouped_df", class(out))
  out
}

#' @export
group_by.sticky_rowwise_df <- function(.data, ...,
                                       .add = FALSE,
                                       .drop = dplyr::group_by_drop_default(.data)) {
  out <- NextMethod()
  class(out) <- c(attr(out, "class_grouped_df"), "sticky_grouped_df", class(out))
  out
}
