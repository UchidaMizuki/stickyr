#' @export
new_tbl_main <- function(x, is_main,
                         main_column = NULL,
                         n = NULL, ...,
                         class = character()) {
  if (is.null(main_column)) {
    stopifnot(is_scalar_character(main_column),
              main_column %in% names(x))
  } else {
    loc <- vapply(x, is_main,
                  FUN.VALUE = logical(1),
                  USE.NAMES = TRUE)
    nms <- names(loc)[loc]
  }

  new_data_frame(x,
                 n = n,
                 is_main = is_main,
                 main_column = main_column, ...,
                 class = c(class, "tbl_main", "tbl"))
}

#' @export
tbl_main <- function(..., .is_main,
                     .size = NULL,
                     .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  new_tbl_main(df_list(...,
                       .size = .size,
                       .name_repair = .name_repair),
               is_main = .is_main)
}
