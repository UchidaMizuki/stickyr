
# arrange -----------------------------------------------------------------

#' @importFrom dplyr arrange
#' @export
arrange.sticky_tbl_df <- function(.data, ..., .by_group = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}

#' @export
arrange.sticky_grouped_df <- function(.data, ..., .by_group = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}

#' @export
arrange.sticky_rowwise_df <- function(.data, ..., .by_group = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}



# count -------------------------------------------------------------------

#' @importFrom dplyr count
#' @export
count.sticky_tbl_df <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
count.sticky_grouped_df <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
count.sticky_rowwise_df <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  restore_sticky_attrs(NextMethod(), x)
}



# filter ------------------------------------------------------------------

#' @importFrom dplyr filter
#' @export
dplyr::filter

#' @export
filter.sticky_tbl_df <- function(.data, ..., .preserve = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}

#' @export
filter.sticky_grouped_df <- function(.data, ..., .preserve = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}

#' @export
filter.sticky_rowwise_df <- function(.data, ..., .preserve = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}



# group_by ----------------------------------------------------------------

#' @importFrom dplyr group_by
#' @export
group_by.sticky_tbl_df <- function(.data, ...,
                                   .add = FALSE,
                                   .drop = dplyr::group_by_drop_default(.data)) {
  restore_sticky_attrs(NextMethod(), .data)
}

#' @export
group_by.sticky_grouped_df <- function(.data, ...,
                                       .add = FALSE,
                                       .drop = dplyr::group_by_drop_default(.data)) {
  restore_sticky_attrs(NextMethod(), .data)
}

#' @export
group_by.sticky_rowwise_df <- function(.data, ...,
                                       .add = FALSE,
                                       .drop = dplyr::group_by_drop_default(.data)) {
  restore_sticky_attrs(NextMethod(), .data)
}



# join --------------------------------------------------------------------

#' @importFrom dplyr left_join
#' @export
left_join.sticky_tbl_df <- function(x, y,
                                    by = NULL,
                                    copy = FALSE,
                                    suffix = c(".x", ".y"), ...,
                                    keep = FALSE) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
left_join.sticky_grouped_df <- function(x, y,
                                        by = NULL,
                                        copy = FALSE,
                                        suffix = c(".x", ".y"), ...,
                                        keep = FALSE) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
left_join.sticky_rowwise_df <- function(x, y,
                                        by = NULL,
                                        copy = FALSE,
                                        suffix = c(".x", ".y"), ...,
                                        keep = FALSE) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @importFrom dplyr right_join
#' @export
right_join.sticky_tbl_df <- function(x, y,
                                     by = NULL,
                                     copy = FALSE,
                                     suffix = c(".x", ".y"), ...,
                                     keep = FALSE) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
right_join.sticky_grouped_df <- function(x, y,
                                         by = NULL,
                                         copy = FALSE,
                                         suffix = c(".x", ".y"), ...,
                                         keep = FALSE) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
right_join.sticky_rowwise_df <- function(x, y,
                                         by = NULL,
                                         copy = FALSE,
                                         suffix = c(".x", ".y"), ...,
                                         keep = FALSE) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @importFrom dplyr inner_join
#' @export
inner_join.sticky_tbl_df <- function(x, y,
                                     by = NULL,
                                     copy = FALSE,
                                     suffix = c(".x", ".y"), ...,
                                     keep = FALSE) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
inner_join.sticky_grouped_df <- function(x, y,
                                         by = NULL,
                                         copy = FALSE,
                                         suffix = c(".x", ".y"), ...,
                                         keep = FALSE) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
inner_join.sticky_rowwise_df <- function(x, y,
                                         by = NULL,
                                         copy = FALSE,
                                         suffix = c(".x", ".y"), ...,
                                         keep = FALSE) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @importFrom dplyr full_join
#' @export
full_join.sticky_tbl_df <- function(x, y,
                                    by = NULL,
                                    copy = FALSE,
                                    suffix = c(".x", ".y"), ...,
                                    keep = FALSE) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
full_join.sticky_grouped_df <- function(x, y,
                                        by = NULL,
                                        copy = FALSE,
                                        suffix = c(".x", ".y"), ...,
                                        keep = FALSE) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
full_join.sticky_rowwise_df <- function(x, y,
                                        by = NULL,
                                        copy = FALSE,
                                        suffix = c(".x", ".y"), ...,
                                        keep = FALSE) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @importFrom dplyr semi_join
#' @export
semi_join.sticky_tbl_df <- function(x, y,
                                    by = NULL,
                                    copy = FALSE, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
semi_join.sticky_grouped_df <- function(x, y,
                                        by = NULL,
                                        copy = FALSE, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
semi_join.sticky_rowwise_df <- function(x, y,
                                        by = NULL,
                                        copy = FALSE, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @importFrom dplyr anti_join
#' @export
anti_join.sticky_tbl_df <- function(x, y,
                                    by = NULL,
                                    copy = FALSE, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
anti_join.sticky_grouped_df <- function(x, y,
                                        by = NULL,
                                        copy = FALSE, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
anti_join.sticky_rowwise_df <- function(x, y,
                                        by = NULL,
                                        copy = FALSE, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @importFrom dplyr nest_join
#' @export
nest_join.sticky_tbl_df <- function(x, y,
                                    by = NULL,
                                    copy = FALSE,
                                    keep = FALSE,
                                    name = NULL, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
nest_join.sticky_grouped_df <- function(x, y,
                                        by = NULL,
                                        copy = FALSE,
                                        keep = FALSE,
                                        name = NULL, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
nest_join.sticky_rowwise_df <- function(x, y,
                                        by = NULL,
                                        copy = FALSE,
                                        keep = FALSE,
                                        name = NULL, ...) {
  restore_sticky_attrs(NextMethod(), x)
}



# relocate ----------------------------------------------------------------

#' @importFrom dplyr relocate
#' @export
relocate.sticky_tbl_df <- function(.data, ..., .preserve = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}

#' @export
relocate.sticky_grouped_df <- function(.data, ..., .preserve = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}

#' @export
relocate.sticky_rowwise_df <- function(.data, ..., .preserve = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}

# rowwise -----------------------------------------------------------------

#' @importFrom dplyr rowwise
#' @export
rowwise.sticky_tbl_df <- function(data, ...) {
  restore_sticky_attrs(NextMethod(), data)
}

#' @export
rowwise.sticky_grouped_df <- function(data, ...) {
  restore_sticky_attrs(NextMethod(), data)
}

#' @export
rowwise.sticky_rowwise_df <- function(data, ...) {
  restore_sticky_attrs(NextMethod(), data)
}



# set ---------------------------------------------------------------------

#' @importFrom generics intersect
#' @export
generics::intersect

#' @export
intersect.sticky_tbl_df <- function(x, y, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
intersect.sticky_grouped_df <- function(x, y, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
intersect.sticky_rowwise_df <- function(x, y, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @importFrom generics setdiff
#' @export
generics::setdiff

#' @export
setdiff.sticky_tbl_df <- function(x, y, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
setdiff.sticky_grouped_df <- function(x, y, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
setdiff.sticky_rowwise_df <- function(x, y, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @importFrom generics union
#' @export
generics::union

#' @export
union.sticky_tbl_df <- function(x, y, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
union.sticky_grouped_df <- function(x, y, ...) {
  restore_sticky_attrs(NextMethod(), x)
}

#' @export
union.sticky_rowwise_df <- function(x, y, ...) {
  restore_sticky_attrs(NextMethod(), x)
}



# slice -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.sticky_tbl_df <- function(.data, ..., .preserve = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}

#' @export
slice.sticky_grouped_df <- function(.data, ..., .preserve = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}

#' @export
slice.sticky_rowwise_df <- function(.data, ..., .preserve = FALSE) {
  restore_sticky_attrs(NextMethod(), .data)
}

