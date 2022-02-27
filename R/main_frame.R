#' @export
new_main_frame <- function(x, is,
                           on = NULL,
                           sum = vec_init, ...,
                           nrow = NULL,
                           class = character()) {
  if (is.null(on)) {
    loc <- vapply(x, is,
                  FUN.VALUE = logical(1),
                  USE.NAMES = TRUE)
    on <- names(loc)[loc]
    size_on <- vec_size(on)

    stopifnot(
      size_on >= 1L
    )

    if (size_on >= 2L) {
      on <- on[[1L]]
      inform(paste0('Focusing, on = "', on, '"'))
    }
  } else {
    stopifnot(
      is_scalar_character(on),
      on %in% names(x),
      is(x[[on]])
    )
  }

  stopifnot(
    is.function(sum)
  )

  tibble::new_tibble(x, ...,
                     nrow = nrow,
                     mainframes_is = is,
                     mainframes_on = on,
                     mainframes_sum = sum, ...,
                     class = c(class, "main_frame"))
}

#' @export
main_frame <- function(..., .is,
                       .on = NULL,
                       .sum = vec_init,
                       .size = NULL,
                       .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  new_main_frame(df_list(...,
                         .size = .size,
                         .name_repair = .name_repair),
                 is = .is,
                 on = .on,
                 sum = .sum)
}

mainframes_is <- function(x) {
  attr(x, "mainframes_is")
}

`mainframes_is<-` <- function(x, value) {
  attr(x, "mainframes_is") <- value
  x
}

mainframes_on <- function(x) {
  attr(x, "mainframes_on")
}

`mainframes_on<-` <- function(x, value) {
  attr(x, "mainframes_on") <- value
  x
}

mainframes_sum <- function(x) {
  attr(x, "mainframes_sum")
}

`mainframes_sum<-` <- function(x, value) {
  attr(x, "mainframes_sum") <- value
  x
}

mainframes_cast <- function(x, to) {
  attr_names <- names(attributes(x))

  attrs <- attributes(to)
  attrs <- attrs[!names(attrs) %in% c(attr_names, "groups")]

  for (i in vec_seq_along(attrs)) {
    attr(x, names(attrs)[[i]]) <- attrs[[i]]
  }

  class <- class(x)
  class(x) <- c(setdiff(class(to), c(class, "grouped_df")), class)
  x
}

mainframes_check <- function(x, to) {
  is <- mainframes_is(to)
  on <- mainframes_on(to)
  stopifnot(
    is(x[[on]])
  )
  x
}

#' @export
as.data.frame.main_frame <- function (x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(as_tibble(x))
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.main_frame <- function(x, ...,
                                 .rows = NULL,
                                 .name_repair = c("check_unique", "unique", "universal", "minimal"),
                                 rownames = pkgconfig::get_config("tibble::rownames", NULL)) {
  out <- NextMethod()
  mainframes_is(out) <- NULL
  mainframes_on(out) <- NULL
  mainframes_sum(out) <- NULL
  out
}

#' @export
vec_cbind_frame_ptype.main_frame <- function(x, ...) {
  tibble::tibble()
}

#' @export
`[.main_frame` <- function(x, i, j, drop = FALSE, ...) {
  on <- mainframes_on(x)

  out <- NextMethod()

  if (i == on) {
    out
  } else {
    if (!on %in% names(out)) {
      out <- vec_cbind(out, x[on])
      out <- mainframes_cast(out, x)
    }
    out
  }
}

#' @export
`[<-.main_frame` <- function (x, i, j, ..., value) {
  mainframes_check(NextMethod(), x)
}

#' @export
`[[<-.main_frame` <- function (x, i, j, ..., value) {
  mainframes_check(NextMethod(), x)
}

#' @export
`$<-.main_frame` <- function (x, name, value) {
  mainframes_check(NextMethod(), x)
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.main_frame <- function(x) {
  c(NextMethod(),
    `Main column` = mainframes_on(x))
}

#' @importFrom dplyr mutate
#' @export
mutate.main_frame <- function(.data, ...) {
  is <- mainframes_is(.data)
  on <- mainframes_on(.data)

  out <- NextMethod()

  if (on %in% names(out)) {
    stopifnot(
      is(out[[on]])
    )
  } else {
    out <- vec_cbind(out, .data[on])
  }
  out
}

#' @importFrom dplyr rename
#' @export
rename.main_frame <- function(.data, ...) {
  names <- names(.data)

  on <- mainframes_on(.data)
  on <- vec_match(on, names)

  loc <- tidyselect::eval_rename(expr(c(...)), .data)
  names[loc] <- names(loc)

  out <- set_names(.data, names)
  mainframes_on(out) <- names[on]
  out
}

#' @importFrom dplyr select
#' @export
select.main_frame <- function(.data, ...) {
  on <- mainframes_on(.data)

  out <- NextMethod()

  if (!on %in% names(out)) {
    out <- vec_cbind(out, .data[on])
  }
  out
}

#' @importFrom dplyr group_by
#' @export
group_by.main_frame <- function(.data, ...,
                                .add = FALSE,
                                .drop = dplyr::group_by_drop_default(.data)) {
  out <- NextMethod()
  class <- c("tbl_df", "tbl", "data.frame")
  class(out) <- c(setdiff(class(.data), class), "grouped_df", class)
  out
}

#' @importFrom dplyr ungroup
#' @export
ungroup.main_frame <- function(x, ...) {
  out <- NextMethod()
  mainframes_cast(out, x)
}

#' @importFrom dplyr summarise
#' @export
summarise.main_frame <- function(.data, ...,
                                 .groups = NULL) {
  is <- mainframes_is(.data)
  on <- mainframes_on(.data)
  sum <- mainframes_sum(.data)

  out <- NextMethod()

  if (!on %in% names(out)) {
    data <- vec_chop(.data[[on]], dplyr::group_rows(.data))
    data <- lapply(data, sum)
    data <- vec_c(!!!data)
    out <- vec_cbind(out,
                     !!on := data)
  }
  out <- mainframes_cast(out, .data)

  stopifnot(
    is(out[[on]])
  )

  out
}
