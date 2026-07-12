drop_hidden_cols <- function(x) {
  sticky_cols <- attr(x, "sticky_cols")
  hidden_cols <- vec_slice(row.names(sticky_cols), !sticky_cols$show)
  x[!names(x) %in% hidden_cols]
}

# strip the sticky subclass to avoid recursing back into our own S3 dispatch
strip_sticky_class <- function(x) {
  class(x) <- setdiff(
    class(x),
    c("sticky_tbl_df", "sticky_grouped_df", "sticky_rowwise_df")
  )
  x
}

apply_sticky_class <- function(x, class_tbl_df, class_grouped_df, class_rowwise_df) {
  if (inherits(x, "grouped_df")) {
    class(x) <- unique(c(class_grouped_df, "sticky_grouped_df", class(x)))
  } else if (inherits(x, "rowwise_df")) {
    class(x) <- unique(c(class_rowwise_df, "sticky_rowwise_df", class(x)))
  } else if (inherits(x, "tbl_df")) {
    class(x) <- unique(c(class_tbl_df, "sticky_tbl_df", class(x)))
  }
  x
}

restore_sticky_attrs <- function(x, data) {
  attrs <- attributes(data)

  sticky_cols <- attrs$sticky_cols
  sticky_attrs <- attrs$sticky_attrs

  class_tbl_df <- attrs$class_tbl_df
  class_grouped_df <- attrs$class_grouped_df
  class_rowwise_df <- attrs$class_rowwise_df

  attrs <- attrs[sticky_attrs]

  x <- exec(
    structure,
    x,
    sticky_cols = sticky_cols,
    sticky_attrs = sticky_attrs,
    !!!attrs,
    class_tbl_df = class_tbl_df,
    class_grouped_df = class_grouped_df,
    class_rowwise_df = class_rowwise_df
  )

  apply_sticky_class(x, class_tbl_df, class_grouped_df, class_rowwise_df)
}

restore_sticky_cols <- function(x, data) {
  sticky_cols <- attr(data, "sticky_cols")
  attr(x, "sticky_cols") <- vec_slice(
    sticky_cols,
    intersect(row.names(sticky_cols), names(x))
  )
  class(x) <- class(data)
  x
}

# compute one summarised row (grouped: one per group) for sticky columns;
# `keep` carries along join-key columns (e.g. group vars) untouched
sticky_summary <- function(data, sticky_cols, exclude = character(), keep = character()) {
  args <- vec_slice(sticky_cols, !row.names(sticky_cols) %in% exclude)
  if (vec_is_empty(args)) {
    return(NULL)
  }

  data <- strip_sticky_class(data)
  col_names <- row.names(args)
  args <- purrr::map2(col_names, args$summary, function(.cols, .fns) {
    expr(dplyr::across(!!.cols, !!.fns))
  })
  tibble::as_tibble(dplyr::summarise(data, !!!args))[c(keep, col_names)]
}
