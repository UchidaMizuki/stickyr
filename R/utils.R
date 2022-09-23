drop_hidden_cols <- function(x) {
  sticky_cols <- attr(x, "sticky_cols")
  hidden_cols <- vec_slice(row.names(sticky_cols), !sticky_cols$show)
  x[!names(x) %in% hidden_cols]
}

restore_sticky_attrs <- function(x, to) {
  attrs <- attributes(to)

  sticky_cols <- attrs$sticky_cols
  sticky_attrs <- attrs$sticky_attrs

  class_tbl_df <- attrs$class_tbl_df
  class_grouped_df <- attrs$class_grouped_df
  class_rowwise_df <- attrs$class_rowwise_df

  attrs <- attrs[sticky_attrs]


  x <- exec(structure, x,
            sticky_cols = sticky_cols,
            sticky_attrs = sticky_attrs,
            !!!attrs,
            class_tbl_df = class_tbl_df,
            class_grouped_df = class_grouped_df,
            class_rowwise_df = class_rowwise_df)

  # add sticky class
  if (inherits(x, "grouped_df")) {
    class(x) <- unique(c(class_grouped_df, "sticky_grouped_df", class(x)))
  } else if (inherits(x, "rowwise_df")) {
    class(x) <- unique(c(class_rowwise_df, "sticky_rowwise_df", class(x)))
  } else if (inherits(x, "tbl_df")) {
    class(x) <- unique(c(class_tbl_df, "sticky_tbl_df", class(x)))
  }
  x
}
