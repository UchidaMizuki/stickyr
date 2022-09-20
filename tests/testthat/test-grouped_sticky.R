test_that("group_by", {
  data <- new_tbl_sticky(list(col_1 = letters,
                              col_2 = letters,
                              col_3 = letters),
                         sticky_col_names = c("col_1", "col_2"),
                         sticky_col_show = list(col_2 = FALSE),
                         attr_1 = "attr_1",
                         sticky_attr_names = "attr_1",
                         class = "tbl_sticky_1",
                         class_grouped = "grouped_sticky_1")

  data |>
    group_by(col_1) |>
    attributes()
})
