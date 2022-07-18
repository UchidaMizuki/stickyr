test_that("group_by", {
  data <- new_tbl_main(list(col_1 = letters,
                            col_2 = letters,
                            col_3 = letters),
                       main_col_names = c("col_1", "col_2"),
                       main_col_show = list(col_2 = FALSE),
                       attr_1 = "attr_1",
                       main_attr_names = "attr_1",
                       class = "tbl_main_1",
                       class_grouped = "grouped_main_1")

  data |>
    group_by(col_1) |>
    attributes()
})
