test_that("new_tbl_main", {
  x <- list(col_1 = letters,
            col_2 = letters)

  expect_error(
    new_tbl_main(x,
                 main_col_names = "col_3")
  )

  expect_error(
    new_tbl_main(x,
                 main_col_names = "col_1",
                 main_col_summary = list(col_3 = vec_init))
  )

  expect_error(
    new_tbl_main(x,
                 main_col_names = "col_1",
                 main_col_summary = list(col_1 = 1))
  )

  expect_error(
    new_tbl_main(x,
                 main_col_names = "col_1",
                 main_col_show = list(col_3 = TRUE))
  )

  expect_error(
    new_tbl_main(x,
                 main_col_names = "col_1",
                 main_col_show = list(col_1 = "zzz"))
  )

  data <- new_tbl_main(x,
                       main_col_names = "col_1",
                       attr_1 = "attr_1",
                       main_attr_names = "attr_1")
  attrs <- attributes(data)

  expect_equal(attrs$main_col_names, "col_1")
  expect_equal(names(attrs$main_col_summary), "col_1")
  expect_equal(names(attrs$main_col_show), "col_1")
  expect_equal(attrs$main_attr_names, "attr_1")
  expect_equal(attrs$attr_1, "attr_1")
})
