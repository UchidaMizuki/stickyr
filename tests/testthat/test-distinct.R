test_that("distinct", {
  data <- new_sticky_tibble(list(col_1 = letters,
                                 col_2 = letters,
                                 col_3 = letters),
                            cols = col_3,
                            class = "my_tbl_df")

  expect_no_error(data |>
                    distinct(col_1, col_3))

  expect_no_error(data |>
                    group_by(col_1) |>
                    distinct(col_3))
})

