test_that("slice", {
  data <- new_sticky_tibble(list(col_1 = letters,
                                 col_2 = letters))

  data_1 <- data |>
    slice(1)

  expect_true(inherits(data_1, "sticky_tbl_df"))

  data_2 <- data |>
    group_by(col_1) |>
    slice(1)

  expect_true(inherits(data_2, "sticky_grouped_df"))

  data_3 <- data |>
    rowwise(col_1) |>
    slice(1)

  expect_true(inherits(data_3, "sticky_rowwise_df"))
})
