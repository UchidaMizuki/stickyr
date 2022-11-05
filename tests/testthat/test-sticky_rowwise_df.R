test_that("sticky_rowwise_df", {
  data <- new_sticky_tibble(list(col_1 = letters,
                                 col_2 = letters,
                                 col_3 = letters),
                            cols = col_1,
                            class = "my_tbl_df",
                            class_rowwise_df = "my_rowwise_df") |>
    rowwise()

  expect_s3_class(data["col_1"], "my_rowwise_df")
  expect_s3_class(data["col_2"], "rowwise_df")

  data_1 <- as_sticky_tibble(data)
  expect_s3_class(data_1, "my_tbl_df")

  data_1 <- ungroup(data)
  expect_s3_class(data_1, "my_tbl_df")

  expect_output(print(data), "Stickers: col_1")
})
