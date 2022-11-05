test_that("summarise", {
  data <- new_sticky_tibble(list(col_1 = letters,
                                 col_2 = letters,
                                 col_3 = letters),
                            cols = col_3,
                            class = "my_tbl_df")

  data_1 <- data |>
    summarise()
  expect_equal(names(data_1), "col_3")

  data_1 <- data |>
    group_by(col_1, col_2) |>
    summarise(.groups = "drop")
  expect_equal(names(data_1), c("col_1", "col_2", "col_3"))

  data_1 <- data |>
    rowwise(col_1, col_2) |>
    summarise(.groups = "drop")
  expect_equal(names(data_1), c("col_1", "col_2", "col_3"))
})
