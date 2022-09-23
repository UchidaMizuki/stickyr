test_that("as_sticky_tibble.sticky_grouped_df", {
  data <- new_sticky_tibble(list(col_1 = letters,
                                 col_2 = letters,
                                 col_3 = letters),
                            class = "my_tbl_df",
                            class_grouped_df = "my_grouped_df") |>
    group_by(col_1, col_2) |>
    as_sticky_tibble()

  expect_true(all(c("my_tbl_df", "sticky_tbl_df") %in% class(data)))
})
