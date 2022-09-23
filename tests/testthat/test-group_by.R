test_that("group_by", {
  data <- new_sticky_tibble(list(col_1 = letters,
                                 col_2 = letters,
                                 col_3 = letters),
                            class_grouped_df = "my_grouped_df") |>
    group_by(col_1, col_2)

  expect_true(all(c("my_grouped_df", "sticky_grouped_df", "grouped_df") %in% class(data)))
})

