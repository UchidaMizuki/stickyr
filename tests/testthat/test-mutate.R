test_that("mutate", {
  data <- new_sticky_tibble(list(col_1 = letters,
                                 col_2 = letters,
                                 col_3 = letters),
                            cols = col_3,
                            class = "my_tbl_df")

  data_1 <- data |>
    mutate(col_4 = col_3,
           .keep = "unused")
  expect_equal(names(data_1), c("col_1", "col_2", "col_4", "col_3"))
})
