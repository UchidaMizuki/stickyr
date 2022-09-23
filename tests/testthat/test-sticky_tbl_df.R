test_that("new_sticky_tibble", {
  x <- list(col_1 = letters,
            col_2 = letters)

  # cols
  expect_output(
    new_sticky_tibble(x,
                      cols = col_1)
  )

  expect_error(
    new_sticky_tibble(x,
                      cols = col_3)
  )

  # col_summary
  expect_output(
    new_sticky_tibble(x,
                      cols = col_1,
                      col_summary = list(col_1 = vec_init))
  )

  expect_error(
    new_sticky_tibble(x,
                      cols = col_1,
                      col_summary = list(col_3 = vec_init))
  )

  # col_show
  expect_output(
    new_sticky_tibble(x,
                      cols = col_1,
                      col_show = col_1)
  )

  expect_error(
    new_sticky_tibble(x,
                      cols = col_1,
                      col_show = col_3)
  )
})
