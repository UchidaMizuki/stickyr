test_that("select", {
  data <- new_sticky_tibble(dplyr::starwars,
                            c(name, height, mass))

  data_1 <- data |>
    dplyr::select(hair_color)

  expect_true(all(c("name", "height", "mass") %in% names(data_1)))

  data_2 <- data |>
    group_by(name) |>
    dplyr::select(hair_color)
})
