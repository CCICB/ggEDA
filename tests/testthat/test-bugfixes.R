
test_that("adding more colours to colours_default OR a custom palette resolves too-many-levels issues", {
  # Error when category has too many levels for colours_default palette (by default colours_default has 6 colours)
  expect_snapshot(
    error = TRUE,
    ggstack(
      data.frame(Category = rep(c("A", "B", "C", "D", "E", "F", "G", "H"), 2)),
      maxlevels = 10
    )
  )

  # Error when category has too many levels for colours_default palette (explicitly setting colours_default to have only two colours)
  expect_snapshot(
    error = TRUE,
    ggstack(
      data.frame(Category = rep(c("A", "B", "C", "D", "E", "F", "G", "H"), 2)),
      maxlevels = 10,
      options = ggstack_options(colours_default = c("red", "black"))
    )
  )

  # Do not error if we fix the issue by adding a custom palette with enough colours for the problematic column.
  expect_no_error(
      ggstack(
        data.frame(Category = rep(c("A", "B", "C", "D", "E", "F", "G", "H"), 2)),
        maxlevels = 10,
        palettes = list("Category" = c(A="red", B="red", C="red", D="red", E="red", "F"="red", "G" = "red" ,"H" = "red"))
     )
    )

  # Do not error if we fix the issue by adding a default palette with enough colours for the problematic column
    expect_no_error(
      ggstack(
        data.frame(Category = rep(c("A", "B", "C", "D", "E", "F", "G", "H"), 2)),
        maxlevels = 10,
        options = ggstack_options(colours_default = rep("red", times = 8)))
    )
})
