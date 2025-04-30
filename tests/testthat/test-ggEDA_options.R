test_that("ggstack_options returns a list with the correct structure", {
  # Runs without error
  expect_no_error(ggstack_options())

  opts <- ggstack_options()

  # Class Checks
  expect_type(opts, "list")
  expect_s3_class(opts, "ggstack_options")
})

test_that("ggstack_options handles legend_nrow and legend_ncol conflict", {
  # Warn user they constrained both legend rows and columns (and that only legend_ncol will be used)
  expect_warning(ggstack_options(legend_nrow = 2, legend_ncol = 3), regexp = "[bB]oth.* were supplied")


  opts <- suppressWarnings(ggstack_options(legend_nrow = 2, legend_ncol = 3))

  expect_null(opts$legend_nrow)
  expect_equal(opts$legend_ncol, 3)
})

test_that("ggstack_options validates argument types correctly", {
  expect_error(ggstack_options(show_legend = "not a boolean"), "show_legend")
  expect_error(ggstack_options(legend_key_size = "not a number"), "legend_key_size")
})

test_that("ggstack_options warns if legend_nrow and legend_ncol are both set", {
  expect_warning(ggstack_options(legend_nrow = 2, legend_ncol = 3), "legend_nrow")
})


test_that("ggstack_options correctly matches argument values", {
  opts <- ggstack_options(legend_position = "left", transform_heatmap = "log10")
  expect_equal(opts$legend_position, "left")
  expect_equal(opts$transform_heatmap, "log10")

  expect_error(ggstack_options(legend_position = "middle"), "legend_position")
  expect_error(ggstack_options(transform_heatmap = "logarithm"), "transform_heatmap")
})
