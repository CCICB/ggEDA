# Data for testing
simple_df <- data.frame(
  id = 1:3,
  a = c(1, 2, 3),
  b = c(2, 4, 6),
  group = factor(c("x", "y", "x"))
)



test_that("ggparallel runs without error using expected inputs", {
  expect_no_error({
    ggparallel(
      data = simple_df,
      col_id = "id",
      col_colour = "group",
      interactive = FALSE,
      verbose=FALSE
    )
  })
})


# Assertion tests for ggparallel input validation
test_that("errors if data is not a data.frame", {
  expect_error(
    ggparallel(data = 1),
    regexp = "must be a data.frame, not a numeric"
  )
})


test_that("errors if options is not correct class", {
  expect_error(
    ggparallel(data = simple_df, options = list()),
    regexp = "Value parsed to 'options' argument must be created by `ggparallel_options()`",
    fixed=TRUE
  )
})


test_that("errors for missing col_colour when highlight is given", {
  expect_error(
    ggparallel(data = simple_df, highlight = "x"),
    regexp = "Must specify a column to colour"
  )
})


test_that("errors for invalid col_colour name", {
  expect_error(
    ggparallel(data = simple_df, col_colour = "not_exist"),
    regexp = "'data' is missing 1 required name: `not_exist`",
    fixed=TRUE
  )
})


test_that("errors for duplicate col_id values", {
  df_dup <- simple_df
  df_dup$id[2] <- df_dup$id[1]
  expect_error(
    ggparallel(data = df_dup, col_id = "id"),
    regexp = "'data[[col_id]]' must have no duplicates! Found 1 duplicated value: 1",
    fixed=TRUE
  )
})

# Specific tests for helper functions

test_that("uniminmax scales vector to [0,1]", {
  v <- c(10, 20, 30)
  expect_equal(uniminmax(v), c(0, 0.5, 1))
})


test_that("get_bounds_info returns correct bounds", {
  bounds <- get_bounds_info(simple_df, cols = c("a", "b"))
  expect_equal(bounds$min, c(1, 2))
  expect_equal(bounds$max, c(3, 6))
  expect_equal(bounds$Feature, c("a", "b"))
})


test_that("rescale with uniminmax adds bounds attribute and rescales", {
  scaled <- rescale(simple_df, cols = c("a", "b"), method = "uniminmax")
  expect_true(!is.null(attr(scaled, "bounds")))
  bnds <- attr(scaled, "bounds")
  expect_equal(bnds$min_scaled, c(0, 0))
  expect_equal(bnds$max_scaled, c(1, 1))
})


test_that("pivot_numerics_longer reshapes data correctly", {
  long_df <- pivot_numerics_longer(simple_df, col_id = "id")
  # Expect two numeric columns * 3 rows = 6 rows
  expect_equal(nrow(long_df), 6)
  expect_true(all(c("Feature", "Value") %in% names(long_df)))
})

# General plotting assessment: ensure no errors and correct object types

test_that("ggparallel returns data frame when return = 'data'", {
  df_out <- ggparallel(
    data = simple_df,
    col_id = "id",
    col_colour = "group",
    return = "data",
    interactive = FALSE,
    verbose = FALSE
  )
  expect_s3_class(df_out, "data.frame")
})


test_that("ggparallel returns ggplot object when interactive = FALSE", {
  p <- ggparallel(
    data = simple_df,
    col_id = "id",
    col_colour = "group",
    interactive = FALSE,
    return = "plot",
    verbose = FALSE
  )
  expect_s3_class(p, "ggplot")
})


test_that("ggparallel returns girafe object when interactive = TRUE", {
  p_int <- ggparallel(
    data = simple_df,
    col_id = "id",
    col_colour = "group",
    interactive = TRUE,
    return = "plot",
    verbose = FALSE
  )
  # girafe objects inherit class 'girafe'
  expect_true(inherits(p_int, "girafe"))
})

test_that("errors if highlight is not a level in col_colour column", {
  df <- data.frame(
    id = 1:4,
    x = c(1, 2, 3, 4),
    y = c(2, 3, 4, 5),
    group = factor(c("A", "A", "B", "B"))
  )

  expect_error(
    ggparallel(
      data = df,
      col_id = "id",
      col_colour = "group",
      highlight = "Z",
      interactive = FALSE,
      verbose=FALSE
    ),
    regexp = "Could not find \\[Z\\] in group column"
  )
})

test_that("no error when valid highlight level is used", {
  df <- data.frame(
    id = 1:4,
    x = c(1, 2, 3, 4),
    y = c(2, 3, 4, 5),
    group = factor(c("A", "A", "B", "B"))
  )

  expect_no_error({
    ggparallel(
      data = df,
      col_id = "id",
      col_colour = "group",
      highlight = "A",
      order_columns_by = "auto",
      interactive = FALSE,
      verbose=FALSE
    )
  })
})

test_that("ggparallel runs with different order_columns_by settings", {
  df <- data.frame(
    id = 1:5,
    x = c(1, 2, 3, 4, 5),
    y = c(2, 4, 6, 8, 10),
    z = c(3, 6, 9, 12, 15),
    group = factor(c("A", "B", "A", "B", "A"))
  )

  expect_no_error(ggparallel(df, col_id = "id", col_colour = "group", order_columns_by = "appearance", interactive = FALSE, verbose = FALSE))
  expect_no_error(ggparallel(df, col_id = "id", col_colour = "group", order_columns_by = "random", interactive = FALSE, verbose = FALSE))
  expect_no_error(ggparallel(df, col_id = "id", col_colour = "group", highlight = "A", order_columns_by = "auto", interactive = FALSE, verbose = FALSE))
  expect_no_error(ggparallel(df, col_id = "id", col_colour = "group", order_columns_by = "auto", interactive = FALSE, verbose = FALSE))
})


test_that("ggparallel runs with bounds rect and bounds labels enabled", {
  df <- data.frame(
    id = 1:5,
    x = c(1, 2, 3, 4, 5),
    y = c(5, 4, 3, 2, 1),
    group = factor(c("A", "A", "B", "B", "A"))
  )

  opts <- ggparallel_options(
    show_bounds_rect = TRUE,
    show_bounds_labels = TRUE,
    max_digits_bounds = 1
  )

  expect_no_error(
    ggparallel(
      data = df,
      col_id = "id",
      col_colour = "group",
      interactive = FALSE,
      verbose = FALSE,
      options = opts
    )
  )
})

test_that("ggparallel supports line width, type and point rendering", {
  df <- data.frame(
    id = 1:4,
    x = c(1, 2, 3, 4),
    y = c(4, 3, 2, 1),
    group = factor(c("A", "B", "A", "B"))
  )

  opts <- ggparallel_options(
    show_points = TRUE,
    line_width = 2,
    line_type = "dashed",
    line_alpha = 0.7
  )

  expect_no_error(
    ggparallel(
      data = df,
      col_id = "id",
      col_colour = "group",
      interactive = FALSE,
      verbose = FALSE,
      options = opts
    )
  )
})

test_that("ggparallel handles all visualisation options simultaneously", {
  df <- data.frame(
    id = 1:3,
    a = c(1, 2, 3),
    b = c(4, 5, 6),
    class = factor(c("C1", "C2", "C1"))
  )

  opts <- ggparallel_options(
    show_bounds_rect = TRUE,
    show_bounds_labels = TRUE,
    show_points = TRUE,
    line_width = 1.5,
    line_type = "solid",
    line_alpha = 0.6,
    max_digits_bounds = 2
  )

  expect_no_error(
    ggparallel(
      data = df,
      col_id = "id",
      col_colour = "class",
      interactive = FALSE,
      verbose = FALSE,
      options = opts
    )
  )
})



