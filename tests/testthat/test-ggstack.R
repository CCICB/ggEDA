# Mock Data
mock_data <- data.frame(
  ID = 1:10,
  Category = rep(c("A", "B", "C", "D", "E"), 2),
  Numeric = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  Logical = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE,TRUE, TRUE, FALSE),
  Category2 = c(rep("A", 7), rep("B", 3)),
  Tooltip = letters[1:10],
  stringsAsFactors = FALSE
)


test_that("ggEDA works as expected", {
  expect_error(suppressMessages(ggstack(mtcars)), NA)
})



test_that("ggEDA works even if col only has 1 valid numeric column", {
  df <- data.frame(
    col1 = c(0, 0, NA, NA, NA, NA, NA, NA, 0, NA),
    col2 = c(0, 0, 0, 0, 0)
  )
  expect_error(suppressWarnings(suppressMessages(ggstack(df, cols_to_plot = "col1"))), NA)
  expect_error(suppressMessages(ggstack(df, cols_to_plot = "col2")), NA)
})


test_that("ggEDA throws error if no plottable columns", {
  expect_error(ggstack(data.frame(), verbose = 0), "No plottable columns found")
})


cli::test_that_cli("ggEDA doesn't warn about columns the user isn't interested in", configs = "plain", {
  # We first define a data-frame which includes a 'Letters' column that has way too many
  # levels for ggEDA to plot. If we try it will warn that it drops
  df <- data.frame(
    ID = 1:19,
    Glasses = c(
      TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE,
      FALSE, FALSE, TRUE, TRUE, FALSE, NA, NA, FALSE
    ),
    Letters = c(
      "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
      "M", "N", "O", "P", "Q", "R", "S"
    )
  )

  # Check the appropriate warning is thrown
  suppressMessages(expect_message(ggstack(df, verbose = 2), "Columns with too many unique values: Letters"))

  # If user only wants to plot glasses, there's no reason to warn about Letters
  suppressMessages(expect_no_message(ggstack(df, cols_to_plot = c("Glasses"), verbose = 2), message = "Columns with too many unique values:"))
})

# Core Tests
test_that("ggEDA returns a plot object", {
  result <- expect_no_error(ggstack(data = mock_data, col_id = "ID", verbose = FALSE))
  expect_s3_class(result, "girafe") # Assuming the output is interactive by default
})

test_that("ggEDA handles missing col_id gracefully", {
  result <- expect_no_error(ggstack(data = mock_data, verbose = FALSE))
  expect_s3_class(result, "girafe")
})

test_that("ggEDA filters columns based on maxlevels", {
  result <- ggstack(data = mock_data, maxlevels = 4, return = "column_info", verbose = FALSE)
  expect_false("Category" %in% result$colnames[result$plottable])
})

test_that("ggEDA applies ignore_column_regex", {
  data <- mock_data
  colnames(data)[2] <- "IgnoreMe_ignore"
  result <- ggstack(data = data, return = "column_info", verbose = FALSE)
  expect_false("IgnoreMe_ignore" %in% result$colnames[result$plottable])
})

test_that("ggEDA does not check any regex when ignore_column_regex = NULL", {
  data <- mock_data
  colnames(data)[2] <- "Do_NOT_IgnoreMe_ignore"

  # Runs without error
  expect_no_error(ggstack(data = data, ignore_column_regex = NULL, return = "column_info", verbose = FALSE))

  # Gets the expected result
  result <- ggstack(data = data, ignore_column_regex = NULL, return = "column_info", verbose = FALSE)
  expect_true("Do_NOT_IgnoreMe_ignore" %in% result$colnames[result$plottable])
})

test_that("ggEDA limits the number of plottable columns", {
  data <- mock_data
  for (i in 1:20) {
    data[[paste0("Col", i)]] <- rnorm(10)
  }
  expect_message(
    ggstack(data = data, max_plottable_cols = 10, verbose = FALSE),
    "Autoplotting > 10 fields by `ggEDA` is not recommended"
  )
})

test_that("ggEDA validates palettes input", {
  palettes <- list(
    Category = c(A = "red", B = "green", C = "blue", D = "black", E = "purple")
  )
  result <- ggstack(data = mock_data, palettes = palettes,return = "column_info", verbose = FALSE)
  expect_equal(result$palette[[which(result$colnames == "Category")]], palettes$Category)

  # Throws error if missing colours for any values
  palettes_incomplete <- list(
    Category = c(A = "red", B = "green", C = "blue", D = "black", "purple")
  )
  expect_error(ggstack(data = mock_data, palettes = palettes_incomplete, verbose = FALSE), "missing 1 required name: `E`")
})


test_that("ggEDA raises error for invalid column inputs", {
  expect_error(ggstack(data = mock_data, col_sort = "NonExistentCol", verbose = FALSE), "Column `NonExistentCol` does not exist in your dataset. Please set the `col_sort` argument to a valid column name.")
  expect_error(ggstack(data = mock_data, col_id = "NonExistentCol", verbose = FALSE), "Column `NonExistentCol` does not exist in your dataset. Please set the `col_id` argument to a valid column name.")
})


test_that("ggEDA returns column information when return='column_info'", {
  result <- ggstack(data = mock_data, return = "column_info", verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("colnames", "coltype", "plottable", "palette") %in% colnames(result)))
})

test_that("ggEDA returns processed data when return='data'", {
  result <- ggstack(data = mock_data, return = "data", verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(all(colnames(mock_data) %in% colnames(result)))
})

test_that("ggEDA handles logical columns with default logical colors", {
  result <- ggstack(data = mock_data, return = "column_info", verbose = FALSE)
  expect_equal(
    result$palette[[which(result$colnames == "Logical")]],
    c("TRUE" = "#648fff", "FALSE" = "#dc267f")
  )
})


test_that("ggEDA gracefully handles non-plottable datasets", {
  data <- data.frame(category = LETTERS)
  expect_error(ggstack(data = data, verbose = FALSE), "No plottable columns found")
})


# Edge Cases
test_that("ggEDA warns about too many unique levels in categorical data", {
  data <- data.frame(ID = 1:10, TooManyLevels = as.factor(1:10))
  suppressMessages(expect_message(
    ggstack(data = data, maxlevels = 5, return = "column_info", verbose = TRUE),
    "must have <= 5 unique values"
  ))
})


test_that("ggEDA can handle interactive and static plot settings", {
  interactive_plot <- ggstack(data = mock_data, interactive = TRUE, verbose = FALSE)
  expect_s3_class(interactive_plot, "girafe")

  static_plot <- ggstack(data = mock_data, interactive = FALSE, verbose = FALSE)
  expect_s3_class(static_plot, "ggplot")
})

#
test_that("ggEDA correctly applies column tooltips", {
  data <- mock_data
  colnames(data)[5] <- "Category_tooltip"
  result <- ggstack(data = data, return = "column_info", verbose = FALSE)
  expect_equal(result$coltooltip[[which(result$colnames == "Category")]], "Category_tooltip")
})

test_that("ggEDA heirarchical sort works", {
  expect_no_error(
    ggstack(data = mock_data, col_sort = c("Category2", "Logical"), verbose = FALSE)
  )

  expect_snapshot(
    ggstack(data = mock_data, col_sort = c("Category2", "Logical"), return = "data", verbose = FALSE)
  )
})

test_that("ggEDA heirarchical sort works", {
  expect_no_error(
    ggstack(data = mock_data, col_sort = c("Category2", "Logical"), verbose = FALSE)
  )

  sorted_result <- ggstack(data = mock_data, col_sort = c("Category2", "Logical"),  order_matches_sort = TRUE, return = "data", verbose = FALSE)
  sorted_result_order_does_not_match_sort <- ggstack(data = mock_data, col_sort = c("Category2", "Logical"),  order_matches_sort = FALSE, return = "data", verbose = FALSE)

  # Since order_matches_sort = TRUE first two columns should be the ones we sort on
  expect_equal(colnames(sorted_result)[1:2], c("Category2", "Logical"))

  # If order_matches_sort = FALSE, column order should stay the same
  expect_equal(colnames(sorted_result_order_does_not_match_sort)[1:2], colnames(mock_data)[1:2])

  # Snapshot the heirarchically sorted result
  expect_snapshot(
    sorted_result
  )

  # Snapshot the heirarchically sorted result
  expect_snapshot(
    ggstack(data = mock_data, col_sort = c("Category2", "Logical"), sort_type = "alphabetical", desc = FALSE,  order_matches_sort = TRUE, return = "data", verbose = FALSE)
  )

  # Snapshot with desc = FALSE
  expect_snapshot(
    ggstack(data = mock_data, col_sort = c("Category2", "Logical"), sort_type = "alphabetical", desc = TRUE,  order_matches_sort = TRUE, return = "data", verbose = FALSE)
  )
})


test_that("ggEDA column reorder works", {
  data <- data.frame(a=LETTERS[1:5], b=LETTERS[6:10], c=LETTERS[11:15])


  # By default column order should not change
  expect_named(
    ggstack(data = data, interactive = FALSE, return = "data", verbose = FALSE)[1:3],
    expected = c("a", "b", "c")
  )

  # When cols_to_plot is supplied, reorder columns to match the supplied vector
  expect_named(
    ggstack(data = data, cols_to_plot = c("c", "b"), interactive = FALSE, return = "data", verbose = FALSE)[1:3],
    expected = c("c", "b", "a")
  )


  # When cols_to_plot & col_sort is supplied, first order by cols_to_plot, then reorder by col_sort
  expect_named(
    ggstack(
      data = data, cols_to_plot = c("c", "b", "a"),
      interactive = FALSE, col_sort = "b", order_matches_sort = TRUE,
      return = "data", verbose = FALSE
    )[1:3],
    expected = c("b", "c", "a")
  )

  # When cols_to_plot & col_sort is supplied but order_matches_sort=FALSE, order cols based on cols_to_plot alone
  expect_named(
    ggstack(
      data = data, cols_to_plot = c("c", "b", "a"),
      interactive = FALSE, col_sort = "b", order_matches_sort = FALSE,
      return = "data", verbose = FALSE
    )[1:3],
    expected = c("c", "b", "a")
  )

})

test_that("convert_binary_numeric_to_factor converts numeric columns with only 0, 1, and NA to factors", {
  # Input data with a numeric column containing 0, 1, and NA
  data <- data.frame(
    col1 = c(0, 1, 0, 1, NA),
    col2 = c("A", "B", "C", "D", "E")
  )

  # Test with convert_binary_numeric_to_factor = TRUE
  result <- ggstack(
    data = data,
    return = "data",
    convert_binary_numeric_to_factor = TRUE,
    verbose = FALSE
  )

  # Check that the column is converted to a factor
  expect_true(is.factor(result$col1))
  expect_equal(levels(result$col1), c("0", "1"))

  # Check that other columns are unaffected
  expect_true(is.character(result$col2))
})

test_that("convert_binary_numeric_to_factor does not convert numeric columns when FALSE", {
  # Input data with a numeric column containing 0, 1, and NA
  data <- data.frame(
    col1 = c(0, 1, 0, 1, NA),
    col2 = c("A", "B", "C", "D", "E")
  )

  # Test with convert_binary_numeric_to_factor = FALSE
  result <- ggstack(
    data = data,
    return = "data",
    convert_binary_numeric_to_factor = FALSE,
    verbose = FALSE
  )

  # Check that the column remains numeric
  expect_true(is.numeric(result$col1))

  # Check that other columns are unaffected
  expect_true(is.character(result$col2))
})

test_that("convert_binary_numeric_to_factor skips non-0-1 numeric columns", {
  # Input data with a numeric column containing non-0-1 values
  data <- data.frame(
    col1 = c(0, 1, 0, 1, NA),
    col2 = c(0, 2, 3, 4, 5)  # This column should not be converted
  )

  # Test with convert_binary_numeric_to_factor = TRUE
  result <- ggstack(
    data = data,
    return = "data",
    convert_binary_numeric_to_factor = TRUE,
    verbose = FALSE
  )

  # Check that the first column is converted to a factor
  expect_true(is.factor(result$col1))
  expect_equal(levels(result$col1), c("0", "1"))

  # Check that the second column remains numeric
  expect_true(is.numeric(result$col2))
})

test_that("convert_binary_numeric_to_factor handles edge cases gracefully", {
  # Input data with no 0-1 numeric columns
  data <- data.frame(
    col1 = c(2, 3, 4, 5, NA),
    col2 = c("A", "B", "C", "D", "E"),
    verbose = FALSE
  )

  # Test with convert_binary_numeric_to_factor = TRUE
  result <- ggstack(
    data = data,
    return = "data",
    convert_binary_numeric_to_factor = TRUE,
    verbose = FALSE
  )

  # Check that no columns are converted to factors
  expect_true(is.numeric(result$col1))
  expect_true(is.character(result$col2))
})

test_that("convert_binary_numeric_to_factor maintains column names", {
  # Input data
  data <- data.frame(
    id = 1:5,
    binary_col = c(0, 1, 0, 1, NA),
    numeric_col = c(10, 20, 30, 40, 50),
    verbose = FALSE
  )

  # Test with convert_binary_numeric_to_factor = TRUE
  result <- ggstack(
    data = data,
    col_id = "id",
    return = "data",
    convert_binary_numeric_to_factor = TRUE,
    verbose = FALSE
  )

  # Check column names are unchanged
  expect_setequal(colnames(result), colnames(data))
})

test_that("convert_binary_numeric_to_factor does NOT affect data if no numeric binary columns exist", {
  # Input data with a numeric column and a col_id column
  data <- data.frame(
    col_id = c(101, 102, 103, 104, 105),  # Should remain unchanged
    numeric_col = c(10, 20, 30, 40, 50)   # Should remain numeric
  )

  # Test with convert_binary_numeric_to_factor = TRUE
  result1 <- ggstack(
    data = data,
    return = "data",
    col_id = "col_id",
    convert_binary_numeric_to_factor = TRUE,
    verbose = FALSE
  )

  # Test with convert_binary_numeric_to_factor = FALSE
  result2 <- ggstack(
    data = data,
    return = "data",
    col_id = "col_id",
    convert_binary_numeric_to_factor = FALSE,
    verbose = FALSE
  )

  expect_identical(result1, result2)
})

test_that("convert_binary_numeric_to_factor does NOT affect col_id even if it only contains 0 & 1", {

  # Input data with a numeric column and a col_id column
  data <- data.frame(
    col_id = c(1, 0),  # Should remain unchanged
    numeric_col = c(10, 20)   # Should remain numeric
  )

  # Test with convert_binary_numeric_to_factor = TRUE and specifying col_id
  result1 <- ggstack(
    data = data,
    return = "data",
    col_id = "col_id",
    convert_binary_numeric_to_factor = TRUE,
    verbose = FALSE
  )


  result2 <- ggstack(
    data = data,
    return = "data",
    col_id = "col_id",
    convert_binary_numeric_to_factor = FALSE,
    verbose = FALSE
  )

  expect_identical(result1, result2)
})


test_that("ggstack renders numeric columns as heatmaps", {
  data <- data.frame(
    ID = 1:5,
    Score = c(10, 20, 30, 40, 50)
  )

  opts <- ggstack_options(
    numeric_plot_type = "heatmap",
    show_values_heatmap = TRUE,
    show_na_marker_heatmap = TRUE
  )

  expect_no_error(
    ggstack(
      data = data,
      col_id = "ID",
      options = opts,
      verbose = FALSE
    )
  )
})

test_that("ggstack heatmap handles NA values and shows labels", {
  data <- data.frame(
    ID = 1:4,
    Measure = c(10, NA, 30, 40)
  )

  opts <- ggstack_options(
    numeric_plot_type = "heatmap",
    show_values_heatmap = TRUE,
    show_na_marker_heatmap = TRUE,
    na_marker = "N/A"
  )

  expect_no_error(
    ggstack(
      data = data,
      col_id = "ID",
      options = opts,
      verbose = FALSE
    )
  )
})
