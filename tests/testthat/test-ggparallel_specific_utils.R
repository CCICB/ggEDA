# example_data <- data.frame(
#   var1 = c(50, 30, 42, 46),
#   var2 = c(100, 115, 108, 108)
# )
#
# example_data <- data.frame(
#   var1 = c(42, 46),
#   var2 = c(108, 108)
# )

test_that("count_edge_crossings works", {
  expect_equal(count_edge_crossings(l = c(50, 30), r = c(100, 115)), 1)
  expect_equal(count_edge_crossings(l = c(50, 30, 40), r = c(100, 115, 40)), 2)
  expect_equal(count_edge_crossings(l = c(50, 30, 40), r = c(100, 115, 108)), 3)
})



test_that("no warnings when lines go to the same point (caused by rescaling issues)", {

  example_data <- data.frame(
    var1 = c(42, 46),
    var2 = c(108, 108)
  )

  expect_no_warning(ggparallel(example_data, verbose = FALSE))
})


test_that("fct_relevel_base moves levels and errors correctly", {
  f1 <- factor(c("a", "b", "c", "a"))
  # move "b" and "c" to front
  out1 <- fct_relevel_base(f1, "b", "c", after = 0)
  expect_equal(levels(out1), c("b", "c", "a"))
  # move "a" to after 1 (so it becomes second)
  out2 <- fct_relevel_base(f1, "a", after = 1)
  expect_equal(levels(out2), c("b", "a", "c"))
  # errors when x not factor
  expect_error(fct_relevel_base(1:3, "a"), "`x` must be a factor")
  # errors when moving absent level
  expect_error(fct_relevel_base(f1, "z"), "The following levels are not present")
})

test_that("is_crossing identifies intersections correctly", {
  expect_true(is_crossing(1, 4, 2, 3))
  expect_false(is_crossing(1, 2, 3, 4))
  # equal endpoints do not count as crossing
  expect_false(is_crossing(1, 2, 1, 2))
})

test_that("count_edge_crossings basic and mismatch lengths", {
  # simple two-line crossing
  expect_equal(count_edge_crossings(c(1,2), c(4,3)), 1)
  # three lines: all cross
  expect_equal(count_edge_crossings(c(1,2,3), c(3,2,1)), 3)
  # length mismatch error
  expect_error(count_edge_crossings(1:3, 1:2), "Length of the two axes")
})

test_that("count_all_edge_crossings full vs approximate vs recalibrate", {
  df <- data.frame(A = 1:5, B = 5:1, C = c(2,3,1,5,4))
  full <- count_all_edge_crossings(df)
  expect_s3_class(full, "data.frame")
  expect_equal(nrow(full), choose(3,2))
  # approximate with prop <1
  approx1 <- count_all_edge_crossings(df, approximate = TRUE, subsample_prop = 0.5)
  expect_equal(ncol(approx1), 3)
  # approximate with integer subsample
  approx2 <- count_all_edge_crossings(df, approximate = TRUE, subsample_prop = 3L)
  expect_equal(nrow(approx2), nrow(full))

  # recalibrate upsamples
  recal <- count_all_edge_crossings(df, approximate=TRUE, subsample_prop=0.8, recalibrate=TRUE)
  # all crossing counts should be >= approximate1
  expect_true(all(recal$crossings >= approx1$crossings))
})

test_that("dist_matrix_edge_crossings returns a symmetric matrix", {
  df <- data.frame(A=1:4, B=c(4,3,2,1))
  mat <- dist_matrix_edge_crossings(df)
  expect_true(inherits(mat, "matrix"))
  expect_true(isSymmetric(mat))
})


test_that("fct_infreq and fct_rev functionality", {
  v <- c("a","a","b","c","b")
  f_inf <- fct_infreq(v)
  expect_equal(levels(f_inf), c("a","b","c"))
  f_rev <- fct_rev(f_inf)
  expect_equal(levels(f_rev), rev(levels(f_inf)))
})

test_that("permutations and permute_axis_names generate correct sizes", {
  p3 <- permutations(3)
  expect_equal(nrow(p3), 6)
  expect_equal(ncol(p3), 3)
  axes <- c("X","Y","Z")
  pm <- permute_axis_names(axes)
  expect_equal(nrow(pm), 6)
})

test_that("feature_vector_to_total_path_distance sums correctly and errors", {
  mx <- matrix(1:4, 2, 2, dimnames = list(c("A","B"),c("A","B")))
  expect_equal(feature_vector_to_total_path_distance(c("A","B"), mx), mx["A","B"])
  expect_error(feature_vector_to_total_path_distance("A", mx), "Cannot sum distance")
})

test_that("create_distance_matrix builds square matrix and respects as.dist", {
  df <- data.frame(col1="A", col2="B", crossings=5)
  mat <- create_distance_matrix(df)
  expect_equal(dim(mat), c(2,2))
  expect_equal(mat["A","B"], 5)
  d <- create_distance_matrix(df, as.dist=TRUE)
  expect_true(inherits(d, "dist"))
  # missing columns error
  bad <- df[,1:2]; names(bad) <- c("x","y")
  expect_error(create_distance_matrix(bad), "must have columns")
})

test_that("mutinfo returns named vector and names when requested", {
  library(datasets)
  mf <- mutinfo(iris[1:3], iris$Species)
  expect_named(mf, c("Petal.Length","Sepal.Length", "Sepal.Width"))
  cols <- mutinfo(iris[1:3], iris$Species, return_colnames = TRUE)
  expect_type(cols, "character")
})

test_that("get_optimal_axis_order works across metrics", {
  df <- data.frame(A=1:5, B=5:1, C=c(2,4,3,1,5))
  # crossings
  ord1 <- get_optimal_axis_order(df, method="auto", metric="crossings", verbose=FALSE)
  expect_type(ord1, "character")
  # crossings_fast
  ord2 <- get_optimal_axis_order(df, method="auto", metric="crossings_fast", verbose=FALSE)
  expect_type(ord2, "character")
  # mutinfo
  ord3 <- get_optimal_axis_order(df, method="auto", metric="mutinfo", verbose=FALSE)
  expect_type(ord3, "character")
  # invalid metric
  expect_error(get_optimal_axis_order(df, metric="foo"), 'must be one of "mutinfo", ')
})
