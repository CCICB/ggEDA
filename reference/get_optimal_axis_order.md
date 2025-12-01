# Optimize Axis Ordering Directly from a Data Frame

Computes the number of edge crossings between all numeric columns in
`data`, converts this information into a distance matrix, and then
determines an optimal ordering of the columns based on the specified
method.

## Usage

``` r
get_optimal_axis_order(
  data,
  verbose = TRUE,
  method = "auto",
  metric = c("mutinfo", "crossings", "crossings_fast"),
  return_detailed = FALSE
)
```

## Arguments

- data:

  A `data.frame` or `tibble` containing the dataset. Only numeric
  columns are considered for edge crossing calculations.

- verbose:

  A logical value; if `TRUE`, prints progress messages.

- method:

  A character string specifying the method. Options are `"auto"`,
  `"brute_force"`, or `"repetitive_nn_with_2opt"`.

- metric:

  which metric should take as the distance between axes to minimise.
  mutual information: minimise mutual distance (1- uniminmax of mutinfo
  similarity matrix calculated by emp) crossings: minimise the total
  number of edge crossings (warning: slow to compute for large
  datasets). crossings_fast: same as above but calculates crossings on a
  subset of data (100 rows)

- return_detailed:

  A logical; if `TRUE`, returns a list with additional data (e.g.,
  intermediate calculations) for debugging.

## Value

A character vector of axis names in the chosen order, or a list with
additional data if `return_detailed = TRUE`.
