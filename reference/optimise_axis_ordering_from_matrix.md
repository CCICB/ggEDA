# Optimise the Ordering of Axes Using Distance Matrix

Finds an ordering of axes that minimises a pairwise distance metric
(usually the number of crossings). Offers brute-force and heuristic
approaches.

## Usage

``` r
optimise_axis_ordering_from_matrix(
  mx,
  method = c("auto", "brute_force", "repetitive_nn_with_2opt"),
  return_detailed = FALSE,
  verbose = TRUE
)
```

## Arguments

- mx:

  A matrix or `dist` object describing pairwise distances between axes.

- method:

  A character string specifying the method. Can be `"auto"`,
  `"brute_force"`, or `"repetitive_nn_with_2opt"`.

- return_detailed:

  Logical; if `TRUE`, returns a list with detailed results for
  debugging.

- verbose:

  Logical; if `TRUE`, prints progress messages.

## Value

If `return_detailed = FALSE`, returns a character vector of axis names
in the chosen order. Otherwise, returns a list with additional data.
