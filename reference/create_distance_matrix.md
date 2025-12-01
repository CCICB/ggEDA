# Create a Distance Matrix from Edge Crossing Data

Converts the results of
[`count_all_edge_crossings()`](https://ccicb.github.io/ggEDA/reference/count_all_edge_crossings.md)
into a distance matrix, where each entry represents the number of
crossings between two columns.

## Usage

``` r
create_distance_matrix(data, as.dist = FALSE)
```

## Arguments

- data:

  A data frame with columns `col1`, `col2`, and `crossings`.

- as.dist:

  Logical; if `TRUE`, converts the matrix to a `dist` object.

## Value

A square matrix of distances, or a `dist` object if `as.dist = TRUE`.
