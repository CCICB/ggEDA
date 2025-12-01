# Compute the Total Path Distance for an Axis Order

Given a sequence of axis names and a distance matrix, sums pairwise
distances along the path.

## Usage

``` r
feature_vector_to_total_path_distance(axis_names, mx)
```

## Arguments

- axis_names:

  A character vector indicating the axis order.

- mx:

  A matrix of distances, with row and column names matching
  `axis_names`.

## Value

A numeric value representing the total distance.
