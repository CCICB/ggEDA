# Count Edge Crossings for All Numeric Column Pairs

Computes the total number of edge crossings between all pairs of numeric
columns in a given dataset.

## Usage

``` r
count_all_edge_crossings(
  data,
  approximate = FALSE,
  subsample_prop = 0.4,
  recalibrate = FALSE
)
```

## Arguments

- data:

  A `data.frame` or `tibble` containing the dataset. Only numeric
  columns are considered for edge crossing calculations.

- approximate:

  estimate crossings based on a subsample of the data. See
  `subsample_prop` for details.

- subsample_prop:

  only used when approximate = TRUE. If 0-1, controls the proportion of
  data to be sampled to speed up computation. If a whole number other
  than 0 or 1, represents the number of rows subsampled

- recalibrate:

  when approximating crossings via subsetting, should number of
  crossings calculated for the subsample be upscaled to match the full
  count. (turned off by default since it amplifies sampling error).

## Value

A `data.frame` with three columns:

- col1:

  The name of the first column in the pair.

- col2:

  The name of the second column in the pair.

- crossings:

  Total number of edge crossings for that pair.

## Details

The function:

1.  Filters the input data to retain only numeric columns.

2.  Computes all possible pairs of numeric columns.

3.  Uses
    [`count_edge_crossings()`](https://ccicb.github.io/ggEDA/reference/count_edge_crossings.md)
    to calculate crossings for each pair.

4.  Returns the results in a summarized data frame.
