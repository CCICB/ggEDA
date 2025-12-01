# Determine Whether Two Edges Cross

Given the positions of two edges on the left and right axes, decides if
they intersect in a parallel coordinates setup.

## Usage

``` r
is_crossing(l1, r1, l2, r2)
```

## Arguments

- l1:

  Numeric position of the first edge on the left axis.

- r1:

  Numeric position of the first edge on the right axis.

- l2:

  Numeric position of the second edge on the left axis.

- r2:

  Numeric position of the second edge on the right axis.

## Value

A logical value. `TRUE` if they cross, `FALSE` otherwise.
