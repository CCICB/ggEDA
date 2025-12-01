# Count Edge Crossings in Parallel Coordinates

Calculates the total number of edge crossings between two numeric
vectors in a 2-column parallel coordinates setup. Each axis represents
one of the columns.

## Usage

``` r
count_edge_crossings(l, r)
```

## Arguments

- l:

  A numeric vector representing values on the left axis. Must have the
  same length as `r`.

- r:

  A numeric vector representing values on the right axis. Must have the
  same length as `l`.

## Value

An integer indicating the total number of edge crossings.

## Details

An edge crossing occurs when two edges intersect between the axes.
Formally, edges \\(l\[i\], r\[i\])\\ and \\(l\[j\], r\[j\])\\ cross if
\\(l\[i\] - l\[j\]) \* (r\[i\] - r\[j\]) \< 0\\.
