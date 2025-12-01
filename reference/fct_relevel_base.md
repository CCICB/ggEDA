# Relevel Factor by Specified Levels

Reorder the levels of a factor by moving specified levels to a new
position.

## Usage

``` r
fct_relevel_base(x, ..., after = 0)
```

## Arguments

- x:

  A factor to be releveled.

- ...:

  Levels to move in the factor.

- after:

  A numeric scalar specifying the position after which the moved levels
  should be placed. Use `0` to place them at the front.

## Value

A factor with the specified levels moved to the chosen position.
