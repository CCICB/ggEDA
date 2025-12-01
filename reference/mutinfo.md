# Compute Mutual Information

Computes mutual information between each feature in the `features` data
frame and the `target` vector. The features are discretized using the
"equalfreq" method from
[`infotheo::discretize()`](https://rdrr.io/pkg/infotheo/man/discretize.html).

## Usage

``` r
mutinfo(features, target, return_colnames = FALSE)
```

## Arguments

- features:

  A data frame of features. These will be discretized using the
  "equalfreq" method (see
  [`infotheo::discretize()`](https://rdrr.io/pkg/infotheo/man/discretize.html)).

- target:

  A vector (character or factor) representing the variable to compute
  mutual information with.

- return_colnames:

  Logical; if `TRUE`, returns the column names from `features` ordered
  by their mutual information with `target` (highest to lowest). If
  `FALSE`, returns mutual information values. (default: `FALSE`)

## Value

If `return_colnames = FALSE`, a named numeric vector of mutual
information scores is returned (one for each column in `features`),
sorted in descending order. The names of the vector correspond to the
column names of `features`. If `return_colnames = TRUE`, only the
ordered column names of `features` are returned.

## Examples

``` r
data(iris)
# Compute mutual information scores
mutinfo(iris[1:4], iris[[5]])
#>  Petal.Width Petal.Length Sepal.Length  Sepal.Width 
#>    0.8328724    0.8278344    0.4292148    0.2499608 

# Get column names ordered by mutual information with target column (most mutual info first)
mutinfo(iris[1:4], iris[[5]], return_colnames = TRUE)
#> [1] "Petal.Width"  "Petal.Length" "Sepal.Length" "Sepal.Width" 
```
