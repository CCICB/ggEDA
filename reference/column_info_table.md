# Parse a tibble and ensure it meets standards

Parse a tibble and ensure it meets standards

## Usage

``` r
column_info_table(
  data,
  maxlevels = 6,
  col_id = NULL,
  cols_to_plot,
  tooltip_column_suffix = "_tooltip",
  ignore_column_regex = "_ignore$",
  palettes,
  colours_default,
  colours_default_logical,
  verbose
)
```

## Arguments

- data:

  data.frame to autoplot (data.frame)

- maxlevels:

  for categorical variables, what is the maximum number of distinct
  values to allow (too many will make it hard to find a palette that
  suits). (number)

- col_id:

  name of column to use as an identifier. If null, artificial IDs will
  be created based on row-number.

- cols_to_plot:

  names of columns in **data** that should be plotted. By default plots
  all valid columns (character)

- tooltip_column_suffix:

  the suffix added to a column name that indicates column should be used
  as a tooltip (string)

- ignore_column_regex:

  a regex string that, if matches a column name, will cause that column
  to be excluded from plotting (string). If NULL no regex check will be
  performed. (default: "\_ignore\$")

- palettes:

  A list of named vectors. List names correspond to **data** column
  names (categorical only). Vector names to levels of columns. Vector
  values are colours, the vector names are used to map values in data to
  a colour.

- colours_default:

  Default colors for categorical variables without a custom palette.

- colours_default_logical:

  Colors for binary variables: a vector of three colors representing
  `TRUE`, `FALSE`, and `NA` respectively (character).

- verbose:

  Numeric value indicating the verbosity level:

  - **2**: Highly verbose, all messages.

  - **1**: Key messages only.

  - **0**: Silent, no messages.

## Value

tibble with the following columns:

1.  colnames

2.  coltype (categorical/numeric/tooltip/invalid)

3.  ndistinct (number of distinct values)

4.  plottable (should this column be plotted)

5.  tooltip_col (the name of the column to use as the tooltip) or NA if
    no obvious tooltip column found
