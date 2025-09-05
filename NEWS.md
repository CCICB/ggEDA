# ggEDA 0.2.0

* **Improved handling of infinite values:** Infinite values are now shown in bar plots as vertical arrows (up/down depending on whether positive or negative). 
Their size is set by the `na_marker_size` size to match '!' character used for missing data. 
For heatmap visualisations, tile colour of infinite values are clamped to the min/max using scales::oob_squish_infinite. 
Arrow markers are optionally added when `show_na_marker_heatmap = TRUE`

* `line_width` of ggparallel now defaults to 0.5 instead of NULL. 

* `cols_to_plot` now dictates order of stacking unless `col_sort` argument is supplied and `order_matches_sort=TRUE`.

* Added `beautify_values` and `beautify_function` argument to `ggparallel_options()`
to provide more control over what text is beautified (made more human readable), and how it is reformatted. 

* Added `beautify_values` and `beautify_function` argument to `ggstack_options()`
to provide more control over what text is beautified (made more human readable), and how it is reformatted. 

# ggEDA 0.1.0

* Added a `NEWS.md` file to track changes to the package.

* Added parallel coordinate plots (`ggparallel()`) and stacked one-dimensional visualisations (`ggstack()`)
