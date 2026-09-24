# adding more colours to colours_default OR a custom palette resolves too-many-levels issues

    Code
      ggstack(data.frame(Category = rep(c("A", "B", "C", "D", "E", "F", "G", "H"), 2)),
      maxlevels = 10)
    Message
      
      -- Running ggstack -------------------------------------------------------------
      
      -- Sorting 
      i Sorting X axis by: Order of appearance
    Condition
      Error in `FUN()`:
      ! Too many unique values in column to assign each a colour using the default palette. Either change the default palette to one that supports colours, reduce the number of levels in this column, or exclude it from the plotting using `cols_to_plot` argument OR maxlevels

---

    Code
      ggstack(data.frame(Category = rep(c("A", "B", "C", "D", "E", "F", "G", "H"), 2)),
      maxlevels = 10, options = ggstack_options(colours_default = c("red", "black")))
    Message
      
      -- Running ggstack -------------------------------------------------------------
      
      -- Sorting 
      i Sorting X axis by: Order of appearance
    Condition
      Error in `FUN()`:
      ! Too many unique values in column to assign each a colour using the default palette. Either change the default palette to one that supports colours, reduce the number of levels in this column, or exclude it from the plotting using `cols_to_plot` argument OR maxlevels

