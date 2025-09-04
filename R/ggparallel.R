#' Parallel Coordinate Plots
#'
#' Visualize relationships between numeric variables and categorical groupings
#' using parallel coordinate plots.
#'
#' @param data A data frame containing the variables to plot.
#' @param col_id The name of the column to use as an identifier. If `NULL`, artificial IDs will be generated based on row numbers. (character)
#' @param col_colour Name of the column to use for coloring lines in the plot. If `NULL`, no coloring is applied. (character)
#' @param highlight A level from `col_colour` to emphasize in the plot. Ignored if `col_colour` is not set. (character)
#' @param interactive Produce interactive ggiraph visualiastion (flag)
#' @param order_columns_by Strategy for ordering columns in the plot. Options include:
#'   \itemize{
#'     \item \strong{"appearance"}: Order columns by their order in `data` (default).
#'     \item \strong{"random"}: Randomly order columns.
#'     \item \strong{"auto"}: Automatically order columns based on context:
#'       \itemize{
#'         \item If `highlight` is set, columns are ordered to maximize separation between the highlighted level and all others, using mutual information.
#'         \item If `col_colour` is set but `highlight` is not, columns are ordered based on mutual information with all classes in `col_colour`.
#'         \item If neither `highlight` nor `col_colour` is set, columns are ordered to minimize the estimated number of crossings, using a repetitive nearest neighbour approach with two-opt refinement.
#'       }
#'   }
#' @param order_observations_by Strategy for ordering lines in the plot. Options include:
#'   \itemize{
#'     \item \strong{"frequency"}: Draw the largest groups first.
#'     \item \strong{"original"}: Preserve the original order in `data`.
#'   } Ignored if `highlight` is set.
#' @param verbose Logical; whether to display informative messages during execution. (default: `TRUE`)
#' @param palette_colour A named vector of colors for categorical levels in `col_colour`. (default: Set2 palette)
#' @param palette_highlight A two-color vector for highlighting (`highlight` and others). (default: `c("red", "grey90")`)
#' @param convert_binary_numeric_to_factor Logical; whether to convert numeric columns containing only 0, 1, and NA to factors. (default: `TRUE`)
#' @param scaling Method for scaling numeric variables. Options include:
#'   \itemize{
#'     \item \strong{"uniminmax"}: Rescale each variable to range \[0, 1\].
#'     \item \strong{"none"}: No rescaling. Use raw values.
#'   }
#' @param return What to return. Options include:
#'   \itemize{
#'     \item \strong{"plot"}: Return the ggplot object (default).
#'     \item \strong{"data"}: Return the processed data used for plotting.
#'   }
#' @param options A list of additional visualization parameters created by `ggparallel_options()`.
#'
#' @return A ggplot object or a processed data frame, depending on the `return` parameter.
#'
#' @examples
#' ggparallel(
#'   data = minibeans,
#'   col_colour = "Class",
#'   order_columns_by = "auto"
#' )
#'
#' ggparallel(
#'   data = minibeans,
#'   col_colour = "Class",
#'   highlight = "DERMASON",
#'   order_columns_by = "auto"
#' )
#'
#' # Customise appearance using options argument
#' ggparallel(
#'   data = minibeans,
#'   col_colour = "Class",
#'   order_columns_by = "auto",
#'   options = ggparallel_options(show_legend = FALSE)
#' )
#'
#' @export
#'
ggparallel <- function(
    data,
    col_id = NULL,
    col_colour = NULL,
    highlight = NULL,
    interactive = TRUE,
    order_columns_by = c("appearance", "random", "auto"),
    order_observations_by = c("frequency", "original"),
    verbose = TRUE,
    palette_colour = palette.colors(palette = "Set2"),
    palette_highlight = c("red", "grey90"),
    convert_binary_numeric_to_factor = TRUE,
    scaling = c("uniminmax", "none"),
    return = c("plot", "data"),
    options = ggparallel_options()
    ){

  # Assertions
  assertions::assert_dataframe(data)
  assertions::assert_class(options, "ggparallel_options", msg = "Value parsed to 'options' argument must be created by {.code ggparallel_options()}")

  # Argument Matching
  return <- rlang::arg_match(return)
  scaling <- rlang::arg_match(scaling)
  order_columns_by <- rlang::arg_match(order_columns_by)
  order_observations_by <- rlang::arg_match(order_observations_by)

  # Conditional assertions
  if(!is.null(col_colour)){
    assertions::assert_string(col_colour)
    assertions::assert_names_include(data, col_colour)

    # Format as factor
    data[[col_colour]] <- factor(data[[col_colour]])
  }
  else
    assertions::assert_null(highlight, msg = "Must specify a column to colour using {.arg col_colour} before choosing a level to highlight using the {.arg highlight} argument.")

  if(!is.null(highlight)){
    assertions::assert_non_null(col_colour, msg = "Must specify a column to colour using {.arg col_colour} before choosing a level to highlight using the {.arg highlight} argument.")
    colour_col_levels <- unique(data[[col_colour]])
    assertions::assert_subset(highlight, colour_col_levels, msg = "Could not find [{highlight}] in {col_colour} column. Please set highlight to a valid level in the {col_colour} column, for example [{colour_col_levels}]")

    #Update palette colour to highlight sample of interest
    boring_levels <- setdiff(colour_col_levels, highlight)
    palette_colour = c(palette_highlight[1], rep(palette_highlight[2], times = length(colour_col_levels)-1))
    names(palette_colour) <- c(highlight, boring_levels)
  }

  # Add col_id column if it user hasn't supplied one
  if (is.null(col_id)) {
    col_id_manually_specified <- FALSE
    col_id <- "DefaultID"
    data[[col_id]] <- seq_len(nrow(data))
  } else {
    col_id_manually_specified <- TRUE
    assertions::assert_string(col_id)
    assertions::assert_names_include(data, names = col_id, msg = "Column {.code {col_id}} does not exist in your dataset. Please set the {.arg col_id} argument to a valid column name.")
    assertions::assert_no_duplicates(data[[col_id]])
  }


  # Autoconvert numerics with only values 0, 1, NA to logicals
  if(convert_binary_numeric_to_factor){
    data <- convert_numerics_with_only_values_0_1_and_NA_to_logicals(data, exclude = col_id)
  }

  # Select only data we'll use to make the plot
  column_types <- coltypes(data, col_id = col_id)
  numeric_column_names <- colnames(data)[column_types == "numeric"]
  categorical_column_names <- colnames(data)[column_types == "categorical"]
  data <- data[,c(numeric_column_names ,col_id, col_colour)]

  assertions::assert_length_greater_than(numeric_column_names, length = 1, msg = "Parallel Coordinate Plots can only be drawn if dataset has >= 2 numeric features. Your dataset has {length(numeric_column_names)}. A ggEDA tileplot may be more appropriate.")

  if(verbose & is.null(col_colour) & length(categorical_column_names) > 0){
    cli::cli_alert_info("To add colour to plot set {.arg col_colour} to one of: {categorical_column_names}")
  }

  # Rescale Data (and add bounds attribute)
  data <- rescale(data, cols = numeric_column_names, method = scaling)

  # Extract bounds data.frame
  bounds <- attr(data, which = "bounds")

  # Pivot Numeric columns
  df_long <- pivot_numerics_longer(data, col_id = col_id, names_to = "Feature", values_to = "Value")

  # Convert Feature Column to factor
  df_long[["Feature"]] <- factor(df_long[["Feature"]])

  # Sort Feature Columns
  if(order_columns_by == "appearance"){
    if(verbose) cli::cli_alert_info("Ordering columns by their {.strong appearance} in the original dataset")
    df_long[["Feature"]] <- fct_relevel_base(df_long[["Feature"]], numeric_column_names)
  }
  else if(order_columns_by == "random"){
    if(verbose) cli::cli_alert_info("Ordering columns {.strong randomly}")
    df_long[["Feature"]] <- fct_relevel_base(
      df_long[["Feature"]],
      sample(
        x = numeric_column_names,
        size = length(numeric_column_names),
        replace = FALSE
        )
      )
  }
  else if(order_columns_by == "auto" & !is.null(highlight)){
    if(verbose) cli::cli_alert_info("Ordering columns based on how well they differentiate 1 group from the rest [{highlight}] (based on mutual information)")

    # Binarise target vector, 1=highlighted group, 0= any other subclass
    target <- ifelse(data[[col_colour]] == highlight, yes = 1, no = 0)
    cols_ranked_by_mutinfo <- mutinfo(
      features = data[!colnames(data) %in% c(col_id, col_colour)],
      target = target,
      return_colnames = TRUE
      )

    df_long[["Feature"]] <- fct_relevel_base(
      df_long[["Feature"]],
      cols_ranked_by_mutinfo
    )
  }
  else if(order_columns_by == "auto" & !is.null(col_colour) & is.null(highlight)){
    if(verbose) cli::cli_alert_info("Ordering columns based on mutual information with [{col_colour}]")
    cols_ranked_by_mutinfo <- mutinfo(
      features = data[!colnames(data) %in% c(col_id, col_colour)],
      target = data[[col_colour]],
      return_colnames = TRUE
    )

    df_long[["Feature"]] <- fct_relevel_base(
      df_long[["Feature"]],
      cols_ranked_by_mutinfo
    )
  }
  else if(order_columns_by == "auto" & is.null(col_colour)){
    if(verbose) cli::cli_alert_info("Ordering columns to minimise crossings")
    cols_ranked_to_minimise_crossings <- get_optimal_axis_order(
      data = data[!colnames(data) %in% c(col_id, col_colour)],
      verbose = verbose,
      metric = "crossings_fast",
      method = "auto"
    )

    df_long[["Feature"]] <- fct_relevel_base(
      df_long[["Feature"]],
      cols_ranked_to_minimise_crossings
    )
  }
  else
    stop("No implementation available for `order_columns_by = [", order_columns_by, "]`. Please create a new github issue to alert maintainer")

  # Set bounds 'Feature' column levels match df_long
  bounds[["Feature"]] <- factor(bounds[["Feature"]], levels = levels(df_long[["Feature"]]))

  # Early return data for debug
  if(return == "data")
    return(df_long)

  # Set up guide
  gl <- ggplot2::guide_legend(
    title.position = options$legend_title_position,
    title = if(options$beautify_text)  options$beautify_function(col_colour) else col_colour,
    nrow = options$legend_nrow,
    ncol = options$legend_ncol
  )

  # Ensure smallest group is drawn on top (or if highlight is set, highlighted level is drawn on top)
  if(!is.null(col_colour)){

    if(!is.null(highlight)){
      df_long[[col_colour]] <- fct_rev(fct_relevel_base(df_long[[col_colour]], highlight))
      df_long <- df_long[order(df_long[[col_colour]]), ]
      df_long[[col_id]] <- fct_inorder(df_long[[col_id]])
    }
    else if(order_observations_by == "frequency"){
      df_long[[col_colour]] <- fct_infreq(df_long[[col_colour]])
      df_long <- df_long[order(df_long[[col_colour]]), ]
      df_long[[col_id]] <- fct_inorder(df_long[[col_id]])
    }
  }

  # Plot Data
  gg <- ggplot(
    data = df_long,
    aes(
      x = .data[["Feature"]],
      y = .data[["Value"]],
      colour = if(!is.null(col_colour)) .data[[col_colour]] else NULL,
      fill = if(!is.null(col_colour)) .data[[col_colour]] else NULL,
      group = .data[[col_id]],
    )) +
    ggplot2::coord_cartesian(clip = 'off')

  # Add bounds
  if(options$show_bounds_rect){
    gg <- gg + ggplot2::geom_linerange(
      inherit.aes = FALSE,
      # width = 0.1,
      data = bounds,
      linewidth = 7,
      colour = "grey90",
      mapping = aes(x = .data[["Feature"]], ymin = .data[["min_scaled"]], ymax = .data[["max_scaled"]])
      )
  }

  # Add Bounds Labels (min & Max
  if(options$show_bounds_labels){

    gg <- gg + ggplot2::geom_label(
      inherit.aes = FALSE,
      data = bounds,
      mapping = aes(x = .data[["Feature"]], y = .data[["max_scaled"]], label = round(max, digits = options$max_digits_bounds)),
      vjust = -0.4,
      fontface = "bold"
    ) +
    ggplot2::geom_label(
      inherit.aes = FALSE,
      data = bounds,
      mapping = aes(x = .data[["Feature"]], y = .data[["min_scaled"]], label = round(min, digits = options$max_digits_bounds)),
      vjust = 1.4,
      fontface = "bold"
    )
  }

  # Add Line
  gg <- gg + ggiraph::geom_line_interactive(
    data = df_long,
    alpha = options$line_alpha,
    linetype = options$line_type,
    hover_nearest = FALSE,
    linewidth = options$line_width,
    aes(
      tooltip = .data[[col_id]], data_id = .data[[col_id]], group = .data[[col_id]]
      )
  )

  # Add Points
  if(options$show_points){
    gg <- gg + geom_point(
      alpha = 1
    ) +
      ggplot2::scale_fill_manual(
        values = palette_colour,
        labels = if(options$beautify_values) options$beautify_function else ggplot2::waiver()
      )

  }

  # Configure Legend, Scales and other Options
  gg <- gg + ggplot2::guides(
    colour = gl,
    fill = gl
    ) +
    xlab(NULL) +
    ggplot2::scale_colour_manual(
      values = palette_colour,
      labels = if(options$beautify_values) options$beautify_function else ggplot2::waiver()
      ) +
    ggplot2::scale_x_discrete(
      position = "top",
      labels = if(options$beautify_text) options$beautify_function else ggplot2::waiver()
    ) +
    ggplot2::scale_y_continuous(expand = c(0.2, 0)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.title = if(options$show_legend_titles) element_text(hjust = 0.5) else element_blank(),
      legend.position = options$legend_position,
      legend.key.size = ggplot2::unit(options$legend_key_size, units = "lines"),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = options$x_axis_gridlines,
      axis.text.x.top = if (options$show_column_names) element_text(
        face = "bold", color = "black", size = options$fontsize_x_axis_text,
        angle = options$x_axis_text_angle,
        hjust = options$x_axis_text_hjust,
        vjust = options$x_axis_text_vjust
        )
      else element_blank()
      )

  if(interactive){
    if (verbose) cli::cli_alert_info("Making plot interactive since `interactive = TRUE`")
    gg <- ggiraph::girafe(
      ggobj = gg,
      width_svg = options$interactive_svg_width,
      height_svg = options$interactive_svg_height,
      options = list(
        ggiraph::opts_hover(css = "stroke-opacity: 1; stroke-width: 3;cursor:pointer;stroke:red;"),
        ggiraph::opts_hover_inv(css = "stroke-opacity: 0.15;"),
        ggiraph::opts_selection(css = "stroke-opacity: 1; stroke-width: 3;stroke:red;", type = "single", only_shiny = FALSE),
        ggiraph::opts_selection_inv(css = "opacity: 0.5")
      )
      )

  }

  return(gg)
}

# Takes a dataframe and pivots all numeric columns longer
pivot_numerics_longer <- function(data, col_id, names_to = "Feature", values_to = "Value"){

  # Identify type of each column (categorical / numeric / id)
  types <- coltypes(col_id = col_id, data = data)

  # Grab Numeric Column names
  numeric_col_names <- colnames(data)[types == "numeric"]

  # Lengthen the numeric columns
  df_long <- stats::reshape(
    as.data.frame(data), # Must be a data.frame and not a tibble here for stats::reshape to behave appropriately
    direction = "long",
    varying = numeric_col_names,
    v.names = values_to,
    times    = numeric_col_names,
    timevar = names_to,
    idvar = ".unused_id_col"
  )

  # Drop rownames
  rownames(df_long) <- NULL

  return(df_long)
}


# Rescale data but also add a 'bounds' attribute containing a data.frame
# with each rescaled features min and max value (both original and rescaled)
# Access this with attr(data, which = "bounds")
rescale <- function(data, cols, method = c("none", "uniminmax")){
  method <- rlang::arg_match(method)

  # Get bounds (original)
  bounds_original <- get_bounds_info(data, cols = cols)

  if(method == "none"){
    # do nothing
  }

  if(method == "uniminmax"){
    # Univariately scale each column in cols to set min value zero and max value of 1
    data[cols] <- lapply(data[cols], uniminmax)
  }

  # Get bounds (rescaled)
  bounds_rescaled <- get_bounds_info(data, cols = cols, suffix = "_scaled")

  # Merge old and rescaled bounds data
  bounds <- cbind(bounds_original, bounds_rescaled[2:ncol(bounds_rescaled)])

  # Add bounds as attribute
  attr(data, which = "bounds") <- bounds

  # Return rescaled data
  return(data)
}

# Rescale a vector so that its min is 0 and max is 1
uniminmax <- function(vec){

  # If theres only 1 value, or 1 unique value, return 0.5
  if(length(unique(vec)) == 1) return(rep(0.5, times = length(unique(vec))))

  # Move floor to zero
  floor_zero <- vec - min(vec, na.rm = TRUE)

  # Set max value to 1 (divide by self)
  floor_zero/max(floor_zero, na.rm = TRUE)
}

get_bounds_info <- function(data, cols, suffix = ""){

  ls_bounds <- lapply(
    data[cols],
    FUN = function(vec){
      c(
        min = min(vec, na.rm = TRUE),
        max = max(vec, na.rm = TRUE)
        )
      }
   )

  df_bounds <- as.data.frame(t(as.data.frame(ls_bounds)))

  # Optionally add a suffix to allow rescale function to have `min_scaled` and `max_scaled` values
  colnames(df_bounds) <- paste0(colnames(df_bounds), suffix)
  df_bounds[["Feature"]] <- cols
  rownames(df_bounds) <- NULL

  df_bounds <- df_bounds[c("Feature", setdiff(colnames(df_bounds), "Feature"))]

  return(df_bounds)
}
