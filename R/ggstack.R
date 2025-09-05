utils::globalVariables(".data")

#' AutoPlot an entire data.frame
#'
#' Visualize all columns in a data frame with ggEDA's vertically aligned plots
#' and automatic plot selection based on variable type.
#' Plots are fully interactive, and custom tooltips can be added.
#'
#' @param data data.frame to autoplot (data.frame)
#' @param maxlevels for categorical variables, what is the maximum number of distinct values to allow (too many will make it hard to find a palette that suits). (number)
#' @param verbose Numeric value indicating the verbosity level:
#'   \itemize{
#'     \item \strong{2}: Highly verbose, all messages.
#'     \item \strong{1}: Key messages only.
#'     \item \strong{0}: Silent, no messages.
#'   }
#' @param col_id name of column to use as an identifier. If null, artificial IDs will be created based on row-number.
#' @param col_sort name of columns to sort on. To do a hierarchical sort, supply a vector of column names in the order they should be sorted (character).
#' @param order_matches_sort should the column plots be stacked top-to-bottom in the order they appear in \code{col_sort} (flag)
#' @param drop_unused_id_levels if col_id is a factor with unused levels, should these be dropped or included in visualisation
#' @param interactive produce interactive ggiraph visualiastion (flag)
#' @param return a string describing what this function should return. Options include:
#'   \itemize{
#'     \item \strong{plot}: Return the ggEDA visualisation (default)
#'     \item \strong{colum_info}: Return a data.frame describing the columns the dataset.
#'     \item \strong{data}: Return the processed dataset used for plotting.
#'   }
#' @param palettes A list of named vectors. List names correspond to \strong{data} column names (categorical only). Vector names to levels of columns. Vector values are colours, the vector names are used to map values in data to a colour.
#' @param sort_type controls how categorical variables are sorted.
#' Numerical variables are always sorted in numerical order irrespective of the value given here.
#' Options are `alphabetical` or `frequency`
#' @param desc sort in descending order (flag)
#' @param limit_plots throw an error when there are > \code{max_plottable_cols} in dataset (flag)
#' @param max_plottable_cols maximum number of columns that can be plotted (default: 10) (number)
#' @param cols_to_plot names of columns in \strong{data} that should be plotted. By default plots all valid columns (character)
#' @param tooltip_column_suffix the suffix added to a column name that indicates column should be used as a tooltip (string)
#' @param ignore_column_regex a regex string that, if matches a column name,  will cause that  column to be excluded from plotting (string). If NULL no regex check will be performed. (default: "_ignore$")
#' @param convert_binary_numeric_to_factor  If a numeric column conatins only values 0, 1, & NA, then automatically convert to a factor.
#' @param options a list of additional visual parameters created by calling [ggstack_options()]. See \code{\link{ggstack_options}} for details.
#'
#' @return ggiraph interactive visualisation
#'
#' @examples
#'
#' # Create Basic Plot
#' ggstack(baseballfans, col_id = "ID", col_sort = "Glasses")
#'
#' # Configure plot ggstack_options()
#' ggstack(
#'   lazy_birdwatcher,
#'   col_sort = "Magpies",
#'   palettes = list(
#'     Birdwatcher = c(Robert = "#E69F00", Catherine = "#999999"),
#'     Day = c(Weekday = "#999999", Weekend = "#009E73")
#'   ),
#'   options = ggstack_options(
#'     show_legend = TRUE,
#'     fontsize_barplot_y_numbers = 12,
#'     legend_text_size = 16,
#'     legend_key_size = 1,
#'     legend_nrow = 1,
#'   )
#' )
#'
#' @export
#'
ggstack <- function(
    data, col_id = NULL, col_sort = NULL,
    order_matches_sort = TRUE,
    maxlevels = 7,
    verbose = 2,
    drop_unused_id_levels = FALSE,
    interactive = TRUE,
    return = c("plot", "column_info", "data"),
    palettes = NULL,
    sort_type = c("frequency", "alphabetical"),
    desc = TRUE,
    limit_plots = TRUE,
    max_plottable_cols = 10,
    cols_to_plot = NULL,
    tooltip_column_suffix = "_tooltip",
    ignore_column_regex = "_ignore$",
    convert_binary_numeric_to_factor = TRUE,
    options = ggstack_options(show_legend = !interactive)) {
  # Data validation
  assertions::assert_dataframe(data)
  assertions::assert_number(maxlevels)
  assertions::assert_flag(drop_unused_id_levels)
  assertions::assert_flag(interactive)
  assertions::assert_flag(limit_plots)
  assertions::assert_flag(desc)
  assertions::assert_string(tooltip_column_suffix)
  if(!is.null(ignore_column_regex)) assertions::assert_string(ignore_column_regex)
  assertions::assert_class(options, "ggstack_options", msg = "The options argument must be created using {.code ggstack_options()}")
  assertions::assert_number(max_plottable_cols)
  assertions::assert_greater_than(max_plottable_cols, 0)
  assertions::assert_flag(order_matches_sort)
  assertions::assert_flag(convert_binary_numeric_to_factor)

  # Conditional checks for non-ggstack_options parameters
  if (!is.null(cols_to_plot)) assertions::assert_names_include(data, names = cols_to_plot)
  if (!is.null(palettes)) assertions::assert_list(palettes)
  if (!all(colnames(data) %in% names(palettes))) assertions::assert_greater_than_or_equal_to(length(options$colours_default), minimum = maxlevels)

  # Argument Matching
  sort_type <- rlang::arg_match(sort_type)
  return <- rlang::arg_match(return)


  # Formatting --------------------------------------------------------------
  cli::cli_div(theme = list(span.warn = list(color = "yellow", "font-weight" = "bold")))
  cli::cli_div(theme = list(span.success = list(color = "darkgreen", "font-weight" = "bold")))

  # Add a title message
  if (verbose >= 1) cli::cli_h1(options$cli_header)

  # Preprocessing -----------------------------------------------------------
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

  # Sort Order (rows/x-axis) -------------------------------------------------------------------
  # convert ID col to factor if not already
  if (!is.factor(data[[col_id]])) {
    data[[col_id]] <- as.factor(data[[col_id]])
  }

  if (verbose) cli::cli_h3("Sorting")

  if (is.null(col_sort)) { # Sort X axis by order of appearance
    if (verbose >= 1) cli::cli_alert_info("Sorting X axis by: Order of appearance")
  }
  else { # Sort X axis based on col_sort
    assertions::assert_character_vector(col_sort)
    assertions::assert_length_greater_than(col_sort, length = 0)
    assertions::assert_names_include(data, names = col_sort, msg = "Column {.code {col_sort}} does not exist in your dataset. Please set the {.arg col_sort} argument to a valid column name.")

    if (verbose >= 1) {
      cli::cli_bullets(c(
        "*" = "Sorting X axis by: {.strong {col_sort}}",
        "*" = "Order type: {.strong {sort_type}}",
        "*" = "Sort order: {.strong {ifelse(desc, 'descending', 'ascending')}}"
      ))
    }

    # Heirarchical Sort by specified columns
    ranks <- lapply(col_sort, function(column_to_sort_by){
      rank::smartrank(data[[column_to_sort_by]], sort_by = sort_type, desc = desc, verbose = FALSE)
    })

    order_hierarchical <- do.call(order, ranks)
    data <- data[order_hierarchical,]
    data[[col_id]] <- fct_inorder(data[[col_id]])
  }

  # Ordering Columns (decides order in which they're stacked). Does not affect sorting of samples
  # Order columns based on cols_to_plot
  if(!is.null(cols_to_plot)){
    data <- data[,c(cols_to_plot, setdiff(colnames(data), cols_to_plot))]
  }

  # Overwrite column order based on col_sort when supplied
  if(order_matches_sort & !is.null(col_sort)) {
    data <- data[, c(col_sort, setdiff(colnames(data), col_sort))]
  }


  # Autoconvert numerics with only values 0, 1, NA to logicals
  if(convert_binary_numeric_to_factor){
    data <- convert_numerics_with_only_values_0_1_and_NA_to_logicals(data, exclude = col_id)
  }


  # Identify Plottable Columns  ------------------------------------------------------------
  df_col_info <- column_info_table(
    data,
    maxlevels = maxlevels,
    col_id = col_id,
    cols_to_plot = cols_to_plot,
    tooltip_column_suffix = tooltip_column_suffix,
    palettes = palettes,
    colours_default = options$colours_default,
    colours_default_logical = options$colours_default_logical,
    ignore_column_regex = ignore_column_regex,
    verbose = verbose
  )

  # Debug Options: return column info or processed dataframe
  if (return == "column_info")
    return(df_col_info)
  else if(return == "data")
   return(data)
  else if(return != "plot")
    stop("No implementation has been written for debug = ", debug, ". Please create a github issue with this error message: https://github.com/CCICB/ggEDA/issues/new")

  # Plot --------------------------------------------------------------------
  if (verbose) cli::cli_h3("Generating Plot")
  n_plottable_cols <- sum(df_col_info$plottable == TRUE)
  plottable_cols <- df_col_info$colnames[df_col_info$plottable]

  if (verbose >= 1) {
    cli::cli_alert_info("Found {.strong {n_plottable_cols}} plottable columns in {.strong data}")
  }

  # Make sure theres not too many plottable cols
  if (limit_plots && n_plottable_cols > max_plottable_cols) {

    df_plottable_data <- data[, plottable_cols, drop=FALSE]

    # If col_sort is not null
    if(!is.null(col_sort)){
      mutinfo_vs_col_sort <- mutinfo(df_plottable_data[-which(plottable_cols %in% col_sort)], target = df_plottable_data[[col_sort[1]]])
      plottable_cols <- names(mutinfo_vs_col_sort)[seq_len(max_plottable_cols-length(col_sort))]
      plottable_cols <- c(col_sort, plottable_cols)
      cli::cli_alert_warning("Autoplotting > {max_plottable_cols} fields by `ggEDA` is not recommended (visualisation ends up very squished). Chossing the {max_plottable_cols}/{n_plottable_cols} plottable columns which maximise mutual information with `{col_sort}`. To show all plottable columns, set {.code limit_plots = FALSE}. Alternatively, manually choose which columns are plotted by setting `cols_to_plot`")
    }
    else {
      optimal_axis_order <- get_optimal_axis_order(data = df_plottable_data, metric = "mutinfo", verbose = FALSE)
      # Only consider the first {max_plottable_cols} columns plottable.
      plottable_cols <- optimal_axis_order[seq_len(max_plottable_cols)]
      col_sort <- plottable_cols[1]
      cli::cli_alert_warning("Autoplotting > {max_plottable_cols} fields by `ggEDA` is not recommended (visualisation ends up very squished). Choosing {max_plottable_cols}/{n_plottable_cols} plottable columns to maximise total mutual information. To show all plottable columns, set {.code limit_plots = FALSE}. Alternatively, manually choose which columns are plotted by setting `cols_to_plot`")
    }

    # Apply New plottable columns
    df_col_info$plottable <- df_col_info$colnames %in% plottable_cols

  }

  # Make sure theres at least 1 plottable column
  if (n_plottable_cols == 0) {
    cli::cli_abort("No plottable columns found")
  }

  gglist <- lapply(
    X = seq_len(nrow(df_col_info)),
    function(i) {
      colname <- df_col_info[["colnames"]][i]
      coltype <- df_col_info[["coltype"]][i]
      coltooltip <- df_col_info[["coltooltip"]][i]
      ndistinct <- df_col_info[["ndistinct"]][i]
      ndistinct_including_na <- df_col_info[["ndistinct_including_na"]][i]
      plottable <- df_col_info[["plottable"]][i]
      palette <- unlist(df_col_info[["palette"]][i])


      # Don't plot if not plottable
      if (!plottable) {
        # If column is the sorting Identifier - don't info about it
        if (colname == col_id) {
          return(NULL)
        }
        if (verbose >= 2) cli::cli_alert_warning("{.warn Skipping} column {.strong {colname}}")
        return(NULL)
      } else {
        if (verbose >= 2) cli::cli_alert_success("{.success Plotting} column {.strong {colname}}")
      }


      ## Create Interactive Geoms Aesthetics ---------------------------------------------
      if (!is.na(coltooltip)) {
        # If user specifies a custom tooltip using _tooltip suffix column
        # we just use that as the tooltip
        data[[coltooltip]] <- ifelse(is.na(data[[coltooltip]]), "", data[[coltooltip]])
        tooltip_text <- data[[coltooltip]]
      } else {
        # Construct the default tooltip
        tooltip_text <- paste0(
          tag_bold(colname), ": ", data[[colname]],

          # Only describe ID column if col_id was manually specified
          if (col_id_manually_specified) {
            paste0(
              "<br/>",
              tag_bold(col_id), ": ", data[[col_id]]
            )
          } else {
            ""
          }
        )
      }
      aes_interactive <- aes(
        data_id = .data[[col_id]],
        tooltip = tooltip_text
      )

      # Draw the actual plot
      ## Categorical -------------------------------------------------------------
      if (coltype == "categorical") {
        gg <- ggplot(
          data,
          aes(
            x = .data[[col_id]],
            y = if (options$beautify_text) beautify(colname) else colname,
            fill = .data[[colname]]
          )
        ) +
          ggiraph::geom_tile_interactive(mapping = aes_interactive, width = options$width, na.rm = TRUE) +
          {
            if (options$show_na_marker_categorical) {
              ggplot2::geom_text(
                data = function(x) {
                  x[is.na(x[[colname]]), , drop = FALSE]
                }, # only add text where value is NA
                aes(label = options$na_marker), size = options$na_marker_size, na.rm = TRUE, vjust = 0.5, color = options$na_marker_colour,
              )
            }
          } +
          ggplot2::scale_x_discrete(drop = drop_unused_id_levels) +
          ggplot2::guides(fill = ggplot2::guide_legend(
            title.position = options$legend_title_position,
            title = if (options$beautify_text) options$beautify_function(colname) else colname,
            nrow = min(ndistinct_including_na, options$legend_nrow),
            ncol = min(ndistinct_including_na, options$legend_ncol),
          )) +
          theme_categorical(
            show_legend_titles = options$show_legend_titles,
            show_legend = options$show_legend,
            legend_position = options$legend_position,
            legend_title_size = options$legend_title_size,
            legend_text_size = options$legend_text_size,
            legend_key_size = options$legend_key_size,
            vertical_spacing = options$vertical_spacing,
            fontsize_y_title = options$fontsize_y_title
          ) +
          ggplot2::ylab(if (options$beautify_text) options$beautify_function(colname) else colname) +
          ggplot2::scale_fill_manual(
            values = palette,
            na.value = options$colours_missing,
            labels = if(options$beautify_values) options$beautify_function else ggplot2::waiver()
          ) +
          ggplot2::scale_y_discrete(position = options$y_axis_position)
      }
      # Numeric Bar -------------------------------------------------------------------------
      else if (coltype == "numeric" && options$numeric_plot_type == "bar") {
        breaks <- sensible_3_breaks(data[[colname]], digits = options$max_digits_barplot_y_numbers)
        labels <- sensible_3_labels(
          data[[colname]],
          axis_label = if (options$beautify_text) beautify(colname) else colname,
          fontsize_y_title = options$fontsize_y_title,
          digits = options$max_digits_barplot_y_numbers
        )
        gg <- ggplot2::ggplot(data, aes(x = .data[[col_id]], y = .data[[colname]])) +
          ggiraph::geom_col_interactive(

            # Replace Inf with NA (avoids plotting bars for inf values since we later set na.rm = TRUE)
            data = function(x){
              x[is.infinite(x[[colname]]), colname] <- NA
              return(x)
              },
            mapping = aes_interactive, width = options$width, na.rm = TRUE
            ) +
          # Add '!' to indicate missing data
          ggplot2::geom_text(
            data = function(x) {
              x[is.na(x[[colname]]), , drop = FALSE]
            }, # only add text where value is NA
            aes(label = options$na_marker, y = 0), size = options$na_marker_size, na.rm = TRUE, vjust = -0.5, color = options$na_marker_colour
          ) +
          # Add '↑' or to indicate positive infinite values
          ggplot2::geom_text(
            data = function(x) {
              x[is.infinite(x[[colname]]), , drop = FALSE]
            }, # only add text where value is NA
            aes(label = ifelse(.data[[colname]] > 0, '\u2191', "\u2193"), y = 0),
            size = options$na_marker_size,
            na.rm = TRUE,
            vjust = -0.5,
            color = options$na_marker_colour
          ) +
          ggplot2::scale_x_discrete(drop = drop_unused_id_levels) +
          ggplot2::scale_y_continuous(
            breaks = breaks,
            labels = labels,
            position = options$y_axis_position,
            limits = c(breaks[3], breaks[1]),
            expand = c(0, 0)
          ) +
          theme_numeric_bar(vertical_spacing = options$vertical_spacing, fontsize_barplot_y_numbers = options$fontsize_barplot_y_numbers)
      }
      # Numeric Heatmap -------------------------------------------------------------------------
      else if (coltype == "numeric" && options$numeric_plot_type == "heatmap") {
        colname_formatted <- if (options$beautify_text) beautify(colname) else colname
        gg <- ggplot2::ggplot(data, aes(
          x = .data[[col_id]],
          y = colname,
          fill = .data[[colname]]
        )) +
          ggiraph::geom_tile_interactive(mapping = aes_interactive, width = options$width, na.rm = TRUE) +
          ggplot2::scale_x_discrete(drop = drop_unused_id_levels) +
          ggplot2::scale_y_discrete(position = options$y_axis_position) +
          {
            if (options$show_na_marker_heatmap) {
              # Add NA marker
              ggplot2::geom_text(
                data = function(x) {
                  x[is.na(x[[colname]]), , drop = FALSE]
                }, # only add text where value is NA
                aes(label = options$na_marker), size = options$na_marker_size, na.rm = TRUE, vjust = 0.5
              )
            }
          } +

          # Add Up / Down arrow for infinite values (only if options$show_na_marker_heatmap)
          {
            if (options$show_na_marker_heatmap) {
              ggplot2::geom_text(
                data = function(x) {
                  x[is.infinite(x[[colname]]), , drop = FALSE]
                }, # only add text where value is NA
                aes(label = ifelse(.data[[colname]] > 0, "\u2191",  "\u2193")),
                size = options$na_marker_size, na.rm = TRUE, vjust = 0.5
              )
            }
          } +
          {
            if (options$show_values_heatmap) {
              ggplot2::geom_text(
                aes(label = .data[[colname]]),
                size = options$fontsize_values_heatmap, color = options$colours_values_heatmap, na.rm = TRUE, vjust = 0.5
              )
            }
          } +
          ggplot2::ylab(colname_formatted) +
          theme_numeric_heatmap(
            show_legend_titles = options$show_legend_titles,
            show_legend = options$show_legend,
            legend_position = options$legend_position,
            legend_title_size = options$legend_title_size,
            legend_text_size = options$legend_text_size,
            legend_key_size = options$legend_key_size,
            vertical_spacing = options$vertical_spacing,
            fontsize_y_title = options$fontsize_y_title
          ) +
          ggplot2::scale_fill_gradient(
            low = options$colours_heatmap_low,
            high = options$colours_heatmap_high,
            na.value = options$colours_missing,
            trans = options$transform_heatmap,
            oob = scales::squish_infinite,
            guide = ggplot2::guide_colorbar(
              direction = if (options$legend_orientation_heatmap == "horizontal") "horizontal" else "vertical",
              title.position = "top",
              title = if (!options$show_legend_title) NULL else colname_formatted,
              title.hjust = 0
            )
          )
      } else {
        cli::cli_abort("Unsure how to plot coltype: {coltype}")
      }
      return(gg)
    }
  )
  names(gglist) <- df_col_info[["colnames"]]

  # Remove null columns
  gglist <- gglist[!vapply(gglist, is.null, logical(1))]

  # Align only axes (not labels)
  gglist <- lapply(gglist, FUN = function(p) {
    patchwork::free(p, type = "label")
  })

  # Get relative heights for plots (make numeric variables taller)
  relheights <- ifelse(
    df_col_info$coltype[df_col_info$plottable] == "numeric",
    yes = options$relative_height_numeric,
    no = 1
  )

  # Align Plots Vertically --------------------------------------------------
  if (verbose >= 2) cli::cli_alert_info("Stacking plots vertically")

  ggpatch <- patchwork::wrap_plots(
    gglist,
    ncol = 1,
    heights = relheights,
    guides = if (options$show_legend & options$legend_position %in% c("bottom", "top")) "collect" else NULL
  )

  if(options$legend_position %in% c("bottom", "top"))
    ggpatch <- ggpatch & theme(legend.position = if(options$show_legend) options$legend_position else "none")

  # Interactivity -----------------------------------------------------------
  if (interactive) {
    if (verbose >= 2) cli::cli_alert_info("Making plot interactive since `interactive = TRUE`")
    ggpatch <- ggiraph::girafe(
      ggobj = ggpatch,
      width_svg = options$interactive_svg_width,
      height_svg = options$interactive_svg_height,
      options = list(
        opts_hover = ggiraph::opts_hover(css = "stroke:black;cursor:pointer;r:5px;")
      )
    )
  } else {
    if (verbose >= 2) cli::cli_alert_info("Rendering static plot. For interactive version set `interactive = TRUE`")
  }

  # Return -----------------------------------------------------------
  return(ggpatch)
}


#' Parse a tibble and ensure it meets standards
#'
#' @inheritParams ggstack
#' @inheritParams ggstack_options
#'
#' @return tibble with the following columns:
#' 1) colnames
#' 2) coltype (categorical/numeric/tooltip/invalid)
#' 3) ndistinct (number of distinct values)
#' 4) plottable (should this column be plotted)
#' 4) tooltip_col (the name of the column to use as the tooltip) or NA if no obvious tooltip column found
#'
#'
column_info_table <- function(data, maxlevels = 6, col_id = NULL, cols_to_plot, tooltip_column_suffix = "_tooltip", ignore_column_regex = "_ignore$", palettes, colours_default, colours_default_logical, verbose) {
  # Assertions
  assertions::assert_string(col_id)
  assertions::assert_names_include(data, col_id)

  # Create Column Info Data
  df_column_info <- data.frame(
    colnames = colnames(data),
    coltype = coltypes(data, col_id),
    coltooltip = coltooltip(data, tooltip_column_suffix),
    ndistinct = colvalues(data),
    anymissing = colmissingness(data)
  )
  df_column_info[["ndistinct_including_na"]] <- df_column_info[["ndistinct"]] + df_column_info[["anymissing"]]

  # Warn if unknown file type in table
  if (c("invalid") %in% df_column_info[["coltype"]]) cli::cli_warn('The following columns will not be plotted due to invalid column types: {df_column_info$colnames[df_column_info$coltype=="invalid"]}')

  # Mark columns as not plottable if
  # 1) they are a categorical variable with more than `maxlevels` distinct values
  # 2) Their coltype is 'invalid', 'id', or 'tooltip'
  # 3) The`cols_to_plot` variable is suppplied and column names are NOT in the list of cols_to_plot
  # 4) Their colnames match the _ignore suffix
  lgl_too_many_levels <- df_column_info$coltype == "categorical" & df_column_info$ndistinct > maxlevels

  df_column_info[["plottable"]] <-
    !lgl_too_many_levels & !df_column_info$coltype %in% c("invalid", "id", "tooltip") &
      (is.null(cols_to_plot) | df_column_info$colnames %in% c(cols_to_plot))

  # Only check colnames_match ignore_column_regex suffix if not NULL
  if(!is.null(ignore_column_regex))
    df_column_info[["plottable"]] <- df_column_info[["plottable"]] & (!grepl(x = df_column_info$colnames, pattern = ignore_column_regex))

  if (sum(lgl_too_many_levels) > 0) {
    char_cols_with_too_many_levels <- df_column_info$colnames[lgl_too_many_levels]
    # Only comment about those columns the user wants to plot
    if (!is.null(cols_to_plot)) char_cols_with_too_many_levels <- char_cols_with_too_many_levels[char_cols_with_too_many_levels %in% cols_to_plot]
    char_cols_with_too_many_levels_formatted <- paste0(char_cols_with_too_many_levels, " (", df_column_info$ndistinct[lgl_too_many_levels], ")")
    if (verbose & length(char_cols_with_too_many_levels > 0)) cli::cli_alert_warning("{.strong Categorical columns} must have {.strong <= {maxlevels} unique values} to be visualised. Columns with too many unique values: {.strong {char_cols_with_too_many_levels_formatted}}")
  }

  # Add palette colours
  df_column_info$palette <- choose_colours(
    data,
    palettes = palettes,
    plottable = df_column_info$plottable,
    ndistinct = df_column_info$ndistinct,
    coltype = df_column_info$coltype,
    colours_default = colours_default,
    colours_default_logical = colours_default_logical
  )

  # Return table describing
  return(df_column_info)
}


coltooltip <- function(data, tooltip_column_suffix) {
  vapply(
    colnames(data),
    function(name) {
      colnames(data)[match(paste0(name, tooltip_column_suffix), colnames(data))]
    },
    character(1)
  )
}

coltypes <- function(data, col_id) {
  # First Pass of coltypes
  char_coltypes <- vapply(data, FUN = function(vec) {
    if (is.character(vec) | is.factor(vec) | is.logical(vec)) {
      return("categorical")
    } else if (is.numeric(vec)) {
      return("numeric")
    } else {
      return("invalid")
    }
  }, FUN.VALUE = character(1))

  # Overwrite coltype to tooltip if `_tooltip` suffix is found
  char_colnames <- colnames(data)
  tooltip_suffix <- "_tooltip"
  has_tooltip_in_name <- grepl(x = char_colnames, pattern = tooltip_suffix, ignore.case = TRUE)
  without_tooltip_has_matched_name <- gsub(x = char_colnames, pattern = tooltip_suffix, replacement = "") %in% char_colnames
  is_tooltip_col <- has_tooltip_in_name & without_tooltip_has_matched_name

  char_coltypes <- ifelse(is_tooltip_col, yes = "tooltip", no = char_coltypes)

  # Overwrite coltype to 'id' if colname == col_id
  char_coltypes <- ifelse(char_colnames == col_id, yes = "id", no = char_coltypes)

  return(char_coltypes)
}

colvalues <- function(data) {
  vapply(data, FUN = function(vec) {
    length(unique(stats::na.omit(vec)))
  }, FUN.VALUE = numeric(1))
}

colmissingness <- function(data) {
  vapply(data, FUN = function(vec) {
    anyNA(vec)
  }, FUN.VALUE = logical(1))
}

choose_colours <- function(data, palettes, plottable, ndistinct, coltype, colours_default, colours_default_logical) {
  assertions::assert_character(colours_default)

  colors <- lapply(seq_len(ncol(data)), FUN = function(i) {
    colname <- colnames(data)[[i]]
    is_plottable <- plottable[[i]]
    is_lgl <- is.logical(data[[colname]])

    if (!is_plottable | coltype[i] != "categorical") {
      return(NULL)
    } else if (colname %in% names(palettes)) {
      colors <- unlist(palettes[[colname]])
      assertions::assert_names_include(colors, names = stats::na.omit(unique(data[[colname]])))
      return(palettes[[colname]])
    } else if (is_lgl) {
      colors <- colours_default_logical
    } else {
      assertions::assert(length(colours_default) >= ndistinct[i], msg = "Too many unique values in column to assign each a colour using the default palette. Either change the default palette to one that supports colours, reduce the number of levels in this column, or exclude it from the plotting using `cols_to_plot` argument OR maxlevels")
      colors <- colours_default
    }
  })

  return(colors)
}





theme_categorical <- function(fontsize_y_title = 12, show_legend = TRUE, show_legend_titles = FALSE, legend_position = "right", legend_title_size = NULL, legend_text_size = NULL, legend_key_size = 0.3, vertical_spacing = 0) {
  ggplot2::theme_minimal() %+replace%

    ggplot2::theme(
      panel.grid = element_blank(),
      axis.text.y.left = element_blank(),
      axis.text.y.right = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = fontsize_y_title, angle = 0),
      axis.title.y.right = element_text(size = fontsize_y_title, angle = 0),
      legend.title = if (show_legend_titles) element_text(size = legend_title_size, face = "bold", hjust = 0) else element_blank(),
      legend.justification = c(0, 0.5),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.location = "panel",
      legend.text = element_text(size = legend_text_size, vjust = 0.5),
      legend.position = if (show_legend) legend_position else "none",
      strip.placement = "outside",
      # plot.background = ggplot2::element_rect(color = "red"),
      # panel.background = ggplot2::element_rect(color = "black"),
      # legend.box.background = ggplot2::element_rect(color = "green"),
      # legend.key = ggplot2::element_rect(colour = "red"),
      legend.key.size = ggplot2::unit(legend_key_size, "lines"),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      legend.key.spacing.y = ggplot2::unit(2, "pt"),
      plot.margin = ggplot2::margin(t = 0, r = 0, b = vertical_spacing, l = 0, unit = "pt"),
    )
}

theme_numeric_bar <- function(vertical_spacing = 0, fontsize_barplot_y_numbers = 8) {
  ggplot2::theme_minimal() %+replace%

    theme(
      panel.grid = element_blank(),
      axis.title.y.right = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.line.y = element_line(linewidth = 0.3),
      axis.line.x = element_blank(),
      axis.text.y.left = ggtext::element_markdown(size = fontsize_barplot_y_numbers, colour = "black"),
      axis.text.y.right = ggtext::element_markdown(size = fontsize_barplot_y_numbers, hjust = 0),
      axis.ticks.y = element_blank(),
      strip.placement = "outside",
      plot.margin = ggplot2::margin(t = 5, r = 0, b = vertical_spacing + 5, l = 0, unit = "pt")
    )
}

theme_numeric_heatmap <- function(fontsize_y_title = 12, show_legend = TRUE, legend_position = "right", show_legend_titles = FALSE, legend_title_size = NULL, legend_text_size = NULL, legend_key_size = 0.3, vertical_spacing = 0) {
  ggplot2::theme_minimal() %+replace%
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_text(size = fontsize_y_title, angle = 0, colour = "black"),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = if (show_legend_titles) element_text(size = legend_title_size, face = "bold", hjust = 0) else element_blank(),
      legend.justification = c(0, 0.5),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.text = element_text(size = legend_text_size),
      legend.position = if (show_legend) legend_position else "none",
      strip.placement = "outside",
      plot.margin = ggplot2::margin(t = 0, r = 0, b = vertical_spacing, l = 0, unit = "pt")
    )
}


tag_bold <- function(x) {
  paste0("<b>", x, "</b>", collapse = "")
}

#' GGplot breaks
#'
#' Find sensible values to add 2 breaks at for a ggplot2 axis
#'
#' @param vector vector fed into ggplot axis you want to define sensible breaks for
#'
#' @return vector of length 2. first element descripts upper break position, lower describes lower break
#'
sensible_2_breaks <- function(vector) {
  upper <- max(vector, na.rm = TRUE)
  lower <- min(0, min(vector, na.rm = TRUE), na.rm = TRUE)
  c(upper, lower)
}

sensible_3_breaks <- function(vector, digits = 3) {
  vector <- vector[!is.infinite(vector)]
  upper <- max(vector, na.rm = TRUE)
  lower <- min(0, min(vector, na.rm = TRUE), na.rm = TRUE)

  # Round
  if (!is.null(digits)) upper <- round_up(upper, digits)
  if (!is.null(digits)) lower <- round_down(lower, digits)

  middle <- mean(c(upper, lower))

  breaks <- c(upper, middle, lower)

  if (upper == lower) {
    return(lower)
  }

  return(breaks)
}

sensible_3_labels <- function(vector, axis_label, fontsize_y_title = 14, digits = 3) {
  vector <- vector[!is.infinite(vector)]
  upper <- max(vector, na.rm = TRUE)
  lower <- min(0, min(vector, na.rm = TRUE), na.rm = TRUE)


  # Round
  if (!is.null(digits)) upper <- round_up(upper, digits)
  if (!is.null(digits)) lower <- round_down(lower, digits)

  axis_label <- paste0("<span style = 'font-size: ", fontsize_y_title, "pt'>", axis_label, "</span>")

  if (lower == upper) {
    return(axis_label)
  }

  as.character(c(upper, axis_label, lower))
}


#' Make strings prettier for printing
#'
#' Takes an input string and 'beautify' by converting underscores to spaces and
#'
#' @param string input string
#' @param autodetect_units automatically detect units (e.g. mm, kg, etc) and wrap in brackets.
#'
#' @return string
#'
beautify <- function(string, autodetect_units = TRUE) {
  # underscores to spaces
  string <- gsub(x = string, pattern = "_", replacement = " ")

  # dots to spaces
  string <- gsub(x = string, pattern = ".", replacement = " ", fixed = TRUE)

  # camelCase to camel Case
  string <- gsub(x = string, pattern = "([a-z])([A-Z])", replacement = "\\1 \\2")

  # Autodetect units (and move to brackets)
  if (autodetect_units) {
    string <- sub("\\sm(\\s|$)", " (m)", string)
    string <- sub("\\smm(\\s|$)", " (mm)", string)
    string <- sub("\\sm(\\s|$)", " (cm)", string)
    string <- sub("\\sm(\\s|$)", " (km)", string)
    string <- sub("\\sg(\\s|$)", " (g)", string)
    string <- sub("\\skg(\\s|$)", " (kg)", string)
    string <- sub("\\smg(\\s|$)", " (mm)", string)
    string <- sub("\\soz(\\s|$)", " (oz)", string)
    string <- sub("\\slb(\\s|$)", " (lb)", string)
    string <- sub("\\sin(\\s|$)", " (in)", string)
    string <- sub("\\sft(\\s|$)", " (ft)", string)
    string <- sub("\\syd(\\s|$)", " (yd)", string)
    string <- sub("\\smi(\\s|$)", " (mi)", string)
  }


  # Capitalise Each Word
  string <- gsub(x = string, pattern = "^([a-z])", perl = TRUE, replacement = ("\\U\\1"))
  string <- gsub(x = string, pattern = " ([a-z])", perl = TRUE, replacement = (" \\U\\1"))

  return(string)
}

round_up <- function(x, digits) {
  multiplier <- 10^digits
  ceiling(x * multiplier) / multiplier
}

round_down <- function(x, digits) {
  multiplier <- 10^digits
  floor(x * multiplier) / multiplier
}

fct_inorder <- function(x){
  factor(x, levels = unique(x))
}


# Autoconvert pseudo-logical to a factor
numeric_only_includes_zero_one_and_na <- function(vec){
  is.numeric(vec) & all(unique(vec) %in% c(0, 1, NA))
}

convert_numerics_with_only_values_0_1_and_NA_to_logicals <- function(data, exclude = NULL) {
  col_is_convertable <- vapply(data, numeric_only_includes_zero_one_and_na, FUN.VALUE = logical(1))

  if(!is.null(exclude))
    col_is_convertable <- col_is_convertable & (colnames(data) != exclude)

  data[, col_is_convertable] <- lapply(data[, col_is_convertable, drop = FALSE], as_binary_factor)
  return(data)
}

as_binary_factor <- function(vec){
  vec <- as.factor(vec)
  levels(vec) <- c(0, 1, NA)
  return(vec)
}

is.positive.infinity <- function(x){
  is.infinite(x) & x > 0
}

is.negative.infinity <- function(x){
  is.infinite(x) & x < 0
}


