#' Relevel Factor by Specified Levels
#'
#' Reorder the levels of a factor by moving specified levels to a new position.
#'
#' @param x A factor to be releveled.
#' @param ... Levels to move in the factor.
#' @param after A numeric scalar specifying the position after which the moved
#'   levels should be placed. Use `0` to place them at the front.
#'
#' @return A factor with the specified levels moved to the chosen position.
#' @keywords internal
fct_relevel_base <- function(x, ..., after = 0) {
  # Ensure input is a factor
  if (!is.factor(x)) {
    stop("`x` must be a factor.")
  }

  # Levels to move, provided as ...
  to_move <- c(...)

  # Original levels
  orig_levels <- levels(x)

  # Check if all levels to move are indeed present
  if (!all(to_move %in% orig_levels)) {
    missing_levels <- to_move[!to_move %in% orig_levels]
    stop(
      "The following levels are not present in `x`: ",
      paste(missing_levels, collapse = ", ")
    )
  }

  # Remove the specified levels from the original ordering
  remaining_levels <- orig_levels[!orig_levels %in% to_move]

  # Insert `to_move` after the `after` position
  new_levels <- append(remaining_levels, to_move, after = after)

  # Re-factor `x` with the new level order
  factor(x, levels = new_levels)
}


#' Count Edge Crossings in Parallel Coordinates
#'
#' Calculates the total number of edge crossings between two numeric vectors in a
#' 2-column parallel coordinates setup. Each axis represents one of the columns.
#'
#' @param l A numeric vector representing values on the left axis. Must have the
#'   same length as `r`.
#' @param r A numeric vector representing values on the right axis. Must have
#'   the same length as `l`.
#'
#' @return An integer indicating the total number of edge crossings.
#'
#' @details An edge crossing occurs when two edges intersect between the axes.
#'   Formally, edges \eqn{(l[i], r[i])} and \eqn{(l[j], r[j])} cross if
#'   \eqn{(l[i] - l[j]) * (r[i] - r[j]) < 0}.
#'
#' @keywords internal
count_edge_crossings <- function(l, r) {
  # Ensure both vectors have the same length
  if (length(l) != length(r)) {
    stop("Length of the two axes must be equal to compute edge crossings.")
  }

  n_points <- length(l)

  # Get every unique combination of 2 indices
  ls_combos <- utils::combn(x = seq_len(n_points), m = 2, simplify = FALSE)

  # For each pair of indices, check whether the coordinates cross
  crossings <- vapply(
    X = ls_combos,
    FUN = function(c_index) {
      index1 <- c_index[1]
      index2 <- c_index[2]
      l1 <- l[index1]
      r1 <- r[index1]
      l2 <- l[index2]
      r2 <- r[index2]
      is_crossing(l1, r1, l2, r2)
    },
    FUN.VALUE = logical(1)
  )

  # Count how many crossing events there are
  sum(crossings)
}


#' Count Edge Crossings for All Numeric Column Pairs
#'
#' Computes the total number of edge crossings between all pairs of numeric
#' columns in a given dataset.
#'
#' @param data A `data.frame` or `tibble` containing the dataset. Only numeric
#'   columns are considered for edge crossing calculations.
#' @param approximate estimate crossings based on a subsample of the data. See \code{subsample_prop} for details.
#' @param subsample_prop only used when approximate = TRUE.
#' If 0-1, controls the proportion of data to be sampled to speed up computation.
#' If a whole number other than 0 or 1, represents the number of rows subsampled
#' @param recalibrate when approximating crossings via subsetting,
#' should number of crossings calculated for the subsample be upscaled to match the full count.
#' (turned off by default since it amplifies sampling error).
#' @return A `data.frame` with three columns:
#'   \describe{
#'     \item{col1}{The name of the first column in the pair.}
#'     \item{col2}{The name of the second column in the pair.}
#'     \item{crossings}{Total number of edge crossings for that pair.}
#'   }
#'
#' @details The function:
#'   \enumerate{
#'     \item Filters the input data to retain only numeric columns.
#'     \item Computes all possible pairs of numeric columns.
#'     \item Uses `count_edge_crossings()` to calculate crossings for each pair.
#'     \item Returns the results in a summarized data frame.
#'   }
#'
#' @keywords internal
count_all_edge_crossings <- function(data, approximate = FALSE, subsample_prop = 0.4, recalibrate = FALSE) {

  assertions::assert(nrow(data) > 2, msg = "Dataset must contain at least 2 rows to count edge crossings")

  # Restrict to numeric columns only
  data <- data[, vapply(data, is.numeric, logical(1))]

  # Compute all column pairs
  ls_column_pairs <- utils::combn(colnames(data), m = 2, simplify = FALSE)

  # Optionally approximate if data is too large
  if (approximate) {

    # How many rows should we sample?
    nrows_to_sample <-
      # If 0-1, sample that proportion of rows
      if (subsample_prop >= 0 && subsample_prop <= 1) {
        round(nrow(data) * subsample_prop)
      }
      # If subsample_prop>1 & >=number of rows in data then don't subsample at all
      else if(subsample_prop >= nrow(data)){
        # Dataset is too small to subsample. Computing crossings on entire dataset
        nrow(data)
      }
      # If subsample_prop > 1, subsample exactly that row #
      else {
        subsample_prop
      }

    # Sample at least 2 rows
    nrows_to_sample <- max(2, nrows_to_sample)

    # Perform the subsampling
    data_subsample <- data[sample(nrow(data), nrows_to_sample), ]
  } else {
    data_subsample <- data
  }

  proportion_subsampled <- nrow(data_subsample) / nrow(data)

  # Calculate crossings for each pair
  crossings <- vapply(
    ls_column_pairs,
    function(df_pair_colnames) {
      count_edge_crossings(
        data_subsample[[df_pair_colnames[1]]],
        data_subsample[[df_pair_colnames[2]]]
      )
    },
    numeric(1)
  )

  # If data was subsampled, we can recalibrate the number of crossings to better predict
  # what it would be in the full dataset size. We turn this off by defualt  since
  # it will amplify any mistakes in the prediction by a power of 2
  if (recalibrate) {
    crossings <- crossings / proportion_subsampled^2
  }

  # Build the result data frame
  df_column_pairs <- as.data.frame(do.call(rbind, ls_column_pairs))
  colnames(df_column_pairs) <- c("col1", "col2")
  df_column_pairs[["crossings"]] <- crossings

  df_column_pairs
}

dist_matrix_edge_crossings <- function(data, approximate = FALSE, subsample = 0.4, recalibrate = FALSE){

  # Compute crossings for all numeric column pairs
  df_crossings <- count_all_edge_crossings(
    data,
    approximate=approximate,
    subsample_prop = subsample,
    recalibrate = recalibrate
  )

  # Convert to a distance matrix
  distance_mx <- create_distance_matrix(df_crossings)
}

# count_edge_crossings_subsample_curve <- function(x, y) {
#   max_length <- length(x)
#   props <- c(0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
#   nrows <- round(props * max_length)
#   props <- nrows / max_length
#
#   compute_crossings_on_each_subset <- function() {
#     vapply(X = props, FUN = function(prop) {
#       indices <- sample(seq_along(x), size = max_length * prop, replace = FALSE)
#       count_edge_crossings(
#         x[indices],
#         y[indices]
#       )
#     }, FUN.VALUE = numeric(1))
#   }
#
#   ls_crossings_n_times <- lapply(1:9, FUN = function(i) {
#     compute_crossings_on_each_subset()
#   })
#
#   mx <- t(do.call(rbind, ls_crossings_n_times))
#   colnames(mx) <- paste0("crossings_rep", seq_len(ncol(mx)))
#   df <- as.data.frame(mx)
#   df$props <- props
#   df <- df[c("props", setdiff(colnames(df), "props"))]
#
#   return(df)
#   # df_curve <- count_edge_crossings_subsample_curve(minibeans$Area, minibeans$Perimeter)
#   # tidyr::pivot_longer(df_curve, )
# }


#' Determine Whether Two Edges Cross
#'
#' Given the positions of two edges on the left and right axes, decides if they
#' intersect in a parallel coordinates setup.
#'
#' @param l1 Numeric position of the first edge on the left axis.
#' @param r1 Numeric position of the first edge on the right axis.
#' @param l2 Numeric position of the second edge on the left axis.
#' @param r2 Numeric position of the second edge on the right axis.
#'
#' @return A logical value. `TRUE` if they cross, `FALSE` otherwise.
#'
#' @keywords internal
is_crossing <- function(l1, r1, l2, r2) {
  # Simple intersection check
  (l1 > l2 & r1 < r2) | (l1 < l2 & r1 > r2)
}


#' Optimise the Ordering of Axes Using Distance Matrix
#'
#' Finds an ordering of axes that minimises a pairwise distance metric (usually
#' the number of crossings). Offers brute-force and heuristic approaches.
#'
#' @param mx A matrix or `dist` object describing pairwise distances between axes.
#' @param method A character string specifying the method. Can be `"auto"`,
#'   `"brute_force"`, or `"repetitive_nn_with_2opt"`.
#' @param return_detailed Logical; if `TRUE`, returns a list with detailed
#'   results for debugging.
#' @param verbose Logical; if `TRUE`, prints progress messages.
#'
#' @return If `return_detailed = FALSE`, returns a character vector of axis
#'   names in the chosen order. Otherwise, returns a list with additional data.
#'
#' @keywords internal
optimise_axis_ordering_from_matrix <- function(mx,
                                               method = c(
                                                 "auto", "brute_force",
                                                 "repetitive_nn_with_2opt"
                                               ),
                                               return_detailed = FALSE,
                                               verbose = TRUE) {
  method <- rlang::arg_match(method)
  axes <- as.character(unique(unlist(dimnames(mx))))
  n_axes <- length(axes)

  # Convert from dist object if needed
  if (inherits(mx, "dist")) {
    mx <- as.matrix(mx)
  }

  # Auto-select brute force if < 8 axes, else heuristic
  if (method == "auto") {
    method <- if (n_axes < 8) "brute_force" else "repetitive_nn_with_2opt"
  }

  if (method == "brute_force") {
    if (verbose) cli::cli_alert_info("Brute forcing solution to axis order.")
    if (length(axes) >= 8) {
      stop(
        "Brute-force solutions for >= 8 axes are not allowed. ",
        "Choose a more efficient method like `repetitive_nn_with_2opt`."
      )
    }

    mx_combinations <- permute_axis_names(axes)

    vec_total_crossings <- apply(
      X = mx_combinations,
      MARGIN = 1,
      FUN = function(axis_order_vec) {
        feature_vector_to_total_path_distance(axis_order_vec, mx = mx)
      }
    )

    optimal_axis_order <- mx_combinations[which.min(vec_total_crossings), ]

    if (return_detailed) {
      full_data <- as.data.frame(mx_combinations)
      colnames(full_data) <- paste0("i", seq_len(ncol(full_data)))
      full_data["crossings"] <- vec_total_crossings

      return(list(
        optimal_axis_order = optimal_axis_order,
        total_crossings = vec_total_crossings[which.min(vec_total_crossings)],
        full_data = full_data
      ))
    }

    return(optimal_axis_order)
  } else if (method == "repetitive_nn_with_2opt") {
    if (verbose) {
      cli::cli_alert_info(
        "Choosing axis order via repetitive nearest neighbour with two-opt refinement"
      )
    }

    rlang::check_installed(
      "TSP",
      reason = "to identify an approximate axis order with repetitive nearest neighbour + two-opt refinement"
    )

    tsp <- TSP::TSP(mx, method = "crossings")
    solution <- TSP::solve_TSP(
      tsp,
      method = "repetitive_nn",
      two_opt = TRUE
    )
    optimal_axis_crossings <- labels(solution)

    total_crossings <- feature_vector_to_total_path_distance(
      optimal_axis_crossings,
      mx = mx
    )

    if (return_detailed) {
      return(list(
        optimal_axis_crossings = optimal_axis_crossings,
        total_crossings = total_crossings,
        tsp_problem = tsp,
        solution = solution
      ))
    }

    return(optimal_axis_crossings)
  } else {
    stop(
      "No implementation for axis ordering method: [", method,
      "]. Please open a new GitHub issue with this error message."
    )
  }
}


#' Create a Distance Matrix from Edge Crossing Data
#'
#' Converts the results of `count_all_edge_crossings()` into a distance matrix,
#' where each entry represents the number of crossings between two columns.
#'
#' @param data A data frame with columns `col1`, `col2`, and `crossings`.
#' @param as.dist Logical; if `TRUE`, converts the matrix to a `dist` object.
#'
#' @return A square matrix of distances, or a `dist` object if `as.dist = TRUE`.
#' @keywords internal
create_distance_matrix <- function(data, as.dist = FALSE) {
  # Verify the required columns exist
  if (!all(c("col1", "col2", "crossings") %in% names(data))) {
    stop("Input data must have columns named 'col1', 'col2', and 'crossings'.")
  }

  # Identify unique variables
  vars <- unique(c(data$col1, data$col2))

  # Initialize an empty matrix
  distance_matrix <- matrix(
    0,
    nrow = length(vars),
    ncol = length(vars),
    dimnames = list(vars, vars)
  )

  # Populate the matrix with crossing values
  for (i in seq_len(nrow(data))) {
    var1 <- data$col1[i]
    var2 <- data$col2[i]
    crossings <- data$crossings[i]

    distance_matrix[var1, var2] <- crossings
    distance_matrix[var2, var1] <- crossings # Ensure symmetry
  }

  # Convert to 'dist' if requested
  if (as.dist) {
    distance_matrix <- as.dist(distance_matrix)
  }

  distance_matrix
}


#' Reorder Factor Levels by Descending Frequency
#'
#' Reorders the levels of a factor by their frequency, in descending order.
#'
#' @param x A factor or an object coerced to a factor.
#'
#' @return A factor with levels ordered by descending frequency.
#' @keywords internal
fct_infreq <- function(x) {
  # Convert input to factor if necessary
  f <- as.factor(x)

  # Get frequency counts for each level
  freq <- table(f)

  # Reorder levels by frequency
  new_levels <- names(sort(freq, decreasing = TRUE))

  # Re-factor with the new level order
  factor(f, levels = new_levels)
}


#' Reverse the Levels of a Factor
#'
#' Reverses the existing level order of a factor.
#'
#' @param x A factor or an object coerced to a factor.
#'
#' @return A factor with reversed levels.
#' @keywords internal
fct_rev <- function(x) {
  # Convert input to factor if necessary
  f <- as.factor(x)
  factor(f, levels = rev(levels(f)))
}


#' Generate Permutations of the Integers 1..n
#'
#' Creates a matrix of all permutations for the integers from 1 to n.
#'
#' @param n Number of elements to permute.
#'
#' @return A matrix where each row is a permutation of 1..n.
#' @keywords internal
permutations <- function(n) {
  if (n == 1) {
    return(matrix(1))
  } else {
    sp <- permutations(n - 1)
    p <- nrow(sp)
    A <- matrix(nrow = n * p, ncol = n)
    for (i in 1:n) {
      # Insert the new element in all possible positions
      A[(i - 1) * p + 1:p, ] <- cbind(i, sp + (sp >= i))
    }
    return(A)
  }
}


#' Generate All Permutations of Axis Names
#'
#' Takes a character vector of axis names and returns a matrix of permutations.
#'
#' @param axis_names A character vector of axis names.
#'
#' @return A matrix where each row represents one permutation of `axis_names`.
#' @keywords internal
permute_axis_names <- function(axis_names) {
  matrix(axis_names[permutations(length(axis_names))],
    ncol = length(axis_names)
  )
}


#' Compute the Total Path Distance for an Axis Order
#'
#' Given a sequence of axis names and a distance matrix, sums pairwise distances
#' along the path.
#'
#' @param axis_names A character vector indicating the axis order.
#' @param mx A matrix of distances, with row and column names matching `axis_names`.
#'
#' @return A numeric value representing the total distance.
#' @keywords internal
feature_vector_to_total_path_distance <- function(axis_names, mx) {
  # Cannot compute path distance for a single axis
  if (length(axis_names) <= 1) {
    stop("Cannot sum distance path for a single axis name.")
  }

  idx1 <- axis_names[-length(axis_names)]
  idx2 <- axis_names[-1]

  # Compute distances along the path
  ls_distances <- mapply(
    function(i1, i2) {
      mx[i1, i2]
    },
    idx1, idx2,
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )

  sum(unlist(ls_distances))
}


# Count inversions between two vectors
# count_inversions <- function(x, y){
#   y_sorted <- y[order(x)]
#   y_ranked <- rank(y_sorted)
#   return(y_ranked)
# }

#' Compute Mutual Information
#'
#' Computes mutual information between each feature in the `features` data frame and the `target` vector.
#' The features are discretized using the "equalfreq" method from [infotheo::discretize()].
#'
#' @param features A data frame of features. These will be discretized using the "equalfreq" method
#' (see [infotheo::discretize()]).
#' @param target A vector (character or factor) representing the variable to compute mutual information with.
#' @param return_colnames Logical; if `TRUE`, returns the column names from `features` ordered by their
#' mutual information with `target` (highest to lowest). If `FALSE`, returns mutual information values. (default: `FALSE`)
#'
#' @return
#' If `return_colnames = FALSE`, a named numeric vector of mutual information scores is returned (one for each column in `features`), sorted in descending order.
#' The names of the vector correspond to the column names of `features`.
#' If `return_colnames = TRUE`, only the ordered column names of `features` are returned.
#'
#' @examples
#' data(iris)
#' # Compute mutual information scores
#' mutinfo(iris[1:4], iris[[5]])
#'
#' # Get column names ordered by mutual information with target column (most mutual info first)
#' mutinfo(iris[1:4], iris[[5]], return_colnames = TRUE)
#'
#' @export
mutinfo <- function(features, target, return_colnames = FALSE){
  rlang::check_installed("infotheo", reason = "to order axes based on mutual information")

  # Assertions & conversions
  assertions::assert_dataframe(features)
  if(is.factor(target)) { target <- as.character(target) }
  assertions::assert_vector(target)

  # Discretize as required for mutual information computations
  discretized <- infotheo::discretize(features, disc = "equalfreq")

  # Compute mutual information between each column in data and the target vector
  mutual_info <- vapply(discretized, FUN = function(feature){
    infotheo::mutinformation(X = feature, Y = target, method = "emp")
  }, FUN.VALUE = numeric(1))

  # mutual info should be a named vector of mutual informations between
  # each feature in the features data.frame and the target vector
  mutual_info <- sort(mutual_info, decreasing = TRUE)

  if(return_colnames){
    return(names(mutual_info))
  }

  return(mutual_info)
}

similarity_matrix_mutinfo <- function(data, normalize = TRUE){
  mx <- infotheo::mutinformation(infotheo::discretize(data), method = "emp")

  if(normalize){
    mx <- uniminmax(mx)
  }

  return(mx)
}

dist_matrix_mutinfo <- function(data){
  1-similarity_matrix_mutinfo(data, normalize = TRUE)
}

# Mutinfo based distance from Joe 1989 https://www.jstor.org/stable/2289859?seq=3
dist_matrix_mutinfo2 <- function(data){
  mx = similarity_matrix_mutinfo(data, normalize = FALSE)
  1 - (1 - exp( -2 *mx )) ^.5
}

# Exported Function -------------------------------------------------------

#' Optimize Axis Ordering Directly from a Data Frame
#'
#' Computes the number of edge crossings between all numeric columns in `data`,
#' converts this information into a distance matrix, and then determines an
#' optimal ordering of the columns based on the specified method.
#'
#' @param data A `data.frame` or `tibble` containing the dataset. Only numeric
#'   columns are considered for edge crossing calculations.
#' @param verbose A logical value; if `TRUE`, prints progress messages.
#' @param method A character string specifying the method. Options are `"auto"`,
#'   `"brute_force"`, or `"repetitive_nn_with_2opt"`.
#' @param return_detailed A logical; if `TRUE`, returns a list with additional
#'   data (e.g., intermediate calculations) for debugging.
#' @param metric which metric should take as the distance between axes to minimise.
#' mutual information: minimise mutual distance (1- uniminmax of mutinfo similarity matrix calculated by emp)
#' crossings: minimise the total number of edge crossings (warning: slow to compute for large datasets).
#' crossings_fast: same as above but calculates crossings on a subset of data (100 rows)
#' @return A character vector of axis names in the chosen order, or a list with
#'   additional data if `return_detailed = TRUE`.
#' @keywords internal
get_optimal_axis_order <- function(data,
                                   verbose = TRUE,
                                   method = "auto",
                                   metric = c("mutinfo", "crossings", "crossings_fast"),
                                   return_detailed = FALSE) {

  metric <- rlang::arg_match(metric)

  # Define Distance matrix Generation Method
  dist_matrix_method <-
    if(metric == "crossings") dist_matrix_edge_crossings
    else if(metric == "crossings_fast") function(x) {
      dist_matrix_edge_crossings(data, approximate = TRUE, subsample = 100, recalibrate = FALSE)
      }
    else if(metric == "mutinfo") dist_matrix_mutinfo2
    else stop("No implementation for distance metric: [",metric,"]. Please create a new issue on github")

  # Create distance matrix
  distance_mx <- dist_matrix_method(data)

  # Use matrix-based ordering logic
  optimal_axis_order <- optimise_axis_ordering_from_matrix(
    distance_mx,
    verbose = verbose,
    method = method,
    return_detailed = return_detailed
  )

  return(optimal_axis_order)
}
