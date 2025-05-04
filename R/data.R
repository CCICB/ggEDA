#' Lazy Birdwatcher Dataset
#'
#' A simulated dataset describing the number of magpies observed by two birdwatchers.
#'
#' @format ## `lazy_birdwatcher`
#' A data frame with 45 rows and 3 columns:
#'
#' \describe{
#'   \item{Magpies}{Number of magpies observed}
#'   \item{Day}{Was the day of observation a weekday or a weekend?}
#'   \item{Birdwatcher}{Name of the birdwatcher}
#' }
"lazy_birdwatcher"


#' Dry Beans Dataset
#'
#' A subsample of the Koklu & Ozkan (2020) dry beans dataset produced by
#' imaging a total of 13,611 grains from 7 varieties of dry beans.
#' The original dataset contains 13,611 observations, but here we include a
#' random subsample of 1000.
#'
#' @format ## `minibeans`
#' A data frame with 1000 rows and 17 columns:
#'
#' \describe{
#'   \item{Area}{The area of a bean zone and the number of pixels within its boundaries.}
#'   \item{Perimeter}{Bean circumference is defined as the length of its border.}
#'   \item{Major axis length}{The distance between the ends of the longest line that can be drawn from a bean.}
#'   \item{Minor axis length}{The longest line that can be drawn from the bean while standing perpendicular to the main axis.}
#'   \item{Aspect ratio}{Defines the relationship between L and l.}
#'   \item{Eccentricity}{Eccentricity of the ellipse having the same moments as the region.}
#'   \item{Convex area}{Number of pixels in the smallest convex polygon that can contain the area of a bean seed.}
#'   \item{Equivalent diameter}{The diameter of a circle having the same area as a bean seed area.}
#'   \item{Extent}{The ratio of the pixels in the bounding box to the bean area.}
#'   \item{Solidity}{Also known as convexity. The ratio of the pixels in the convex shell to those found in beans.}
#'   \item{Roundness}{Calculated with the following formula: (4piA)/(P^2).}
#'   \item{Compactness}{Measures the roundness of an object: Ed/L.}
#'   \item{ShapeFactor1}{Shape factor 1.}
#'   \item{ShapeFactor2}{Shape factor 2.}
#'   \item{ShapeFactor3}{Shape factor 3.}
#'   \item{ShapeFactor4}{Shape factor 4.}
#'   \item{Class}{Seker, Barbunya, Bombay, Cali, Dermosan, Horoz, and Sira.}
#' }
#'
#' @source Koklu, M, and IA Ozkan. 2020. Multiclass Classification of Dry Beans Using Computer Vision and Machine Learning Techniques. Computers and Electronics in Agriculture, 174: 105507. doi: 10.1016/j.compag.2020.105507, https://doi.org/10.24432/C50S4B
"minibeans"



#' Baseball Fans Dataset
#'
#' An artificially generated dataset describing
#' basic demographics and accessorization choices of baseball fans as part of a
#' a hypothetical market research study from stadium merchandise vendors. None of the data are real; they were
#' produced for illustrative and testing purposes only.
#'
#' @format ## `baseballfans`
#' A data frame with 19 rows and 10 columns:
#' \describe{
#' \item{ID}{Unique integer identifier for each individual.}
#' \item{Age}{Age in years at time of observation.}
#' \item{Gender}{Self‐reported gender (“Male” or “Female”).}
#' \item{EyeColour}{Eye color (“Brown”, “Green”, “Blue”), or missing (NA) if not recorded.}
#' \item{Height}{Height in centimeters; missing (NA) if not recorded.}
#' \item{HairColour}{Hair color (“Black”, “Blond”, “Red”, “Brown”).}
#' \item{Glasses}{Logical flag (TRUE/FALSE) indicating whether the individual wears glasses.}
#' \item{WearingHat}{Logical flag (TRUE/FALSE) indicating whether the individual is wearing a hat.}
#' \item{WearingHat_tooltip}{Type of hat worn, if any (e.g., “baseball cap”, “stetson”, “fedora”, “top hat”); empty when WearingHat == FALSE.}
#' \item{Date}{Date of observation in day/month/year format (e.g., 9/05/2023). Stored as character vector}
#' }
#'
#' #' @source
#' Synthetic data; no real persons were observed.
#'
#' @details
#' This mock dataset was created to demonstrate ggEDA functionality. All entries are fictional.
"baseballfans"
