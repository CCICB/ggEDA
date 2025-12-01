# Baseball Fans Dataset

An artificially generated dataset describing basic demographics and
accessorization choices of baseball fans as part of a a hypothetical
market research study from stadium merchandise vendors. None of the data
are real; they were produced for illustrative and testing purposes only.

## Usage

``` r
baseballfans
```

## Format

### `baseballfans`

A data frame with 19 rows and 10 columns:

- ID:

  Unique integer identifier for each individual.

- Age:

  Age in years at time of observation.

- Gender:

  Self‐reported gender (“Male” or “Female”).

- EyeColour:

  Eye color (“Brown”, “Green”, “Blue”), or missing (NA) if not recorded.

- Height:

  Height in centimeters; missing (NA) if not recorded.

- HairColour:

  Hair color (“Black”, “Blond”, “Red”, “Brown”).

- Glasses:

  Logical flag (TRUE/FALSE) indicating whether the individual wears
  glasses.

- WearingHat:

  Logical flag (TRUE/FALSE) indicating whether the individual is wearing
  a hat.

- WearingHat_tooltip:

  Type of hat worn, if any (e.g., “baseball cap”, “stetson”, “fedora”,
  “top hat”); empty when WearingHat == FALSE.

- Date:

  Date of observation in day/month/year format (e.g., 9/05/2023). Stored
  as character vector

\#' @source Synthetic data; no real persons were observed.

## Details

This mock dataset was created to demonstrate ggEDA functionality. All
entries are fictional.
