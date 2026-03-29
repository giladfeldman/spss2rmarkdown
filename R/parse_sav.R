# parse_sav.R
# Parse SPSS .SAV data files using haven

#' Parse an SPSS .SAV file
#'
#' Reads an SPSS .sav data file and extracts the data, variable metadata,
#' value labels, and summary information.
#'
#' @param sav_path Path to the .SAV file
#' @return List containing data, metadata, and summary info
#' @export
parse_sav <- function(sav_path) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' is required. Install with: install.packages('haven')")
  }

  # Read SPSS data file with all metadata preserved
  data <- haven::read_sav(sav_path, user_na = TRUE)

  # Extract variable metadata
  var_info <- data.frame(
    name = names(data),
    label = sapply(data, function(x) {
      lbl <- attr(x, "label")
      if (is.null(lbl)) "" else lbl
    }),
    type = sapply(data, function(x) {
      if (haven::is.labelled(x)) {
        "labelled"
      } else {
        class(x)[1]
      }
    }),
    format = sapply(data, function(x) {
      fmt <- attr(x, "format.spss")
      if (is.null(fmt)) "" else fmt
    }),
    n_missing = sapply(data, function(x) sum(is.na(x))),
    stringsAsFactors = FALSE
  )

  # Extract value labels for each variable
  value_labels <- lapply(data, function(x) {
    labels <- attr(x, "labels")
    if (is.null(labels)) return(NULL)
    data.frame(
      value = unname(labels),
      label = names(labels),
      stringsAsFactors = FALSE
    )
  })
  names(value_labels) <- names(data)

  # Remove NULL entries
  value_labels <- value_labels[!sapply(value_labels, is.null)]

  list(
    data = data,
    metadata = var_info,
    value_labels = value_labels,
    n_obs = nrow(data),
    n_vars = ncol(data),
    path = sav_path
  )
}

#' Generate R code to recreate the data loading
#'
#' @param sav_info Result from parse_sav()
#' @param new_path Path to use in generated code
#' @return Character string of R code
#' @export
generate_data_load_code <- function(sav_info, new_path = "data.sav") {
  glue::glue('
# Load SPSS data file
library(haven)
data <- read_sav("{new_path}")

# Dataset overview
# Variables: {sav_info$n_vars}
# Observations: {sav_info$n_obs}
')
}

#' Convert haven labelled variables to factors
#'
#' @param data Data frame from haven::read_sav()
#' @return Data frame with labelled variables converted to factors
convert_labelled_to_factors <- function(data) {
  for (col in names(data)) {
    if (haven::is.labelled(data[[col]])) {
      data[[col]] <- haven::as_factor(data[[col]])
    }
  }
  data
}
