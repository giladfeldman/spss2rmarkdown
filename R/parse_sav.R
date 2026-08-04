# parse_sav.R
# Parse SPSS .SAV data files using haven

#' Parse an SPSS .SAV file
#'
#' Reads an SPSS .sav data file and extracts the data, variable metadata,
#' value labels, and summary information.
#'
#' @param sav_path Path to the .SAV file
#' @return List containing data, metadata, and summary info
#' @examples
#' \dontrun{
#' # Parse an SPSS .sav data file
#' sav_info <- parse_sav("survey_data.sav")
#' sav_info$n_obs
#' sav_info$n_vars
#' head(sav_info$metadata)
#' }
#' @export
parse_sav <- function(sav_path) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' is required. Install with: install.packages('haven')")
  }

  # Read SPSS data file with all metadata preserved
  data <- haven::read_sav(sav_path, user_na = TRUE)

  # Extract variable metadata (with safe extraction for unusual SAV files)
  var_names <- names(data)
  n_vars <- length(var_names)
  var_info <- tryCatch(
    data.frame(
      name = var_names,
      label = vapply(data, function(x) {
        lbl <- attr(x, "label")
        if (is.null(lbl) || length(lbl) != 1) "" else as.character(lbl)
      }, character(1)),
      type = vapply(data, function(x) {
        if (haven::is.labelled(x)) "labelled" else class(x)[1]
      }, character(1)),
      format = vapply(data, function(x) {
        fmt <- attr(x, "format.spss")
        if (is.null(fmt) || length(fmt) != 1) "" else as.character(fmt)
      }, character(1)),
      n_missing = vapply(data, function(x) sum(is.na(x)), numeric(1)),
      stringsAsFactors = FALSE
    ),
    error = function(e) {
      # Fallback: minimal metadata if attribute extraction fails
      data.frame(
        name = var_names,
        label = rep("", n_vars),
        type = vapply(data, function(x) class(x)[1], character(1)),
        format = rep("", n_vars),
        n_missing = rep(0L, n_vars),
        stringsAsFactors = FALSE
      )
    }
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
#' @examples
#' \dontrun{
#' sav_info <- parse_sav("data.sav")
#' cat(generate_data_load_code(sav_info, new_path = "data.sav"))
#' }
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
