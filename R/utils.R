# utils.R
# Utility functions for SPSS2R

# Suppress R CMD check NOTEs for NSE variables used in ggplot2
utils::globalVariables(c(".data", "se", "ci_width"))

#' Null-coalescing operator
#' Returns x if not NULL, otherwise y
#' @param x Left-hand value
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) if (!is.null(x)) x else y

#' Safe variable quoting for R code
#' Backticks variable names with special characters
#'
#' @param var Variable name
#' @return Properly quoted variable name
quote_var <- function(var) {
  if (grepl("[^a-zA-Z0-9_.]", var) || grepl("^[0-9]", var)) {
    paste0("`", var, "`")
  } else {
    var
  }
}

#' Create output directory with timestamp
#'
#' @param base_name Base name for the directory
#' @param output_root Root output directory
#' @return Path to created directory
create_output_dir <- function(base_name, output_root = "output") {
  timestamp <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
  dir_name <- paste0(timestamp, "-", gsub("[^a-zA-Z0-9_-]", "_", base_name))
  dir_path <- file.path(output_root, dir_name)
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  dir_path
}

#' Validate that required columns exist in data
#'
#' @param data Data frame
#' @param vars Required variable names
#' @return TRUE if all exist, error otherwise
validate_vars <- function(data, vars) {
  missing <- vars[!vars %in% names(data)]
  if (length(missing) > 0) {
    stop(paste("Variables not found in data:", paste(missing, collapse = ", ")))
  }
  TRUE
}

#' Convert factor to numeric if possible
#' Handles haven labelled variables
#'
#' @param x Vector
#' @return Numeric vector
safe_as_numeric <- function(x) {
  if (haven::is.labelled(x)) {
    x <- as.numeric(x)
  } else if (is.factor(x)) {
    x <- as.numeric(as.character(x))
  } else if (is.character(x)) {
    x <- as.numeric(x)
  }
  x
}

#' Log message with timestamp
#'
#' @param msg Message to log
#' @param level Log level (INFO, WARN, ERROR)
log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s: %s\n", timestamp, level, msg))
}
