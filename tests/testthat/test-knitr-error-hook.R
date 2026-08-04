# test-knitr-error-hook.R
# R-0010 (cross-project capability transfer from JAMOVItoRmarkdown):
# The generated .Rmd setup chunk must install a GLOBAL knitr error hook that
# downgrades KNOWN jmv/R compatibility errors (attr mismatches, var() on a
# factor, missing variables/packages) to friendly HTML info-notes, so one
# incompatible analysis does not make the whole knitted report look broken.
# UNKNOWN errors must still surface verbatim.
#
# These tests exercise the *generated output* of generate_rmd() (not a hand
# written string), and assert the injected hook is real, syntactically valid R,
# and does not regress the pre-existing setup-chunk contents.

# Extract the body of a named knitr chunk from generated .Rmd lines.
# Mirrors the fence-pair scan used in test-pipeline.R.
.extract_chunk_body <- function(rmd_lines, chunk_label) {
  start_pat <- paste0("^```\\{r ", chunk_label, "[,}]")
  start <- grep(start_pat, rmd_lines)
  if (length(start) != 1L) {
    return(NULL)
  }
  fences <- grep("^```", rmd_lines)
  end <- fences[fences > start][1]
  if (is.na(end)) {
    return(NULL)
  }
  # body is between the opening fence and the closing fence (exclusive)
  if (end - start < 2L) {
    return(character(0))
  }
  rmd_lines[(start + 1L):(end - 1L)]
}

# Generate a real .Rmd from the package's own testdata fixtures and return its
# lines. Fails (not skips) if fixtures are missing, so the test cannot silently
# pass by skipping.
.generate_fixture_rmd_lines <- function() {
  sav_file <- system.file("testdata", "Normality-experiment-1-data.sav",
                          package = "spss2rmarkdown")
  sps_file <- system.file("testdata", "Normality-experiment-1-syntax.sps",
                          package = "spss2rmarkdown")
  testthat::expect_true(nzchar(sav_file) && file.exists(sav_file),
                        info = "testdata .sav fixture must be installed")
  testthat::expect_true(nzchar(sps_file) && file.exists(sps_file),
                        info = "testdata .sps fixture must be installed")

  sav_result <- parse_sav(sav_file)
  sps_result <- parse_sps(sps_file)
  converted <- convert_all_commands(sps_result, sav_result)

  out_dir <- tempfile("spss2r_hook_test_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  base_name <- tools::file_path_sans_ext(basename(sav_file))
  base_name <- gsub("[^a-zA-Z0-9._-]", "_", base_name)

  rmd_result <- generate_rmd(
    sav_data = sav_result,
    sav_path = sav_file,
    parsed_syntax = sps_result,
    converted_code = converted,
    output_dir = out_dir,
    options = list(include_original = TRUE),
    base_name = base_name
  )
  testthat::expect_true(file.exists(rmd_result$rmd_path))
  readLines(rmd_result$rmd_path, warn = FALSE)
}

test_that("generated setup chunk installs a knitr error hook", {
  rmd_lines <- .generate_fixture_rmd_lines()
  setup_body <- .extract_chunk_body(rmd_lines, "setup")
  expect_false(is.null(setup_body), info = "setup chunk must exist in generated .Rmd")

  setup_text <- paste(setup_body, collapse = "\n")
  # The hook itself
  expect_match(setup_text, "knitr::knit_hooks\\$set\\(error", fixed = FALSE,
               info = "setup chunk must register a knitr error hook")
  # Known-pattern downgrade machinery + info-note class
  expect_match(setup_text, "known_patterns")
  expect_match(setup_text, "alert alert-info", fixed = TRUE)
  # A couple of the representative known jmv/R patterns must be present
  expect_true(grepl("there is no package called", setup_text, fixed = TRUE),
              info = "missing-package pattern must be covered")
  expect_true(grepl("attr", setup_text, fixed = TRUE) &&
                grepl("attributeName", setup_text, fixed = TRUE),
              info = "attr(data, attributeName) pattern must be covered")
})

test_that("generated setup chunk parses as valid R (escaping is correct)", {
  rmd_lines <- .generate_fixture_rmd_lines()
  setup_body <- .extract_chunk_body(rmd_lines, "setup")
  expect_false(is.null(setup_body))

  setup_text <- paste(setup_body, collapse = "\n")
  parsed <- tryCatch(parse(text = setup_text),
                     error = function(e) structure(NULL, msg = conditionMessage(e)))
  expect_false(is.null(parsed),
               info = paste("setup chunk must parse; error:", attr(parsed, "msg")))
})

test_that("injecting the hook does not regress pre-existing setup-chunk lines", {
  rmd_lines <- .generate_fixture_rmd_lines()
  setup_body <- .extract_chunk_body(rmd_lines, "setup")
  expect_false(is.null(setup_body))

  setup_text <- paste(setup_body, collapse = "\n")
  # Pre-existing contract: opts_chunk$set with error = TRUE, and library(jmv)
  expect_match(setup_text, "knitr::opts_chunk\\$set")
  expect_match(setup_text, "error = TRUE", fixed = TRUE)
  expect_match(setup_text, "library(jmv)", fixed = TRUE)
  expect_match(setup_text, "options(jmv.progress = FALSE)", fixed = TRUE)
})
