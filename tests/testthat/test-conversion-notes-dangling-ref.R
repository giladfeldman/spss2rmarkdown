# test-conversion-notes-dangling-ref.R — the no-data guard's "see the
# Conversion Notes section" promise must not dangle.
#
# Found by the 2026-08-04 Sonnet canary audit on the corpus Syntax_Hyp4 (a
# no-paired-.sav item): the load-data stub and the data-guard chunk both tell
# the reader to consult the "Conversion Notes" section for diagnostics, but
# the section was only emitted when the caller passed a non-empty
# conversion_notes list — so the no-data document referenced a section that
# did not exist. Watched RED against the unfixed generator.

test_that("no-.sav document always contains the Conversion Notes section it references", {
  parsed <- getFromNamespace("parse_single_command", "spss2rmarkdown")(
    "DESCRIPTIVES VARIABLES=x y.")
  conv <- getFromNamespace("convert_spss_to_r", "spss2rmarkdown")(parsed, NULL)
  out_dir <- tempfile(); dir.create(out_dir)
  res <- generate_rmd(
    sav_data = NULL,
    sav_path = file.path(out_dir, "script.sps"),
    parsed_syntax = list(parsed),
    converted_code = list(conv),
    output_dir = out_dir,
    base_name = "noswav_test",
    conversion_notes = list(),
    options = list(include_original = TRUE)
  )
  rmd <- paste(readLines(res$rmd_path, warn = FALSE), collapse = "\n")
  # The guard text references the section...
  expect_match(rmd, "Conversion Notes", fixed = TRUE)
  # ...and the section heading itself must exist.
  expect_match(rmd, "## Conversion Notes", fixed = TRUE)
})

test_that("with-data document without notes stays free of an empty notes section", {
  df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  sav <- list(data = df, n_vars = 2L, n_obs = 3L, metadata = NULL)
  out_dir <- tempfile(); dir.create(out_dir)
  sav_path <- file.path(out_dir, "mini.sav")
  haven::write_sav(df, sav_path)
  parsed <- getFromNamespace("parse_single_command", "spss2rmarkdown")(
    "DESCRIPTIVES VARIABLES=x y.")
  conv <- getFromNamespace("convert_spss_to_r", "spss2rmarkdown")(parsed, sav)
  res <- generate_rmd(
    sav_data = sav,
    sav_path = sav_path,
    parsed_syntax = list(parsed),
    converted_code = list(conv),
    output_dir = out_dir,
    base_name = "withsav_test",
    conversion_notes = list(),
    options = list(include_original = TRUE)
  )
  rmd <- paste(readLines(res$rmd_path, warn = FALSE), collapse = "\n")
  # No dangling reference is emitted on the with-data path, so no synthesized
  # section is needed (behavior unchanged).
  expect_false(grepl("## Conversion Notes", rmd, fixed = TRUE))
})
