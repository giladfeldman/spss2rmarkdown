# Regression tests for the multi-.sav merge + the anovaRM missing-measure guard.
#
# Surfaced by round-3-spss item Analyses.sps (3apxv), which is paired with TWO
# row-aligned .sav files (DataMainDV.sav 25 cols, DataQuest.sav 13 cols, both 40
# rows, sharing GROUP). The syntax references variables spanning both files with
# no MATCH FILES, so loading only the primary made jmv fail ("Argument 'vars'
# contains 'AGE' which is not present in the data"). generate_rmd() now merges
# row-aligned secondary .sav files (union of columns, primary wins on shared).
# Separately, GLMs referencing variables computed in an upstream session (absent
# here) made jmv::anovaRM fail with a cryptic "'names' attribute [N] must be the
# same length as the vector [0]" — now guarded with a clear skip note.

make_min_syntax <- function() {
  # A minimal parsed syntax + converted-code pair so generate_rmd runs.
  list(list(command_type = "COMMENT", raw = "* test."))
}

make_min_converted <- function() {
  # generate_rmd() indexes converted_code with a logical vector built from each
  # entry's $is_transformation, so an entry (list-of-lists) is required — a bare
  # empty list() makes the subscript invalid.
  list(list(r_code = "# noop", analysis_type = "Descriptives",
            is_transformation = FALSE))
}

test_that("generate_rmd emits a row-aligned merge block for secondary .sav files", {
  sav <- withr::local_tempfile(fileext = ".sav")
  df <- data.frame(GROUP = c(1, 1, 2, 2), Y = c(10, 11, 12, 13))
  haven::write_sav(df, sav)
  outdir <- withr::local_tempdir()

  res <- generate_rmd(
    sav_data = list(data = df), sav_path = sav,
    parsed_syntax = make_min_syntax(), converted_code = make_min_converted(),
    output_dir = outdir, base_name = "t",
    secondary_sav_files = c("DataQuest.sav", "Extra Covariates.sav")
  )
  rmd <- readLines(res$rmd_path, warn = FALSE)
  txt <- paste(rmd, collapse = "\n")

  # The merge loop is emitted over the secondary basenames.
  expect_true(grepl("Merge additional paired .sav files", txt, fixed = TRUE))
  expect_true(grepl('"DataQuest.sav"', txt, fixed = TRUE))
  expect_true(grepl('"Extra Covariates.sav"', txt, fixed = TRUE))
  # It only binds NEW columns and guards on matching row count.
  expect_true(grepl("setdiff(names(.sd), names(data))", txt, fixed = TRUE))
  expect_true(grepl("nrow(.sd) == nrow(data)", txt, fixed = TRUE))
})

test_that("generate_rmd emits NO merge block when there are no secondary .sav files", {
  sav <- withr::local_tempfile(fileext = ".sav")
  df <- data.frame(GROUP = c(1, 2), Y = c(1, 2))
  haven::write_sav(df, sav)
  outdir <- withr::local_tempdir()

  res <- generate_rmd(
    sav_data = list(data = df), sav_path = sav,
    parsed_syntax = make_min_syntax(), converted_code = make_min_converted(),
    output_dir = outdir, base_name = "t",
    secondary_sav_files = character()
  )
  txt <- paste(readLines(res$rmd_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("Merge additional paired .sav files", txt, fixed = TRUE))
})

test_that("the secondary-merge basename filter drops the primary and non-.sav entries", {
  sav <- withr::local_tempfile(fileext = ".sav")
  df <- data.frame(GROUP = c(1, 2), Y = c(1, 2))
  haven::write_sav(df, sav)
  outdir <- withr::local_tempdir()

  # Pass the primary's own basename + a .csv; both must be filtered out so the
  # merge loop is not emitted (nothing genuinely secondary remains).
  res <- generate_rmd(
    sav_data = list(data = df), sav_path = sav,
    parsed_syntax = make_min_syntax(), converted_code = make_min_converted(),
    output_dir = outdir, base_name = "t",
    secondary_sav_files = c(basename(sav), "notes.csv")
  )
  txt <- paste(readLines(res$rmd_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("Merge additional paired .sav files", txt, fixed = TRUE))
})

test_that("convert_glm guards repeated-measures ANOVA against missing rmCells measures", {
  # A GLM whose within-subjects DVs are not in the data must NOT emit a bare
  # jmv::anovaRM (which would throw the cryptic names-attribute cascade); it must
  # emit a presence check that skips with a clear note.
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  cg  <- getFromNamespace("convert_glm", "spss2rmarkdown")
  cmd <- paste0("GLM AppCSM_Median_Pre AppCSM_Median_Post AvoidCSM_Median_Pre ",
                "AvoidCSM_Median_Post BY Group\n  /WSFACTOR = cs 2 aa 2\n",
                "  /EMMEANS=TABLES(cs)")
  p <- psc(cmd)
  out <- cg(p, NULL)$r_code
  expect_true(grepl("jmv::anovaRM", out, fixed = TRUE))
  expect_true(grepl(".rm_missing <- setdiff(.rm_measures, names(data))", out, fixed = TRUE))
  expect_true(grepl("Analysis skipped", out, fixed = TRUE))
})
