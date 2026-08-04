# Regression test for R-0047: the multi-.sav positional merge (see
# test-multi-sav-merge.R) gates on nrow(.sd) == nrow(data), but equal row
# counts are not proof of alignment. Two paired .sav files with the SAME N
# but a DIFFERENT case/sort order pass that gate and get cbind-ed
# positionally anyway, silently misjoining every row.
#
# Fix: when a shared ID-like column exists (canonical name match, e.g. ID/
# CASE/SUBJECT/PARTICIPANT, or any other shared column name as fallback),
# generate_rmd()'s merge block now checks it aligns row-for-row before
# binding; a same-N different-sort mismatch is skipped with a loud NOTE
# instead of silently merging. Verified here by actually eval()-ing the
# generated merge code against on-disk fixtures (not just grepping the
# generated text), so this test exercises the runtime behavior the rec
# flagged as unverified.

make_min_syntax <- function() {
  list(list(command_type = "COMMENT", raw = "* test."))
}

make_min_converted <- function() {
  list(list(r_code = "# noop", analysis_type = "Descriptives",
            is_transformation = FALSE))
}

# Extract just the secondary-merge for-loop from the generated Rmd's
# load-data chunk so it can be eval()-ed standalone against fixture data
# frames, without needing to knit the whole document.
extract_merge_block <- function(rmd_path) {
  txt <- paste(readLines(rmd_path, warn = FALSE), collapse = "\n")
  start <- regexpr("# ---- Merge additional paired .sav files", txt, fixed = TRUE)
  stopifnot(start > 0)
  rest <- substring(txt, start)
  end <- regexpr("\n# Convert haven_labelled", rest, fixed = TRUE)
  stopifnot(end > 0)
  substring(rest, 1, end - 1)
}

test_that("equal-N but differently-sorted secondary .sav is skipped with a loud NOTE (ID column present)", {
  outdir <- withr::local_tempdir()
  old_wd <- setwd(outdir)
  on.exit(setwd(old_wd), add = TRUE)

  # Primary: sorted by ID 1..4. Secondary: SAME 4 rows, SAME ID column, but
  # sorted in reverse -- same N, different order. A positional bind would
  # attach secondary's AGE for ID 4 to primary's ID 1, etc.
  data <- data.frame(ID = 1:4, GROUP = c(1, 1, 2, 2))
  sec_df <- data.frame(ID = 4:1, AGE = c(40, 30, 20, 10))
  sec_path <- file.path(outdir, "secondary.sav")
  haven::write_sav(sec_df, sec_path)

  sav <- file.path(outdir, "primary.sav")
  haven::write_sav(data, sav)

  res <- generate_rmd(
    sav_data = list(data = data), sav_path = sav,
    parsed_syntax = make_min_syntax(), converted_code = make_min_converted(),
    output_dir = outdir, base_name = "t",
    secondary_sav_files = "secondary.sav"
  )

  block <- extract_merge_block(res$rmd_path)
  # Sanity: the emitted code references the ID-pattern guard added for R-0047.
  expect_true(grepl(".s2r_id_pattern", block, fixed = TRUE))

  out <- capture.output(eval(parse(text = block)))
  msg <- paste(out, collapse = " ")

  # The mismatch must be caught: AGE must NOT be silently merged onto `data`,
  # and a clear note must explain why (not just a generic skip).
  expect_false("AGE" %in% names(data))
  expect_true(grepl("does not align row-for-row", msg, fixed = TRUE))
  expect_true(grepl("secondary.sav", msg, fixed = TRUE))
})

test_that("equal-N, same-sort secondary .sav with a matching ID column merges normally", {
  outdir <- withr::local_tempdir()
  old_wd <- setwd(outdir)
  on.exit(setwd(old_wd), add = TRUE)

  data <- data.frame(ID = 1:4, GROUP = c(1, 1, 2, 2))
  sec_df <- data.frame(ID = 1:4, AGE = c(10, 20, 30, 40))
  sec_path <- file.path(outdir, "secondary.sav")
  haven::write_sav(sec_df, sec_path)

  sav <- file.path(outdir, "primary.sav")
  haven::write_sav(data, sav)

  res <- generate_rmd(
    sav_data = list(data = data), sav_path = sav,
    parsed_syntax = make_min_syntax(), converted_code = make_min_converted(),
    output_dir = outdir, base_name = "t",
    secondary_sav_files = "secondary.sav"
  )

  block <- extract_merge_block(res$rmd_path)
  capture.output(eval(parse(text = block)))

  # Properly aligned merges must still work -- the guard must not be so
  # conservative it blocks the legitimate case.
  expect_true("AGE" %in% names(data))
  expect_equal(as.numeric(data$AGE), c(10, 20, 30, 40))
})

test_that("no shared ID-like or other column: merge proceeds under the documented row-order caveat", {
  outdir <- withr::local_tempdir()
  old_wd <- setwd(outdir)
  on.exit(setwd(old_wd), add = TRUE)

  # No shared column at all between primary and secondary -- nothing to
  # verify alignment against. Existing (pre-R-0047) behavior is preserved:
  # merge on row-count match, but the emitted code now documents the
  # precondition explicitly.
  data <- data.frame(GROUP = c(1, 1, 2, 2), Y = c(5, 6, 7, 8))
  sec_df <- data.frame(AGE = c(10, 20, 30, 40))
  sec_path <- file.path(outdir, "secondary.sav")
  haven::write_sav(sec_df, sec_path)

  sav <- file.path(outdir, "primary.sav")
  haven::write_sav(data, sav)

  res <- generate_rmd(
    sav_data = list(data = data), sav_path = sav,
    parsed_syntax = make_min_syntax(), converted_code = make_min_converted(),
    output_dir = outdir, base_name = "t",
    secondary_sav_files = "secondary.sav"
  )

  block <- extract_merge_block(res$rmd_path)
  # The row-order precondition is now documented in the generated code.
  expect_true(grepl("POSITIONALLY", block, fixed = TRUE))

  capture.output(eval(parse(text = block)))
  expect_true("AGE" %in% names(data))
  expect_equal(as.numeric(data$AGE), c(10, 20, 30, 40))
})
