# Three ways two SPSS datasets could be treated as one, each producing a
# silently wrong number. All three were raised by the Grok 4.6 seat of a
# three-provider consult on 2026-09-09 against commit 6e2008b, and each was
# reproduced locally before being fixed.
#
# The shared cause: the dataset store was keyed purely on the .sav BASENAME, so
# anything that should be a SEPARATE dataset with the same underlying file
# collapsed into one slot and inherited the other's mutations.

test_that("a repeated GET FILE of the same file is a FRESH dataset", {
  # Grok 4.6, 2026-09-09. SPSS re-reads from disk on every GET FILE, discarding
  # computed columns:
  #
  #   GET FILE='w.sav'.  COMPUTE age = 99.  GET FILE='w.sav'.  DES age.
  #
  # SPSS prints the ORIGINAL age. Keyed on the filename alone, the second block
  # reused the first block's MUTATED frame and printed 99 -- no error, no
  # warning, a plausible wrong number. Each GET now gets its own instance key.
  ann <- getFromNamespace("annotate_dataset_state", "spss2rmarkdown")
  out <- ann(list(
    list(command_type = "GET FILE",     raw = "GET FILE='w.sav'"),
    list(command_type = "COMPUTE",      raw = "COMPUTE age = 99"),
    list(command_type = "GET FILE",     raw = "GET FILE='w.sav'"),
    list(command_type = "DESCRIPTIVES", raw = "des age"),
    list(command_type = "GET FILE",     raw = "GET FILE='w.sav'"),
    list(command_type = "DESCRIPTIVES", raw = "des age")))
  k <- vapply(out, function(x) x$dataset_key, character(1))
  expect_equal(k[1:2], c("w.sav", "w.sav"))
  # The second and third GET must NOT reuse the first instance's key.
  expect_equal(k[3:4], c("w.sav#2", "w.sav#2"))
  expect_equal(k[5:6], c("w.sav#3", "w.sav#3"))
  expect_equal(length(unique(k)), 3L)
})

test_that("a re-read instance resolves to the same file on disk", {
  # The instance suffix must not leak into the filename, or the second GET
  # would raise "not provided" for a file that is right there.
  out_dir <- withr::local_tempdir()
  generate_rmd(
    sav_data = list(data = data.frame(A = 1:3)), sav_path = "w.sav",
    parsed_syntax = list(list(raw = "des A.", command_type = "DESCRIPTIVES"),
                         list(raw = "des A.", command_type = "DESCRIPTIVES")),
    converted_code = list(
      list(r_code = 'jmv::descriptives(data = data, vars = c("A"))',
           analysis_type = "Descriptive Statistics", variables = "A",
           dataset_key = "w.sav", order = 1),
      list(r_code = 'jmv::descriptives(data = data, vars = c("A"))',
           analysis_type = "Descriptive Statistics", variables = "A",
           dataset_key = "w.sav#2", order = 2)),
    output_dir = out_dir, base_name = "reread")
  rmd <- paste(readLines(file.path(out_dir, "reread.Rmd"), warn = FALSE), collapse = "\n")
  expect_true(grepl('.s2r_activate("w.sav#2")', rmd, fixed = TRUE))

  env <- new.env(parent = globalenv())
  d0 <- data.frame(A = 1:3)
  assign("data", d0, envir = env)
  assign("normalize_spss_names", function(x) toupper(x), envir = env)
  assign("clean_data", function(d) d, envir = env)
  helper <- regmatches(rmd, regexpr("(?s)\\.s2r_ds <- new\\.env.*invisible\\(value\\)\\n\\}", rmd, perl = TRUE))
  # Bind the render-time helpers the way the RENDERED report does: by evaluating
  # the document's OWN s2r-helpers chunk. The hand-written `assign()` stubs above
  # are NOT sufficient -- they are a reimplementation of .s2r_helpers_chunk() and
  # drift from it in silence. Measured 2026-09-11: `.s2r_key_looks_like_path` was
  # added to the report and not to the stub list, so under an INSTALLED package
  # the emitted .s2r_get() died with `could not find function
  # ".s2r_key_looks_like_path"` instead of naming the absent dataset. pkgload
  # masked it -- load_all() puts the package internals on the search path, so
  # `R CMD check` on the built tarball was the only run that could see it, and it
  # is the SECOND time this exact drift has happened (the first was
  # `.s2r_case_hint`, see test-missing-variable-preflight.R).
  #
  # The helpers chunk is emitted at position 8 of the document, before anything
  # that calls it, so this is also the faithful order.
  .hel <- regmatches(rmd, regexpr("(?s)```[{]r s2r-helpers.*?\n```", rmd, perl = TRUE))
  stopifnot(length(.hel) == 1L, nchar(.hel) > 0L)
  .hbody <- sub("(?s)^```[{][^}]*[}]\n", "", .hel, perl = TRUE)
  .hbody <- sub("(?s)```\\s*$", "", .hbody, perl = TRUE)
  eval(parse(text = .hbody), envir = env)
  eval(parse(text = helper), envir = env)
  act <- get(".s2r_activate", envir = env)
  commit <- get(".s2r_commit", envir = env)

  # Instance 1 is the seeded primary; mutate it.
  d1 <- act("w.sav"); d1$AGE <- 99; commit("w.sav", d1)
  expect_true("AGE" %in% names(act("w.sav")))
  # Instance 2 must NOT see that column. It is not on disk here, so the honest
  # outcome is the NAMED "not provided" error -- never the mutated frame.
  err <- tryCatch({ act("w.sav#2"); NULL }, error = function(e) conditionMessage(e))
  expect_false(is.null(err))
  expect_match(err, "w.sav", fixed = TRUE)
  # The instance suffix must be stripped before the filename is reported.
  expect_false(grepl("#2", err, fixed = TRUE))
})

test_that("DATASET COPY does not alias the dataset it was copied from", {
  # Grok 4.6, 2026-09-09. SPSS COPY is an INDEPENDENT duplicate:
  #
  #   DATASET COPY B. / ACTIVATE B. / COMPUTE score=1.
  #   ACTIVATE A. / COMPUTE score=2. / ACTIVATE B. / DES score.
  #   SPSS on B: 1.  Aliased to A's slot: 2.
  #
  # A faithful copy would need the source's state at that point in the syntax,
  # which the transformations-before-analyses emission order does not preserve.
  # So the copy is refused BY NAME rather than aliased and silently wrong.
  ann <- getFromNamespace("annotate_dataset_state", "spss2rmarkdown")
  out <- ann(list(
    list(command_type = "GET FILE", raw = "GET FILE='w.sav'"),
    list(command_type = "DATASET",  raw = "DATASET NAME A"),
    list(command_type = "DATASET",  raw = "DATASET COPY B"),
    list(command_type = "DATASET",  raw = "DATASET ACTIVATE B"),
    list(command_type = "COMPUTE",  raw = "COMPUTE score = 1"),
    list(command_type = "DATASET",  raw = "DATASET ACTIVATE A"),
    list(command_type = "COMPUTE",  raw = "COMPUTE score = 2"),
    list(command_type = "DATASET",  raw = "DATASET ACTIVATE B"),
    list(command_type = "DESCRIPTIVES", raw = "des score")))
  k <- vapply(out, function(x) x$dataset_key, character(1))
  # COPY does not change the active dataset.
  expect_equal(k[3], "w.sav")
  # Activating the copy is NOT the source's key.
  expect_false(identical(k[4], "w.sav"))
  expect_true(startsWith(k[4], "#copy-of#"))
  # Returning to A is the real file again, and the final analysis is on the copy.
  expect_equal(k[6], "w.sav")
  expect_true(startsWith(k[9], "#copy-of#"))
})

test_that("activating a DATASET COPY raises a named error, not a number", {
  out_dir <- withr::local_tempdir()
  generate_rmd(
    sav_data = list(data = data.frame(A = 1:3)), sav_path = "w.sav",
    parsed_syntax = list(list(raw = "des A.", command_type = "DESCRIPTIVES"),
                         list(raw = "des A.", command_type = "DESCRIPTIVES")),
    converted_code = list(
      list(r_code = 'jmv::descriptives(data = data, vars = c("A"))',
           analysis_type = "Descriptive Statistics", variables = "A",
           dataset_key = "w.sav", order = 1),
      list(r_code = 'jmv::descriptives(data = data, vars = c("A"))',
           analysis_type = "Descriptive Statistics", variables = "A",
           dataset_key = "#copy-of#w.sav", order = 2)),
    output_dir = out_dir, base_name = "copy")
  rmd <- paste(readLines(file.path(out_dir, "copy.Rmd"), warn = FALSE), collapse = "\n")
  env <- new.env(parent = globalenv())
  assign("data", data.frame(A = 1:3), envir = env)
  assign("normalize_spss_names", function(x) toupper(x), envir = env)
  assign("clean_data", function(d) d, envir = env)
  helper <- regmatches(rmd, regexpr("(?s)\\.s2r_ds <- new\\.env.*invisible\\(value\\)\\n\\}", rmd, perl = TRUE))
  # Bind the render-time helpers the way the RENDERED report does: by evaluating
  # the document's OWN s2r-helpers chunk. The hand-written `assign()` stubs above
  # are NOT sufficient -- they are a reimplementation of .s2r_helpers_chunk() and
  # drift from it in silence. Measured 2026-09-11: `.s2r_key_looks_like_path` was
  # added to the report and not to the stub list, so under an INSTALLED package
  # the emitted .s2r_get() died with `could not find function
  # ".s2r_key_looks_like_path"` instead of naming the absent dataset. pkgload
  # masked it -- load_all() puts the package internals on the search path, so
  # `R CMD check` on the built tarball was the only run that could see it, and it
  # is the SECOND time this exact drift has happened (the first was
  # `.s2r_case_hint`, see test-missing-variable-preflight.R).
  #
  # The helpers chunk is emitted at position 8 of the document, before anything
  # that calls it, so this is also the faithful order.
  .hel <- regmatches(rmd, regexpr("(?s)```[{]r s2r-helpers.*?\n```", rmd, perl = TRUE))
  stopifnot(length(.hel) == 1L, nchar(.hel) > 0L)
  .hbody <- sub("(?s)^```[{][^}]*[}]\n", "", .hel, perl = TRUE)
  .hbody <- sub("(?s)```\\s*$", "", .hbody, perl = TRUE)
  eval(parse(text = .hbody), envir = env)
  eval(parse(text = helper), envir = env)
  act <- get(".s2r_activate", envir = env)
  err <- tryCatch({ act("#copy-of#w.sav"); NULL }, error = function(e) conditionMessage(e))
  expect_false(is.null(err))
  expect_match(err, "DATASET COPY", fixed = TRUE)
  expect_match(err, "w.sav", fixed = TRUE)
  # CONTROL: the source itself still resolves normally.
  expect_s3_class(act("w.sav"), "data.frame")
})

test_that("the positional secondary merge stands down when datasets are switched", {
  # Grok 4.6, 2026-09-09. Positionally cbind-ing other uploaded .sav files into
  # the primary is a GUESS for scripts that reference variables spanning files
  # with no MATCH FILES. A script that switches datasets explicitly is not
  # guessing, and doing both lets a wave-keyed analysis succeed on a column
  # borrowed from a same-N sibling wave.
  mk <- function(keys) Map(function(k, i) list(
      r_code = 'jmv::descriptives(data = data, vars = c("A"))',
      analysis_type = "Descriptive Statistics", variables = "A",
      dataset_key = k, order = i), keys, seq_along(keys))
  syn <- lapply(seq_along(c(1, 2)), function(i)
    list(raw = "des A.", command_type = "DESCRIPTIVES"))

  # Multi-dataset: merge block suppressed, store emitted.
  d1 <- withr::local_tempdir()
  generate_rmd(sav_data = list(data = data.frame(A = 1:3)), sav_path = "w2013.sav",
               parsed_syntax = syn, converted_code = mk(c("w2013.sav", "w2015.sav")),
               output_dir = d1, base_name = "multi",
               secondary_sav_files = c("w2015.sav", "w2017.sav"))
  r1 <- paste(readLines(file.path(d1, "multi.Rmd"), warn = FALSE), collapse = "\n")
  expect_false(grepl("Merge additional paired", r1, fixed = TRUE))
  expect_true(grepl(".s2r_ds <- new.env", r1, fixed = TRUE))

  # CONTROL, single dataset: the merge must still happen exactly as before.
  d2 <- withr::local_tempdir()
  generate_rmd(sav_data = list(data = data.frame(A = 1:3)), sav_path = "w2013.sav",
               parsed_syntax = syn, converted_code = mk(c(NA_character_, NA_character_)),
               output_dir = d2, base_name = "single",
               secondary_sav_files = c("w2015.sav", "w2017.sav"))
  r2 <- paste(readLines(file.path(d2, "single.Rmd"), warn = FALSE), collapse = "\n")
  expect_true(grepl("Merge additional paired", r2, fixed = TRUE))
  expect_false(grepl(".s2r_ds <- new.env", r2, fixed = TRUE))
})
