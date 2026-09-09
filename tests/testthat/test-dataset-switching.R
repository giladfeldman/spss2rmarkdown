# A script that switches datasets must run each command against the ACTIVE one.
#
# SPSS keeps several datasets open at once:
#
#   GET FILE='wave_2013.sav'.     -> that file becomes the active dataset
#   DATASET NAME DataSet4.        -> names the active dataset
#   DATASET ACTIVATE DataSet4.    -> makes that named dataset active again
#
# Four corpus items in the uh3n8 deposit run the SAME analyses against
# 2-3 waves this way. We loaded ONE primary dataset and row-merged the others
# positionally (a no-op with a Note when the row counts differ, which they do:
# 1828 / 287 / 287), so every command ran against one wave.
#
# MEASURED END TO END on "Syntax 6_reliability measures.sps" (uh3n8), rendered
# against the three real .sav files and compared with the frozen SPSS ground
# truth for that file.
#
# The three waves genuinely differ, which is what makes the item decidable:
#   2013  n=1828  has NONE of int04, int05, ext07, risk01
#   2015  n=287   has int04, int05, risk01 -- not ext07, ext04, risk00
#   2017  n=287   has int04, int05, risk01 -- not ext04, risk00
#
# So SPSS failed on exactly five commands, and the gold records exactly those
# five: int04, ext07, risk01 on the 2013 wave; ext04, risk00 on the 2017 wave.
#
# AFTER the fix our rendered report fires exactly those five errors, by name --
# 'INT04','INT05' / 'EXT07','EXT09','EXT10','EXT11' / 'RISK01'..'RISK07' /
# 'EXT04' / 'RISK00' -- and the four analyses SPSS ran successfully now produce
# its numbers, which we never produced before:
#
#   Cronbach's alpha  ours 0.5785012 -> gold .579   (2015 int04 int05)
#                     ours 0.8069569 -> gold .807   (2015 ext09 ext10 ext11)
#                     ours 0.8742308 -> gold .874   (2015 risk01..risk07 riskma)
#   item-rest         ours 0.4135428 -> gold .414
#                     ours .6654/.6465/.6642 -> gold .665/.647/.664
#                     ours 0.7084792 -> gold .708 (RISK01)
#
# NOTE for anyone re-checking by eye: the 2015 and 2017 files carry IDENTICAL
# int04/int05 columns (both n=287, means 3.432056 / 4.076655), so two analyses
# legitimately report the same alpha. That is the data, not a stale cache --
# verified by reading both .sav files directly.
#
# The design point that makes this work: generate_rmd() emits ALL
# transformations before ALL analyses, so command order is not preserved across
# that boundary. A per-dataset MUTABLE STORE is what keeps the meaning intact --
# each wave's COMPUTEs mutate only their own frame, so an analysis still sees
# them, and the same variable recomputed with a different formula per wave
# (Syntax 7 does this for risk01_prep2, intcom and risk_per) cannot collide.

test_that("annotate_dataset_state tracks GET FILE, DATASET NAME and ACTIVATE", {
  # These raws deliberately KEEP the trailing "." command terminator, which
  # split_commands() strips before the annotator ever sees it in the real
  # pipeline. That is not a production input, so the terminator-in-the-name
  # bug this fixture caught was never live -- but the annotator is cheap to
  # make robust either way, and the end-to-end test below (which does go
  # through parse_sps) is the authoritative one.
  ann <- getFromNamespace("annotate_dataset_state", "spss2rmarkdown")
  key <- function(x) x$dataset_key

  cmds <- list(
    list(command_type = "DESCRIPTIVES", raw = "des x."),                       # 1
    list(command_type = "GET FILE",  raw = "GET\n  FILE='wave_2013.sav'."),    # 2
    list(command_type = "DATASET",   raw = "DATASET NAME DataSet4 WINDOW=FRONT."), # 3
    list(command_type = "RELIABILITY", raw = "RELIABILITY /VARIABLES=a b."),   # 4
    list(command_type = "GET FILE",  raw = "GET\n  FILE='wave_2015.sav'."),    # 5
    list(command_type = "DATASET",   raw = "DATASET NAME DataSet5."),          # 6
    list(command_type = "RELIABILITY", raw = "RELIABILITY /VARIABLES=a b."),   # 7
    list(command_type = "DATASET",   raw = "DATASET ACTIVATE DataSet4."),      # 8
    list(command_type = "RELIABILITY", raw = "RELIABILITY /VARIABLES=a b."),   # 9
    list(command_type = "DATASET",   raw = "dataset activate dataset5."),      # 10 lower-case
    list(command_type = "RELIABILITY", raw = "RELIABILITY /VARIABLES=a b."),   # 11
    list(command_type = "DATASET",   raw = "DATASET ACTIVATE NeverOpened."),   # 12
    list(command_type = "RELIABILITY", raw = "RELIABILITY /VARIABLES=a b.")    # 13
  )
  out <- ann(cmds)

  # Before any GET FILE the active dataset is whatever the researcher had open,
  # which the .sps does not record: NA means "the primary dataset".
  expect_true(is.na(key(out[[1]])))
  expect_equal(key(out[[2]]), "wave_2013.sav")
  expect_equal(key(out[[4]]), "wave_2013.sav")
  expect_equal(key(out[[7]]), "wave_2015.sav")
  # ACTIVATE returns to the earlier file, and is case-insensitive on both the
  # verb and the dataset name.
  expect_equal(key(out[[9]]),  "wave_2013.sav")
  expect_equal(key(out[[11]]), "wave_2015.sav")
  # Activating a dataset we never saw opened is UNKNOWN, not "keep the last
  # one" -- silently keeping it would attribute this command to the wrong wave.
  expect_true(is.na(key(out[[13]])))
})

test_that("a GET FILE whose path is unreadable still counts as a switch", {
  ann <- getFromNamespace("annotate_dataset_state", "spss2rmarkdown")
  out <- ann(list(
    list(command_type = "GET FILE", raw = "GET FILE='wave_2013.sav'."),
    list(command_type = "GET FILE", raw = "GET FILE."),           # no path at all
    list(command_type = "DESCRIPTIVES", raw = "des x.")))
  expect_equal(out[[1]]$dataset_key, "wave_2013.sav")
  # Staying on wave_2013 here would silently credit the next block's commands
  # to the previous wave.
  expect_true(is.na(out[[3]]$dataset_key))
})

test_that("a directory path in GET FILE reduces to the basename", {
  ann <- getFromNamespace("annotate_dataset_state", "spss2rmarkdown")
  out <- ann(list(list(command_type = "GET FILE",
                       raw = "GET FILE='C:\\\\studies\\\\wave 2015.sav'.")))
  expect_equal(out[[1]]$dataset_key, "wave 2015.sav")
})

test_that("the real corpus item resolves to its three waves, in order", {
  # END-TO-END through parse_sps(), not a direct call to the annotator.
  sps <- file.path(withr::local_tempdir(), "Syntax 6.sps")
  writeLines(c(
    "GET", "  FILE='Data after syntax 5_multivariate outliers removed_2013.sav'.",
    "DATASET NAME DataSet4 WINDOW=FRONT.", "dataset activate DataSet4.",
    "RELIABILITY", "  /VARIABLES=int04 int05", "  /MODEL=ALPHA.",
    "GET", "  FILE='Data after syntax 5_multivariate outliers removed_2015.sav'.",
    "DATASET NAME DataSet5 WINDOW=FRONT.", "DATASET ACTIVATE DataSet5.",
    "RELIABILITY", "  /VARIABLES=int04 int05", "  /MODEL=ALPHA.",
    "GET", "  FILE='Data after syntax 5_multivariate outliers removed_2017.sav'.",
    "DATASET NAME DataSet27 WINDOW=FRONT.", "DATASET ACTIVATE DataSet27.",
    "RELIABILITY", "  /VARIABLES=int04 int05", "  /MODEL=ALPHA."), sps)

  cmds <- parse_sps(sps)
  rel <- Filter(function(cc) isTRUE(cc$command_type == "RELIABILITY"), cmds)
  expect_length(rel, 3L)
  expect_equal(vapply(rel, function(cc) cc$dataset_key, character(1)),
               c("Data after syntax 5_multivariate outliers removed_2013.sav",
                 "Data after syntax 5_multivariate outliers removed_2015.sav",
                 "Data after syntax 5_multivariate outliers removed_2017.sav"))
})

test_that("a single-dataset script emits NO dataset-switching machinery", {
  # The unchanged path. A report that never switches datasets must look exactly
  # as it did before, or this feature has a blast radius over the whole corpus.
  out_dir <- withr::local_tempdir()
  generate_rmd(
    sav_data = list(data = data.frame(A = 1:3)), sav_path = "only.sav",
    parsed_syntax = list(list(raw = "des A.", command_type = "DESCRIPTIVES")),
    converted_code = list(list(
      r_code = 'jmv::descriptives(data = data, vars = c("A"))',
      analysis_type = "Descriptive Statistics", variables = "A",
      dataset_key = NA_character_, order = 1)),
    output_dir = out_dir, base_name = "single")
  rmd <- paste(readLines(file.path(out_dir, "single.Rmd"), warn = FALSE), collapse = "\n")
  expect_false(grepl(".s2r_activate", rmd, fixed = TRUE))
  expect_false(grepl(".s2r_ds", rmd, fixed = TRUE))
  expect_false(grepl("Several datasets are in play", rmd, fixed = TRUE))
})

test_that("a multi-dataset script activates the right dataset per analysis", {
  out_dir <- withr::local_tempdir()
  generate_rmd(
    sav_data = list(data = data.frame(A = 1:3)), sav_path = "wave_2013.sav",
    parsed_syntax = list(
      list(raw = "des A.", command_type = "DESCRIPTIVES"),
      list(raw = "des A.", command_type = "DESCRIPTIVES")),
    converted_code = list(
      list(r_code = 'jmv::descriptives(data = data, vars = c("A"))',
           analysis_type = "Descriptive Statistics", variables = "A",
           dataset_key = "wave_2013.sav", order = 1),
      list(r_code = 'jmv::descriptives(data = data, vars = c("A"))',
           analysis_type = "Descriptive Statistics", variables = "A",
           dataset_key = "wave_2015.sav", order = 2)),
    output_dir = out_dir, base_name = "multi")
  rmd <- paste(readLines(file.path(out_dir, "multi.Rmd"), warn = FALSE), collapse = "\n")

  expect_true(grepl(".s2r_ds <- new.env", rmd, fixed = TRUE))
  expect_true(grepl('.s2r_activate("wave_2013.sav")', rmd, fixed = TRUE))
  expect_true(grepl('.s2r_activate("wave_2015.sav")', rmd, fixed = TRUE))
  # The primary is seeded so it is never re-read from disk.
  expect_true(grepl('.s2r_ds[[toupper("wave_2013.sav")]] <- data', rmd, fixed = TRUE))
  # The switch must precede the missing-variable pre-flight, or every analysis
  # would be pre-flighted against the previous wave's columns and name the
  # wrong variables.
  act <- regexpr('.s2r_activate("wave_2015.sav")', rmd, fixed = TRUE)
  pre <- regexpr(".s2r_missing_vars(data,", substring(rmd, act), fixed = TRUE)
  expect_gt(pre, 0)
})

test_that("a dataset the job never received is a NAMED error, not a fallback", {
  # The whole point. Falling back to the primary would print entirely plausible
  # numbers computed from the wrong wave -- the one outcome that must not stand.
  out_dir <- withr::local_tempdir()
  generate_rmd(
    sav_data = list(data = data.frame(A = 1:3)), sav_path = "wave_2013.sav",
    parsed_syntax = list(list(raw = "des A.", command_type = "DESCRIPTIVES"),
                         list(raw = "des A.", command_type = "DESCRIPTIVES")),
    converted_code = list(
      list(r_code = 'jmv::descriptives(data = data, vars = c("A"))',
           analysis_type = "Descriptive Statistics", variables = "A",
           dataset_key = "wave_2013.sav", order = 1),
      list(r_code = 'jmv::descriptives(data = data, vars = c("A"))',
           analysis_type = "Descriptive Statistics", variables = "A",
           dataset_key = "absent_wave.sav", order = 2)),
    output_dir = out_dir, base_name = "missing")
  rmd <- paste(readLines(file.path(out_dir, "missing.Rmd"), warn = FALSE), collapse = "\n")

  # Run the emitted helper exactly as the report defines it.
  env <- new.env(parent = globalenv())
  assign("data", data.frame(A = 1:3), envir = env)
  assign("normalize_spss_names", function(x) toupper(x), envir = env)
  assign("clean_data", function(d) d, envir = env)
  # Capture through .s2r_commit's closing brace: a lazy match to the first
  # "\n}" now stops inside .s2r_activate's own guard clauses.
  helper <- regmatches(rmd, regexpr("(?s)\\.s2r_ds <- new\\.env.*invisible\\(value\\)\\n\\}",
                                    rmd, perl = TRUE))
  expect_gt(nchar(helper), 0)
  eval(parse(text = helper), envir = env)
  act <- get(".s2r_activate", envir = env)

  # A dataset that IS present resolves; the absent one raises, naming itself.
  expect_s3_class(act(NA_character_), "data.frame")
  err <- tryCatch({ act("absent_wave.sav"); NULL }, error = function(e) conditionMessage(e))
  expect_true(!is.null(err))
  expect_match(err, "absent_wave.sav", fixed = TRUE)
  expect_match(err, "not provided", fixed = TRUE)
})
