# A jmv analysis on a variable that is not in the data must name that variable.
#
# Found 2026-09-05 while running the conversion corpus. When a generated .Rmd
# calls a jmv analysis with a variable absent from `data`, jmv raises
#
#   'names' attribute [49] must be the same length as the vector [0]
#
# where 49 is ncol(data). The message names NO variable, so:
#   1. the researcher reading the HTML report learns nothing actionable, and
#   2. an automated comparison against the original SPSS output is left
#      "fired errors name no variable token this detector can extract",
#      leaving us unable to say whether the researcher's syntax is broken
#      (SOURCE_DEFECT) or we broke it (OURS).
#
# Reproduced locally against jmv 2.7.7 / R 4.4.0 with the corpus pairing
# uh3n8 "Syntax 7_building variable indexes.sps" + "Data 1_Ground file_Complete
# dataset all years.sav" (49 cols, no INTCOM); the two-sided control
# jmv::descriptives(vars = <an existing column>) succeeds, so jmv itself is
# healthy and the defect is purely the message.
#
# The fix is a GENERAL pre-flight emitted into every analysis chunk from the
# variable list the converter already declares in `conv$variables` — never a
# per-file or per-variable special case, and it does NOT repair the
# researcher's syntax: a missing variable stays an error, just a named one.

# ---------------------------------------------------------------------------
# Pull the analysis-1 chunk BODY out of a generated .Rmd and run it, with the
# render-time helpers bound exactly as .s2r_helpers_chunk() binds them. This
# exercises the real emission site rather than a reimplementation of it.
.s2r_eval_analysis_chunk <- function(rmd, data) {
  chunk <- regmatches(rmd, regexpr("(?s)```[{]r analysis-1.*?```", rmd, perl = TRUE))
  stopifnot(length(chunk) == 1L, nchar(chunk) > 0L)
  body <- sub("(?s)^```[{][^}]*[}]\n", "", chunk, perl = TRUE)
  body <- sub("(?s)```\\s*$", "", body, perl = TRUE)
  env <- new.env(parent = globalenv())
  # Bind the helpers the way the RENDERED report does: by evaluating the
  # document's OWN s2r-helpers chunk. A hand-written list here is a
  # reimplementation of .s2r_helpers_chunk() and drifts from it in silence --
  # .s2r_case_hint was added to the report and not to the list, so under an
  # installed package every missing-variable error came back as "could not
  # find function \".s2r_case_hint\"" instead of naming the variable. pkgload
  # masked it: load_all() puts the internals on the search path, so R CMD
  # check was the only run that could see it.
  hel <- regmatches(rmd, regexpr("(?s)```[{]r s2r-helpers.*?\n```", rmd, perl = TRUE))
  stopifnot(length(hel) == 1L, nchar(hel) > 0L)
  hbody <- sub("(?s)^```[{][^}]*[}]\n", "", hel, perl = TRUE)
  hbody <- sub("(?s)```\\s*$", "", hbody, perl = TRUE)
  eval(parse(text = hbody), envir = env)
  assign("data", data, envir = env)
  capture.output(eval(parse(text = body), envir = env))
}

test_that("the pre-flight helper names exactly the absent variables", {
  miss <- getFromNamespace(".s2r_missing_vars", "spss2rmarkdown")
  d <- data.frame(A = 1:3, B = 4:6)
  expect_equal(miss(d, c("A", "B")), character(0))
  expect_equal(miss(d, c("A", "INTCOM", "INT04")), c("INTCOM", "INT04"))
  # Degenerate inputs must be no-ops, never a spurious error: an analysis with
  # no declared variables, and the no-.sav path where `data` is NULL.
  expect_equal(miss(d, character(0)), character(0))
  expect_equal(miss(d, NULL), character(0))
  expect_equal(miss(NULL, c("A")), character(0))
})

test_that("only variables the emitted code actually references are pre-flighted", {
  # FALSE-POSITIVE GUARD, measured before the fix was written.
  #
  # A corpus-wide probe over all 190 .sps files (992 analyses, 945 of which
  # declare `variables`) found 48 declared names that never appear in the
  # emitted r_code at all — parser artifacts from one malformed paired T-TEST
  # ("T.TEST", "X.PAIRED.", "PAIRS.CRAGUN_NRNS_RG"). Pre-flighting those would
  # abort analyses that currently run fine.
  #
  # Intersecting the declared list with the names the code actually mentions is
  # both general and strictly correct: a variable the call never references
  # cannot be the reason the call failed.
  keep <- getFromNamespace(".s2r_preflight_vars", "spss2rmarkdown")
  code <- 'jmv::ttestPS(data = data, pairs = list(list(i1 = "AGE", i2 = "AGE.2")))'
  expect_equal(keep(code, c("AGE", "AGE.2", "T.TEST", "X.PAIRED.")),
               c("AGE", "AGE.2"))
  # A name must not match inside a longer identifier.
  expect_equal(keep('jmv::descriptives(data = data, vars = c("INT045"))', "INT04"),
               character(0))
  expect_equal(keep("1 + 1", character(0)), character(0))
  expect_equal(keep("1 + 1", NULL), character(0))
})

test_that("the generated analysis chunk pre-flights the declared variables", {
  gen <- getFromNamespace("generate_rmd", "spss2rmarkdown")
  conv <- list(list(
    r_code = 'jmv::descriptives(data = data, vars = c("INTCOM"))',
    analysis_type = "Descriptive Statistics",
    variables = "INTCOM",
    order = 1))
  out_dir <- withr::local_tempdir()
  gen(NULL, "dummy.sav",
      list(list(raw = "DESCRIPTIVES VARIABLES=intcom.", command_type = "DESCRIPTIVES")),
      conv, output_dir = out_dir)
  rmd <- paste(readLines(list.files(out_dir, pattern = "[.]Rmd$", full.names = TRUE)[1],
                         warn = FALSE), collapse = "\n")
  # The helper must be DEFINED in the .Rmd (deparse-injected), or the chunk
  # would fail at render time with "could not find function".
  expect_true(grepl(".s2r_missing_vars <- function", rmd, fixed = TRUE))
  # ...and CALLED, with the converter-declared variable list.
  expect_true(grepl(".s2r_missing_vars(data,", rmd, fixed = TRUE))
  expect_true(grepl('"INTCOM"', rmd, fixed = TRUE))
})

test_that("an analysis with no declared variables still emits a runnable chunk", {
  # The guard is emitted at a single site for EVERY analysis, so converters
  # that declare no `variables` must degrade to a no-op rather than break.
  gen <- getFromNamespace("generate_rmd", "spss2rmarkdown")
  conv <- list(list(r_code = "1 + 1", analysis_type = "Test", order = 1))
  out_dir <- withr::local_tempdir()
  gen(NULL, "dummy.sav",
      list(list(raw = "FREQUENCIES x.", command_type = "FREQUENCIES")),
      conv, output_dir = out_dir)
  rmd <- paste(readLines(list.files(out_dir, pattern = "[.]Rmd$", full.names = TRUE)[1],
                         warn = FALSE), collapse = "\n")
  out <- .s2r_eval_analysis_chunk(rmd, data.frame(A = 1:3))
  expect_false(any(grepl("Analysis error", out)))
})

test_that("the fired error names the missing variables in a form triage can read", {
  # This is the end-to-end contract with the automated comparison against
  # the original SPSS output: such tooling extracts variable
  # tokens from a fired "**Analysis error:**" paragraph with the regex
  #   /['‘’“”`]([A-Za-z_][A-Za-z0-9_.]{1,63})['‘’“”`]/g
  # so the names must be QUOTED, or the item stays UNDECIDABLE no matter how
  # readable the sentence is to a human.
  gen <- getFromNamespace("generate_rmd", "spss2rmarkdown")
  conv <- list(list(
    r_code = 'jmv::descriptives(data = data, vars = c("INTCOM", "INT04"))',
    analysis_type = "Descriptive Statistics",
    variables = c("INTCOM", "INT04"),
    order = 1))
  out_dir <- withr::local_tempdir()
  gen(NULL, "dummy.sav",
      list(list(raw = "DESCRIPTIVES VARIABLES=intcom int04.",
                command_type = "DESCRIPTIVES")),
      conv, output_dir = out_dir)
  rmd <- paste(readLines(list.files(out_dir, pattern = "[.]Rmd$", full.names = TRUE)[1],
                         warn = FALSE), collapse = "\n")

  # A 49-column frame with neither INTCOM nor INT04 — the shape that produced
  # the "'names' attribute [49] ... vector [0]" message in the corpus.
  d <- as.data.frame(matrix(rnorm(49 * 5), nrow = 5))
  names(d) <- paste0("V", seq_len(49))

  out <- paste(.s2r_eval_analysis_chunk(rmd, d), collapse = " ")
  expect_match(out, "Analysis error", fixed = TRUE)
  # A missing variable must STILL be an error — we relay source failures, we
  # do not repair the researcher's syntax.
  expect_false(grepl("'names' attribute", out, fixed = TRUE))
  quoted <- regmatches(
    out,
    gregexpr("['\u2018\u2019\u201c\u201d`]([A-Za-z_][A-Za-z0-9_.]{1,63})['\u2018\u2019\u201c\u201d`]",
             out, perl = TRUE))[[1]]
  expect_true(any(grepl("INTCOM", quoted, fixed = TRUE)))
  expect_true(any(grepl("INT04", quoted, fixed = TRUE)))
})

test_that("a present variable is never pre-flighted into a spurious error", {
  # Two-sided control: the same generated chunk against data that HAS the
  # variables must run clean. A guard that fires on everything would satisfy
  # the test above while breaking every working report.
  gen <- getFromNamespace("generate_rmd", "spss2rmarkdown")
  conv <- list(list(
    r_code = 'jmv::descriptives(data = data, vars = c("INTCOM"))',
    analysis_type = "Descriptive Statistics",
    variables = "INTCOM",
    order = 1))
  out_dir <- withr::local_tempdir()
  gen(NULL, "dummy.sav",
      list(list(raw = "DESCRIPTIVES VARIABLES=intcom.", command_type = "DESCRIPTIVES")),
      conv, output_dir = out_dir)
  rmd <- paste(readLines(list.files(out_dir, pattern = "[.]Rmd$", full.names = TRUE)[1],
                         warn = FALSE), collapse = "\n")
  out <- paste(.s2r_eval_analysis_chunk(rmd, data.frame(INTCOM = rnorm(20))),
               collapse = " ")
  expect_false(grepl("not present in the dataset", out, fixed = TRUE))
})

test_that("a converter that guards its own variables is not pre-flighted twice", {
  # REGRESSION, measured 2026-09-05. convert_glm()'s repeated-measures branch
  # already checks its within-subjects measures against names(data) and reports
  # a deliberate SOFT "**Analysis skipped:**" note. Adding the general
  # pre-flight on top reported the same condition twice, and the harder report
  # won: the corpus/"Analyses" went GREEN -> YELLOW with no change in what the
  # analysis actually did. A converter that sets `self_guards_variables` owns
  # the reporting; the generator stands down.
  gen <- getFromNamespace("generate_rmd", "spss2rmarkdown")
  conv <- list(list(
    r_code = paste(
      '.rm_measures <- c("A_PRE", "A_POST")',
      '.rm_missing <- setdiff(.rm_measures, names(data))',
      'if (length(.rm_missing) > 0) {',
      '  cat("**Analysis skipped:** missing:", paste(.rm_missing, collapse = ", "), "\n\n")',
      '} else {',
      '  jmv::descriptives(data = data, vars = .rm_measures)',
      '}', sep = "
"),
    analysis_type = "Repeated-Measures ANOVA",
    variables = c("A_PRE", "A_POST"),
    self_guards_variables = TRUE,
    order = 1))
  out_dir <- withr::local_tempdir()
  gen(NULL, "dummy.sav",
      list(list(raw = "GLM A_Pre A_Post /WSFACTOR=t 2.", command_type = "GLM")),
      conv, output_dir = out_dir)
  rmd <- paste(readLines(list.files(out_dir, pattern = "[.]Rmd$", full.names = TRUE)[1],
                         warn = FALSE), collapse = "
")
  # The guard is still EMITTED (uniform template) but carries no variables.
  expect_true(grepl(".s2r_missing_vars(data, character(0))", rmd, fixed = TRUE))
  out <- paste(.s2r_eval_analysis_chunk(rmd, data.frame(B = 1:3)), collapse = " ")
  expect_false(grepl("Analysis error", out, fixed = TRUE))
  expect_match(out, "Analysis skipped", fixed = TRUE)
})

test_that("the repeated-measures converter declares that it self-guards", {
  # Pins the producer side: a future edit dropping the flag silently
  # reintroduces the double report.
  conv <- getFromNamespace("convert_glm", "spss2rmarkdown")
  parsed <- list(variables = list(
    dependent = c("A_PRE", "A_POST"),
    factors = character(), covariates = character(),
    ws_factors = list(list(name = "t", n = 2L))))
  res <- conv(parsed, NULL)
  expect_true(isTRUE(res$self_guards_variables))
})

test_that("a case-only near miss is named as OUR normalisation bug, not a missing variable", {
  # The comparison is deliberately case-SENSITIVE: the converter uppercases every
  # SPSS name and the loader uppercases every column, so they agree by
  # construction. A case-only disagreement means a converter path forgot to
  # normalise — the variable IS in the data and the fault is ours. Reported as a
  # bare "not present in the dataset" that is indistinguishable from a genuinely
  # absent variable. Measured cost 2026-09-09: a sibling session lost ~40 minutes
  # to `variables not present in the dataset: 'meanbenevo'` with MEANBENEVO right
  # there, because `with_vars` was missing from `var_name_keys`.
  hint <- getFromNamespace(".s2r_case_hint", "spss2rmarkdown")
  d <- data.frame(MEANBENEVO = 1:3, AGE = 4:6)

  out <- hint(d, "meanbenevo")
  expect_match(out, "case mismatch", fixed = TRUE)
  expect_match(out, "MEANBENEVO", fixed = TRUE)

  # TWO-SIDED: a genuinely absent variable gets NO hint. A hint that fired on
  # everything would bury the signal it exists to raise.
  expect_identical(hint(d, "NOSUCHVAR"), "")
  expect_identical(hint(d, character(0)), "")
  expect_identical(hint(NULL, "meanbenevo"), "")
})

test_that("the case hint adds NO token an automated comparison would extract", {
  # The suggestion must stay UNQUOTED. Tooling that compares our fired errors
  # against the original SPSS output pulls variable tokens out of QUOTED names
  # and judges a paragraph on all of them together, so a quoted suggestion would
  # inject a token that is not part of the researcher's request and could flip
  # that paragraph's verdict.
  hint <- getFromNamespace(".s2r_case_hint", "spss2rmarkdown")
  out <- hint(data.frame(MEANBENEVO = 1:3), "meanbenevo")
  QUOTED <- "['\u2018\u2019\u201c\u201d`]([A-Za-z_][A-Za-z0-9_.]{1,63})['\u2018\u2019\u201c\u201d`]"
  found <- regmatches(out, gregexpr(QUOTED, out, perl = TRUE))[[1]]
  expect_length(found, 0L)
  # ...and the control: the same extractor DOES find a quoted name, so the
  # emptiness above is a real zero and not a broken pattern.
  probe <- "variables not present in the dataset: 'MEANBENEVO'"
  expect_gt(length(regmatches(probe, gregexpr(QUOTED, probe, perl = TRUE))[[1]]), 0L)
})
