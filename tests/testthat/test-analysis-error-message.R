# The analysis-chunk error handler must always render a NON-EMPTY reason.
#
# Found 2026-08-04 on the corpus/"Syntax_ Exploring factor analysis for
# refining variables_risk": the report contained
#   <p><strong>Analysis error:</strong></p>
# with no message at all. The underlying condition was a real and diagnosable
# one ("Can't subset columns that don't exist. x Columns `PASTI00`, `PAMA02`,
# ... don't exist." — the .sps needs EFA_2013.sav, which is absent from the OSF
# materials), but the user saw a blank error.
#
# Cause: the handler did `cat("**Analysis error:**", e$message, "\n")`.
# rlang/vctrs conditions carry multi-line, UTF-8 (U+2716) messages, and
# conditionMessage() on some condition objects returns character(0) — cat()
# then contributes nothing, silently swallowing the diagnosis.
#
# A blank error is worse than a verbose one: it tells the user something failed
# but gives them nothing to act on, and it makes the defect look like a harness
# artifact rather than a data problem.

test_that("the error handler renders a message for a multi-line rlang condition", {
  fmt <- getFromNamespace(".s2r_format_error", "spss2rmarkdown")
  e <- tryCatch(
    vctrs::vec_slice(data.frame(a = 1), c("nope")),
    error = function(e) e)
  msg <- fmt(e)
  expect_true(nzchar(msg))
  expect_false(grepl("^\\s*$", msg))
})

test_that("the error handler never returns an empty string", {
  fmt <- getFromNamespace(".s2r_format_error", "spss2rmarkdown")
  # Pathological conditions that previously produced a blank report line.
  empty_cond <- structure(class = c("simpleError", "error", "condition"),
                          list(message = character(0), call = NULL))
  expect_true(nzchar(fmt(empty_cond)))

  blank_cond <- simpleError("")
  expect_true(nzchar(fmt(blank_cond)))

  na_cond <- structure(class = c("simpleError", "error", "condition"),
                       list(message = NA_character_, call = NULL))
  expect_true(nzchar(fmt(na_cond)))
})

test_that("the error handler preserves an ordinary message verbatim", {
  fmt <- getFromNamespace(".s2r_format_error", "spss2rmarkdown")
  expect_match(fmt(simpleError("object 'x' not found")), "object 'x' not found",
               fixed = TRUE)
})

test_that("multi-line messages are flattened to stay inside one report line", {
  fmt <- getFromNamespace(".s2r_format_error", "spss2rmarkdown")
  out <- fmt(simpleError("first line\nsecond line"))
  expect_false(grepl("\n", out, fixed = TRUE))
  expect_match(out, "first line", fixed = TRUE)
  expect_match(out, "second line", fixed = TRUE)
})

test_that("generated chunks call the formatter, not bare e$message", {
  # Guards the actual emission SITES: a future edit reverting either handler to
  # `cat(..., e$message, ...)` reintroduces the blank-error defect. Both the
  # analysis handler and the transformation handler had it.
  gen <- getFromNamespace("generate_rmd", "spss2rmarkdown")
  conv <- list(list(r_code = "stop('boom')", analysis_type = "Test", order = 1))
  out_dir <- withr::local_tempdir()
  gen(NULL, "dummy.sav",
      list(list(raw = "FREQUENCIES x.", command_type = "FREQUENCIES")),
      conv, output_dir = out_dir)
  rmds <- list.files(out_dir, pattern = "\\.Rmd$", full.names = TRUE)
  expect_gt(length(rmds), 0)
  rmd <- paste(readLines(rmds[1], warn = FALSE), collapse = "\n")
  expect_true(grepl(".s2r_format_error(e)", rmd, fixed = TRUE))
  expect_false(grepl("cat(\"\\n\\n**Analysis error:**\", e$message", rmd, fixed = TRUE))
  # The helper must also be DEFINED in the generated Rmd, or the chunk would
  # fail with "could not find function" at render time.
  expect_true(grepl(".s2r_format_error <- function", rmd, fixed = TRUE))
})
