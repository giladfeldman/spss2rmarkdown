# Regression tests for FACTOR and MEANS variable extraction.
#
# Surfaced 2026-08-04 by the corpus item
# "Syntax_ Exploring factor analysis for refining variables_risk", which
# rendered YELLOW with two `object '.res' not found` analysis errors.
#
# Root cause (identical in shape to the EXAMINE defect fixed in
# test-examine-aggregate.R): neither FACTOR nor MEANS had a branch in
# extract_variables(), so `variables$all` stayed empty for EVERY syntax form.
# convert_factor()/convert_means() then emitted the bare comment
# "# FACTOR: No variables specified" as the RHS of the generated
# `.res <- <r_code>` assignment. A comment is not an expression, so `.res`
# was never bound and the following `s2r_render_tables(.res)` raised
# "object '.res' not found" — a hard error where a graceful skip was intended.
#
# Two independent guards below:
#   1. parse-level  — FACTOR/MEANS must actually extract their variables.
#   2. codegen-level — no converter may emit a comment-only r_code, because
#      that always produces the `.res <- # ...` cascade regardless of command.

test_that("FACTOR /VARIABLES on a continuation line extracts the analysis variables", {
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  p <- psc(paste0(
    "FACTOR\n",
    "/VARIABLES pasti00 pama02 risk01 risk02 risk03\n",
    "/MISSING PAIRWISE\n",
    "/ANALYSIS pasti00 pama02 risk01 risk02 risk03\n",
    "/PRINT INITIAL CORRELATION KMO EXTRACTION ROTATION\n",
    "/EXTRACTION PC\n",
    "/ROTATION VARIMAX\n",
    "/METHOD=CORRELATION"))
  expect_equal(unname(p$command_type), "FACTOR")
  expect_equal(p$variables$all,
               c("pasti00", "pama02", "risk01", "risk02", "risk03"))
})

test_that("FACTOR with an inline /VARIABLES clause also extracts variables", {
  ev <- getFromNamespace("extract_variables", "spss2rmarkdown")
  r <- ev("FACTOR /VARIABLES a1 a2 a3 /EXTRACTION PC", "FACTOR")
  expect_equal(r$all, c("a1", "a2", "a3"))
})

test_that("FACTOR /ANALYSIS is not double-counted into all", {
  ev <- getFromNamespace("extract_variables", "spss2rmarkdown")
  r <- ev(paste0("FACTOR\n/VARIABLES a1 a2\n/ANALYSIS a1 a2\n/EXTRACTION PC"),
          "FACTOR")
  expect_equal(r$all, c("a1", "a2"))
})

test_that("convert_factor emits a real analysis call, never a comment stub", {
  # Engine-agnostic on purpose: this test pins the INVARIANT (a real factor
  # analysis is emitted for a well-formed FACTOR command), not the mechanism.
  # The engine moved from jmv::efa to psych on 2026-08-04 because jmv::efa is
  # non-deterministic in the worker environment; see test-factor-psych-engine.R,
  # which owns the engine choice and the ground-truth numbers.
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  cf  <- getFromNamespace("convert_factor", "spss2rmarkdown")
  p <- psc("FACTOR\n/VARIABLES a1 a2 a3\n/EXTRACTION PC\n/ROTATION VARIMAX")
  out <- cf(p, NULL)
  expect_match(out$r_code, "psych::|jmv::efa")
  expect_true(grepl("a1", out$r_code, fixed = TRUE))
  expect_false(grepl("No variables specified", out$r_code, fixed = TRUE))
  # The generated code must be a parseable EXPRESSION, since the emitter wraps
  # it as `.res <- <r_code>`. A comment-only body silently yields no binding.
  expect_silent(parse(text = paste0(".res <- ", out$r_code)))
})

test_that("MEANS /TABLES extracts dependent and BY grouping variables", {
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  p <- psc("MEANS\n/TABLES = score BY condition\n/CELLS MEAN COUNT STDDEV")
  expect_equal(unname(p$command_type), "MEANS")
  expect_true("score" %in% p$variables$all)
  expect_true("condition" %in% p$variables$all)
  expect_equal(p$variables$dependent, "score")
  expect_equal(p$variables$factors, "condition")
})

test_that("MEANS TABLES without the /TABLES keyword still extracts variables", {
  ev <- getFromNamespace("extract_variables", "spss2rmarkdown")
  r <- ev("MEANS score BY condition", "MEANS")
  expect_true(all(c("score", "condition") %in% r$all))
})

test_that("convert_means emits a real jmv call, never a comment stub", {
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  cm  <- getFromNamespace("convert_means", "spss2rmarkdown")
  p <- psc("MEANS\n/TABLES = score BY condition\n/CELLS MEAN COUNT STDDEV")
  out <- cm(p, NULL)
  expect_false(grepl("No variables specified", out$r_code, fixed = TRUE))
  expect_silent(parse(text = paste0(".res <- ", out$r_code)))
})

# --- Guard 2: the emitter-level safety net -----------------------------------
# Converters are allowed to degrade to a comment-only stub; the generator is
# what must never emit an unbound `.res`. The guard therefore lives at the
# single substitution point (.s2r_expression_safe_rcode in rmd_generator.R),
# so it covers every current AND future stub converter — there are ~9 of them
# ("# DESCRIPTIVES/FREQUENCIES/RELIABILITY/MEANS/EXAMINE/FACTOR/QUICK
# CLUSTER/RANK: No variables specified"), and patching each one individually
# would leave the next one written to reintroduce the same cascade.

test_that("a comment-only stub is made a parseable no-op before substitution", {
  safe <- getFromNamespace(".s2r_expression_safe_rcode", "spss2rmarkdown")
  for (stub in c("# FACTOR: No variables specified",
                 "# MEANS: No variables specified",
                 "# QUICK CLUSTER: No variables specified",
                 "# RANK: No variables specified")) {
    out <- safe(stub)
    expect_silent(parse(text = paste0(".res <- ", out)))
    # The explanatory comment must survive into the generated .Rmd.
    expect_true(grepl(stub, out, fixed = TRUE))
    # And the no-op must actually evaluate to NULL, so s2r_render_tables(.res)
    # renders nothing instead of erroring.
    env <- new.env()
    eval(parse(text = paste0(".res <- ", out)), envir = env)
    expect_null(env$.res)
  }
})

test_that("real converter code passes through .s2r_expression_safe_rcode untouched", {
  safe <- getFromNamespace(".s2r_expression_safe_rcode", "spss2rmarkdown")
  real <- "jmv::efa(\n  data = data,\n  vars = c('a1','a2')\n)"
  expect_identical(safe(real), real)
})

test_that("a no-variable FACTOR/MEANS chunk BINDS .res instead of erroring", {
  # The real defect was a RUNTIME error, not a parse error. In the emitted
  # chunk the stub comment swallows the newline, so
  #     .res <- # FACTOR: No variables specified
  #     s2r_render_tables(.res)
  # parses as the single expression `.res <- s2r_render_tables(.res)` — the
  # chunk PARSES fine, then reads `.res` before assigning it, raising
  # "object '.res' not found". Asserting parse-success would therefore pass
  # with OR without the fix; this test asserts the real invariant by
  # EVALUATING the chunk exactly as the generator emits it.
  safe <- getFromNamespace(".s2r_expression_safe_rcode", "spss2rmarkdown")
  emit <- function(rhs) paste0(
    "tryCatch({\n  .res <- ", rhs,
    "\n  s2r_render_tables(.res)\n}, error = function(e) ",
    "cat('Analysis error:', conditionMessage(e)))")

  for (fn in c("convert_factor", "convert_means")) {
    cv <- getFromNamespace(fn, "spss2rmarkdown")
    out <- cv(list(variables = list(all = character()), raw = "X"), NULL)

    render_env <- new.env()
    render_env$s2r_render_tables <- function(x) invisible(x)

    # Unfixed substitution reproduces the production cascade.
    broken <- capture.output(
      eval(parse(text = emit(out$r_code)), envir = render_env))
    expect_true(any(grepl("object '.res' not found", broken, fixed = TRUE)))

    # Fixed substitution renders nothing and raises no Analysis error.
    fixed <- capture.output(
      eval(parse(text = emit(safe(out$r_code))), envir = render_env))
    expect_false(any(grepl("Analysis error", fixed, fixed = TRUE)))
  }
})
