# C-0007 -- a SINGLE-CHARACTER SPSS variable name was never upper-cased.
#
# THE DEFECT. convert_spss_expression() normalises variable names with
#   gsub("\\b([A-Za-z_][A-Za-z0-9_.]*[A-Za-z0-9_])\\b", "\\U\\1", ...)
# whose character class needs TWO OR MORE characters. A comment beneath it said
# "Also handle single-char variable names" and no such rule existed. The loader
# upper-cases every column, so `data$a` resolved to nothing:
#
#   convert_spss_expression("(a = 1 AND b = 2)")   -> "(a == 1 & b == 2)"
#   convert_spss_expression("(aa = 1 AND bb = 2)") -> "(AA == 1 & BB == 2)"
#
# CORPUS INCIDENCE, measured at the real call site before the fix. Across the
# 209 generated SPSS .Rmd in the test corpus there are 38,196 dplyr::mutate()
# calls; 1,817 of them carry a bare lower-case single-character identifier, and
# 1,798 of those come from documents whose source syntax is SPSS's MATRIX
# sublanguage or a DEFINE macro (PROCESS internals -- scratch scalars like
# n, i, j, b, not dataset variables). That leaves 19 lines in ONE genuine
# source file, whose syntax reads
#
#   if (x = -6 and sweep = 1) x = INTCMC04.
#
# six times over. convert_if() normalises the assignment TARGET, so the emitted
# line was `mutate(X = if_else(x == -6 & SWEEP == 1, INTCMC04, X))` -- the same
# variable upper-case on one side and lower-case on the other. It fails LOUD,
# which is why this was ranked P2 rather than P1.
#
# WHY THE FIX IS NOT A BLANKET RULE. The normalising pass runs AFTER this
# function has already emitted R code into the same string, so an unguarded
# single-character rule would also hit the `c` of the `c(...)` that ANY()
# emits. The guard is a lookahead for an opening parenthesis: a single letter
# followed by `(` is a call, not a variable.

test_that("a single-character variable name is upper-cased like any other", {
  expect_equal(convert_spss_expression("(a = 1 AND b = 2)"), "(A == 1 & B == 2)")
  # CONTROL: the two-character form, which always worked. If this ever fails
  # the whole normalising pass is broken, not just the single-char rule.
  expect_equal(convert_spss_expression("(aa = 1 AND bb = 2)"), "(AA == 1 & BB == 2)")
})

test_that("the real corpus line that fails today is fixed", {
  expect_equal(convert_spss_expression("x = -6 AND sweep = 1"),
               "X == -6 & SWEEP == 1")
})

test_that("R tokens this function itself emits are NOT clobbered", {
  # ANY() emits `%in% c(...)`. A blanket single-character rule would upper-case
  # that `c` and the chunk would die on `C(1, 2, 3)`.
  expect_match(convert_spss_expression("ANY(x, 1, 2, 3)"), "%in% c(", fixed = TRUE)
  expect_false(grepl("%in% C(", convert_spss_expression("ANY(x, 1, 2, 3)"), fixed = TRUE))
  # ...and the argument beside it still normalises, so the guard is a guard and
  # not a switch that turned the rule off.
  expect_match(convert_spss_expression("ANY(x, 1, 2, 3)"), "X %in%", fixed = TRUE)
})

test_that("neither %in% nor an exponent is mistaken for a variable", {
  expect_match(convert_spss_expression("ANY(q1, 1, 2)"), "%in%", fixed = TRUE)
  expect_equal(convert_spss_expression("age > 1e-5"), "AGE > 1e-5")
})

test_that("a quoted literal is still untouched (C-0006 must not regress)", {
  expect_equal(convert_spss_expression("cond = 'Netherlands OR Belgium'"),
               "COND == 'Netherlands OR Belgium'")
  # A single letter INSIDE a literal is data, not a name.
  expect_equal(convert_spss_expression("cond = 'a b c'"), "COND == 'a b c'")
})

test_that("T and F become explicit COLUMN references, never R's TRUE/FALSE", {
  # base R defines T and F as TRUE and FALSE. Inside dplyr::mutate() a column
  # called T shadows them -- so a bare `T` is correct while the column exists
  # and silently becomes TRUE the moment it does not, turning a loud missing
  # variable into a condition true for every row.
  expect_equal(convert_spss_expression("t = 1"), '.data[["T"]] == 1')
  expect_equal(convert_spss_expression("f = 0"), '.data[["F"]] == 0')
  # The same repair applies to a variable the researcher already wrote upper
  # case, which this function has always emitted as a bare T.
  expect_equal(convert_spss_expression("T = 1"), '.data[["T"]] == 1')
  # CONTROL: TRUE and FALSE themselves are not touched, and neither is a
  # longer name that merely begins with T or F.
  expect_match(convert_spss_expression("SUM(q1, q2)"), "na.rm = TRUE", fixed = TRUE)
  expect_equal(convert_spss_expression("TEMP > 1"), "TEMP > 1")
  expect_equal(convert_spss_expression("FIRST < 2"), "FIRST < 2")
})

test_that("the emitted expression still parses as R", {
  for (e in c("(a = 1 AND b = 2)", "ANY(x, 1, 2, 3)", "MEAN(a, bb, c)",
              "t = 1", "x = -6 AND sweep = 1", "SUM(q1, q2)",
              "RANGE(a, 1, 5)", "MISSING(z)")) {
    expect_silent(parse(text = convert_spss_expression(e)))
  }
})

# --- the consumer the T/F rewrite broke -------------------------------------
# Raised by consult seat `sonnet` (anthropic), 2026-09-10, reviewing 2e80403,
# and reproduced here before the fix. The FILTER recompute decides whether it
# can rebuild a filter column by asking `all.vars()` which variables the
# expression needs. `all.vars()` reports `.data[["T"]]` as the name `.data`:
#
#   all.vars(quote(.data[["T"]] == 1 & AGE > 2))   ->   ".data"  "AGE"
#
# `.data` is never a column, so the check was permanently non-empty, the
# recompute never ran, and the stale column stored in the .sav won -- which is
# the ADIFILT-class defect test-filter-recompute-wins.R exists to prevent,
# re-entering through the fix for a different one.

test_that(".s2r_expr_vars reads a .data[[...]] reference as its COLUMN name", {
  f <- getFromNamespace(".s2r_expr_vars", "spss2rmarkdown")
  expect_equal(f(quote(.data[["T"]])), "T")
  expect_setequal(f(quote(.data[["T"]] == 1 & AGE > 2)), c("T", "AGE"))
  # CONTROL: it must still behave like all.vars() on everything else.
  expect_setequal(f(quote(X == 1 & AGE > 2)), c("X", "AGE"))
  expect_equal(f(quote(1 + 2)), character(0))
  # ...and `.data` itself is never reported as a variable.
  expect_false(".data" %in% f(quote(.data[["T"]] == 1 & AGE > 2)))
})

test_that("RED-FIRST CONTROL: all.vars() alone gets this wrong", {
  # If this ever starts passing, R's all.vars() has changed and the helper is
  # no longer needed -- which is worth knowing, not worth silently keeping.
  expect_equal(all.vars(quote(.data[["T"]])), ".data")
})

test_that("the FILTER recompute still fires for a filter column named T", {
  conv <- function(syntax) {
    f <- tempfile(fileext = ".sps"); writeLines(syntax, f); on.exit(unlink(f))
    convert_all_commands(parse_sps(f), NULL)
  }
  res <- conv(paste("COMPUTE T = AGE > 40.",
                    "FILTER BY T.",
                    "DESCRIPTIVES VARIABLES=SCORE.", sep = "\n"))
  code <- paste(vapply(res, function(r) r$r_code %||% "", character(1)),
                collapse = "\n")
  skip_if_not(grepl(".s2r_fmiss", code, fixed = TRUE),
              "this syntax did not take the FILTER recompute path")
  # The check must be the helper, never a bare all.vars() -- that is what made
  # `.s2r_fmiss` permanently non-empty.
  expect_match(code, ".s2r_expr_vars(quote(", fixed = TRUE)
  expect_false(grepl("all.vars(quote(", code, fixed = TRUE))
})

test_that("BEHAVIOUR: the column is found, and its absence is LOUD", {
  skip_if_not_installed("dplyr")
  code <- convert_spss_expression("s * 2 + t")
  have <- data.frame(T = c(1, 2, 3), S = c(9, 9, 9))
  expect_equal(eval(parse(text = paste0("dplyr::mutate(have, R = ", code, ")$R"))),
               c(19, 20, 21))
  # TWO-SIDED: with the column gone this must ERROR. Before the guard the same
  # expression evaluated `T` to TRUE and returned a number for every row.
  gone <- data.frame(S = c(9, 9, 9))
  expect_error(eval(parse(text = paste0("dplyr::mutate(gone, R = ", code, ")"))))
})
