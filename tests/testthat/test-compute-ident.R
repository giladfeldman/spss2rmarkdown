# R-0009 (cross-project, ported from STATA2Rmarkdown's is_valid_r_ident):
# every generated R identifier must be gated through is_valid_r_ident() with a
# NOTE fallback. normalize_spss_names() maps specials -> "." and X-prefixes
# numeric-leading names, so for almost any non-empty input it yields a VALID R
# identifier. The dangerous residual case is a degenerate/empty target (e.g. an
# unexpanded macro that normalizes to "") which slips past the is.null() guard
# and produces unparseable R like `dplyr::mutate( = expr)` — silently corrupting
# the generated report. is_valid_r_ident() converts that into a visible NOTE.

# --- is_valid_r_ident() unit behaviour -------------------------------------

test_that("is_valid_r_ident accepts ordinary R identifiers", {
  expect_true(is_valid_r_ident("NEWVAR"))
  expect_true(is_valid_r_ident("x"))
  expect_true(is_valid_r_ident("X.1"))
  expect_true(is_valid_r_ident("a_b.c"))
})

test_that("is_valid_r_ident rejects empty, NULL, and multi-element input", {
  expect_false(is_valid_r_ident(""))
  expect_false(is_valid_r_ident(NULL))
  expect_false(is_valid_r_ident(character(0)))
  expect_false(is_valid_r_ident(c("A", "B")))
})

test_that("is_valid_r_ident rejects a name that does not start with a letter/underscore", {
  expect_false(is_valid_r_ident("1abc"))
  expect_false(is_valid_r_ident(".5"))
})

# --- convert_compute -------------------------------------------------------

test_that("convert_compute emits a normal mutate for a valid target (baseline preserved)", {
  res <- convert_compute(
    list(variables = list(target = "newvar", expression = "A + 1"),
         raw = "COMPUTE newvar = A + 1."), NULL)
  expect_match(res$r_code, "dplyr::mutate\\(NEWVAR = ")
  expect_false(grepl("NOTE \\[SPSS\\]", res$r_code))
})

test_that("COMPUTE x = $SYSMIS becomes mutate(X = NA), not broken `= $SYSMIS`", {
  # $SYSMIS is the SPSS system-missing CONSTANT. Passed through verbatim it
  # leaked a bare `$` -> `dplyr::mutate(X = $SYSMIS)` -> "unexpected '$'", which
  # nuked the chunk and every downstream IF referencing x (the corpus
  # Study2_Recall_MainAnalyses: COMPUTE GROUP_new = $SYSMIS then IF (GROUP=1)...).
  res <- convert_compute(
    list(variables = list(target = "GROUP_new", expression = "$SYSMIS"),
         raw = "COMPUTE GROUP_new = $SYSMIS."), NULL)
  expect_match(res$r_code, "dplyr::mutate(GROUP_NEW = NA", fixed = TRUE)
  expect_false(grepl("$SYSMIS", res$r_code, fixed = TRUE))
  expect_silent(parse(text = res$r_code))
})

test_that("convert_spss_expression translates $SYSMIS to NA (constant, not function)", {
  expect_equal(trimws(convert_spss_expression("$SYSMIS")), "NA")
  expect_equal(trimws(convert_spss_expression("$sysmis")), "NA")   # case-insensitive
  # The SYSMIS(x)/MISSING(x) FUNCTION forms are unaffected by the constant rule.
  # The ARGUMENT is upper-cased since C-0007 (2026-09-10): these two expectations
  # used to read `is.na(y)` / `is.na(x)`, which was the single-character defect
  # written down as an expectation -- the loader upper-cases every column, so a
  # lower-case `y` matched nothing. Two-char names were always upper-cased here.
  expect_match(convert_spss_expression("SYSMIS(y)"), "is.na(Y)", fixed = TRUE)
  expect_match(convert_spss_expression("MISSING(x)"), "is.na(X)", fixed = TRUE)
  expect_match(convert_spss_expression("MISSING(xx)"), "is.na(XX)", fixed = TRUE)
})

# R-0096: $SYSMIS was the only bare-$ SPSS system variable handled, so any
# other bare-$ token (e.g. $CASENUM) still leaked through verbatim and broke
# the generated R the same way $SYSMIS used to (`unexpected '$'`). $CASENUM is
# SPSS's per-case sequential row number and is common in real syntax (e.g.
# `COMPUTE ID = $CASENUM.`, `SELECT IF ($CASENUM <= 100)`); it has a faithful,
# deterministic R equivalent in dplyr::row_number(), unlike the run-timestamp
# system variables ($DATE/$DATE11/$JDATE/$TIME) or the output-formatting ones
# ($LENGTH/$WIDTH), which are excluded -- see NEWS.md / todo.md for why.
#
# Cross-model review (Sonnet, 2026-08-04) flagged that row_number() only
# matches true SPSS $CASENUM semantics when no SELECT IF/FILTER/SORT
# CASES/SPLIT FILE precedes the reference in the same .sps -- reproduced
# locally (SELECT IF (X>1) then COMPUTE ID=$CASENUM gave row_number() 1,2,3
# instead of SPSS's true surviving case numbers 2,3,5). Since
# convert_spss_expression has no visibility into surrounding commands to rule
# that out, convert_compute()/convert_if() emit a visible `# NOTE [SPSS]`
# caveat comment (checked below) instead of silently trusting the value.
test_that("COMPUTE x = $CASENUM becomes mutate(X = dplyr::row_number()), not broken `= $CASENUM`", {
  res <- convert_compute(
    list(variables = list(target = "ID", expression = "$CASENUM"),
         raw = "COMPUTE ID = $CASENUM."), NULL)
  expect_match(res$r_code, "dplyr::mutate(ID = dplyr::row_number()", fixed = TRUE)
  # The executable mutate() line itself must be pure R -- no leaked bare `$`.
  mutate_line <- grep("dplyr::mutate\\(", strsplit(res$r_code, "\n")[[1]], value = TRUE)
  expect_false(grepl("$CASENUM", mutate_line, fixed = TRUE))
  # The generated caveat comment is EXPECTED to name $CASENUM (that's the
  # point of the caveat), so it deliberately still appears in the full r_code.
  expect_match(res$r_code, "# NOTE \\[SPSS\\]: \\$CASENUM translated to dplyr::row_number\\(\\)")
  expect_silent(parse(text = res$r_code))
})

test_that("convert_spss_expression translates $CASENUM to dplyr::row_number() (constant, not function)", {
  expect_equal(trimws(convert_spss_expression("$CASENUM")), "dplyr::row_number()")
  expect_equal(trimws(convert_spss_expression("$casenum")), "dplyr::row_number()")   # case-insensitive
})

test_that("convert_compute emits a NOTE (not broken `mutate( = `) for a degenerate target", {
  res <- convert_compute(
    list(variables = list(target = "", expression = "A + 1"),
         raw = "COMPUTE  = A + 1."), NULL)
  expect_match(res$r_code, "# NOTE \\[SPSS\\]: COMPUTE target is not a static R identifier")
  expect_false(grepl("mutate\\(\\s*=", res$r_code))
})

# --- convert_if ------------------------------------------------------------

test_that("convert_if emits a NOTE for a degenerate target", {
  res <- convert_if(
    list(variables = list(condition = "AGE > 18", target = "", expression = "1"),
         raw = "IF (age > 18)  = 1."), NULL)
  expect_match(res$r_code, "# NOTE \\[SPSS\\]: IF target is not a static R identifier")
  expect_false(grepl("mutate\\(\\s*=", res$r_code))
})

# --- convert_recode --------------------------------------------------------

test_that("convert_recode emits a NOTE when a target identifier is degenerate", {
  res <- convert_recode(
    list(variables = list(source_vars = "A", target_vars = ""),
         raw = "RECODE A (1=2) INTO  ."), NULL)
  expect_match(res$r_code, "# NOTE \\[SPSS\\]: RECODE")
  expect_false(grepl("mutate\\(\\s*=", res$r_code))
})
