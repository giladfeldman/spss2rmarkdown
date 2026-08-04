# SPSS `var1 TO varN` range expansion.
#
# Open finding #6 (2026-06-29 iterate handoff): when a .sps is paired with
# MULTIPLE .sav files, or the range names were COMPUTE-created at runtime (so
# they are in no .sav at all), the SAV-dictionary lookup fails and the literal
# keyword "TO" leaked into the generated jmv/dplyr call -> jmv crash
# ("'names' attribute [N] must be the same length as the vector [0]").
#
# The fix adds a lexical numeric-sibling expander (.expand_to_numeric_lexical)
# used as the fallback in BOTH the vector path (expand_to_syntax, for
# RELIABILITY/DESCRIPTIVES/FREQUENCIES var lists) and the string path
# (expand_spss_to_ranges, for COMPUTE/COUNT expressions).

# --- .expand_to_numeric_lexical() unit behaviour ---------------------------

test_that("lexical expander handles a plain numeric suffix (item1 TO item10)", {
  expect_equal(
    .expand_to_numeric_lexical("item1", "item10"),
    paste0("item", 1:10)
  )
})

test_that("lexical expander preserves a constant trailing field (risk01_prep2 TO risk07_prep2)", {
  expect_equal(
    .expand_to_numeric_lexical("risk01_prep2", "risk07_prep2"),
    sprintf("risk%02d_prep2", 1:7)
  )
})

test_that("lexical expander preserves zero-padding width", {
  expect_equal(
    .expand_to_numeric_lexical("V08", "V12"),
    c("V08", "V09", "V10", "V11", "V12")
  )
})

test_that("lexical expander supports a trailing letter suffix (Q1a TO Q5a)", {
  expect_equal(
    .expand_to_numeric_lexical("Q1a", "Q5a"),
    paste0("Q", 1:5, "a")
  )
})

test_that("lexical expander returns NULL for non-numeric-sibling endpoints", {
  expect_null(.expand_to_numeric_lexical("age", "income"))     # no digits
  expect_null(.expand_to_numeric_lexical("a1", "b5"))          # different prefix
  expect_null(.expand_to_numeric_lexical("item10", "item1"))   # descending
  expect_null(.expand_to_numeric_lexical("x1", ""))            # empty endpoint
})

test_that("lexical expander handles a digit-then-letter-then-digit name (x1y2 TO x1y9)", {
  # Common prefix "x1y", varying trailing integer 2..9.
  expect_equal(
    .expand_to_numeric_lexical("x1y2", "x1y9"),
    paste0("x1y", 2:9)
  )
})

test_that("lexical expander does not materialise a pathological range", {
  expect_null(.expand_to_numeric_lexical("v1", "v999999"))
})

# --- expand_to_syntax(): vector path, no SAV dictionary --------------------

test_that("expand_to_syntax expands numeric siblings when all_names is NULL", {
  out <- expand_to_syntax(c("item1", "TO", "item10"), NULL)
  expect_equal(out, paste0("item", 1:10))
  expect_false("TO" %in% out)
})

test_that("expand_to_syntax expands when the named vars are absent from the dictionary", {
  # Dictionary holds unrelated vars (simulates wrong primary .sav).
  out <- expand_to_syntax(c("risk01_prep2", "TO", "risk07_prep2"),
                          c("AGE", "GENDER", "RISK01"))
  expect_equal(out, sprintf("risk%02d_prep2", 1:7))
  expect_false("TO" %in% out)
})

test_that("expand_to_syntax still prefers the SAV dictionary order when present", {
  # Dictionary order is authoritative for true SAV-resident ranges
  # (handles non-numeric or non-contiguous naming the lexical path can't).
  out <- expand_to_syntax(c("A", "TO", "C"), c("A", "B", "C", "D"))
  expect_equal(out, c("A", "B", "C"))
})

test_that("expand_to_syntax keeps surrounding vars intact", {
  out <- expand_to_syntax(c("first", "item1", "TO", "item3", "last"), NULL)
  expect_equal(out, c("first", "item1", "item2", "item3", "last"))
})

test_that("expand_to_syntax leaves a genuinely unexpandable range as-is", {
  # Different prefixes, not in dictionary -> cannot expand; must NOT crash.
  out <- expand_to_syntax(c("alpha", "TO", "omega"), NULL)
  expect_equal(out, c("alpha", "TO", "omega"))
})

# --- expand_spss_to_ranges(): string path ----------------------------------

test_that("expand_spss_to_ranges expands numeric siblings with no SAV info", {
  expect_equal(
    expand_spss_to_ranges("MEAN(item1 TO item4)", NULL),
    "MEAN(item1, item2, item3, item4)"
  )
})

test_that("expand_spss_to_ranges expands via lexical fallback when not contiguous in dict", {
  sav_info <- list(data = setNames(data.frame(a = 1, b = 2), c("AGE", "GENDER")))
  expect_equal(
    expand_spss_to_ranges("SUM(q01 TO q03)", sav_info),
    "SUM(q01, q02, q03)"
  )
})

test_that("expand_spss_to_ranges prefers SAV order for dictionary-resident ranges", {
  sav_info <- list(data = setNames(data.frame(1, 2, 3, 4),
                                   c("X1", "X2", "X3", "X4")))
  expect_equal(
    expand_spss_to_ranges("MEAN(X1 TO X3)", sav_info),
    "MEAN(X1, X2, X3)"
  )
})

# --- expand_spss_variables(): cross-dataset union + unresolved flag ----------

test_that("expand_spss_variables expands a range against the cross-dataset union", {
  # Endpoints absent from the primary sav_info but present in the union.
  cmd <- list(variables = list(all = c("item1", "TO", "item3")))
  out <- expand_spss_variables(cmd, sav_info = list(metadata = list(name = c("AGE", "SEX"))),
                               all_var_names = c("AGE", "SEX", "ITEM1", "ITEM2", "ITEM3"))
  expect_equal(out$variables$all, c("ITEM1", "ITEM2", "ITEM3"))
  expect_null(out$.unresolved_to)
})

test_that("expand_spss_variables falls back to the lexical numeric expansion", {
  # No union, endpoints in no dataset -> lexical numeric-sibling expansion.
  cmd <- list(variables = list(all = c("q1", "TO", "q4")))
  out <- expand_spss_variables(cmd, sav_info = list(metadata = list(name = NULL)))
  expect_equal(out$variables$all, c("q1", "q2", "q3", "q4"))
  expect_null(out$.unresolved_to)
})

test_that("expand_spss_variables flags a genuinely unresolvable range", {
  # Different prefixes, in no dataset, not a numeric sibling -> cannot expand;
  # must set .unresolved_to (so the caller emits a clean skip note) and keep
  # the literal tokens rather than silently producing a broken jmv call.
  cmd <- list(variables = list(all = c("alpha", "TO", "omega")))
  out <- expand_spss_variables(cmd, sav_info = list(metadata = list(name = NULL)))
  expect_true(isTRUE(out$.unresolved_to))
  expect_equal(out$.unresolved_to_ranges, "alpha TO omega")
})
