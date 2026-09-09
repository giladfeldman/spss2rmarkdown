# A quoted string literal must survive expression translation BYTE-FOR-BYTE.
#
# THE DEFECT (measured 2026-09-09). convert_spss_expression() ends with a
# blanket "normalise variable names to UPPERCASE" pass:
#
#   gsub("\\b([A-Za-z_][A-Za-z0-9_.]*[A-Za-z0-9_])\\b", "\\U\\1", r_expr, perl = TRUE)
#
# It has no idea what a string literal is, so it upper-cases the CONTENTS of
# one. Its own comment claimed it protected literals; it never did.
#
#   SELECT IF trialcode NE 'AppAvoidTraining'.
#     -> SELECT IF TRIALCODE != 'APPAVOIDTRAINING'
#
# SPSS string comparison is CASE-SENSITIVE, so that condition matches nothing
# and the SELECT IF keeps every row SPSS would have dropped. No error, no
# warning, a plausible mean over a sample that still contains the training
# trials. Same class as C-0001: a believable number from the wrong rows.
#
# The word-form logical operators have the same blind spot: a literal
# containing AND / OR / NOT / EQ / NE as a whole word is rewritten into an R
# operator inside the quotes.
#
# INCIDENCE, measured over all 190 corpus .sps: 25 files contain at least one
# COMPUTE / IF / SELECT IF whose quoted literal is altered by the translation.
# None of them can DEMONSTRATE a wrong number, because the two that ship data
# are both source defects -- gta94's `Fluently` is undefined in its .sav (SPSS
# itself errors, gold line 3327, "Error # 4285 ... Text: Fluently") and 3apxv's
# DataQuest.sav has no `ID` column at all. That is a fact about this corpus,
# NOT a reason to leave the translation wrong: the live service converts the
# researcher's own data, where the variable does exist.

test_that("a mixed-case string literal is not upper-cased", {
  cvt <- getFromNamespace("convert_spss_expression", "spss2rmarkdown")
  expect_equal(cvt("(trialcode NE 'AppAvoidTraining')"),
               "(TRIALCODE != 'AppAvoidTraining')")
  expect_equal(cvt('(ID ~= "Partttt32")'), '(ID != "Partttt32")')
  # Both quote styles, and more than one literal in one expression.
  expect_equal(cvt("(blockcode = 'Congruent' OR blockcode = 'Incongruent')"),
               "(BLOCKCODE == 'Congruent' | BLOCKCODE == 'Incongruent')")
})

test_that("a word-form operator inside a literal stays part of the string", {
  cvt <- getFromNamespace("convert_spss_expression", "spss2rmarkdown")
  # AND / OR / NOT / EQ / NE are rewritten to R operators OUTSIDE quotes...
  # (Two-character names deliberately: the uppercase pass requires 2+ chars, so
  # a SINGLE-character SPSS variable is left lower case and will not match the
  # upper-cased column the loader produces. That is a separate, pre-existing
  # defect -- filed in todo.md -- and pinning it here would conflate two bugs.
  # It is not fixed alongside this one because the uppercase pass runs after R
  # code has already been emitted, where a blanket single-char rule would also
  # hit the `c` in `c(...)`.)
  expect_equal(cvt("(aa = 1 AND bb = 2)"), "(AA == 1 & BB == 2)")
  # ...and must not be touched INSIDE them.
  expect_equal(cvt("(cond = 'AND')"), "(COND == 'AND')")
  expect_equal(cvt("(cond = 'Not Applicable')"), "(COND == 'Not Applicable')")
  expect_equal(cvt("(cond = 'Netherlands OR Belgium')"),
               "(COND == 'Netherlands OR Belgium')")
})

test_that("TWO-SIDED: variable names outside literals are still upper-cased", {
  # The whole point of the uppercase pass is that the loader upper-cases every
  # column name. A fix that protected literals by disabling the pass would be
  # worse than the defect, so pin the behaviour it exists for.
  cvt <- getFromNamespace("convert_spss_expression", "spss2rmarkdown")
  expect_equal(cvt("(age > 18)"), "(AGE > 18)")
  expect_equal(cvt("(Attention = 1)"), "(ATTENTION == 1)")
  expect_equal(cvt("(AttnCheck = 2 and Gender < 3)"),
               "(ATTNCHECK == 2 & GENDER < 3)")
})

test_that("literal protection does not break the rules that READ a literal", {
  # DATEDIFF's third argument is a quoted UNIT that the translator must read and
  # lower-case into lubridate's `unit =`. It runs before the protection, and
  # this pins that it still does.
  cvt <- getFromNamespace("convert_spss_expression", "spss2rmarkdown")
  out <- cvt("DATEDIFF(end_date, start_date, 'years')")
  expect_match(out, 'unit = "years"', fixed = TRUE)
  expect_match(out, "lubridate::time_length", fixed = TRUE)
})

test_that("an apostrophe-bearing double-quoted literal is not shredded", {
  cvt <- getFromNamespace("convert_spss_expression", "spss2rmarkdown")
  expect_equal(cvt('(country = "Cote d\'Ivoire")'), '(COUNTRY == "Cote d\'Ivoire")')
})
