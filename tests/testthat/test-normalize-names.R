# normalize_spss_names(): R-safe uppercase variable names.
#
# Regression: international .sav files (e.g. a Hebrew-coded dataset with columns
# TV<aleph>1 / TV<bet>1 / TV<gimel>1) used to collapse every non-ASCII letter to
# "." -> all four became "TV.1" -> DUPLICATE columns -> dplyr aborts every
# downstream transform with "Can't transform a data frame with duplicate names",
# silently failing the whole report. The fix maps each non-ASCII char to a
# unique uppercase "_UXXXX_" codepoint token, keeping the mapping injective while
# leaving ASCII names exactly as before.

test_that("ASCII names are normalized exactly as before (no behavior change)", {
  expect_equal(normalize_spss_names("age"), "AGE")
  expect_equal(normalize_spss_names("TV_exposure"), "TV_EXPOSURE")
  expect_equal(normalize_spss_names("gap.TV"), "GAP.TV")     # literal dot preserved
  expect_equal(normalize_spss_names("var 2"), "VAR.2")       # space -> .
  expect_equal(normalize_spss_names("a-b"), "A.B")           # hyphen -> .
  expect_equal(normalize_spss_names("filter_$"), "FILTER_.") # $ -> .
  expect_equal(normalize_spss_names("2ndvar"), "X2NDVAR")    # numeric-leading -> X-prefix
})

test_that("non-ASCII letters keep distinct names injective (no duplicate collapse)", {
  heb <- c("TVא1", "TVב1", "TVג1", "TVד1")  # aleph/bet/gimel/dalet
  out <- normalize_spss_names(heb)
  expect_length(unique(out), 4L)          # the bug: these all became "TV.1"
  expect_false(any(duplicated(out)))
  expect_true(all(grepl("^TV_U05D[0-3]_1$", out)))
})

test_that("codepoints differing only in a hex digit stay distinct", {
  # U+05D0 (aleph) vs U+05E0 (nun): a lowercase-hex token would have collided
  # after the [^A-Z0-9_] pass; uppercase hex survives intact.
  out <- normalize_spss_names(c("Xא", "Xנ"))
  expect_false(out[1] == out[2])
})

test_that("normalization is position-independent (alone == in a batch)", {
  # Critical: a name normalized on its own (a syntax reference) must equal the
  # same name normalized inside the full column vector (the .sav), or syntax and
  # data disagree.
  batch <- normalize_spss_names(c("TVא1", "TVב1", "TVג1"))
  expect_equal(normalize_spss_names("TVב1"), batch[2])
})

test_that("NULL / NA / empty are handled", {
  expect_null(normalize_spss_names(NULL))
  expect_equal(normalize_spss_names(""), "")
  expect_true(is.na(normalize_spss_names(NA_character_)))
})
