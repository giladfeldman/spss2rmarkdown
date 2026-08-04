# Regression tests for COUNT with multi-value criteria.
#
# Surfaced 2026-08-04 by round-3-spss item
# "Full Syntax Self-Reporting News Use In-situ and in Retrospect", which
# rendered YELLOW with "Error in parse(text = input): unexpected ','".
#
# Source syntax (line 13 of that .sps):
#   COUNT num_validday1=PlatformA1 ... PlatformA10(1,2,3).
#
# convert_count() interpolated the criterion text verbatim into an `==`
# comparison, so a value LIST produced
#   data[['N']] <- (data[['A1']] == 1,2,3) + ...
# and a THRU range produced
#   data[['N']] <- (data[['A1']] == 1 THRU 3) + ...
# Neither is valid R. Because the emitted code is unparseable, knitr fails the
# WHOLE chunk, so every COUNT in the file is lost — not just the one command.
#
# SPSS semantics being pinned here:
#   * COUNT counts, per row, how many of the listed variables match the
#     criterion; a value list means "matches ANY of these values".
#   * A non-matching or MISSING value contributes 0 — COUNT never yields NA.

parse_count <- function(txt) {
  getFromNamespace("parse_single_command", "spss2rmarkdown")(txt)
}
conv_count <- function(txt) {
  getFromNamespace("convert_count", "spss2rmarkdown")(parse_count(txt), NULL)
}

test_that("COUNT with a value list emits parseable R", {
  out <- conv_count("COUNT n=A1 A2 A3(1,2,3).")
  expect_silent(parse(text = out$r_code))
  expect_false(grepl("== 1,2,3", out$r_code, fixed = TRUE))
})

test_that("COUNT with a THRU range emits parseable R", {
  out <- conv_count("COUNT n=A1 A2(1 THRU 3).")
  expect_silent(parse(text = out$r_code))
  expect_false(grepl("THRU", out$r_code, ignore.case = TRUE))
})

test_that("COUNT with a single value still emits parseable R (no regression)", {
  out <- conv_count("COUNT n=A1 A2(1).")
  expect_silent(parse(text = out$r_code))
})

# --- Semantics, not just syntax -----------------------------------------------
# Evaluate the generated code against a known frame and compare to the counts
# SPSS would produce. Parse-only assertions would let a wrong-but-valid
# expression (e.g. `==` against only the first value) through.

eval_count <- function(txt, data) {
  out <- conv_count(txt)
  env <- new.env(); env$data <- data
  eval(parse(text = out$r_code), envir = env)
  as.numeric(env$data[["N"]])
}

test_that("COUNT value list counts a match on ANY listed value", {
  d <- data.frame(A1 = c(1, 2, 3, 4, 9), A2 = c(3, 4, 1, 4, 9))
  # row1: A1=1 yes, A2=3 yes -> 2 | row2: 2 yes, 4 no -> 1
  # row3: 3 yes, 1 yes -> 2    | row4: 4 no, 4 no -> 0 | row5: 9 no, 9 no -> 0
  expect_equal(eval_count("COUNT n=A1 A2(1,2,3).", d), c(2, 1, 2, 0, 0))
})

test_that("COUNT THRU range is inclusive of both endpoints", {
  d <- data.frame(A1 = c(1, 3, 4, 0), A2 = c(2, 5, 3, 9))
  # 1 THRU 3 -> row1: 1 yes, 2 yes = 2 | row2: 3 yes, 5 no = 1
  # row3: 4 no, 3 yes = 1              | row4: 0 no, 9 no = 0
  expect_equal(eval_count("COUNT n=A1 A2(1 THRU 3).", d), c(2, 1, 1, 0))
})

test_that("COUNT treats missing values as non-matches, never NA", {
  d <- data.frame(A1 = c(1, NA, NA), A2 = c(NA, 2, NA))
  # SPSS: a missing value simply does not match; the count is still defined.
  expect_equal(eval_count("COUNT n=A1 A2(1,2,3).", d), c(1, 1, 0))
})

test_that("COUNT single value keeps correct semantics", {
  d <- data.frame(A1 = c(1, 2, 1), A2 = c(1, 1, 2))
  expect_equal(eval_count("COUNT n=A1 A2(1).", d), c(2, 1, 1))
})

# --- Mixed / keyword criteria -------------------------------------------------
# Found by sweeping the corpus for other COUNT shapes after the first fix:
# test-corpus/spss/archive/code-only/osf/osf_67e67b55954d018c24bc_250304.sps uses
#   count misloJEZ = jEZlo1 ... (missing, lo thru -1).
#   count misHealth = nochrom mmse smmse (lo thr -1).
# i.e. a criterion is really a LIST OF TERMS (value | range | MISSING), matched
# with OR — and SPSS accepts THR/THRU/THROUGH. The first version of this fix
# handled only a pure value list or a single range, so "missing,lo thru -1"
# still emitted unparseable R and "lo thr -1" silently became %in% c(lo,thr,-1).

test_that("COUNT MISSING keyword counts missings instead of emitting a symbol", {
  d <- data.frame(A1 = c(1, NA, NA), A2 = c(NA, 2, NA))
  expect_equal(eval_count("COUNT n=A1 A2(MISSING).", d), c(1, 1, 2))
})

test_that("COUNT mixed MISSING + range criterion is parseable and correct", {
  d <- data.frame(A1 = c(NA, -3, 5, 0), A2 = c(-1, 2, NA, 7))
  # matches: is.na(x) OR x <= -1
  # row1: A1 NA yes, A2 -1 yes -> 2 | row2: -3 yes, 2 no -> 1
  # row3: 5 no, NA yes -> 1        | row4: 0 no, 7 no -> 0
  expect_equal(eval_count("COUNT n=A1 A2(missing, lo thru -1).", d), c(2, 1, 1, 0))
})

test_that("COUNT accepts the THR abbreviation as a range keyword", {
  out <- conv_count("COUNT n=A1 A2(lo thr -1).")
  expect_silent(parse(text = out$r_code))
  expect_false(grepl("%in%", out$r_code, fixed = TRUE))
  d <- data.frame(A1 = c(-5, 0), A2 = c(-1, 3))
  expect_equal(eval_count("COUNT n=A1 A2(lo thr -1).", d), c(2, 0))
})

test_that("COUNT criterion mixing discrete values and a range matches either", {
  d <- data.frame(A1 = c(9, 1, 4), A2 = c(2, 9, 3))
  # matches: x == 9 OR (x >= 1 & x <= 3)
  # row1: 9 yes, 2 yes -> 2 | row2: 1 yes, 9 yes -> 2 | row3: 4 no, 3 yes -> 1
  expect_equal(eval_count("COUNT n=A1 A2(9, 1 THRU 3).", d), c(2, 2, 1))
})

test_that("every COUNT criterion form in the corpus emits parseable R", {
  for (crit in c("1", "1,2,3", "1 THRU 3", "LO THRU 3", "1 THRU HI",
                 "MISSING", "SYSMIS", "missing,lo thru -1", "lo thr -1",
                 "9, 1 THROUGH 3")) {
    out <- conv_count(sprintf("COUNT n=A1 A2(%s).", crit))
    expect_silent(parse(text = out$r_code))
  }
})
