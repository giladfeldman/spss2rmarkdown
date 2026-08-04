# R-0008 (cross-project, from STATA2Rmarkdown's find_unquoted_word): SPSS `IF`
# condition parsing must be quote-aware. The old lazy regex `\\((.+?)\\)\\s*(\\w+)
# \\s*=\\s*(.+)` finds the FIRST ")" after which a `word = expr` tail matches.
# Usually backtracking lands on the real close-paren — but when a quoted string
# literal in the condition itself contains `) <word> = ...`, the regex mis-splits
# at the in-string ")", corrupting the condition AND the target. A quote/paren-
# aware scan (find_matching_paren) finds the real close-paren regardless.
#
# Note: per-command extraction lands under `$variables` (extract_variables()),
# so condition/target are at cmd$variables$condition / cmd$variables$target.

parse_one_if <- function(spss_text) {
  tmp <- tempfile(fileext = ".sps")
  on.exit(unlink(tmp))
  writeLines(spss_text, tmp)
  result <- parse_sps(tmp)
  if (length(result) > 0) result[[1]] else NULL
}

test_that("IF parses a basic condition (no quotes) — baseline preserved", {
  cmd <- parse_one_if("IF (age > 18) adult = 1.\n")
  expect_equal(unname(cmd$command_type), "IF")
  expect_equal(cmd$variables$condition, "age > 18")
  expect_equal(cmd$variables$target, "adult")
})

test_that("IF is quote-aware: an in-string ') word =' does not mis-split (double quotes)", {
  # The ")" after `a` is followed by ` b = c") y = 2`, which the lazy regex would
  # wrongly split as condition=`note = "a`, target=`b`.
  cmd <- parse_one_if('IF (note = "a) b = c") y = 2.\n')
  expect_equal(unname(cmd$command_type), "IF")
  expect_equal(cmd$variables$condition, 'note = "a) b = c"')
  expect_equal(cmd$variables$target, "y")
})

test_that("IF is quote-aware: an in-string ') word =' does not mis-split (single quotes)", {
  cmd <- parse_one_if("IF (s = 'x) z = 9') w = 3.\n")
  expect_equal(unname(cmd$command_type), "IF")
  expect_equal(cmd$variables$condition, "s = 'x) z = 9'")
  expect_equal(cmd$variables$target, "w")
})
