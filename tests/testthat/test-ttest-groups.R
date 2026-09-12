# test-ttest-groups.R — T-TEST GROUPS=g(v1 v2) value-pair semantics.
#
# Regression tests for the 2026-08-04 iterate cycle-1 findings on
# Study2_Recall_MainAnalyses_Syntax (the corpus): SPSS
#   T-TEST GROUPS=GROUP(1 0) /VARIABLES=a b c d e
# means "compare cases with GROUP==1 against cases with GROUP==0, in that
# order, for EACH of the five variables". The converter (a) never subset the
# data to the two listed values -> jmv::ttestIS hard-errored "Grouping
# variable 'GROUP' must have exactly 2 levels" on the 3-level variable,
# (b) ignored the listed ORDER, so jmv's ascending factor levels flipped the
# sign of t / mean-difference whenever the syntax listed the higher code
# first (GROUP(1 0)), and (c) dropped every DV after the first (vars[1]) —
# silent value loss. All three watched RED against the unfixed converter.

parse_one <- function(txt) {
  getFromNamespace("parse_single_command", "spss2rmarkdown")(txt)
}
conv_one <- function(txt) {
  p <- parse_one(txt)
  getFromNamespace("convert_spss_to_r", "spss2rmarkdown")(p, NULL)
}

# Synthetic 3-level dataset with known group means:
#   GROUP == 1  -> Y centred on 10
#   GROUP == 0  -> Y centred on  5
#   GROUP == -1 -> Y centred on  0   (must be EXCLUDED by (1 0))
# Column names are UPPERCASE because the conversion pipeline normalizes both
# the emitted variable references and (in the generated Rmd) the loaded .sav
# columns to SPSS-normalized uppercase.
.ttest_df <- local({
  set.seed(42)
  data.frame(
    GROUP = rep(c(1, 0, -1), each = 20),
    Y  = c(rnorm(20, 10), rnorm(20, 5), rnorm(20, 0)),
    Y2 = c(rnorm(20, 3), rnorm(20, 3), rnorm(20, 3))
  )
})

.ttest_tbl <- function(res) {
  df <- as.data.frame(res$ttest)
  names(df)[1] <- "var"
  df
}
.col <- function(df, pattern) {
  nm <- grep(pattern, names(df), value = TRUE)[1]
  if (is.na(nm)) stop("no column matching ", pattern, " in: ",
                      paste(names(df), collapse = ", "))
  df[[nm]]
}

test_that("parse captures the GROUPS value pair and the full DV list", {
  p <- parse_one(paste0(
    "T-TEST GROUPS=GROUP(1 0)\n",
    "  /MISSING=ANALYSIS\n",
    "  /VARIABLES=Perceived_listening Self_insight Self_Esteem Guilt Motivation\n",
    "  /CRITERIA=CI(.95)."))
  expect_equal(gsub("\\s", "", p$variables$group_values %||% ""), "10")
  expect_equal(length(p$variables$variables), 5)
})

test_that("GROUPS(v1 v2) subsets to the two listed values and runs on a 3-level variable", {
  conv <- conv_one("T-TEST GROUPS=GROUP(1 0)\n  /VARIABLES=y y2.")
  data <- .ttest_df
  res <- eval(parse(text = conv$r_code))
  df <- .ttest_tbl(res)
  expect_true(all(c("Y", "Y2") %in% df$var))
})

test_that("GROUPS order defines the sign: first-listed value is group 1", {
  conv <- conv_one("T-TEST GROUPS=GROUP(1 0)\n  /VARIABLES=y.")
  data <- .ttest_df
  res <- eval(parse(text = conv$r_code))
  df <- .ttest_tbl(res)
  row <- df[df$var == "Y", ]
  md <- suppressWarnings(as.numeric(.col(row, "^md\\[")))
  # mean(GROUP==1) - mean(GROUP==0) ~ +5 — SPSS lists GROUP(1 0) so the
  # difference must be POSITIVE. Ascending factor levels would give -5.
  expect_true(is.finite(md))
  expect_gt(md, 0)
  # And the excluded -1 level must not participate: 40 cases, not 60.
  desc <- as.data.frame(res$desc)
  ns <- suppressWarnings(as.numeric(c(.col(desc, "^num\\[1"), .col(desc, "^num\\[2"))))
  expect_equal(sum(ns, na.rm = TRUE), 40)
})

test_that("all listed DVs are analysed, not just the first", {
  conv <- conv_one("T-TEST GROUPS=GROUP(1 0)\n  /VARIABLES=y y2.")
  expect_match(conv$r_code, '"y"', ignore.case = TRUE)
  expect_match(conv$r_code, '"y2"', ignore.case = TRUE)
})

test_that("single cut value: >= v forms group 1, < v group 2, all cases kept", {
  conv <- conv_one("T-TEST GROUPS=GROUP(1)\n  /VARIABLES=y.")
  data <- .ttest_df
  res <- eval(parse(text = conv$r_code))
  df <- .ttest_tbl(res)
  row <- df[df$var == "Y", ]
  md <- suppressWarnings(as.numeric(.col(row, "^md\\[")))
  # group1 = GROUP >= 1 (mean 10, n=20); group2 = GROUP < 1 (pooled mean 2.5,
  # n=40) -> positive difference; nothing excluded.
  expect_gt(md, 0)
  desc <- as.data.frame(res$desc)
  ns <- suppressWarnings(as.numeric(c(.col(desc, "^num\\[1"), .col(desc, "^num\\[2"))))
  expect_equal(sum(ns, na.rm = TRUE), 60)
})

test_that("bare GROUPS=g (no parens) keeps the passthrough behavior", {
  conv <- conv_one("T-TEST GROUPS=COND\n  /VARIABLES=y.")
  # No value list -> no subsetting block; data passed through as-is
  # (conservative: unverifiable by any corpus GT, so behavior unchanged).
  expect_match(conv$r_code, "data = data", fixed = TRUE)
})

test_that("quoted string group values subset on the character column", {
  conv <- conv_one("T-TEST GROUPS=arm('treat' 'ctrl')\n  /VARIABLES=y.")
  data <- data.frame(
    ARM = rep(c("treat", "ctrl", "other"), each = 10),
    Y = c(rnorm(10, 4), rnorm(10, 1), rnorm(10, 99))
  )
  res <- eval(parse(text = conv$r_code))
  df <- .ttest_tbl(res)
  row <- df[df$var == "Y", ]
  desc <- as.data.frame(res$desc)
  ns <- suppressWarnings(as.numeric(c(.col(desc, "^num\\[1"), .col(desc, "^num\\[2"))))
  expect_equal(sum(ns, na.rm = TRUE), 20)  # 'other' excluded
  md <- suppressWarnings(as.numeric(.col(row, "^md\\[")))
  expect_gt(md, 0)  # 'treat' listed first: mean(treat) - mean(ctrl) ~ +3
})
