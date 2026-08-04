parse_one_ttest <- function(spss_text) {
  tmp <- tempfile(fileext = ".sps")
  on.exit(unlink(tmp))
  writeLines(spss_text, tmp)
  result <- parse_sps(tmp)
  result[[1]]$command_type <- unname(result[[1]]$command_type)
  result[[1]]
}

test_that("T-TEST PAIRS = X1 X2 WITH X3 X4 (PAIRED) -> 2 pairs (X1,X3) and (X2,X4)", {
  parsed <- parse_one_ttest("T-TEST PAIRS = x1 x2 WITH x3 x4 (PAIRED).")
  pp <- parsed$variables$pair_pairs

  expect_length(pp, 2)
  expect_equal(pp[[1]]$i1, "x1")
  expect_equal(pp[[1]]$i2, "x3")
  expect_equal(pp[[2]]$i1, "x2")
  expect_equal(pp[[2]]$i2, "x4")
  expect_true(parsed$variables$paired_keyword)
})

test_that("T-TEST PAIRS = X1 WITH X2 -> 1 pair (X1,X2)", {
  parsed <- parse_one_ttest("T-TEST PAIRS = x1 WITH x2.")
  pp <- parsed$variables$pair_pairs

  expect_length(pp, 1)
  expect_equal(pp[[1]]$i1, "x1")
  expect_equal(pp[[1]]$i2, "x2")
  expect_false(isTRUE(parsed$variables$paired_keyword))
})

test_that("T-TEST PAIRS = X1 X2 WITH X3 (recycle shorter side per SPSS)", {
  # SPSS recycles the shorter side: PAIRS = a b WITH c -> (a,c),(b,c)
  parsed <- parse_one_ttest("T-TEST PAIRS = x1 x2 WITH x3.")
  pp <- parsed$variables$pair_pairs

  expect_length(pp, 2)
  expect_equal(pp[[1]]$i1, "x1")
  expect_equal(pp[[1]]$i2, "x3")
  expect_equal(pp[[2]]$i1, "x2")
  expect_equal(pp[[2]]$i2, "x3")
})

test_that("T-TEST PAIRS without WITH falls back to consecutive pairing", {
  parsed <- parse_one_ttest("T-TEST PAIRS = x1 x2 x3 x4.")
  pp <- parsed$variables$pair_pairs

  expect_length(pp, 2)
  expect_equal(pp[[1]]$i1, "x1")
  expect_equal(pp[[1]]$i2, "x2")
  expect_equal(pp[[2]]$i1, "x3")
  expect_equal(pp[[2]]$i2, "x4")
})

test_that("Variable names with underscores are tokenized as a single name", {
  parsed <- parse_one_ttest("T-TEST PAIRS = eager_gain_choices WITH vigilant_gain_choices (PAIRED).")
  pp <- parsed$variables$pair_pairs

  expect_length(pp, 1)
  expect_equal(pp[[1]]$i1, "eager_gain_choices")
  expect_equal(pp[[1]]$i2, "vigilant_gain_choices")
})

test_that("convert_ttest emits jmv::ttestPS when pair_pairs is set", {
  parsed <- parse_one_ttest("T-TEST PAIRS = a b WITH c d (PAIRED).")
  res <- spss2rmarkdown:::convert_ttest(
    parsed,
    list(variables = character(), data = data.frame())
  )
  expect_match(res$r_code, "jmv::ttestPS", fixed = TRUE)
  expect_match(res$r_code, 'i1 = "A", i2 = "C"', fixed = TRUE)
  expect_match(res$r_code, 'i1 = "B", i2 = "D"', fixed = TRUE)
  expect_equal(res$analysis_type, "Paired Samples T-Test")
})

test_that("Multi-line T-TEST command (with subcommands on later lines) still extracts pair_pairs", {
  cmd <- "T-TEST PAIRS=proeager previgilant WITH provigilant preeager (PAIRED)\n  /ES DISPLAY(TRUE)\n  /CRITERIA=CI(.95)."
  parsed <- parse_one_ttest(cmd)
  pp <- parsed$variables$pair_pairs

  expect_length(pp, 2)
  expect_equal(pp[[1]]$i1, "proeager")
  expect_equal(pp[[1]]$i2, "provigilant")
  expect_equal(pp[[2]]$i1, "previgilant")
  expect_equal(pp[[2]]$i2, "preeager")
})
