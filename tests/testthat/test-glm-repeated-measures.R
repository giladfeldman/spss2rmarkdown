parse_one <- function(spss_text) {
  tmp <- tempfile(fileext = ".sps")
  on.exit(unlink(tmp))
  writeLines(spss_text, tmp)
  result <- parse_sps(tmp)
  if (length(result) > 0) {
    result[[1]]$command_type <- unname(result[[1]]$command_type)
    result[[1]]
  } else NULL
}

test_that("GLM /WSFACTOR captures all DVs and within-subjects factor specs", {
  cmd <- "GLM proeager provigilant preeager previgilant
  /WSFACTOR=recall 2 Polynomial task 2 Polynomial
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(recall)
  /EMMEANS=TABLES(task)
  /EMMEANS=TABLES(recall*task)
  /PRINT=DESCRIPTIVE ETASQ
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=recall task recall*task."

  parsed <- parse_one(cmd)
  v <- parsed$variables

  expect_equal(parsed$command_type, "GLM")
  expect_equal(v$dependent, c("proeager", "provigilant", "preeager", "previgilant"))
  expect_length(v$ws_factors, 2)
  expect_equal(v$ws_factors[[1]]$name, "recall")
  expect_equal(v$ws_factors[[1]]$n, 2L)
  expect_equal(v$ws_factors[[2]]$name, "task")
  expect_equal(v$ws_factors[[2]]$n, 2L)
})

test_that("convert_glm routes WSFACTOR commands to jmv::anovaRM with correct rmCells", {
  cmd <- "GLM proeager provigilant preeager previgilant
  /WSFACTOR=recall 2 Polynomial task 2 Polynomial
  /METHOD=SSTYPE(3)."

  parsed <- parse_one(cmd)
  sav_info <- list(variables = c("PROEAGER","PROVIGILANT","PREEAGER","PREVIGILANT"),
                   data = data.frame())
  res <- spss2rmarkdown:::convert_glm(parsed, sav_info)

  expect_match(res$r_code, "jmv::anovaRM", fixed = TRUE)
  # Last factor varies fastest in the DV listing
  expect_match(res$r_code, 'measure = "PROEAGER".*c\\("L1", "L1"\\)')
  expect_match(res$r_code, 'measure = "PROVIGILANT".*c\\("L1", "L2"\\)')
  expect_match(res$r_code, 'measure = "PREEAGER".*c\\("L2", "L1"\\)')
  expect_match(res$r_code, 'measure = "PREVIGILANT".*c\\("L2", "L2"\\)')
  expect_equal(res$analysis_type, "Repeated-Measures ANOVA")
})

test_that("T-TEST PAIRS=a b WITH c d (PAIRED) parses elementwise pairs", {
  cmd <- "T-TEST PAIRS=proeager previgilant WITH provigilant preeager (PAIRED)
  /ES DISPLAY(TRUE) STANDARDIZER(SD)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS."

  parsed <- parse_one(cmd)
  pp <- parsed$variables$pair_pairs
  expect_length(pp, 2)
  expect_equal(pp[[1]]$i1, "proeager")
  expect_equal(pp[[1]]$i2, "provigilant")
  expect_equal(pp[[2]]$i1, "previgilant")
  expect_equal(pp[[2]]$i2, "preeager")
  expect_true(parsed$variables$paired_keyword)
})

test_that("convert_ttest emits jmv::ttestPS with the full pairs list", {
  cmd <- "T-TEST PAIRS=gaineager nonlossvigilant WITH gainvigilant nonlosseager (PAIRED)."
  parsed <- parse_one(cmd)
  res <- spss2rmarkdown:::convert_ttest(parsed, list(variables = character(), data = data.frame()))

  expect_match(res$r_code, "jmv::ttestPS", fixed = TRUE)
  expect_match(res$r_code, 'i1 = "GAINEAGER", i2 = "GAINVIGILANT"', fixed = TRUE)
  expect_match(res$r_code, 'i1 = "NONLOSSVIGILANT", i2 = "NONLOSSEAGER"', fixed = TRUE)
  expect_equal(res$analysis_type, "Paired Samples T-Test")
})

test_that("T-TEST PAIRS without WITH falls back to consecutive pairing", {
  cmd <- "T-TEST PAIRS=eager_gain_choices WITH vigilant_gain_choices (PAIRED)."
  parsed <- parse_one(cmd)
  pp <- parsed$variables$pair_pairs
  expect_length(pp, 1)
  expect_equal(pp[[1]]$i1, "eager_gain_choices")
  expect_equal(pp[[1]]$i2, "vigilant_gain_choices")
})
