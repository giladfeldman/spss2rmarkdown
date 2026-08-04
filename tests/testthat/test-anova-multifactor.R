# test-anova-multifactor.R
# Verify ANOVA / UNIANOVA / GLM extract all factors after BY,
# including factors whose names contain W/I/T/H (the prior [^/WITH]
# character-class regex silently truncated those).

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

test_that("UNIANOVA y BY a b c expands all three factors", {
  cmd <- parse_one("UNIANOVA score BY group region cohort.\n")
  expect_equal(cmd$command_type, "UNIANOVA")
  expect_equal(cmd$variables$factors, c("group", "region", "cohort"))
})

test_that("GLM with multi-factor BY captures all factors", {
  cmd <- parse_one("GLM dep BY a b c.\n")
  expect_equal(cmd$variables$factors, c("a", "b", "c"))
})

test_that("Bare ANOVA y BY a b c is recognized and expands", {
  cmd <- parse_one("ANOVA outcome BY treatment cohort site.\n")
  expect_equal(cmd$command_type, "ANOVA")
  expect_equal(cmd$variables$factors, c("treatment", "cohort", "site"))
})

test_that("Factors containing W, I, T, H are not truncated", {
  # The prior [^/WITH] regex (with ignore.case) excluded letters W/I/T/H,
  # truncating any factor whose name contained them.
  cmd <- parse_one("UNIANOVA y BY treatment height width.\n")
  expect_equal(cmd$variables$factors, c("treatment", "height", "width"))
})

test_that("WITH covariate clause is captured for ANCOVA", {
  cmd <- parse_one("UNIANOVA y BY a b WITH cov1 cov2.\n")
  expect_equal(cmd$variables$factors, c("a", "b"))
  expect_equal(cmd$variables$covariates, c("cov1", "cov2"))
})

test_that("Legacy ANOVA with (min,max) ranges strips ranges", {
  cmd <- parse_one("ANOVA y BY group(1,3) treatment(1,2).\n")
  expect_equal(cmd$variables$factors, c("group", "treatment"))
})

test_that("/SUBCOMMAND after factors does not pollute factor list", {
  cmd <- parse_one("UNIANOVA y BY a b c\n  /DESIGN = a b c a*b.\n")
  expect_equal(cmd$variables$factors, c("a", "b", "c"))
})
