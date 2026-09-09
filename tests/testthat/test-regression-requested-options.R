# REGRESSION must emit the statistics the syntax ASKED for, not a fixed template.
#
# THE DEFECT (measured 2026-09-09). convert_regression() hard-coded seventeen
# jmv::linReg options to TRUE regardless of the command. The worst of them is
# `durbin = TRUE`: jmv computes the Durbin-Watson p by SIMULATION, and the
# generated .Rmd sets no seed anywhere (0 occurrences of set.seed in a
# 33,000-line document). Six identical calls, identical data, N = 210:
#
#   autocorrelation  0.1300234  — identical all six
#   DW statistic     1.731849   — identical all six
#   p                .042 .050 .042 .034 .062 .044   — FIVE distinct values
#
# A researcher re-knitting the identical document moves that result across
# p = .05. For a tool whose entire purpose is reproducible reports, that is the
# most serious defect class there is.
#
# And SPSS never asked for it. Two-sided control over the round-2 sample4 pair:
#
#   "/RESIDUALS" in the .sps ............   0
#   "DURBIN"     in the .sps ............   0
#   "Durbin"     in the frozen SPSS gold    0
#   CONTROL "REGRESSION" in the .sps ...  203   <- the zeros are real zeros
#   CONTROL "Model Summary" in the gold   256
#
# So the report invented 203 autocorrelation tables SPSS never produced, each
# with a p that changes on every knit.
#
# Found during peer review; reproduced independently here before any code was
# changed.
#
# WHAT IS STILL ALWAYS ON, and why: SPSS's Model Summary prints R, R Square and
# Adjusted R Square by default, its ANOVA table is in /STATISTICS DEFAULTS, and
# its Coefficients table always carries the standardized Beta. Those map to
# r / r2 / r2Adj / modelTest / anova / stdEst and stay TRUE. AIC, BIC and RMSE
# appear in no SPSS REGRESSION table at all, so they are off unless asked for.

conv <- function(syntax) {
  f <- tempfile(fileext = ".sps")
  writeLines(syntax, f)
  on.exit(unlink(f))
  convert_spss_to_r(parse_sps(f)[[1]], NULL)
}

test_that("a plain REGRESSION asks for no Durbin-Watson, so none is emitted", {
  r <- conv("REGRESSION /DEPENDENT=Y /METHOD=ENTER X1 X2.")
  expect_match(r$r_code, "durbin = FALSE", fixed = TRUE)
  expect_false(grepl("durbin = TRUE", r$r_code, fixed = TRUE))
})

test_that("TWO-SIDED: /RESIDUALS DURBIN does turn it on", {
  # Without this the test above could pass on a converter that had simply
  # deleted the option, which would be a different bug.
  r <- conv("REGRESSION /DEPENDENT=Y /METHOD=ENTER X1 X2 /RESIDUALS DURBIN.")
  expect_match(r$r_code, "durbin = TRUE", fixed = TRUE)
})

test_that("the statistics SPSS prints by default are still emitted", {
  r <- conv("REGRESSION /DEPENDENT=Y /METHOD=ENTER X1 X2.")
  for (o in c("r = TRUE", "r2 = TRUE", "r2Adj = TRUE",
              "modelTest = TRUE", "anova = TRUE", "stdEst = TRUE")) {
    expect_match(r$r_code, o, fixed = TRUE)
  }
})

test_that("statistics that appear in no SPSS REGRESSION table are off", {
  r <- conv("REGRESSION /DEPENDENT=Y /METHOD=ENTER X1 X2.")
  for (o in c("aic = FALSE", "bic = FALSE", "rmse = FALSE",
              "ciStdEst = FALSE", "collin = FALSE", "cooks = FALSE",
              "norm = FALSE", "qqPlot = FALSE", "resPlots = FALSE",
              "ci = FALSE")) {
    expect_match(r$r_code, o, fixed = TRUE)
  }
})

test_that("each gate is driven by its OWN subcommand, not by any request", {
  # A file that asks for one thing must not switch on the others -- otherwise
  # the template has merely moved rather than gone.
  r <- conv("REGRESSION /STATISTICS COEFF OUTS R ANOVA COLLIN /DEPENDENT=Y /METHOD=ENTER X1.")
  expect_match(r$r_code, "collin = TRUE", fixed = TRUE)
  expect_match(r$r_code, "durbin = FALSE", fixed = TRUE)
  expect_match(r$r_code, "ci = FALSE", fixed = TRUE)

  r2 <- conv("REGRESSION /STATISTICS COEFF OUTS R ANOVA CI /DEPENDENT=Y /METHOD=ENTER X1.")
  expect_match(r2$r_code, "ci = TRUE", fixed = TRUE)
  expect_match(r2$r_code, "collin = FALSE", fixed = TRUE)

  r3 <- conv("REGRESSION /DEPENDENT=Y /METHOD=ENTER X1 /SCATTERPLOT=(*ZRESID ,*ZPRED).")
  expect_match(r3$r_code, "resPlots = TRUE", fixed = TRUE)
  expect_match(r3$r_code, "durbin = FALSE", fixed = TRUE)
})

test_that("/STATISTICS ALL turns on the ones ALL covers", {
  r <- conv("REGRESSION /STATISTICS ALL /DEPENDENT=Y /METHOD=ENTER X1.")
  expect_match(r$r_code, "collin = TRUE", fixed = TRUE)
  expect_match(r$r_code, "ci = TRUE", fixed = TRUE)
  # ...but ALL is a /STATISTICS keyword and says nothing about /RESIDUALS.
  expect_match(r$r_code, "durbin = FALSE", fixed = TRUE)
})

test_that("the jmv call is still the LAST expression, so the tables render", {
  # s2r_render_tables() renders the value of the chunk; if anything follows the
  # jmv call the whole analysis silently produces no tables.
  r <- conv("REGRESSION /DEPENDENT=Y /METHOD=ENTER X1 X2.")
  expect_match(r$r_code, "resPlots = (TRUE|FALSE)\\s*\\)\\s*\\}\\s*$")
})

# ---------------------------------------------------------------------------
# TRUNCATED SUBCOMMAND NAMES. Raised by consult seat `sonnet` (anthropic),
# 2026-09-09, reviewing f619fff: .spss_regression_options was refactored onto
# the shared .spss_sub_has, which matches a subcommand by PREFIX so that SPSS's
# own abbreviations work. That was the intended behaviour for the plot gates and
# it silently changed REGRESSION too -- correct, but with nothing pinning it.
#
# Reproduced here before this test was written: `/STAT ALL` + `/RESID DURBIN
# NORMPROB` returns a list IDENTICAL to the full-word form, and a REGRESSION
# with neither subcommand returns every option FALSE. That bare-command zero is
# the control that makes the match a real match rather than a gate stuck open.
# ---------------------------------------------------------------------------

test_that("SPSS's truncated /STAT and /RESID are read as the full subcommands", {
  o <- getFromNamespace(".spss_regression_options", "spss2rmarkdown")
  full  <- o("REGRESSION /STATISTICS ALL /DEPENDENT=Y /METHOD=ENTER X1 /RESIDUALS DURBIN NORMPROB.")
  trunc <- o("REGRESSION /STAT ALL /DEPENDENT=Y /METHOD=ENTER X1 /RESID DURBIN NORMPROB.")
  expect_identical(trunc, full)
  # Not vacuous: the full form really does switch things on.
  expect_true(full$collin)
  expect_true(full$durbin)
  expect_true(full$qqPlot)
})

test_that("TWO-SIDED: a REGRESSION with neither subcommand turns nothing on", {
  # A prefix matcher that fired on everything would satisfy the test above.
  o <- getFromNamespace(".spss_regression_options", "spss2rmarkdown")
  bare <- o("REGRESSION /DEPENDENT=Y /METHOD=ENTER X1.")
  expect_false(any(unlist(bare)))
})
