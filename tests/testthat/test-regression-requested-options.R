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
# And SPSS never asked for it. Two-sided control over the `sample4` pair:
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

test_that("the emitted call is still the LAST expression, so the tables render", {
  # s2r_render_tables() renders the VALUE of the chunk; if anything follows the
  # analysis call the whole analysis silently produces no tables. Since C-0008
  # the jmv call is WRAPPED in s2r_spss_reg_tables(), which returns the same
  # results object -- so the value is unchanged and the wrapper is what has to
  # come last.
  r <- conv("REGRESSION /DEPENDENT=Y /METHOD=ENTER X1 X2.")
  expect_match(r$r_code, "model_comp = (TRUE|FALSE)\\s*\\)\\s*\\}\\s*$")
  expect_match(r$r_code, "s2r_spss_reg_tables(\n  jmv::linReg(", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# C-0011 -- THE OTHER DIRECTION. Every gate above answers "did SPSS ask for
# this?" only in the REMOVING sense. Nothing checked for output SPSS DID ask
# for, or for a table jmv volunteers that SPSS never printed. Measured over all
# 190 corpus .sps: `CHANGE` on a /STATISTICS line in 27 files (324
# occurrences), `ZPP` in 14 files; `COLLIN`, which the gates above already
# cover, in 15 -- so these are as common as the ones already handled.
# ---------------------------------------------------------------------------

test_that("Model Comparisons is suppressed unless /STATISTICS CHANGE asked for it", {
  # jmv::linReg has NO `modelComp` argument (35 formals, none of them that
  # name): jmv emits the table itself whenever there are two or more blocks.
  # Without /STATISTICS CHANGE, SPSS prints one Model Summary row per model and
  # no change test at all, so an unrequested Model Comparisons table is the
  # same invention class as the Durbin-Watson one.
  r <- conv("REGRESSION /DEPENDENT=Y /METHOD=ENTER X1 /METHOD=ENTER X2.")
  expect_match(r$r_code, "model_comp = FALSE", fixed = TRUE)
})

test_that("TWO-SIDED: /STATISTICS CHANGE lets Model Comparisons through", {
  r <- conv(paste("REGRESSION /STATISTICS COEFF OUTS R ANOVA CHANGE",
                  "/DEPENDENT=Y /METHOD=ENTER X1 /METHOD=ENTER X2."))
  expect_match(r$r_code, "model_comp = TRUE", fixed = TRUE)
  # ...and says out loud which part of SPSS's table jmv cannot reproduce.
  expect_match(r$r_code, "/STATISTICS CHANGE requested", fixed = TRUE)
  expect_match(r$r_code, "intercept-only", fixed = TRUE)
})

test_that("a single-block CHANGE request is REPORTED, not silently dropped", {
  # jmv emits no comparison at all for one block, so SPSS's change-from-null
  # row has no equivalent. Saying nothing would be the silent-omission half of
  # the same defect.
  r <- conv("REGRESSION /STATISTICS CHANGE /DEPENDENT=Y /METHOD=ENTER X1.")
  expect_match(r$r_code, "single /METHOD block", fixed = TRUE)
  expect_match(r$r_code, "has no equivalent and is not reported", fixed = TRUE)
})

test_that("ZPP is recorded as unavailable, never approximated", {
  # jmv::linReg has no zero-order / partial / part correlation option at all.
  r <- conv(paste("REGRESSION /STATISTICS COEFF OUTS R ANOVA COLLIN TOL ZPP",
                  "/DEPENDENT=Y /METHOD=ENTER X1 X2."))
  expect_match(r$r_code, "/STATISTICS ZPP (zero-order, partial and part correlations)",
               fixed = TRUE)
  expect_match(r$r_code, "reported as absent", fixed = TRUE)
  # The COLLIN/TOL half of the same command still works -- so the note is an
  # addition, not a replacement for the gating.
  expect_match(r$r_code, "collin = TRUE", fixed = TRUE)
})

test_that("TWO-SIDED: a command asking for neither gets neither note", {
  r <- conv("REGRESSION /DEPENDENT=Y /METHOD=ENTER X1 X2.")
  expect_false(grepl("NOTE [SPSS]", r$r_code, fixed = TRUE))
})

# --- the rest of the reverse-direction sweep -------------------------------
# Every REGRESSION subcommand keyword the corpus uses was enumerated over 998
# REGRESSION blocks; the ones jmv::linReg has no option for are named in the
# report instead of vanishing. Counts are in .spss_regression_options().

test_that("output SPSS was asked for and jmv cannot give is NAMED", {
  r <- conv(paste("REGRESSION /DESCRIPTIVES MEAN STDDEV CORR SIG N",
                  "/STATISTICS COEFF OUTS R ANOVA",
                  "/DEPENDENT=Y /METHOD=ENTER X1 /METHOD=ENTER X2",
                  "/SAVE ZPRED ZRESID."))
  expect_match(r$r_code, "the syntax also asked for", fixed = TRUE)
  expect_match(r$r_code, "/DESCRIPTIVES (the Descriptive Statistics", fixed = TRUE)
  expect_match(r$r_code, "/STATISTICS OUTS (the Excluded Variables table)", fixed = TRUE)
  expect_match(r$r_code, "/SAVE (SPSS writes the saved diagnostics", fixed = TRUE)
  expect_match(r$r_code, "reported as absent", fixed = TRUE)
})

test_that("OUTS is only named where SPSS would have had something to exclude", {
  # OUTS sits on 670 of the corpus's 679 /STATISTICS lines. With one ENTER
  # block SPSS prints no Excluded Variables table either, so a note there would
  # be noise rather than a finding.
  one <- conv("REGRESSION /STATISTICS COEFF OUTS R ANOVA /DEPENDENT=Y /METHOD=ENTER X1.")
  expect_false(grepl("Excluded Variables", one$r_code, fixed = TRUE))
  two <- conv(paste("REGRESSION /STATISTICS COEFF OUTS R ANOVA",
                    "/DEPENDENT=Y /METHOD=ENTER X1 /METHOD=ENTER X2."))
  expect_match(two$r_code, "Excluded Variables", fixed = TRUE)
  # A selection method also excludes variables, but /METHOD=STEPWISE returns on
  # the olsrr path well above the jmv emission, so none of these notes -- and
  # none of the option gating -- applies to it at all. Pinned so a later change
  # that routes stepwise through jmv cannot do so silently.
  step <- conv("REGRESSION /STATISTICS COEFF OUTS R ANOVA /DEPENDENT=Y /METHOD=STEPWISE X1 X2.")
  expect_match(step$r_code, "olsrr::ols_step_both_p", fixed = TRUE)
  expect_false(grepl("jmv::linReg", step$r_code, fixed = TRUE))
})

test_that("/MISSING PAIRWISE is a WARNING, because it changes the NUMBERS", {
  # jmv::linReg deletes listwise and offers no alternative, so this is not a
  # missing table -- every coefficient is estimated on a different N than SPSS
  # used. 24 of the corpus's 675 /MISSING subcommands ask for it.
  r <- conv("REGRESSION /MISSING PAIRWISE /DEPENDENT=Y /METHOD=ENTER X1 X2.")
  expect_match(r$r_code, "WARNING [SPSS]: /MISSING PAIRWISE", fixed = TRUE)
  expect_match(r$r_code, "can differ from the SPSS output", fixed = TRUE)
  # TWO-SIDED: the 651-use LISTWISE form, which jmv does match, gets no warning.
  l <- conv("REGRESSION /MISSING LISTWISE /DEPENDENT=Y /METHOD=ENTER X1 X2.")
  expect_false(grepl("PAIRWISE", l$r_code, fixed = TRUE))
})

test_that("the Durbin-Watson note is emitted only where SPSS asked for it", {
  on  <- conv("REGRESSION /DEPENDENT=Y /METHOD=ENTER X1 /RESIDUALS DURBIN.")
  off <- conv("REGRESSION /DEPENDENT=Y /METHOD=ENTER X1.")
  expect_match(on$r_code, "SPSS prints the Durbin-Watson", fixed = TRUE)
  expect_false(grepl("SPSS prints the Durbin-Watson", off$r_code, fixed = TRUE))
  # The suppression itself is unconditional: there is no case in which jmv's
  # simulated p is the right thing to print.
  expect_match(on$r_code,  "durbin_p = FALSE", fixed = TRUE)
  expect_match(off$r_code, "durbin_p = FALSE", fixed = TRUE)
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
  # `ci_level` is a LEVEL, not a switch: it carries the width of an interval
  # that `ci`/`ciStdEst` decide whether to emit at all, so its default is not
  # something being "turned on". Every other field is a gate and must be FALSE.
  gates <- bare[!names(bare) %in% "ci_level"]
  expect_true(all(vapply(gates, is.logical, logical(1))))
  expect_false(any(unlist(gates)))
  expect_equal(bare$ci_level, 95)
})
