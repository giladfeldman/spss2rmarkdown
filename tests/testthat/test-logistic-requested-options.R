# LOGISTIC REGRESSION must emit what SPSS prints -- no more, and no less.
#
# TWO DEFECTS, and the second is the one that was actually reaching reports.
#
# (1) C-0009, the one that was filed: convert_logistic() hard-coded fourteen
#     jmv::logRegBin options TRUE regardless of the command, so an ROC curve, an
#     AUC, an AIC, a BIC and a McFadden R-square were reported for every
#     logistic model. None of them is computed by simulation, so unlike the
#     Durbin-Watson p they were wrong to invent but STABLE.
#
# (2) The one found while measuring (1): extract_variables() had NO branch for
#     LOGISTIC REGRESSION, so every such command in the corpus arrived at the
#     converter with no dependent variable and left as the comment stub
#     `# LOGISTIC REGRESSION: Missing DV`. Measured at the real call site over
#     the 47 rendered corpus documents: 36 stubs across 3 documents, and ZERO
#     occurrences of `logRegBin` in any of them. Two-sided control on the same
#     47: REGRESSION appears in 20 and emits real jmv::linReg calls, so the zero
#     was a real zero and not a broken probe. The researcher's whole logistic
#     regression was absent from the report.
#
# WHAT SPSS ACTUALLY PRINTS, read off five frozen SPSS listings rather than off
# the documentation -- see .spss_logistic_options() for the tables, including
# the /PRINT=CI(n) control (3 listings with CI show `C.I.for EXP(B)`, 2 without
# it show none, and `Exp(B)` is present in all five as the presence control).

lconv <- function(syntax) {
  f <- tempfile(fileext = ".sps")
  writeLines(syntax, f)
  on.exit(unlink(f))
  convert_spss_to_r(parse_sps(f)[[1]], NULL)
}

BARE <- paste("LOGISTIC REGRESSION VARIABLES Sinst1",
              "  /METHOD=ENTER HISEI InstE",
              "  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).",
              sep = "\n")

test_that("a LOGISTIC REGRESSION is converted at all", {
  # RED before the parser branch existed: this returned the Missing-DV stub.
  r <- lconv(BARE)
  expect_match(r$r_code, "jmv::logRegBin(", fixed = TRUE)
  expect_match(r$r_code, 'dep = "SINST1"', fixed = TRUE)
  expect_match(r$r_code, "covs = c('HISEI', 'INSTE')", fixed = TRUE)
  expect_false(grepl("Missing DV", r$r_code, fixed = TRUE))
})

test_that("the statistics SPSS prints unasked are on", {
  # Verified against a frozen SPSS listing for a command carrying NO /PRINT at
  # all: Omnibus Tests of Model Coefficients; Model Summary with -2 Log
  # likelihood, Cox & Snell R Square and Nagelkerke R Square; Classification
  # Table; Variables in the Equation with B, S.E., Wald, df, Sig., Exp(B).
  r <- lconv(BARE)
  for (o in c("modelTest = TRUE", "dev = TRUE", "OR = TRUE", "class = TRUE",
              "acc = TRUE", "spec = TRUE", "sens = TRUE")) {
    expect_match(r$r_code, o, fixed = TRUE)
  }
  expect_match(r$r_code, 'pseudoR2 = c("r2cs", "r2n")', fixed = TRUE)
})

test_that("statistics no SPSS LOGISTIC REGRESSION prints are off, always", {
  # Two-sided over all 131 frozen SPSS listings: AUC 0 files, "Area Under the
  # Curve" 0, "ROC Curve" 0, Akaike 0 -- against Nagelkerke 5, Cox & Snell 5
  # and Hosmer 3 as the presence controls that make those zeros real. The one
  # McFadden listing is PLUM and the one BIC listing is MIXED.
  for (syntax in c(BARE, sub("/CRITERIA", "/PRINT=ALL\n  /CRITERIA", BARE, fixed = TRUE))) {
    r <- lconv(syntax)
    for (o in c("aic = FALSE", "bic = FALSE", "auc = FALSE", "rocPlot = FALSE",
                "omni = FALSE", "ci = FALSE")) {
      expect_match(r$r_code, o, fixed = TRUE)
    }
    expect_false(grepl("r2mf", r$r_code, fixed = TRUE))
  }
})

test_that("the odds-ratio interval follows /PRINT CI and nothing else", {
  off <- lconv(BARE)
  expect_match(off$r_code, "ciOR = FALSE", fixed = TRUE)

  on <- lconv(sub("/CRITERIA", "/PRINT=GOODFIT CI(95)\n  /CRITERIA", BARE, fixed = TRUE))
  expect_match(on$r_code, "ciOR = TRUE", fixed = TRUE)
  # SPSS's interval is for EXP(B), never for B, so jmv's `ci` stays off even
  # when CI was asked for.
  expect_match(on$r_code, "ci = FALSE", fixed = TRUE)

  # A /PRINT that does NOT name CI must not switch it on -- otherwise the gate
  # would be reading the subcommand rather than the keyword.
  neither <- lconv(sub("/CRITERIA", "/PRINT=summary GOODFIT\n  /CRITERIA", BARE, fixed = TRUE))
  expect_match(neither$r_code, "ciOR = FALSE", fixed = TRUE)
})

test_that("a /PRINT keyword jmv cannot honour is REPORTED, not dropped", {
  r <- lconv(sub("/CRITERIA", "/PRINT=GOODFIT\n  /CRITERIA", BARE, fixed = TRUE))
  expect_match(r$r_code, "Hosmer-Lemeshow", fixed = TRUE)
  expect_match(r$r_code, "omitted rather than replaced", fixed = TRUE)
  # TWO-SIDED: a command that asked for nothing gets no note.
  expect_false(grepl("NOTE [SPSS]", lconv(BARE)$r_code, fixed = TRUE))
})

test_that("covariates are read from a WITH clause as well as from /METHOD", {
  r <- lconv("LOGISTIC REGRESSION CandyChoice WITH AgeYears CorruptLV\n  /PRINT=CI(95).")
  expect_match(r$r_code, "covs = c('AGEYEARS', 'CORRUPTLV')", fixed = TRUE)
  expect_match(r$r_code, 'dep = "CANDYCHOICE"', fixed = TRUE)
})

test_that("an interaction term is reported, not silently forwarded or dropped", {
  # `a*b` is a name no column matches, so passing it into covs would be a
  # guaranteed error and dropping it in silence would change the model without
  # saying so.
  r <- lconv(paste("LOGISTIC REGRESSION CandyChoice WITH AgeYears CorruptLV",
                   "  /METHOD=ENTER AgeYears CorruptLV AgeYears*CorruptLV",
                   "  /PRINT=CI(95).", sep = "\n"))
  expect_match(r$r_code, "MAIN-EFFECTS model", fixed = TRUE)
  expect_match(r$r_code, "AgeYears*CorruptLV", fixed = TRUE)
  expect_false(grepl("'AGEYEARS[*]CORRUPTLV'", r$r_code))
})

test_that("a command with no covariates says so instead of fitting nothing", {
  r <- lconv("LOGISTIC REGRESSION VARIABLES Y\n  /CRITERIA=PIN(.05) POUT(.10).")
  expect_match(r$r_code, "no covariates found", fixed = TRUE)
  expect_false(grepl("logRegBin", r$r_code, fixed = TRUE))
})

# --- categorical predictors and the subcommands that change the MODEL --------
# Raised by consult seat `sonnet` (anthropic), 2026-09-10, reviewing 2e80403;
# incidence measured here afterwards over 187 LOGISTIC REGRESSION blocks in 31
# corpus files: /CONTRAST 38 blocks in 4 files, /CATEGORICAL 0, /METHOD=BSTEP 0,
# /METHOD=FSTEP 0, /SELECT 0 -- against /METHOD=ENTER 114 as the control that
# makes those zeros real. So /CONTRAST is the live one and the rest are latent.

test_that("a /CONTRAST variable is a FACTOR, not a continuous covariate", {
  # Without this it lands in `covs` and jmv fits ONE coefficient for a linear
  # trend across arbitrary category codes, where SPSS fits one contrast per
  # level. Measured on a 3-category predictor: continuous gives a single
  # Estimate; `factors` gives `2 - 1` and `3 - 1`, which is what SPSS's
  # Indicator(1) produces. A silently different number, not an error.
  r <- lconv(paste("LOGISTIC REGRESSION VARIABLES smoke",
                   "  /METHOD=ENTER age sex schooltype",
                   "  /CONTRAST (sex)=Indicator(1)",
                   "  /CONTRAST (schooltype)=Indicator(1).", sep = "\n"))
  expect_match(r$r_code, "factors = c('SEX', 'SCHOOLTYPE')", fixed = TRUE)
  expect_match(r$r_code, "covs = c('AGE')", fixed = TRUE)
  # ...and they are still IN the model, not merely moved out of covs.
  expect_match(r$r_code, 'blocks = list(list("AGE", "SEX", "SCHOOLTYPE"))', fixed = TRUE)
  # Indicator and Simple are what jmv's factor coding already does, so neither
  # earns a note.
  expect_false(grepl("/CONTRAST asked for", r$r_code, fixed = TRUE))
})

test_that("/CATEGORICAL declares a factor the same way", {
  r <- lconv("LOGISTIC REGRESSION VARIABLES y\n  /CATEGORICAL = grp\n  /METHOD=ENTER age grp.")
  expect_match(r$r_code, "factors = c('GRP')", fixed = TRUE)
  expect_match(r$r_code, "covs = c('AGE')", fixed = TRUE)
})

test_that("TWO-SIDED: with no /CONTRAST there is no factors argument at all", {
  r <- lconv(BARE)
  expect_false(grepl("factors =", r$r_code, fixed = TRUE))
})

test_that("a contrast coding jmv cannot reproduce is NAMED", {
  r <- lconv("LOGISTIC REGRESSION VARIABLES y\n  /METHOD=ENTER a b\n  /CONTRAST (b)=Deviation.")
  expect_match(r$r_code, "/CONTRAST asked for DEVIATION coding", fixed = TRUE)
  expect_match(r$r_code, "not the ones SPSS would print", fixed = TRUE)
})

test_that("a STEPWISE method is a WARNING, not a silently different model", {
  # SPSS's BSTEP/FSTEP fit a REDUCED model. jmv::logRegBin has no stepwise
  # equivalent, so forcing every predictor in publishes different coefficients
  # under the researcher's method name.
  for (m in c("BSTEP", "FSTEP")) {
    r <- lconv(paste0("LOGISTIC REGRESSION VARIABLES y\n  /METHOD=", m, " a b."))
    expect_match(r$r_code, paste0("/METHOD=", m, " is a STEPWISE method"), fixed = TRUE)
    expect_match(r$r_code, "FORCES EVERY PREDICTOR IN", fixed = TRUE)
  }
  # TWO-SIDED: ENTER is what jmv does, so it gets no warning.
  expect_false(grepl("STEPWISE method", lconv(BARE)$r_code, fixed = TRUE))
})

test_that("/SELECT is a WARNING, because the model is fitted on the wrong cases", {
  r <- lconv("LOGISTIC REGRESSION VARIABLES y\n  /SELECT sex EQ 1\n  /METHOD=ENTER a b.")
  expect_match(r$r_code, "restricts this analysis to a", fixed = TRUE)
  expect_match(r$r_code, "fitted on ALL cases", fixed = TRUE)
  expect_false(grepl("/SELECT", lconv(BARE)$r_code, fixed = TRUE))
})

test_that("the CI LEVEL is carried, not just the fact that CI was asked for", {
  # `.spss_sub_has()` strips parentheticals (it must, or /SAVE=PRED(COOK) reads
  # as a Cook's-distance request), so CI(90) and CI(99) were indistinguishable
  # from CI(95) and jmv's 95% default was published as SPSS's interval. Raised
  # independently by seats `sol` (openai) and `grok` (xai), 2026-09-10.
  # Latent in this corpus -- all 448 CI(n) keywords ask for 95% -- but a wrong
  # interval on a real coefficient with nothing erroring.
  expect_match(lconv("LOGISTIC REGRESSION VARIABLES y /METHOD=ENTER a /PRINT=CI(90).")$r_code,
               "ciWidthOR = 90", fixed = TRUE)
  expect_match(lconv("LOGISTIC REGRESSION VARIABLES y /METHOD=ENTER a /PRINT=CI(.99).")$r_code,
               "ciWidthOR = 99", fixed = TRUE)
  # SPSS accepts the level as a percentage or a proportion; both reach 95.
  for (k in c("CI(95)", "CI(.95)", "CI(.9500)")) {
    expect_match(lconv(paste0("LOGISTIC REGRESSION VARIABLES y /METHOD=ENTER a /PRINT=", k, "."))$r_code,
                 "ciWidthOR = 95", fixed = TRUE)
  }
  # TWO-SIDED: no interval requested, no width emitted.
  expect_false(grepl("ciWidthOR", lconv(BARE)$r_code, fixed = TRUE))
})

test_that("`=` is optional after /METHOD, and a (LR) qualifier is not a variable", {
  r <- lconv("LOGISTIC REGRESSION VARIABLES y\n  /METHOD BSTEP(LR) a b.")
  expect_match(r$r_code, "covs = c('A', 'B')", fixed = TRUE)
  expect_false(grepl("'LR'", r$r_code, fixed = TRUE))
  expect_match(r$r_code, "STEPWISE method", fixed = TRUE)
})

test_that("an `a BY b` interaction is recorded, not left as a bare BY", {
  r <- lconv("LOGISTIC REGRESSION VARIABLES y /METHOD=ENTER a b a BY b.")
  expect_false(grepl("'BY'", r$r_code, fixed = TRUE))
  expect_match(r$r_code, "MAIN-EFFECTS model", fixed = TRUE)
  expect_match(r$r_code, "a*b", fixed = TRUE)
  # CONTROL: the English word "by" in a comment must not become an interaction.
  # All 3 `BY` occurrences in this corpus's LOGISTIC blocks are exactly that.
  plain <- lconv("LOGISTIC REGRESSION VARIABLES y /METHOD=ENTER a b.")
  expect_false(grepl("MAIN-EFFECTS model", plain$r_code, fixed = TRUE))
})

test_that("SPSS's truncated /PRINT keywords are read as the full ones", {
  o <- getFromNamespace(".spss_logistic_options", "spss2rmarkdown")
  full  <- o("LOGISTIC REGRESSION VARIABLES Y /METHOD=ENTER X /PRINT=GOODFIT CI(95).")
  trunc <- o("LOGISTIC REGRESSION VARIABLES Y /METHOD=ENTER X /PRIN=GOOD CI(95).")
  expect_identical(trunc, full)
  expect_true(full$ci_or)
  expect_true(full$goodfit)
  # TWO-SIDED: a command with no /PRINT turns nothing on, so the prefix matcher
  # is not simply stuck open.
  bare <- o("LOGISTIC REGRESSION VARIABLES Y /METHOD=ENTER X /CRITERIA=PIN(.05).")
  # `ci_level` is a LEVEL, not a switch -- `ci_or` decides whether an interval
  # is emitted at all, and its default width is not a request. Every other
  # field is a gate and must be FALSE.
  gates <- bare[!names(bare) %in% "ci_level"]
  expect_true(all(vapply(gates, is.logical, logical(1))))
  expect_false(any(unlist(gates)))
  expect_equal(bare$ci_level, 95)
})
