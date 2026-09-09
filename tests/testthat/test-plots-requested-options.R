# Plots are emitted when the SYNTAX asks for them, never by template.
#
# THE DEFECT (measured 2026-09-09). Four converters hard-coded jmv plot options
# to TRUE regardless of the command. The cost is not cosmetic: it is the
# project's most frequent hard failure. Renders die with
#
#   unable to allocate bitmap
#   unable to start png() device
#   processing of the plot ran out of memory
#
# inside jmv::corrMatrix(..., plots = TRUE, plotDens = TRUE, plotStats = TRUE),
# reproduced across ten independent sessions. One file
# alone drove 142 corrMatrix calls, every one of them drawing a full scatter
# matrix that SPSS never produced.
#
# TWO-SIDED CONTROL over the 287-file .sps test corpus, counting plot
# subcommands per command block:
#
#   CORRELATIONS   487 blocks    0 plot subcommands
#   T-TEST         297 blocks    0 plot subcommands
#   DESCRIPTIVES   431 blocks    0 plot subcommands
#   FREQUENCIES    462 blocks   36 (/HISTOGRAM 30, /PIECHART 4, /BARCHART 2)
#   EXAMINE        157 blocks  147 (/PLOT)
#   GRAPH          173 blocks  150 (/HISTOGRAM 88, /SCATTERPLOT 62)
#
# The zeros are real zeros: the same scan finds 333 plot subcommands elsewhere,
# so the detector works. SPSS CORRELATIONS and T-TEST have no plot subcommand
# in the language at all -- scatterplots come from GRAPH and PLOT, which are
# separate commands. FREQUENCIES had 462 histograms drawn for 30 requested.
#
# WHAT STAYS ON, and why: EXAMINE with no /PLOT still prints BOXPLOT and
# STEMLEAF by default in SPSS, so `box` stays TRUE there. jmv has no stem-and-
# leaf and no pie chart, so /PIECHART is recorded as unconverted rather than
# approximated by a bar chart.
#
# Options are emitted explicitly as FALSE rather than omitted, matching the
# convention set by test-regression-requested-options.R, so the generated
# report records the decision instead of hiding it.

conv <- function(syntax) {
  f <- tempfile(fileext = ".sps")
  writeLines(syntax, f)
  on.exit(unlink(f))
  convert_spss_to_r(parse_sps(f)[[1]], NULL)
}

# ---- CORRELATIONS: no plot subcommand exists in SPSS ------------------------

test_that("CORRELATIONS draws no scatter matrix", {
  r <- conv("CORRELATIONS /VARIABLES=a b c /PRINT=TWOTAIL NOSIG.")
  expect_match(r$r_code, "plots = FALSE", fixed = TRUE)
  expect_match(r$r_code, "plotDens = FALSE", fixed = TRUE)
  expect_match(r$r_code, "plotStats = FALSE", fixed = TRUE)
  expect_false(grepl("plots = TRUE", r$r_code, fixed = TRUE))
})

test_that("CORRELATIONS still emits the coefficients SPSS does print", {
  # Guards against 'fixing' this by deleting the analysis.
  r <- conv("CORRELATIONS /VARIABLES=a b c.")
  expect_match(r$r_code, "jmv::corrMatrix", fixed = TRUE)
  expect_match(r$r_code, "pearson = TRUE", fixed = TRUE)
  expect_match(r$r_code, "sig = TRUE", fixed = TRUE)
})

# ---- T-TEST: no plot subcommand exists in SPSS ------------------------------

test_that("T-TEST GROUPS draws no plot", {
  r <- conv("T-TEST GROUPS=grp(0 1) /VARIABLES=y.")
  expect_match(r$r_code, "plots = FALSE", fixed = TRUE)
  expect_false(grepl("plots = TRUE", r$r_code, fixed = TRUE))
})

test_that("T-TEST still emits the statistics SPSS does print", {
  r <- conv("T-TEST GROUPS=grp(0 1) /VARIABLES=y.")
  expect_match(r$r_code, "jmv::ttestIS", fixed = TRUE)
  expect_match(r$r_code, "students = TRUE", fixed = TRUE)
  expect_match(r$r_code, "desc = TRUE", fixed = TRUE)
})

# ---- FREQUENCIES: /HISTOGRAM and /BARCHART do exist -------------------------

test_that("a plain FREQUENCIES draws neither histogram nor bar chart", {
  r <- conv("FREQUENCIES VARIABLES=a b /ORDER=ANALYSIS.")
  expect_match(r$r_code, "hist = FALSE", fixed = TRUE)
  expect_match(r$r_code, "bar = FALSE", fixed = TRUE)
  expect_match(r$r_code, "freq = TRUE", fixed = TRUE)
})

test_that("TWO-SIDED: /HISTOGRAM turns the histogram on", {
  r <- conv("FREQUENCIES VARIABLES=a b /HISTOGRAM /ORDER=ANALYSIS.")
  expect_match(r$r_code, "hist = TRUE", fixed = TRUE)
  expect_match(r$r_code, "bar = FALSE", fixed = TRUE)
})

test_that("TWO-SIDED: /BARCHART turns the bar chart on", {
  r <- conv("FREQUENCIES VARIABLES=a b /BARCHART FREQ.")
  expect_match(r$r_code, "bar = TRUE", fixed = TRUE)
  expect_match(r$r_code, "hist = FALSE", fixed = TRUE)
})

test_that("/PIECHART is recorded as unconverted, not approximated by a bar", {
  # jmv::descriptives has no pie argument -- confirmed against formals().
  # Substituting a bar chart would be a 'no pretending' defect.
  r <- conv("FREQUENCIES VARIABLES=a /PIECHART.")
  expect_match(r$r_code, "PIECHART", fixed = TRUE)
  expect_match(r$r_code, "bar = FALSE", fixed = TRUE)
})

# ---- EXAMINE: /PLOT exists, and has a non-empty SPSS default ----------------

test_that("EXAMINE with no /PLOT keeps only the boxplot SPSS prints by default", {
  r <- conv("EXAMINE VARIABLES=y BY grp /STATISTICS=DESCRIPTIVES.")
  expect_match(r$r_code, "box = TRUE", fixed = TRUE)
  expect_match(r$r_code, "hist = FALSE", fixed = TRUE)
  expect_match(r$r_code, "dens = FALSE", fixed = TRUE)
  expect_match(r$r_code, "qq = FALSE", fixed = TRUE)
})

test_that("TWO-SIDED: /PLOT HISTOGRAM NPPLOT turns those on", {
  r <- conv("EXAMINE VARIABLES=y /PLOT HISTOGRAM NPPLOT.")
  expect_match(r$r_code, "hist = TRUE", fixed = TRUE)
  expect_match(r$r_code, "qq = TRUE", fixed = TRUE)
})

test_that("/PLOT NONE draws nothing at all", {
  r <- conv("EXAMINE VARIABLES=y /PLOT NONE.")
  for (o in c("hist = FALSE", "dens = FALSE", "box = FALSE", "qq = FALSE")) {
    expect_match(r$r_code, o, fixed = TRUE)
  }
})

test_that("EXAMINE still emits the descriptives SPSS prints", {
  r <- conv("EXAMINE VARIABLES=y.")
  expect_match(r$r_code, "jmv::descriptives", fixed = TRUE)
  expect_match(r$r_code, "median = TRUE", fixed = TRUE)
  expect_match(r$r_code, "sw = TRUE", fixed = TRUE)
})

# ---- a keyword must be read inside its own subcommand -----------------------

test_that("a variable named HISTOGRAM does not switch the histogram on", {
  r <- conv("FREQUENCIES VARIABLES=HISTOGRAM /ORDER=ANALYSIS.")
  expect_match(r$r_code, "hist = FALSE", fixed = TRUE)
})

# ---- FOUND BY CONSULT (sonnet, 2026-09-09) ----------------------------------
#
# Two findings from the cross-model review of commit 90bde10, both reproduced
# against the 287-file corpus before anything was changed:
#
# 1. ABBREVIATED KEYWORDS. SPSS lets a subcommand be truncated to its shortest
#    unambiguous form, so `/HIST` draws a histogram. The gates matched only the
#    full word, so a truncated request converted with the plot OFF -- the exact
#    direction that silently removes a chart from a published report.
#    Measured exposure inside the commands actually gated: ZERO. FREQUENCIES
#    carried /HISTOGRAM 32, /PIECHART 4, /BARCHART 2 and no truncation; EXAMINE's
#    /PLOT carried BOXPLOT 137, HISTOGRAM 77, STEMLEAF 64, NPPLOT 46, NONE 2 and
#    no truncation. The 32 full forms are the control that makes the zero a real
#    zero. So this is a latent defect, not an active one -- fixed anyway, because
#    a prefix match here can only turn a plot ON, which is the safe direction.
#
# 2. STEMLEAF / SPREADLEVEL WERE DROPPED IN SILENCE. jmv has no stem-and-leaf and
#    no spread-vs-level plot, and unlike /PIECHART nothing recorded the omission.
#    STEMLEAF appears 64 times inside EXAMINE's /PLOT in the corpus, so this is
#    active, not latent. A requested output that vanishes without a trace is the
#    worst kind of defect this package can have: it is invisible.

test_that("a truncated /HIST still draws the histogram", {
  r <- conv("FREQUENCIES VARIABLES=a b /HIST.")
  expect_match(r$r_code, "hist = TRUE", fixed = TRUE)
})

test_that("a truncated /BAR still draws the bar chart", {
  r <- conv("FREQUENCIES VARIABLES=a /BAR.")
  expect_match(r$r_code, "bar = TRUE", fixed = TRUE)
})

test_that("a truncated /PIE is still recorded as unconverted", {
  r <- conv("FREQUENCIES VARIABLES=a /PIE.")
  expect_match(r$r_code, "PIECHART", fixed = TRUE)
})

test_that("truncated EXAMINE plot keywords still count", {
  r <- conv("EXAMINE VARIABLES=y /PLOT HIST NPPL.")
  expect_match(r$r_code, "hist = TRUE", fixed = TRUE)
  expect_match(r$r_code, "qq = TRUE", fixed = TRUE)
})

test_that("TWO-SIDED: a prefix shorter than three characters is NOT a match", {
  # Guards the fix from becoming an over-match. /HI is ambiguous and must not
  # silently switch a histogram on.
  r <- conv("FREQUENCIES VARIABLES=a /HI.")
  expect_match(r$r_code, "hist = FALSE", fixed = TRUE)
})

test_that("TWO-SIDED: an unrelated subcommand does not switch a chart on", {
  # /NTILES shares no prefix with a chart keyword; /FORMAT and /ORDER are the
  # two most common FREQUENCIES subcommands in the corpus (366 and 51).
  r <- conv("FREQUENCIES VARIABLES=a /NTILES=4 /FORMAT=NOTABLE /ORDER=ANALYSIS.")
  expect_match(r$r_code, "hist = FALSE", fixed = TRUE)
  expect_match(r$r_code, "bar = FALSE", fixed = TRUE)
})

test_that("EXAMINE /PLOT STEMLEAF records the plot jmv cannot draw", {
  r <- conv("EXAMINE VARIABLES=y /PLOT STEMLEAF.")
  expect_match(r$r_code, "STEMLEAF", fixed = TRUE)
  # ... and does not quietly substitute a different plot for it.
  expect_match(r$r_code, "box = FALSE", fixed = TRUE)
  expect_match(r$r_code, "hist = FALSE", fixed = TRUE)
})

test_that("EXAMINE /PLOT SPREADLEVEL records the plot jmv cannot draw", {
  r <- conv("EXAMINE VARIABLES=y /PLOT SPREADLEVEL(1).")
  expect_match(r$r_code, "SPREADLEVEL", fixed = TRUE)
})

test_that("TWO-SIDED: a plot jmv CAN draw gets no unconverted note", {
  r <- conv("EXAMINE VARIABLES=y /PLOT BOXPLOT HISTOGRAM NPPLOT.")
  expect_false(grepl("STEMLEAF", r$r_code, fixed = TRUE))
  expect_false(grepl("SPREADLEVEL", r$r_code, fixed = TRUE))
  expect_match(r$r_code, "box = TRUE", fixed = TRUE)
})

# ---- FOUND BY CONSULT (sol / openai, 2026-09-09) ---------------------------
#
# Second provider, reviewing the same commit. It independently confirmed both of
# sonnet's findings -- two providers, two vendors, same two defects -- and added
# six more. EVERY ONE BELOW WAS REPRODUCED HERE, with a two-sided control, before
# any code was changed. The reproductions are in the test bodies.
#
# Three of them are the ON direction (the report claims output SPSS never
# produced) and three are the OFF direction (SPSS output that vanishes without a
# trace). The project rule covers both: WE CONVERT THE SYNTAX, and anything jmv
# cannot draw is RECORDED rather than dropped.

test_that("a blank after the slash does not switch every EXAMINE plot off", {
  # sol #3. `.spss_has_sub` allowed `/ PLOT` but `.spss_sub_has` could not slice
  # it, so the subcommand was found and then read as empty. SPSS permits blanks
  # around slashes. REPRODUCED: `/ PLOT=HISTOGRAM` gave hist=FALSE while the
  # control `/PLOT=HISTOGRAM` gave hist=TRUE.
  r <- conv("EXAMINE VARIABLES=y / PLOT=HISTOGRAM.")
  expect_match(r$r_code, "hist = TRUE", fixed = TRUE)
})

test_that("TWO-SIDED: no blank still works", {
  r <- conv("EXAMINE VARIABLES=y /PLOT=HISTOGRAM.")
  expect_match(r$r_code, "hist = TRUE", fixed = TRUE)
})

test_that("EXAMINE's DEFAULT stem-and-leaf is recorded, not dropped", {
  # sol #4. SPSS's omitted-/PLOT default is BOXPLOT *and* STEMLEAF. We keep the
  # boxplot and jmv cannot draw the stem-and-leaf, so the omission must be on the
  # page. REPRODUCED: box=TRUE and no note of any kind.
  r <- conv("EXAMINE VARIABLES=y.")
  expect_match(r$r_code, "box = TRUE", fixed = TRUE)
  expect_match(r$r_code, "STEMLEAF", fixed = TRUE)
})

test_that("NPPLOT records the detrended Q-Q plot jmv does not draw", {
  # sol #7. SPSS NPPLOT produces a normal AND a detrended Q-Q plot;
  # jmv::descriptives(qq = TRUE) draws one ordinary Q-Q via stat_qq.
  # REPRODUCED: qq=TRUE, nothing recording the second plot.
  r <- conv("EXAMINE VARIABLES=y /PLOT=NPPLOT.")
  expect_match(r$r_code, "qq = TRUE", fixed = TRUE)
  expect_match(r$r_code, "detrended", ignore.case = TRUE)
})

test_that("TWO-SIDED: a boxplot-only request records no detrended note", {
  r <- conv("EXAMINE VARIABLES=y /PLOT=BOXPLOT.")
  expect_false(grepl("detrended", r$r_code, ignore.case = TRUE))
})

test_that("FREQUENCIES /HISTOGRAM NORMAL records the normal curve it cannot draw", {
  # sol #8. SPSS's NORMAL superimposes a fitted normal curve. jmv's `dens` is a
  # KERNEL density, not a fitted normal, so substituting it would be pretending.
  # REPRODUCED: hist=TRUE and no record of NORMAL.
  r <- conv("FREQUENCIES VARIABLES=x /HISTOGRAM NORMAL.")
  expect_match(r$r_code, "hist = TRUE", fixed = TRUE)
  expect_match(r$r_code, "NORMAL", fixed = TRUE)
  expect_match(r$r_code, "dens = FALSE", fixed = TRUE)
})

test_that("TWO-SIDED: a plain /HISTOGRAM records no normal curve", {
  r <- conv("FREQUENCIES VARIABLES=x /HISTOGRAM.")
  expect_match(r$r_code, "hist = TRUE", fixed = TRUE)
  expect_false(grepl("NORMAL", r$r_code, fixed = TRUE))
})

test_that("a slash inside a quoted string does not fabricate a residual plot", {
  # sol #9. PRE-EXISTING, and the same fabrication class as the defect this file
  # exists for: the report claims a plot SPSS never produced. REPRODUCED:
  # norm=TRUE and qqPlot=TRUE from a /RESIDUALS that lives inside a string
  # literal, against norm=FALSE qqPlot=FALSE for the same command without it.
  r <- conv(paste("REGRESSION SELECT sex EQ '/RESIDUALS NORMPROB'",
                  "/VARIABLES=y x /DEPENDENT=y /METHOD=ENTER x."))
  expect_match(r$r_code, "norm = FALSE", fixed = TRUE)
  expect_match(r$r_code, "qqPlot = FALSE", fixed = TRUE)
})

test_that("TWO-SIDED: a REAL /RESIDUALS NORMPROB still turns them on", {
  r <- conv(paste("REGRESSION /VARIABLES=y x /DEPENDENT=y",
                  "/METHOD=ENTER x /RESIDUALS NORMPROB."))
  expect_match(r$r_code, "norm = TRUE", fixed = TRUE)
  expect_match(r$r_code, "qqPlot = TRUE", fixed = TRUE)
})

test_that("a saved variable NAMED cook is not a request for Cook's distance", {
  # sol #10. PRE-EXISTING. In `/SAVE=PRED(COOK)` the parenthesised token is the
  # NAME of the saved predicted-value column, not the COOK statistic.
  # REPRODUCED: cooks=TRUE, identical to a genuine /SAVE=COOK.
  r <- conv(paste("REGRESSION /VARIABLES=y x /DEPENDENT=y",
                  "/METHOD=ENTER x /SAVE=PRED(COOK)."))
  expect_match(r$r_code, "cooks = FALSE", fixed = TRUE)
})

test_that("TWO-SIDED: a genuine /SAVE=COOK still turns it on", {
  r <- conv(paste("REGRESSION /VARIABLES=y x /DEPENDENT=y",
                  "/METHOD=ENTER x /SAVE=COOK."))
  expect_match(r$r_code, "cooks = TRUE", fixed = TRUE)
})
