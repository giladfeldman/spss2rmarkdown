# The SAME document, rendered TWICE, must give the SAME Durbin-Watson result.
#
# THE DEFECT, and why the earlier fix did not close it. Gating `durbin` on
# `/RESIDUALS DURBIN` stopped the converter from emitting the table where SPSS
# never asked for one, but that was only half the problem. jmv computes the
# Durbin-Watson p by SIMULATION, and the generated .Rmd sets no seed: of the 47
# documents rendered from the test corpus after the gate shipped, exactly ONE
# contained `set.seed`, and it sat above a `kmeans()` call. So for the five
# corpus .sps that DO carry `/RESIDUALS ... DURBIN` the gate correctly switched
# the option back on and the p went on moving. Four sessions reproduced that on
# identical data with four DISJOINT sets of values -- .042-.062, .054-.082,
# .898-.966, .864-.972 -- several of them straddling p = .05.
#
# WHY THE FIX IS SUPPRESSION AND NOT A SEED. The deciding question was what
# SPSS itself prints, and it was answered from a frozen SPSS listing rather
# than by assertion: SPSS's Model Summary carries a `Durbin-Watson` column
# holding ONE number and no p anywhere on the row (the only `Sig.` there
# belongs to the F Change column of the Change Statistics block). A seed would
# therefore have left an INVENTED number in the report and merely stopped it
# moving -- worse than leaving it unstable, because a number that stops moving
# stops looking suspicious.
#
# The assertion that actually pins the defect is the render-twice one below.
# An emission-shape test cannot: the converter emitted `durbin = TRUE` both
# before and after this fix.

.dw_fixture <- function(n = 210, seed = 20260910) {
  # Independent errors, which is what makes the p land mid-range (~.09-.15)
  # and vary. With autocorrelated errors jmv's p pins at 0.000 and the
  # two-sided control below could not tell a working suppression from a
  # constant. Measured before this test was written: 12 unsuppressed renders
  # of this fixture gave 11 distinct p values in [0.090, 0.148].
  set.seed(seed)
  x1 <- stats::rnorm(n)
  x2 <- stats::rnorm(n)
  data.frame(Y = 0.5 * x1 + 0.3 * x2 + stats::rnorm(n), X1 = x1, X2 = x2)
}

.dw_render <- function(d, suppress) {
  res <- jmv::linReg(data = d, dep = "Y", covs = c("X1", "X2"),
                     blocks = list(list("X1", "X2")), refLevels = list(),
                     durbin = TRUE)
  if (suppress) res <- s2r_spss_reg_tables(res, durbin_p = FALSE)
  res$models[[1]]$assump$durbin$asString()
}

test_that("TWO-SIDED CONTROL: without the fix the same render gives a different p", {
  skip_if_not_installed("jmv")
  d <- .dw_fixture()
  raw <- replicate(12, .dw_render(d, suppress = FALSE))
  # If this ever collapses to 1 the fixture has gone degenerate and the test
  # below is no longer proving anything -- fail loudly rather than pass.
  expect_gt(length(unique(raw)), 1L)
  expect_true(all(grepl("p", raw, fixed = TRUE)))
})

test_that("the same regression rendered TWICE yields the SAME Durbin-Watson table", {
  skip_if_not_installed("jmv")
  d <- .dw_fixture()
  fixed <- replicate(12, .dw_render(d, suppress = TRUE))
  expect_equal(length(unique(fixed)), 1L)
})

test_that("only the statistic SPSS prints survives the suppression", {
  skip_if_not_installed("jmv")
  d <- .dw_fixture()
  s <- .dw_render(d, suppress = TRUE)
  expect_match(s, "DW Statistic", fixed = TRUE)
  # BOTH of jmv's other columns go. The p is simulated; `Autocorrelation` is
  # deterministic but SPSS's Model Summary has no such column either, so
  # publishing it presents a number the source software never produced. Raised
  # by consult seats `sol` (openai) and `grok` (xai), 2026-09-10 -- the earlier
  # version of this test pinned Autocorrelation as required to survive.
  expect_false(grepl("Autocorrelation Test", s, fixed = TRUE))
  hdr <- grep("DW Statistic", strsplit(s, "\n", fixed = TRUE)[[1]], value = TRUE)[[1]]
  expect_false(grepl("(^|\\s)p(\\s|$)", hdr))
  expect_false(grepl("Autocorrelation", hdr, fixed = TRUE))
})

test_that("SPSS prints Durbin-Watson ONCE, so intermediate blocks are hidden", {
  # In the frozen SPSS listing this fix is built on, model 1's Durbin-Watson
  # cell is BLANK and only the final model carries a value. jmv builds the
  # table per block, so every intermediate block published a statistic SPSS
  # left empty. Raised by seat `grok` (xai), checked against that listing.
  skip_if_not_installed("jmv")
  d <- .dw_fixture()
  res <- jmv::linReg(data = d, dep = "Y", covs = c("X1", "X2"),
                     blocks = list(list("X1"), list("X2")), refLevels = list(),
                     durbin = TRUE)
  res <- s2r_spss_reg_tables(res, durbin_p = FALSE)
  expect_false(isTRUE(res$models[[1]]$assump$durbin$visible))
  expect_true(isTRUE(res$models[[2]]$assump$durbin$visible))
})

test_that("a hide that does NOT take is reported, not swallowed", {
  # FAIL LOUD, NOT OPEN -- all three seats raised this independently. A bare
  # tryCatch would publish the invented p under any jmv whose internals have
  # moved, with nothing red. The helper reads `$visible` back and prints a
  # conversion note when the hide did not take.
  fake <- list(models = list(list(assump = list(durbin = list(
    getColumn = function(n) list(setVisible = function(v) invisible(NULL),
                                 visible = TRUE),
    setVisible = function(v) invisible(NULL), visible = TRUE)))))
  out <- capture.output(s2r_spss_reg_tables(fake))
  expect_true(any(grepl("could not suppress", out, fixed = TRUE)))
})

test_that("s2r_spss_reg_tables leaves a results object it cannot read alone", {
  # The helper is deparsed into every generated .Rmd and runs against whatever
  # jmv the reader has installed. A differently shaped results object must
  # degrade to "changed nothing", never kill the chunk -- and must not emit a
  # spurious note for a table that simply is not there.
  expect_identical(s2r_spss_reg_tables(list(a = 1)), list(a = 1))
  expect_identical(s2r_spss_reg_tables(NULL), NULL)
  expect_identical(s2r_spss_reg_tables("not a result", model_comp = FALSE),
                   "not a result")
  expect_silent(s2r_spss_reg_tables(list(a = 1)))
})

test_that("the generated document BINDS the helper it calls", {
  # `s2r_spss_reg_tables` is emitted into every regression chunk. If it is not
  # also registered in .s2r_helpers_chunk() the chunk dies at knit time with
  # "could not find function" -- and pkgload::load_all() hides exactly that,
  # because it puts package internals on the search path. This test reads the
  # helpers chunk the generator actually writes.
  hel <- .s2r_helpers_chunk()
  expect_match(hel, "s2r_spss_reg_tables <- function", fixed = TRUE)
  # CONTROL: a helper that has always been there, so a broken regex would show
  # up as both failing rather than as a silent pass.
  expect_match(hel, "s2r_render_tables <- function", fixed = TRUE)
})
