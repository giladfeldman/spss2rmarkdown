# test-reliability-no-omega.R — RELIABILITY must not fabricate omega.
#
# Found by the 2026-08-04 Sonnet canary audit on round-2-spss sample4: every
# `RELIABILITY /MODEL=ALPHA` translation emitted jmv::reliability(...,
# omegaScale = TRUE, omegaItems = TRUE) — McDonald's omega, a statistic SPSS's
# RELIABILITY procedure never computes. The report therefore showed scale- and
# item-level statistics the source software's output does not contain, for
# every RELIABILITY block corpus-wide. Watched RED against the unfixed
# converter (emission contained omegaScale/omegaItems).

conv_rel <- function(txt) {
  p <- getFromNamespace("parse_single_command", "spss2rmarkdown")(txt)
  getFromNamespace("convert_spss_to_r", "spss2rmarkdown")(p, NULL)
}

test_that("RELIABILITY /MODEL=ALPHA emits alpha statistics but no omega", {
  conv <- conv_rel(paste0(
    "RELIABILITY\n",
    "  /VARIABLES=v3 v8 v19 v23 v29 v40\n",
    "  /SCALE('ALL VARIABLES') ALL\n",
    "  /MODEL=ALPHA\n",
    "  /SUMMARY=TOTAL."))
  expect_match(conv$r_code, "alphaScale = TRUE", fixed = TRUE)
  expect_match(conv$r_code, "itemRestCor = TRUE", fixed = TRUE)
  expect_match(conv$r_code, "alphaItems = TRUE", fixed = TRUE)
  expect_false(grepl("omega", conv$r_code, ignore.case = TRUE))
})
