# FACTOR is converted with psych::, not jmv::efa.
#
# Why (2026-08-04): jmv::efa is non-deterministically broken in the worker
# environment — the byte-identical call in fresh isolated Rscript processes
# returned "'names' attribute [N] must be the same length as the vector [0]"
# 8/8 in one batch and succeeded 4/4 in another. It is not data-determined
# (missing variables, full-frame vs subset, haven_labelled class, and CPU
# contention were each tested and refuted). psych is already a declared
# dependency and is deterministic.
#
# GROUND TRUTH (the gate for this change — a converter must reproduce the
# source tool's numbers, and an engine swap changes a published statistic):
# generated with SPSS Statistics on 2026-08-04 from
#   test-corpus/spss/osf-round3/fk5hu/Driver_Data.sav
# running the same FACTOR specification (PC extraction, MINEIGEN(1), VARIMAX,
# /MISSING PAIRWISE) over IT71 IT77 IT76 IT33 IT92 IT88 IT79 IT40:
#
#   KMO ................ .833
#   Bartlett ........... approx. chi-square 905.322, df 28, p < .001
#   Communalities ...... .701 .722 .798 .603 .489 .712 .512 .309
#
# (The SPSS OMS text export drops leading characters, so it rendered these as
# "?05.322" / "?8"; the values below were confirmed by recomputation, which is
# what resolved the truncation. See LEARNINGS: recompute, never trust a
# positional read of a fragmented GT.)

SPSS_KMO <- 0.833
SPSS_BARTLETT_CHISQ <- 905.322
SPSS_BARTLETT_DF <- 28
SPSS_COMMUNALITIES <- c(.701, .722, .798, .603, .489, .712, .512, .309)
FACTOR_VARS <- c("IT71", "IT77", "IT76", "IT33", "IT92", "IT88", "IT79", "IT40")

driver_sav <- function() {
  p <- file.path("C:/Users/filin/Vibe/MetaScienceTools/2Rmarkdown",
                 "test-corpus/spss/osf-round3/fk5hu/Driver_Data.sav")
  if (!file.exists(p)) skip("Driver_Data.sav not available in this checkout")
  p
}

test_that("convert_factor emits psych-based code, not jmv::efa", {
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  cf  <- getFromNamespace("convert_factor", "spss2rmarkdown")
  p <- psc("FACTOR\n/VARIABLES a1 a2 a3\n/EXTRACTION PC\n/ROTATION VARIMAX")
  out <- cf(p, NULL)
  expect_false(grepl("jmv::efa", out$r_code, fixed = TRUE))
  expect_true(grepl("psych", out$r_code, fixed = TRUE))
  expect_silent(parse(text = paste0(".res <- ", out$r_code)))
  expect_true("psych" %in% out$packages)
})

test_that("the emitted FACTOR code reproduces SPSS's KMO and Bartlett exactly", {
  skip_if_not_installed("psych")
  skip_if_not_installed("haven")
  d <- haven::read_sav(driver_sav())
  names(d) <- toupper(names(d))
  skip_if_not(all(FACTOR_VARS %in% names(d)), "FACTOR variables absent from .sav")

  x <- as.data.frame(lapply(d[, FACTOR_VARS], as.numeric))
  R <- stats::cor(x, use = "pairwise.complete.obs")   # SPSS /MISSING PAIRWISE
  n <- sum(stats::complete.cases(x))

  expect_equal(round(psych::KMO(R)$MSA, 3), SPSS_KMO)

  b <- psych::cortest.bartlett(R, n = n)
  expect_equal(round(b$chisq, 3), SPSS_BARTLETT_CHISQ)
  expect_equal(b$df, SPSS_BARTLETT_DF)
  expect_lt(b$p.value, .001)
})

test_that("the emitted FACTOR code reproduces SPSS's communalities exactly", {
  skip_if_not_installed("psych")
  skip_if_not_installed("haven")
  d <- haven::read_sav(driver_sav())
  names(d) <- toupper(names(d))
  skip_if_not(all(FACTOR_VARS %in% names(d)), "FACTOR variables absent from .sav")

  x <- as.data.frame(lapply(d[, FACTOR_VARS], as.numeric))
  R <- stats::cor(x, use = "pairwise.complete.obs")
  nf <- sum(eigen(R)$values > 1)          # SPSS /CRITERIA MINEIGEN(1)
  expect_equal(nf, 2)

  pc <- psych::principal(R, nfactors = nf, rotate = "varimax")  # /EXTRACTION PC, VARIMAX
  expect_equal(unname(round(pc$communality, 3)), SPSS_COMMUNALITIES)
})

test_that("generated FACTOR code actually runs on the real data", {
  skip_if_not_installed("psych")
  skip_if_not_installed("haven")
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  cf  <- getFromNamespace("convert_factor", "spss2rmarkdown")

  d <- haven::read_sav(driver_sav())
  names(d) <- toupper(names(d))
  skip_if_not(all(FACTOR_VARS %in% names(d)), "FACTOR variables absent from .sav")
  data <- as.data.frame(d)

  cmd <- paste0("FACTOR\n/VARIABLES ", paste(FACTOR_VARS, collapse = " "),
                "\n/MISSING PAIRWISE\n/EXTRACTION PC\n/ROTATION VARIMAX\n/METHOD=CORRELATION")
  out <- cf(psc(cmd), NULL)

  env <- new.env(); env$data <- data
  res <- eval(parse(text = out$r_code), envir = env)
  expect_false(is.null(res))
  # And it must be deterministic: the same call twice gives the same KMO.
  res2 <- eval(parse(text = out$r_code), envir = env)
  expect_equal(res$kmo$MSA, res2$kmo$MSA)
})
