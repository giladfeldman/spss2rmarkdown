# Regression tests for the defects the three-provider /consult round of
# 2026-09-09 (Sonnet / anthropic, Sol / openai, Grok / xai) found in the first
# WITH-keyword fix, plus the SPSS-gold anchors that pin the numbers.
#
# Every one of these was REPRODUCED locally before being fixed. Each test was
# watched FAIL against the unfixed tree.
#
# Ground truth for the gold-anchored tests is the frozen SPSS output at
#     osf_5a91c46cda91d4000fb0_sample4.txt
# which lives outside this repo (the corpus is maintained separately
# only), so those tests skip when it is not reachable rather than pretending.

parse_one <- function(spss_text) {
  f <- tempfile(fileext = ".sps"); on.exit(unlink(f)); writeLines(spss_text, f)
  p <- parse_sps(f); p[[1]]$command_type <- unname(p[[1]]$command_type); p[[1]]
}

# ---------------------------------------------------------------------------
# Sol finding 6 / Grok F2 -- with_vars was never normalized, so the y-side kept
# its lowercase source spelling while the data columns are uppercased. The
# case-sensitive pre-flight then aborted the analysis with
#   variables not present in the dataset: 'meanbenevo'
# i.e. the old PARTIAL CORR failure mode moved from the token WITH onto the
# y-set. The unit tests never saw it because they call the converters directly
# and skip convert_spss_to_r().
# ---------------------------------------------------------------------------
test_that("the WITH side is name-normalized like every other variable field", {
  conv <- convert_spss_to_r(parse_one("CORRELATIONS /VARIABLES=a with meanbenevo."),
                            NULL, all_var_names = c("A", "MEANBENEVO"))
  expect_equal(conv$variables, c("A", "MEANBENEVO"))
  expect_true(grepl('"MEANBENEVO"', conv$r_code, fixed = TRUE))
  expect_false(grepl('"meanbenevo"', conv$r_code, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# All three seats -- PARTIAL CORR with WITH but no BY fell back to
# jmv::corrMatrix over the UNION, which is the square-matrix over-reporting bug
# this pair of functions exists to fix, reintroduced for that one case.
# ---------------------------------------------------------------------------
test_that("PARTIAL CORR with WITH but no BY keeps the rectangular block", {
  conv <- convert_partial_corr(parse_one("PARTIAL CORR /VARIABLES=a b c with x y."), NULL)
  expect_false(grepl("corrMatrix", conv$r_code, fixed = TRUE))

  set.seed(2)
  d <- as.data.frame(matrix(stats::rnorm(60 * 5), ncol = 5))
  names(d) <- c("a", "b", "c", "x", "y")
  out <- eval(parse(text = conv$r_code), envir = list2env(list(data = d)))
  # 3 x-variables x {r, p, N} rows; Variable + Statistic + 2 y-columns
  expect_equal(dim(out), c(9L, 4L))
  expect_equal(names(out)[3:4], c("x", "y"))
  # the square union would have contained an a-b and an x-y pair; it must not
  expect_setequal(unique(out$Variable[nzchar(out$Variable)]), c("a", "b", "c"))
})

# ---------------------------------------------------------------------------
# Sonnet 3 / Sol / Grok F5 -- BY was peeled BEFORE WITH, so a malformed
# "A B BY ctrl WITH c d" put the y-side variables into the CONTROL list and
# silently partialled out two variables the researcher asked to correlate.
# ---------------------------------------------------------------------------
test_that("BY appearing before WITH does not swallow the y-set into controls", {
  s <- split_corr_variables_clause("a b BY ctrl WITH c d")
  expect_equal(s$x, c("a", "b"))
  expect_equal(s$y, c("c", "d"))
  expect_equal(s$controls, "ctrl")
})

test_that("the ordinary WITH ... BY order is unchanged", {
  s <- split_corr_variables_clause("a b c with x y z BY ctrl")
  expect_equal(s$x, c("a", "b", "c"))
  expect_equal(s$y, c("x", "y", "z"))
  expect_equal(s$controls, "ctrl")
})

# ---------------------------------------------------------------------------
# Sol 1-2 / Grok F1, F4 -- /MISSING was parsed but never read, so a LISTWISE
# command was computed pairwise. Every r, p and N in the block moves.
# ---------------------------------------------------------------------------
test_that("CORRELATIONS honours /MISSING=LISTWISE", {
  set.seed(9)
  d <- data.frame(a = stats::rnorm(120), b = stats::rnorm(120),
                  x = stats::rnorm(120))
  d$b[1:40] <- NA                      # missing on a variable NOT in the a-x pair

  pw <- convert_correlations(parse_one(
    "CORRELATIONS /VARIABLES=a b with x /MISSING=PAIRWISE."), NULL)
  lw <- convert_correlations(parse_one(
    "CORRELATIONS /VARIABLES=a b with x /MISSING=LISTWISE."), NULL)

  o_pw <- eval(parse(text = pw$r_code), envir = list2env(list(data = d)))
  o_lw <- eval(parse(text = lw$r_code), envir = list2env(list(data = d)))

  # The Variable label sits on the first row of each 3-row block, so select on
  # the Statistic row and take the block for x-variable "a" (the first block).
  n_pw <- as.numeric(o_pw[o_pw$Statistic == "N", "x"])[1]
  n_lw <- as.numeric(o_lw[o_lw$Statistic == "N", "x"])[1]
  expect_equal(n_pw, 120)              # pairwise: a and x are complete
  expect_equal(n_lw, 80)               # listwise: the 40 b-missing rows go too

  ref <- with(d[stats::complete.cases(d), ], stats::cor.test(a, x))
  r_lw <- as.numeric(o_lw[o_lw$Statistic == "Pearson r", "x"])[1]
  expect_equal(r_lw, round(unname(ref$estimate), 3), tolerance = 1e-9)
})

test_that("PARTIAL CORR defaults to LISTWISE and honours /MISSING=ANALYSIS", {
  skip_if_not_installed("ppcor")
  set.seed(4)
  d <- data.frame(a = stats::rnorm(150), b = stats::rnorm(150),
                  x = stats::rnorm(150), z = stats::rnorm(150))
  d$b[1:50] <- NA

  # No /MISSING at all -> SPSS's documented default is LISTWISE, so the
  # b-missing rows must be dropped from the a x x cell too.
  dflt <- convert_partial_corr(parse_one("PARTIAL CORR /VARIABLES=a b with x BY z."), NULL)
  o <- eval(parse(text = dflt$r_code), envir = list2env(list(data = d)))
  expect_equal(unique(o$Missing), "LISTWISE")
  expect_equal(unique(o$N), 100)
  expect_equal(length(unique(o$df)), 1L)   # one shared case set -> one df

  an <- convert_partial_corr(parse_one(
    "PARTIAL CORR /VARIABLES=a b with x BY z /MISSING=ANALYSIS."), NULL)
  o2 <- eval(parse(text = an$r_code), envir = list2env(list(data = d)))
  expect_equal(unique(o2$Missing), "ANALYSIS")
  # a x x is evaluated on its own valid cases, so its N is the full 150
  expect_equal(o2$N[o2$Variable == "a" & o2$With == "x"], 150)
})

# ---------------------------------------------------------------------------
# Sol 3 -- /PRINT=ONETAIL and /SIGNIFICANCE=ONETAIL were ignored, so a
# two-tailed p was printed under a heading that said two-tailed while the
# command had asked for one. 18 of the corpus file's CORRELATIONS ask for it.
# ---------------------------------------------------------------------------
test_that("CORRELATIONS /PRINT=ONETAIL halves p and relabels the row", {
  set.seed(6)
  d <- data.frame(a = 1:40, x = (1:40) + stats::rnorm(40, sd = 12))

  two <- convert_correlations(parse_one(
    "CORRELATIONS /VARIABLES=a with x /PRINT=TWOTAIL NOSIG."), NULL)
  one <- convert_correlations(parse_one(
    "CORRELATIONS /VARIABLES=a with x /PRINT=ONETAIL NOSIG."), NULL)

  o2 <- eval(parse(text = two$r_code), envir = list2env(list(data = d)))
  o1 <- eval(parse(text = one$r_code), envir = list2env(list(data = d)))

  expect_true(any(o2$Statistic == "Sig. (2-tailed)"))
  expect_true(any(o1$Statistic == "Sig. (1-tailed)"))
  p2 <- as.numeric(o2[o2$Statistic == "Sig. (2-tailed)", "x"])
  p1 <- as.numeric(o1[o1$Statistic == "Sig. (1-tailed)", "x"])
  expect_equal(p1, round(p2 / 2, 3), tolerance = 1e-6)
})

test_that("PARTIAL CORR /SIGNIFICANCE=ONETAIL is parsed and applied", {
  skip_if_not_installed("ppcor")
  p <- parse_one("PARTIAL CORR /VARIABLES=a with x BY z /SIGNIFICANCE=ONETAIL.")
  expect_equal(p$options$significance, "ONETAIL")
  conv <- convert_partial_corr(p, NULL)
  expect_true(grepl("Sig. (1-tailed)", conv$r_code, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Sol 4 -- a variable named on BOTH sides gave r = 1 with p = 0, asserting a
# significant result for a variable against itself. SPSS prints 1.000 with the
# significance blank.
# ---------------------------------------------------------------------------
test_that("a self-correlation reports r = 1 with no significance", {
  d <- data.frame(a = stats::rnorm(30), x = stats::rnorm(30))
  conv <- convert_correlations(parse_one("CORRELATIONS /VARIABLES=a with a x."), NULL)
  out <- eval(parse(text = conv$r_code), envir = list2env(list(data = d)))
  expect_equal(as.numeric(out[out$Statistic == "Pearson r", "a"]), 1)
  expect_true(is.na(as.numeric(out[grepl("Sig", out$Statistic), "a"])))
})

# ---------------------------------------------------------------------------
# A benign warning must not be able to null a good coefficient. cor.test
# already returns NA for a zero-variance pair (measured 2026-09-09), so
# catching warnings could only ever discard a real number.
# ---------------------------------------------------------------------------
test_that("a zero-variance column yields NA, and a good pair is unaffected", {
  d <- data.frame(a = stats::rnorm(30), k = 1, x = stats::rnorm(30))
  conv <- convert_correlations(parse_one("CORRELATIONS /VARIABLES=a with k x."), NULL)
  out <- eval(parse(text = conv$r_code), envir = list2env(list(data = d)))
  expect_true(is.na(as.numeric(out[out$Statistic == "Pearson r", "k"])))
  expect_false(is.na(as.numeric(out[out$Statistic == "Pearson r", "x"])))
})

# ---------------------------------------------------------------------------
# GOLD ANCHOR -- the emitted numbers against real SPSS output.
# ---------------------------------------------------------------------------
# The paired .sps/.sav corpus and its frozen SPSS output are NOT part of this
# package -- they are far too large to ship and are not ours to redistribute.
# Point SPSS2R_GOLD_CORPUS at the corpus root to run the gold anchors; without
# it they skip, and the skip says so rather than passing silently.
corpus_dir <- function() {
  root <- Sys.getenv("SPSS2R_GOLD_CORPUS", unset = "")
  if (!nzchar(root)) return("")
  file.path(root, "round-2-spss")
}

test_that("CORRELATIONS ... WITH reproduces the frozen SPSS output cell for cell", {
  base <- corpus_dir()
  sav  <- file.path(base, "3-converted-local", "spss", "osf_wdnpx_sample4.sav")
  sps  <- file.path(base, "3-converted-local", "spss",
                    "osf_5a91c46cda91d4000fb0_sample4.sps")
  skip_if_not(nzchar(base) && file.exists(sav) && file.exists(sps),
              "gold corpus not reachable -- set SPSS2R_GOLD_CORPUS to the corpus root to run this anchor")

  sd    <- parse_sav(sav)
  cmds  <- parse_sps(sps)
  convs <- convert_all_commands(cmds, sd)
  data  <- sd$data
  names(data) <- normalize_spss_names(names(data))
  data[] <- lapply(data, function(v) if (inherits(v, "haven_labelled")) as.numeric(v) else v)

  Y <- c("MAR1","MAR2","MAR3","MAR4","MAR5","MAR6","MAR7","MAR8","MAR9","MAR10","MEANMAR")
  k <- NULL
  for (i in seq_along(cmds)) {
    v <- cmds[[i]]$variables
    if (identical(unname(cmds[[i]]$command_type), "CORRELATIONS") &&
        identical(toupper(v$main_vars %||% ""), c("MEAN_IDEN_ISRA", "MEAN_IDEN_ARAB")) &&
        identical(toupper(v$with_vars %||% ""), Y)) { k <- i; break }
  }
  skip_if(is.null(k), "target command not found in the corpus file")

  out <- eval(parse(text = convs[[k]]$r_code), envir = list2env(list(data = data)))

  # osf_5a91c46cda91d4000fb0_sample4.txt:2346-2354 (Filter mean_iden_isra > 3.5,
  # /MISSING=PAIRWISE -- note N varies across the row, which is what pairwise
  # deletion means and what the emitter must reproduce).
  gold <- list(
    MEAN_IDEN_ISRA = list(r = c(.114,.072,.111,.053,.252,.188,.245,.260,-.014,-.013,.168),
                          p = c(.263,.483,.278,.605,.012,.063,.015,.010,.891,.902,.097),
                          n = c(98,98,97,99,99,99,99,98,99,99,99)),
    MEAN_IDEN_ARAB = list(r = c(.168,.111,.163,.073,.330,.073,.173,.190,.144,-.002,.182),
                          p = c(.098,.278,.111,.473,.001,.474,.088,.060,.155,.983,.071),
                          n = c(98,98,97,99,99,99,99,98,99,99,99)))
  xs <- unique(out$Variable[nzchar(out$Variable)])
  expect_equal(toupper(xs), names(gold))
  for (xi in seq_along(xs)) {
    g <- gold[[toupper(xs[xi])]]; base_row <- (xi - 1) * 3 + 1
    for (j in seq_along(Y)) {
      # SPSS prints three decimals; compare at exactly that precision.
      expect_equal(round(as.numeric(out[base_row,     Y[j]]), 3), g$r[j],
                   info = paste("r", xs[xi], Y[j]))
      expect_equal(round(as.numeric(out[base_row + 1, Y[j]]), 3), g$p[j],
                   info = paste("p", xs[xi], Y[j]))
      expect_equal(as.numeric(out[base_row + 2, Y[j]]), g$n[j],
                   info = paste("N", xs[xi], Y[j]))
    }
  }
})

test_that("PARTIAL CORR ... WITH ... BY reproduces the frozen SPSS output", {
  base <- corpus_dir()
  sav  <- file.path(base, "3-converted-local", "spss", "osf_wdnpx_sample4.sav")
  sps  <- file.path(base, "3-converted-local", "spss",
                    "osf_5a91c46cda91d4000fb0_sample4.sps")
  skip_if_not(nzchar(base) && file.exists(sav) && file.exists(sps),
              "gold corpus not reachable -- set SPSS2R_GOLD_CORPUS to the corpus root to run this anchor")
  skip_if_not_installed("ppcor")

  sd    <- parse_sav(sav)
  cmds  <- parse_sps(sps)
  convs <- convert_all_commands(cmds, sd)
  data  <- sd$data
  names(data) <- normalize_spss_names(names(data))
  data[] <- lapply(data, function(v) if (inherits(v, "haven_labelled")) as.numeric(v) else v)

  X <- c("MEANMERGER","MEANINTERSEC","MEANCOMPART","R_MEANDOMARAB1",
         "MEANDOMARAB","MEANDOMISR","MEANBIISAR")
  Y <- c("MEANBENEVO","MEANUNIVERS","MEANSELFDIREC","MEANSTIMUL","MEANHEDON",
         "MEANACHIV","MEANPOWER","MEANSECUR","MEANCONFORM","MEANTRADITION")
  k <- NULL
  for (i in seq_along(cmds)) {
    v <- cmds[[i]]$variables
    if (identical(unname(cmds[[i]]$command_type), "PARTIAL CORR") &&
        identical(toupper(v$main_vars %||% ""), X) &&
        identical(toupper(v$with_vars %||% ""), Y) &&
        identical(toupper(v$controls %||% ""), "MRAT")) { k <- i; break }
  }
  skip_if(is.null(k), "target command not found in the corpus file")

  out <- eval(parse(text = convs[[k]]$r_code), envir = list2env(list(data = data)))

  expect_equal(nrow(out), length(X) * length(Y))
  expect_equal(unique(out$df), 94)      # LISTWISE: one shared case set (N = 97)
  expect_equal(unique(out$N), 97)

  # osf_5a91c46cda91d4000fb0_sample4.txt:11740-11766
  gold_r <- list(
    MEANMERGER   = c(.125,.238,.085,-.137,.078,-.101,-.054,-.155,.050,-.102),
    MEANINTERSEC = c(-.080,.083,.164,.130,.192,-.108,-.098,.014,-.093,-.161),
    MEANCOMPART  = c(-.036,.185,-.123,.070,-.035,-.134,.071,.055,-.140,.053))
  gold_p <- list(
    MEANMERGER   = c(.225,.019,.408,.182,.452,.330,.602,.132,.630,.321),
    MEANINTERSEC = c(.439,.420,.111,.207,.061,.295,.343,.890,.366,.117),
    MEANCOMPART  = c(.725,.071,.234,.497,.734,.192,.489,.597,.174,.607))
  for (x in names(gold_r)) for (j in seq_along(Y)) {
    row <- out[out$Variable == x & out$With == Y[j], , drop = FALSE]
    expect_equal(nrow(row), 1L, info = paste(x, Y[j]))
    expect_equal(round(as.numeric(row[["Partial r"]]), 3), gold_r[[x]][j],
                 info = paste("partial r", x, Y[j]))
    expect_equal(round(as.numeric(row[["Sig. (2-tailed)"]]), 3), gold_p[[x]][j],
                 info = paste("p", x, Y[j]))
  }
})

# ---------------------------------------------------------------------------
# Found 2026-09-09 while checking a shape a peer review flagged (a keyword
# standing alone at the end of its line, the case a382a69 fixed elsewhere).
# `/VARIABLES=` is OPTIONAL in CORRELATIONS -- the list may follow the command
# keyword directly. Without a fallback the clause came back NULL, the splitter
# returned nothing, and the command degraded to the square matrix: the exact
# over-reporting this work exists to remove, for exactly the commands that use
# WITH. Measured over the corpus: 11 of 322 CORRELATIONS / PARTIAL CORR
# commands omit /VARIABLES=, and 5 of those use WITH -- e.g.
#   osf_6493fae2a2a2f4056c43_Gerontologist-2021.sps
#   osf_67e67b55954d018c24bc_250304.sps      "correlations Jnwsize with Jnwbuurt."
# ---------------------------------------------------------------------------
test_that("CORRELATIONS without /VARIABLES= still honours WITH", {
  v <- parse_one("correlations a b with x y.")$variables
  expect_equal(v$main_vars, c("a", "b"))
  expect_equal(v$with_vars, c("x", "y"))

  # the corpus form: a single pair, no subcommands at all
  v2 <- parse_one("correlations Jnwsize with Jnwbuurt.")$variables
  expect_equal(v2$main_vars, "Jnwsize")
  expect_equal(v2$with_vars, "Jnwbuurt")

  # and the keyword-alone-on-its-line shape, with no slash anywhere
  v3 <- parse_one("CORRELATIONS\n  a b\n  with x y.")$variables
  expect_equal(v3$main_vars, c("a", "b"))
  expect_equal(v3$with_vars, c("x", "y"))
})

test_that("PARTIAL CORR without /VARIABLES= still honours WITH and BY", {
  v <- parse_one("partial corr a b with x y BY ctrl.")$variables
  expect_equal(v$main_vars, c("a", "b"))
  expect_equal(v$with_vars, c("x", "y"))
  expect_equal(v$controls, "ctrl")
})
