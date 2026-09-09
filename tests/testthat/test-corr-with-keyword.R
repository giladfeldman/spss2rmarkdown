# Regression tests for the SPSS `WITH` keyword in CORRELATIONS and PARTIAL CORR.
#
# Measured 2026-09-09 against the corpus file
#   osf_5a91c46cda91d4000fb0_sample4.sps
# where 14 of 16 PARTIAL CORR and 107 of 142 CORRELATIONS commands use WITH:
#
#   * PARTIAL CORR let the literal keyword survive as a variable name ("WITH"),
#     which the analysis-chunk pre-flight in rmd_generator.R then rejected with
#     "variables not present in the dataset: 'WITH'" -- killing the whole
#     analysis. 14 of the 16 produced no output at all.
#   * extract_variables_clause() DROPPED the "BY"/"WITH"/"ALL" tokens instead of
#     honouring them, so CORRELATIONS received the union of both sides and
#     emitted a full square matrix where SPSS reports only a rectangular block
#     -- coefficients presented as if the researcher had asked for them.
#
# SPSS semantics: `/VARIABLES = A B WITH X Y Z BY ctrl` requests the |x| * |y|
# CROSS pairs of {A,B} x {X,Y,Z}, not every pair of the union.

parse_one <- function(spss_text) {
  tmp <- tempfile(fileext = ".sps")
  on.exit(unlink(tmp))
  writeLines(spss_text, tmp)
  p <- parse_sps(tmp)
  p[[1]]$command_type <- unname(p[[1]]$command_type)
  p[[1]]
}

# ---------------------------------------------------------------- parser ----

test_that("the WITH keyword never survives as a variable name (PARTIAL CORR)", {
  parsed <- parse_one(
    "PARTIAL CORR\n  /VARIABLES=a b c with x y z BY ctrl\n  /SIGNIFICANCE=TWOTAIL.")
  expect_false("WITH" %in% toupper(parsed$variables$all))
  expect_false("BY"   %in% toupper(parsed$variables$all))
})

test_that("PARTIAL CORR splits /VARIABLES into x-set, y-set and controls", {
  parsed <- parse_one(
    "PARTIAL CORR\n  /VARIABLES=a b c with x y z BY ctrl\n  /SIGNIFICANCE=TWOTAIL.")
  v <- parsed$variables
  expect_equal(v$main_vars, c("a", "b", "c"))
  expect_equal(v$with_vars, c("x", "y", "z"))
  expect_equal(v$controls,  "ctrl")
})

test_that("PARTIAL CORR without WITH keeps every non-control var in the x-set", {
  parsed <- parse_one("PARTIAL CORR /VARIABLES=a b c BY ctrl.")
  v <- parsed$variables
  expect_equal(v$main_vars, c("a", "b", "c"))
  expect_length(v$with_vars, 0)
  expect_equal(v$controls, "ctrl")
})

test_that("the WITH keyword never survives as a variable name (CORRELATIONS)", {
  parsed <- parse_one(
    "CORRELATIONS\n  /VARIABLES=a b with x y\n  /PRINT=TWOTAIL NOSIG.")
  expect_false("WITH" %in% toupper(parsed$variables$all))
  expect_equal(parsed$variables$main_vars, c("a", "b"))
  expect_equal(parsed$variables$with_vars, c("x", "y"))
})

test_that("CORRELATIONS without WITH leaves the y-set empty", {
  parsed <- parse_one("CORRELATIONS /VARIABLES=a b c /PRINT=TWOTAIL NOSIG.")
  expect_equal(parsed$variables$main_vars, c("a", "b", "c"))
  expect_length(parsed$variables$with_vars, 0)
})

test_that("lowercase 'with' spanning a newline is honoured (corpus form)", {
  # Verbatim shape of osf_5a91c46cda91d4000fb0_sample4.sps line 2048: the
  # keyword is lowercase and ends the physical line.
  parsed <- parse_one(paste0(
    "PARTIAL CORR\n",
    "  /VARIABLES=pow_isra ach_isra with\n",
    "    meanbenevo meanunivers BY mrat\n",
    "  /SIGNIFICANCE=TWOTAIL."))
  v <- parsed$variables
  expect_equal(v$main_vars, c("pow_isra", "ach_isra"))
  expect_equal(v$with_vars, c("meanbenevo", "meanunivers"))
  expect_equal(v$controls, "mrat")
})

# ------------------------------------------------------------- converter ----

test_that("PARTIAL CORR emits |x| * |y| cross pairs, not C(n,2) of the union", {
  skip_if_not_installed("ppcor")
  parsed <- parse_one("PARTIAL CORR /VARIABLES=a b c with x y z BY ctrl.")
  conv <- convert_partial_corr(parsed, NULL)
  expect_false(grepl("WITH", conv$r_code))

  set.seed(1)
  d <- as.data.frame(matrix(stats::rnorm(70 * 7), ncol = 7))
  names(d) <- c("a", "b", "c", "x", "y", "z", "ctrl")
  out <- eval(parse(text = conv$r_code), envir = list2env(list(data = d)))

  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 9L)               # 3 x 3, NOT choose(6, 2) == 15
  expect_setequal(
    paste(out$Variable, out$With),
    as.vector(outer(c("a", "b", "c"), c("x", "y", "z"), paste)))
  # no x-x or y-y pair ever appears
  expect_true(all(out$Variable %in% c("a", "b", "c")))
  expect_true(all(out$With %in% c("x", "y", "z")))
})

test_that("PARTIAL CORR cross pairs reproduce ppcor::pcor.test exactly", {
  skip_if_not_installed("ppcor")
  parsed <- parse_one("PARTIAL CORR /VARIABLES=a b with x BY ctrl.")
  conv <- convert_partial_corr(parsed, NULL)

  set.seed(7)
  d <- as.data.frame(matrix(stats::rnorm(80 * 4), ncol = 4))
  names(d) <- c("a", "b", "x", "ctrl")
  out <- eval(parse(text = conv$r_code), envir = list2env(list(data = d)))

  ref <- ppcor::pcor.test(d$a, d$x, d[, "ctrl", drop = FALSE])
  cell <- out[out$Variable == "a" & out$With == "x", "Partial r"]
  expect_equal(as.numeric(cell), round(ref$estimate, 3), tolerance = 1e-9)
})

test_that("PARTIAL CORR without WITH still enumerates C(n,2) of the x-set", {
  skip_if_not_installed("ppcor")
  parsed <- parse_one("PARTIAL CORR /VARIABLES=a b c BY ctrl.")
  conv <- convert_partial_corr(parsed, NULL)

  set.seed(3)
  d <- as.data.frame(matrix(stats::rnorm(60 * 4), ncol = 4))
  names(d) <- c("a", "b", "c", "ctrl")
  out <- eval(parse(text = conv$r_code), envir = list2env(list(data = d)))
  expect_equal(nrow(out), 3L)               # choose(3, 2)
})

test_that("CORRELATIONS with WITH emits only the requested block", {
  parsed <- parse_one("CORRELATIONS /VARIABLES=a b with x y z /PRINT=TWOTAIL.")
  conv <- convert_correlations(parsed, NULL)

  # jmv::corrMatrix has no `with` option, so the square-matrix call must be gone
  expect_false(grepl("corrMatrix", conv$r_code, fixed = TRUE))
  # and the emitted variables must be exactly the two sides, keyword-free
  expect_equal(conv$variables, c("a", "b", "x", "y", "z"))
  expect_false("WITH" %in% toupper(conv$variables))

  set.seed(11)
  d <- as.data.frame(matrix(stats::rnorm(50 * 5), ncol = 5))
  names(d) <- c("a", "b", "x", "y", "z")
  out <- eval(parse(text = conv$r_code), envir = list2env(list(data = d)))
  # rows = 2 x-variables x {r, p, N}; columns = Variable, Statistic + 3 y-vars
  expect_equal(dim(out), c(6L, 5L))
  expect_equal(names(out)[3:5], c("x", "y", "z"))
})

test_that("CORRELATIONS without WITH still uses jmv::corrMatrix", {
  parsed <- parse_one("CORRELATIONS /VARIABLES=a b c /PRINT=TWOTAIL.")
  conv <- convert_correlations(parsed, NULL)
  expect_true(grepl("corrMatrix", conv$r_code, fixed = TRUE))
})

# --------------------------------------------------------- numeric truth ----

test_that("the emitted CORRELATIONS block reproduces stats::cor.test exactly", {
  set.seed(42)
  data <- data.frame(a = rnorm(60), b = rnorm(60), x = rnorm(60), y = rnorm(60))
  data$x[1:5] <- NA                          # force pairwise deletion to matter

  parsed <- parse_one("CORRELATIONS /VARIABLES=a b with x y /PRINT=TWOTAIL.")
  conv <- convert_correlations(parsed, NULL)
  out <- eval(parse(text = conv$r_code), envir = list2env(list(data = data)))

  expect_true(is.data.frame(out))
  ct <- stats::cor.test(data$a, data$x)
  cell <- out[out[[1]] == "a" & grepl("Pearson", out[[2]]), "x"]
  expect_equal(as.numeric(cell), unname(round(ct$estimate, 3)), tolerance = 1e-9)
})
