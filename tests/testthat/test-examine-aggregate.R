# Regression tests for EXAMINE variable extraction and AGGREGATE conversion.
#
# Both were surfaced by round-3-spss item 2019F_SPSS4UROP:
#   * EXAMINE had NO branch in extract_variables(), so `variables$all` stayed
#     empty -> convert_examine() emitted "# EXAMINE: No variables specified"
#     -> an invalid `.res <- # EXAMINE: ...` assignment -> an
#     "object '.res' not found" cascade over every downstream chunk.
#   * AGGREGATE (MODE = ADDVARIABLES) was emitted as an "Unsupported command",
#     so the aggregate columns were never created and a downstream
#     `COMPUTE x_meancentered = x - x_mean` produced all-NA, making jmv fail
#     with "Argument 'covs' contains" for the mean-centered predictor.

test_that("EXAMINE VARIABLES = dv1 dv2 BY factor extracts dvs + factor", {
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  p <- psc(paste0("EXAMINE VARIABLES = READ WRITE MATH SCIENCE SOCST BY gender\n",
                  "   /PLOT = BOXPLOT\n   /STATISTICS = DESCRIPTIVES\n   /NOTOTAL."))
  expect_equal(unname(p$command_type), "EXAMINE")
  expect_equal(p$variables$dependent, c("READ", "WRITE", "MATH", "SCIENCE", "SOCST"))
  expect_equal(p$variables$factors, "gender")
  expect_true(all(c("READ", "gender") %in% p$variables$all))
})

test_that("convert_examine maps DVs to vars and BY factor to splitBy (no empty stub)", {
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  ce  <- getFromNamespace("convert_examine", "spss2rmarkdown")
  p <- psc("EXAMINE VARIABLES = READ MATH BY gender /STATISTICS = DESCRIPTIVES.")
  out <- ce(p, NULL)$r_code
  expect_false(grepl("No variables specified", out, fixed = TRUE))
  expect_true(grepl("jmv::descriptives", out, fixed = TRUE))
  expect_true(grepl("vars = c('READ', 'MATH')", out, fixed = TRUE))
  expect_true(grepl("splitBy = c('gender')", out, fixed = TRUE))
})

test_that("EXAMINE with no BY still yields a valid descriptives call", {
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  ce  <- getFromNamespace("convert_examine", "spss2rmarkdown")
  p <- psc("EXAMINE VARIABLES = READ WRITE /PLOT = NONE.")
  out <- ce(p, NULL)$r_code
  expect_true(grepl("jmv::descriptives", out, fixed = TRUE))
  expect_false(grepl("splitBy", out, fixed = TRUE))
  expect_false(grepl("No variables specified", out, fixed = TRUE))
})

test_that("AGGREGATE whole-sample MODE=ADDVARIABLES becomes ungrouped mutate", {
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  ca  <- getFromNamespace("convert_aggregate", "spss2rmarkdown")
  p <- psc(paste0("AGGREGATE\n  /OUTFILE = * MODE = ADDVARIABLES\n  /BREAK =\n",
                  "  /read_mean = MEAN(read)\n  /math_mean = MEAN(math)."))
  out <- ca(p, NULL)
  expect_false(isTRUE(out$unsupported))
  expect_true(grepl("dplyr::mutate", out$r_code, fixed = TRUE))
  expect_true(grepl("`READ_MEAN` = mean(READ, na.rm = TRUE)", out$r_code, fixed = TRUE))
  expect_true(grepl("`MATH_MEAN` = mean(MATH, na.rm = TRUE)", out$r_code, fixed = TRUE))
  expect_false(grepl("dplyr::group_by", out$r_code, fixed = TRUE))
})

test_that("AGGREGATE with a BREAK variable groups before mutating (no name truncation)", {
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  ca  <- getFromNamespace("convert_aggregate", "spss2rmarkdown")
  # 'gender' contains an 'n' — the old [^/\\n] class (literal backslash+n) would
  # truncate it to 'ge'; the perl \n fix keeps the whole name.
  p <- psc("AGGREGATE /OUTFILE=* MODE=ADDVARIABLES /BREAK=gender /grpmean = MEAN(read).")
  out <- ca(p, NULL)$r_code
  expect_true(grepl("dplyr::group_by(`GENDER`)", out, fixed = TRUE))
  expect_true(grepl("dplyr::ungroup()", out, fixed = TRUE))
  expect_false(grepl("group_by(`GE`)", out, fixed = TRUE))
})

test_that("AGGREGATE maps SUM/SD/MIN/MAX/MEDIAN/N functions", {
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  ca  <- getFromNamespace("convert_aggregate", "spss2rmarkdown")
  p <- psc(paste0("AGGREGATE /OUTFILE=* MODE=ADDVARIABLES /BREAK=\n",
                  "  /s = SUM(x) /d = SD(x) /lo = MIN(x) /hi = MAX(x) /md = MEDIAN(x) /n = N(x)."))
  out <- ca(p, NULL)$r_code
  expect_true(grepl("sum(X, na.rm = TRUE)", out, fixed = TRUE))
  expect_true(grepl("sd(X, na.rm = TRUE)", out, fixed = TRUE))
  expect_true(grepl("min(X, na.rm = TRUE)", out, fixed = TRUE))
  expect_true(grepl("max(X, na.rm = TRUE)", out, fixed = TRUE))
  expect_true(grepl("median(X, na.rm = TRUE)", out, fixed = TRUE))
  expect_true(grepl("sum(!is.na(X))", out, fixed = TRUE))
})

test_that("AGGREGATE writing to a real OUTFILE (not active dataset) passes through", {
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  ca  <- getFromNamespace("convert_aggregate", "spss2rmarkdown")
  p <- psc("AGGREGATE /OUTFILE='agg.sav' /BREAK=grp /gm = MEAN(x).")
  out <- ca(p, NULL)
  expect_true(isTRUE(out$unsupported))
})

# --- ONEWAY dv BY factor extraction (regression: 2019F_SPSS4UROP) ---
# `\\w`/`\\s` are NOT character classes inside an R (TRE) bracket expression, so
# the old "([\\w\\s,]+)\\s+BY\\s+(\\w+)" never matched a real DV name -> empty
# var list -> broken ".res <- # ONEWAY: Missing DV or factor" stub and cascade.
test_that("ONEWAY dv BY factor with multi-line /CONTRAST extracts dv + factor", {
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  p <- psc(paste0("ONEWAY READ BY program\n   /CONTRAST = -1, 1, 0\n",
                  "   /CONTRAST = -1, 0, 1\n   /CONTRAST = -1, 2, 1."))
  expect_equal(p$variables$dependent, "READ")
  expect_equal(p$variables$factor, "program")
  expect_equal(p$variables$all, c("READ", "program"))
})

test_that("ONEWAY factor name with an underscore is not truncated", {
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  p <- psc("ONEWAY SOCST BY ses_program\n   /CONTRAST = -1, 0, 1, -1, 0, 1, -1, 0, 1.")
  expect_equal(p$variables$dependent, "SOCST")
  expect_equal(p$variables$factor, "ses_program")
})

test_that("convert_oneway does not pass the unsupported effectSize arg to anovaOneW", {
  psc <- getFromNamespace("parse_single_command", "spss2rmarkdown")
  co  <- getFromNamespace("convert_oneway", "spss2rmarkdown")
  p <- psc("ONEWAY READ BY program /CONTRAST = -1, 1, 0.")
  out <- co(p, NULL)$r_code
  expect_true(grepl("jmv::anovaOneW", out, fixed = TRUE))
  # jmv::anovaOneW has no effectSize argument; passing it throws at render.
  expect_false(grepl("effectSize", out, fixed = TRUE))
  expect_false("effectSize" %in% names(formals(jmv::anovaOneW)))
})
