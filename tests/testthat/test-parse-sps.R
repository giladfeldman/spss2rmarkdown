# test-parse-sps.R
# Tests for SPSS command recognition and variable extraction

# Helper to parse a single command from a string
parse_one <- function(spss_text) {
  tmp <- tempfile(fileext = ".sps")
  on.exit(unlink(tmp))
  writeLines(spss_text, tmp)
  result <- parse_sps(tmp)
  if (length(result) > 0) {
    # Strip names from command_type (parser uses regex patterns as names)
    result[[1]]$command_type <- unname(result[[1]]$command_type)
    result[[1]]
  } else NULL
}

# =============================================================================
# Command Recognition
# =============================================================================

test_that("DESCRIPTIVES is recognized", {
  cmd <- parse_one("DESCRIPTIVES VARIABLES=age income.\n")
  expect_equal(cmd$command_type, "DESCRIPTIVES")
})

test_that("FREQUENCIES is recognized", {
  cmd <- parse_one("FREQUENCIES VARIABLES=gender.\n")
  expect_equal(cmd$command_type, "FREQUENCIES")
})

test_that("FREQ abbreviation maps to FREQUENCIES", {
  cmd <- parse_one("freq gender.\n")
  expect_equal(cmd$command_type, "FREQUENCIES")
})

test_that("CORRELATIONS is recognized", {
  cmd <- parse_one("CORRELATIONS\n/VARIABLES=x y z.\n")
  expect_equal(cmd$command_type, "CORRELATIONS")
})

test_that("T-TEST is recognized", {
  cmd <- parse_one("T-TEST GROUPS=gender\n/VARIABLES=score.\n")
  expect_equal(cmd$command_type, "T-TEST")
})

test_that("ONEWAY is recognized", {
  cmd <- parse_one("ONEWAY dv BY group.\n")
  expect_equal(cmd$command_type, "ONEWAY")
})

test_that("REGRESSION is recognized", {
  cmd <- parse_one("REGRESSION\n/DEPENDENT y\n/METHOD=ENTER x1 x2.\n")
  expect_equal(cmd$command_type, "REGRESSION")
})

test_that("LOGISTIC REGRESSION is recognized", {
  cmd <- parse_one("LOGISTIC REGRESSION dv\n/METHOD=ENTER iv.\n")
  expect_equal(cmd$command_type, "LOGISTIC REGRESSION")
})

test_that("GLM is recognized", {
  cmd <- parse_one("GLM dv BY factor1 factor2.\n")
  expect_equal(cmd$command_type, "GLM")
})

test_that("UNIANOVA is recognized", {
  cmd <- parse_one("UNIANOVA dv BY factor1 factor2.\n")
  expect_equal(cmd$command_type, "UNIANOVA")
})

test_that("MANOVA is recognized", {
  cmd <- parse_one("MANOVA dv1 dv2 BY group.\n")
  expect_equal(cmd$command_type, "MANOVA")
})

test_that("CROSSTABS is recognized", {
  cmd <- parse_one("CROSSTABS var1 BY var2.\n")
  expect_equal(cmd$command_type, "CROSSTABS")
})

test_that("RELIABILITY is recognized", {
  cmd <- parse_one("RELIABILITY\n/VARIABLES=item1 item2 item3.\n")
  expect_equal(cmd$command_type, "RELIABILITY")
})

test_that("FACTOR is recognized", {
  cmd <- parse_one("FACTOR\n/VARIABLES=v1 v2 v3.\n")
  expect_equal(cmd$command_type, "FACTOR")
})

test_that("EXAMINE is recognized", {
  cmd <- parse_one("EXAMINE VARIABLES=score.\n")
  expect_equal(cmd$command_type, "EXAMINE")
})

test_that("COMPUTE is recognized", {
  cmd <- parse_one("COMPUTE newvar=oldvar*2.\n")
  expect_equal(cmd$command_type, "COMPUTE")
})

test_that("COUNT is recognized", {
  cmd <- parse_one("COUNT cnt=v1 v2 v3 (1).\n")
  expect_equal(cmd$command_type, "COUNT")
})

test_that("RECODE is recognized", {
  cmd <- parse_one("RECODE var1 (1=2)(2=1) INTO var1r.\n")
  expect_equal(cmd$command_type, "RECODE")
})

test_that("IF is recognized", {
  cmd <- parse_one("IF (group=1) score=score+10.\n")
  expect_equal(cmd$command_type, "IF")
})

test_that("SELECT IF is recognized", {
  cmd <- parse_one("SELECT IF (age >= 18).\n")
  expect_equal(cmd$command_type, "SELECT IF")
})

test_that("FILTER BY is recognized", {
  cmd <- parse_one("FILTER BY filter_var.\n")
  expect_equal(cmd$command_type, "FILTER")
})

test_that("FILTER OFF is recognized", {
  cmd <- parse_one("FILTER OFF.\n")
  expect_equal(cmd$command_type, "FILTER")
})

test_that("SORT CASES is recognized", {
  cmd <- parse_one("SORT CASES BY id.\n")
  expect_equal(cmd$command_type, "SORT CASES")
})

test_that("DELETE VARIABLES is recognized", {
  cmd <- parse_one("DELETE VARIABLES temp1 temp2.\n")
  expect_equal(cmd$command_type, "DELETE VARIABLES")
})

test_that("SPLIT FILE LAYERED BY is recognized", {
  cmd <- parse_one("SPLIT FILE LAYERED BY group.\n")
  expect_equal(cmd$command_type, "SPLIT FILE")
})

test_that("SPLIT FILE OFF is recognized", {
  cmd <- parse_one("SPLIT FILE OFF.\n")
  expect_equal(cmd$command_type, "SPLIT FILE")
})

test_that("EXECUTE is recognized", {
  cmd <- parse_one("EXECUTE.\n")
  expect_equal(cmd$command_type, "EXECUTE")
})

test_that("EXEC abbreviation maps to EXECUTE", {
  cmd <- parse_one("exec.\n")
  expect_equal(cmd$command_type, "EXECUTE")
})

# --- 3-letter command abbreviations (SPSS accepts these everywhere) ----------
# The OSF corpus uses DES/FRE/EXE/SEL IF/VAL LAB/VAR LAB heavily; without
# these the procedures fell through to UNSUPPORTED and produced no output.

test_that("DES abbreviation maps to DESCRIPTIVES", {
  cmd <- parse_one("DES TV_exposure total_TV gap.\n")
  expect_equal(cmd$command_type, "DESCRIPTIVES")
  expect_equal(cmd$variables$all, c("TV_exposure", "total_TV", "gap"))
})

test_that("FRE abbreviation maps to FREQUENCIES", {
  cmd <- parse_one("FRE a b c.\n")
  expect_equal(cmd$command_type, "FREQUENCIES")
  # trailing "." must not cling to the last variable
  expect_equal(cmd$variables$all, c("a", "b", "c"))
})

test_that("EXE abbreviation maps to EXECUTE", {
  cmd <- parse_one("exe.\n")
  expect_equal(cmd$command_type, "EXECUTE")
})

test_that("REL abbreviation maps to RELIABILITY", {
  cmd <- parse_one("REL /VARIABLES=a b c /MODEL=ALPHA.\n")
  expect_equal(cmd$command_type, "RELIABILITY")
})

test_that("EXA abbreviation maps to EXAMINE", {
  cmd <- parse_one("EXA VARIABLES=x /PLOT BOXPLOT.\n")
  expect_equal(cmd$command_type, "EXAMINE")
})

test_that("SEL IF abbreviation maps to SELECT IF", {
  cmd <- parse_one("sel if mv_outlier=0.\n")
  expect_equal(cmd$command_type, "SELECT IF")
})

test_that("VAR LAB abbreviation maps to VARIABLE LABELS", {
  cmd <- parse_one("VAR LAB Morning 'Morning total'.\n")
  expect_equal(cmd$command_type, "VARIABLE LABELS")
})

test_that("VAL LAB abbreviation maps to VALUE LABELS", {
  cmd <- parse_one("VAL LAB Morning 1 'a' 2 'b'.\n")
  expect_equal(cmd$command_type, "VALUE LABELS")
})

test_that("full command names are unaffected by abbreviation aliases", {
  # Guard: the short aliases must not shadow longer commands sharing a prefix.
  expect_equal(parse_one("DATASET NAME d3 WINDOW=FRONT.\n")$command_type, "DATASET")
  expect_equal(parse_one("EXECUTE.\n")$command_type, "EXECUTE")
  expect_equal(parse_one("DELETE VARIABLES x y.\n")$command_type, "DELETE VARIABLES")
  expect_equal(parse_one("VARSTOCASES /MAKE x FROM a b.\n")$command_type, "VARSTOCASES")
})

test_that("VARIABLE LABELS is recognized", {
  cmd <- parse_one("VARIABLE LABELS var1 'My Variable'.\n")
  expect_equal(cmd$command_type, "VARIABLE LABELS")
})

test_that("VALUE LABELS is recognized", {
  cmd <- parse_one("VALUE LABELS gender 1 'Male' 2 'Female'.\n")
  expect_equal(cmd$command_type, "VALUE LABELS")
})

test_that("FORMATS is recognized", {
  cmd <- parse_one("FORMATS var1 (F8.2).\n")
  expect_equal(cmd$command_type, "FORMATS")
})

test_that("USE ALL is recognized", {
  cmd <- parse_one("USE ALL.\n")
  expect_equal(cmd$command_type, "USE ALL")
})

test_that("PARTIAL CORR is recognized", {
  cmd <- parse_one("PARTIAL CORR\n/VARIABLES=x y BY z.\n")
  expect_equal(cmd$command_type, "PARTIAL CORR")
})

test_that("MEANS is recognized", {
  cmd <- parse_one("MEANS TABLES=dv BY group.\n")
  expect_equal(cmd$command_type, "MEANS")
})

test_that("NPAR TESTS is recognized", {
  cmd <- parse_one("NPAR TESTS\n/MANN-WHITNEY=score BY group.\n")
  expect_equal(cmd$command_type, "NPAR TESTS")
})

# =============================================================================
# Variable Extraction
# =============================================================================

test_that("RECODE extracts all source and target vars", {
  cmd <- parse_one("RECODE v1 v2 v3 (1=2)(2=1) INTO v1r v2r v3r.\n")
  expect_equal(length(cmd$variables$source_vars), 3)
  expect_equal(length(cmd$variables$target_vars), 3)
})

test_that("REGRESSION captures multiple METHOD blocks", {
  cmd <- parse_one("REGRESSION\n/DEPENDENT y\n/METHOD=ENTER x1 x2\n/METHOD=ENTER x3.\n")
  expect_equal(length(cmd$variables$method_blocks), 2)
  expect_equal(length(cmd$variables$method_blocks[[1]]), 2)
})

test_that("COUNT extracts target and count_value", {
  cmd <- parse_one("COUNT cnt = v1 v2 v3 (1).\n")
  expect_equal(cmd$variables$target, "cnt")
  expect_equal(cmd$variables$count_value, "1")
})

test_that("IF extracts condition, target, expression", {
  cmd <- parse_one("IF (group=1) score=score+10.\n")
  expect_false(is.null(cmd$variables$condition))
  expect_equal(cmd$variables$target, "score")
})

test_that("FILTER OFF sets filter_off flag", {
  cmd <- parse_one("FILTER OFF.\n")
  expect_true(isTRUE(cmd$variables$filter_off))
})

test_that("FILTER BY extracts filter variable", {
  cmd <- parse_one("FILTER BY myfilter.\n")
  expect_equal(cmd$variables$filter_var, "myfilter")
})

test_that("SPLIT FILE OFF sets split_off flag", {
  cmd <- parse_one("SPLIT FILE OFF.\n")
  expect_true(isTRUE(cmd$variables$split_off))
})

test_that("Comments are removed during parsing", {
  cmd <- parse_one("* This is a comment.\nDESCRIPTIVES VARIABLES=x.\n")
  expect_equal(cmd$command_type, "DESCRIPTIVES")
})
