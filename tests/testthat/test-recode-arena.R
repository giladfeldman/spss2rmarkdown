# Regression tests for the RECODE defects surfaced by an external diagnostic
# report (2026-08-04) plus latent defects found while verifying it.
#
# Prior behavior (each verified to FAIL before the fix):
#   * lowercase `thru` entered the THRU branch (case-insensitive grepl) but the
#     case-SENSITIVE strsplit left hi = NA -> "missing value where TRUE/FALSE
#     needed" and the whole RECODE emitted an error comment.
#   * comma/space value lists emitted invalid R: `X == 1,2,3 ~ 1`.
#   * ELSE=COPY emitted `TRUE ~ COPY` -> "object 'COPY' not found" at run time.
#   * RECODE ... INTO without ELSE copied unmatched values from the source;
#     SPSS leaves them system-missing in the target.

parse_one_cmd <- function(syntax) {
  f <- tempfile(fileext = ".sps")
  writeLines(syntax, f)
  on.exit(unlink(f))
  parse_sps(f)[[1]]
}

convert_one <- function(syntax) {
  convert_spss_to_r(parse_one_cmd(syntax), NULL)
}

run_recode <- function(syntax, data) {
  r <- convert_one(syntax)
  expect_false(grepl("Error converting", r$r_code),
               label = paste0("conversion of `", syntax, "` succeeded"))
  eval(parse(text = r$r_code))
  data
}

test_that("lowercase thru converts and reproduces the arena scenario", {
  data <- data.frame(ITEM4 = c(1, 3, 4, 5, NA, 7))
  data <- run_recode("RECODE ITEM4 (4 thru 5=1) (1 thru 3=0) INTO ITEM4R.", data)
  expect_equal(data$ITEM4R, c(0, 0, 1, 1, NA, NA))
})

test_that("lowercase lo/hi range keywords work", {
  data <- data.frame(X = c(-2, 0, 3))
  data <- run_recode("RECODE X (lo thru 0=9) INTO Y.", data)
  expect_equal(data$Y, c(9, 9, NA))

  data2 <- data.frame(X = c(0, 1, 5))
  data2 <- run_recode("RECODE X (1 thru hi=1) INTO Y.", data2)
  expect_equal(data2$Y, c(NA, 1, 1))
})

test_that("comma-separated value lists emit %in% and execute", {
  data <- data.frame(X = c(1, 2, 3, 4, 5, 6))
  data <- run_recode("RECODE X (1,2,3=1) (4,5=0) INTO Y.", data)
  expect_equal(data$Y, c(1, 1, 1, 0, 0, NA))
})

test_that("space-separated value lists execute", {
  data <- data.frame(X = c(1, 2, 3, 9))
  data <- run_recode("RECODE X (1 2 3=1) INTO Y.", data)
  expect_equal(data$Y, c(1, 1, 1, NA))
})

test_that("mixed range + discrete value list executes", {
  data <- data.frame(X = c(1, 2, 3, 4, 5))
  data <- run_recode("RECODE X (1 thru 3, 5=1) INTO Y.", data)
  expect_equal(data$Y, c(1, 1, 1, NA, 1))
})

test_that("ELSE=COPY copies unmatched source values", {
  data <- data.frame(V = c(2, 4, 7, NA))
  data <- run_recode("RECODE V (4 THRU 5=1) (ELSE=COPY) INTO W.", data)
  expect_equal(data$W, c(2, 1, 7, NA))
})

test_that("INTO without ELSE leaves unmatched values system-missing (SPSS)", {
  data <- data.frame(X = c(4, 7))
  data <- run_recode("RECODE X (4 THRU 5=1) INTO Y.", data)
  expect_equal(data$Y, c(1, NA))
})

test_that("in-place RECODE keeps unmatched values unchanged", {
  data <- data.frame(X = c(4, 7))
  data <- run_recode("RECODE X (4 THRU 5=1).", data)
  expect_equal(data$X, c(1, 7))
})

test_that("string recodes still work (in-place, unmatched unchanged)", {
  data <- data.frame(GRP = c("a", "c", "z"), stringsAsFactors = FALSE)
  data <- run_recode("RECODE GRP ('a'='b') ('c'='d').", data)
  expect_equal(data$GRP, c("b", "d", "z"))
})

test_that("SYSMIS input with ELSE executes", {
  data <- data.frame(X = c(NA, 3))
  data <- run_recode("RECODE X (SYSMIS=9) (ELSE=1) INTO M1.", data)
  expect_equal(data$M1, c(9, 1))
})

test_that("quoted string values containing spaces stay one value", {
  data <- data.frame(GRP = c("a b", "a", "b"), stringsAsFactors = FALSE)
  data <- run_recode("RECODE GRP ('a b'='x') (ELSE=COPY).", data)
  expect_equal(data$GRP, c("x", "a", "b"))
})

test_that("quoted string values containing commas stay one value", {
  data <- data.frame(V = c("1,2", "1", "2"), stringsAsFactors = FALSE)
  data <- run_recode("RECODE V ('1,2'='y') INTO W.", data)
  expect_equal(data$W, c("y", NA, NA))
})

test_that("quoted string values containing = parse into old/new correctly", {
  data <- data.frame(V = c("a=b", "c"), stringsAsFactors = FALSE)
  data <- run_recode("RECODE V ('a=b'='x') (ELSE=COPY).", data)
  expect_equal(data$V, c("x", "c"))
})

test_that("quoted list mixes: two quoted values in one rule", {
  data <- data.frame(V = c("a b", "c d", "e"), stringsAsFactors = FALSE)
  data <- run_recode("RECODE V ('a b', 'c d'='x') (ELSE=COPY).", data)
  expect_equal(data$V, c("x", "x", "e"))
})

test_that("THRU inside a quoted literal is not treated as a range", {
  data <- data.frame(V = c("1 thru 3", "z"), stringsAsFactors = FALSE)
  data <- run_recode("RECODE V ('1 thru 3'='x') (ELSE=COPY).", data)
  expect_equal(data$V, c("x", "z"))
})

# --- Codex cross-review findings (2026-08-04), each reproduced before fixing ---

test_that("THRU range plus extra space-separated value in one spec", {
  data <- data.frame(X = c(1, 3, 5, 7))
  data <- run_recode("RECODE X (1 THRU 3 5=9) INTO Y.", data)
  expect_equal(data$Y, c(9, 9, 9, NA))
})

test_that("LO THRU range plus extra value in one spec", {
  data <- data.frame(X = c(-1, 0, 50, 99))
  data <- run_recode("RECODE X (LO THRU 0 99=1) INTO Y.", data)
  expect_equal(data$Y, c(1, 1, NA, 1))
})

test_that("INTO an EXISTING target preserves its unmatched values (SPSS)", {
  data <- data.frame(X = c(4, 7), Y = c(88, 99))
  data <- run_recode("RECODE X (4 THRU 5=1) INTO Y.", data)
  expect_equal(data$Y, c(1, 99))
})

test_that("unparseable RECODE spec fails loudly, never emits invalid R", {
  r <- convert_one("RECODE X (1 thru=1) INTO Y.")
  expect_true(grepl("Error converting", r$r_code))
  expect_false(grepl("case_when", r$r_code))
})

test_that("regression surfaces analysed N and still returns the jmv call", {
  r <- convert_one("REGRESSION /DEPENDENT=Y /METHOD=ENTER X1 X2.")
  expect_true(grepl("N analysed (listwise)", r$r_code, fixed = TRUE))
  expect_true(grepl("complete.cases", r$r_code, fixed = TRUE))
  expect_true(grepl("jmv::linReg", r$r_code, fixed = TRUE))
  # The braces block must RETURN the linReg result for s2r_render_tables():
  # the analysis call must be the last expression in the emitted code.
  # Since C-0008 (2026-09-10) the jmv call is WRAPPED in s2r_spss_reg_tables(),
  # which returns the same results object -- so the value is unchanged and the
  # wrapper's own last argument, `model_comp`, is what now closes the block.
  expect_true(grepl("model_comp = (TRUE|FALSE)\\s*\\)\\s*\\}\\s*$", r$r_code))
  expect_true(grepl("s2r_spss_reg_tables(", r$r_code, fixed = TRUE))
})
