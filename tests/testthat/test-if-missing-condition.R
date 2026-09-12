# SPSS `IF` semantics on a MISSING condition: the assignment is NOT made and
# the target keeps its prior value. dplyr::if_else(cond, val, old) instead
# yields NA where cond is NA — destroying values SPSS would have preserved.
#
# Real-world catch (corpus item "Math is cool", 2026-08-04): the .sav declares
# age=-66 as user-missing, haven::read_sav (user_na = FALSE) imports it as NA;
# `IF (age = -66) MissingData=1.` / `IF (age ~= -66) MissingData=0.` then
# turned all 61 shipped MissingData=1 rows into NA, so the downstream
# `T-TEST GROUPS=MissingData(0 1)` failed with "Grouping variable must have
# exactly 2 levels" while SPSS's own ground-truth output ran it with both
# groups. The fix: emit `missing = <target>` so a missing condition leaves the
# case unchanged, matching SPSS.

convert_one_if <- function(spss_text) {
  tmp <- tempfile(fileext = ".sps")
  on.exit(unlink(tmp))
  writeLines(spss_text, tmp)
  cmds <- parse_sps(tmp)
  convert_if(cmds[[1]], sav_info = NULL)
}

test_that("convert_if emits missing= so a missing condition leaves the target unchanged", {
  res <- convert_one_if("IF (age = -66) MissingData=1.\n")
  expect_match(res$r_code, "missing\\s*=", info = "if_else must carry missing= (SPSS: missing condition => no assignment)")
})

test_that("IF with NA condition preserves prior target values end-to-end", {
  res <- convert_one_if("IF (age = -66) MissingData=1.\n")
  data <- data.frame(AGE = c(10, NA, NA, 20), MISSINGDATA = c(0, 1, 1, 0))
  eval(parse(text = res$r_code))
  # SPSS: rows with missing AGE keep their prior MissingData (1), not NA.
  expect_identical(data$MISSINGDATA, c(0, 1, 1, 0))
})

test_that("IF still assigns where the condition is definite", {
  res <- convert_one_if("IF (age ~= -66) MissingData=0.\n")
  data <- data.frame(AGE = c(10, NA, -66), MISSINGDATA = c(9, 9, 9))
  eval(parse(text = res$r_code))
  expect_identical(data$MISSINGDATA, c(0, 9, 9))
})
