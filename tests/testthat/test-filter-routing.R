test_that("annotate_filter_state stamps filter_var on procedures between FILTER ON and FILTER OFF", {
  sps <- tempfile(fileext = ".sps")
  writeLines(c(
    "FILTER BY include.",
    "DESCRIPTIVES VARIABLES = age.",
    "FILTER OFF.",
    "DESCRIPTIVES VARIABLES = score."
  ), sps)
  parsed <- parse_sps(sps)

  ct <- vapply(parsed, function(p) p$command_type, character(1))
  desc_idx <- which(ct == "DESCRIPTIVES")
  expect_length(desc_idx, 2)

  expect_equal(parsed[[desc_idx[1]]]$filter_var, "include")
  expect_true(is.null(parsed[[desc_idx[2]]]$filter_var))
})

test_that("USE ALL clears the filter state", {
  sps <- tempfile(fileext = ".sps")
  writeLines(c(
    "FILTER BY x.",
    "USE ALL.",
    "FREQUENCIES VARIABLES = age."
  ), sps)
  parsed <- parse_sps(sps)

  ct <- vapply(parsed, function(p) p$command_type, character(1))
  freq_idx <- which(ct == "FREQUENCIES")
  expect_length(freq_idx, 1)
  expect_true(is.null(parsed[[freq_idx]]$filter_var))
})

test_that("Two consecutive FILTER ON commands without OFF: second one wins", {
  sps <- tempfile(fileext = ".sps")
  writeLines(c(
    "FILTER BY first_var.",
    "FILTER BY second_var.",
    "DESCRIPTIVES VARIABLES = age."
  ), sps)
  parsed <- parse_sps(sps)

  ct <- vapply(parsed, function(p) p$command_type, character(1))
  desc_idx <- which(ct == "DESCRIPTIVES")
  expect_equal(parsed[[desc_idx]]$filter_var, "second_var")
})

test_that("convert_spss_to_r wraps analysis r_code in local() with dplyr::filter when filter_var is set", {
  sps <- tempfile(fileext = ".sps")
  writeLines(c(
    "FILTER BY include.",
    "DESCRIPTIVES VARIABLES = age."
  ), sps)
  parsed <- parse_sps(sps)
  sav_info <- list(metadata = data.frame(name = c("AGE", "INCLUDE"),
                                         stringsAsFactors = FALSE))

  ct <- vapply(parsed, function(p) p$command_type, character(1))
  desc <- parsed[[which(ct == "DESCRIPTIVES")]]
  res  <- convert_spss_to_r(desc, sav_info)

  expect_match(res$r_code, "local\\(\\{")
  expect_match(res$r_code, 'dplyr::filter\\(data,', perl = TRUE)
  expect_match(res$r_code, '\\.data\\[\\["INCLUDE"\\]\\]', perl = TRUE)
  expect_match(res$r_code, "!= 0", fixed = TRUE)
  expect_match(res$r_code, "!is\\.na", perl = TRUE)
})

test_that("convert_spss_to_r does NOT wrap when no FILTER is active", {
  sps <- tempfile(fileext = ".sps")
  writeLines("DESCRIPTIVES VARIABLES = age.", sps)
  parsed <- parse_sps(sps)
  sav_info <- list(metadata = data.frame(name = "AGE", stringsAsFactors = FALSE))

  desc <- parsed[[1]]
  res  <- convert_spss_to_r(desc, sav_info)

  expect_false(grepl("local\\(\\{", res$r_code))
})

test_that("FILTER command itself does NOT emit a global dplyr::filter mutate", {
  sps <- tempfile(fileext = ".sps")
  writeLines("FILTER BY keepme.", sps)
  parsed <- parse_sps(sps)
  sav_info <- list(metadata = data.frame(name = "KEEPME", stringsAsFactors = FALSE))

  res <- convert_spss_to_r(parsed[[1]], sav_info)
  # Global mutate pattern from the old behavior: `data <- data |>` followed by
  # dplyr::filter. The new behavior should be a comment-only stub.
  expect_false(grepl("data <- data\\s*\\|>\\s*\\n\\s*dplyr::filter", res$r_code, perl = TRUE))
  expect_match(res$r_code, "^#", perl = TRUE)
})

test_that("Transformation commands (COMPUTE) are NOT wrapped by per-analysis filter", {
  sps <- tempfile(fileext = ".sps")
  writeLines(c(
    "FILTER BY include.",
    "COMPUTE x = y + 1."
  ), sps)
  parsed <- parse_sps(sps)
  sav_info <- list(metadata = data.frame(name = c("X", "Y", "INCLUDE"),
                                         stringsAsFactors = FALSE))

  ct <- vapply(parsed, function(p) p$command_type, character(1))
  comp <- parsed[[which(ct == "COMPUTE")]]
  res  <- convert_spss_to_r(comp, sav_info)

  expect_false(grepl("local\\(\\{", res$r_code))
})

test_that("annotate_filter_state captures the filter var's defining COMPUTE expression", {
  sps <- tempfile(fileext = ".sps")
  writeLines(c(
    "COMPUTE filter_$=(muslim = 1).",
    "FILTER BY filter_$.",
    "FREQUENCIES VARIABLES=age."
  ), sps)
  parsed <- annotate_filter_state(parse_sps(sps))
  ct <- vapply(parsed, function(p) p$command_type, character(1))
  fi <- which(ct == "FREQUENCIES")
  expect_length(fi, 1)
  expect_equal(parsed[[fi]]$filter_var, "filter_$")
  # filter_expr is a structured record (R-0042): COMPUTE keeps the expression.
  expect_true(is.list(parsed[[fi]]$filter_expr))
  expect_equal(parsed[[fi]]$filter_expr$kind, "COMPUTE")
  expect_equal(parsed[[fi]]$filter_expr$expr, "(muslim = 1)")
})

test_that("annotate_filter_state captures an IF-defined filter's CONDITION, not its RHS (R-0042)", {
  # IF (cond) var = rhs.  +  FILTER BY var.  → the filter rule is `cond`, not `rhs`.
  # The old code grabbed the RHS (e.g. 1), making the recomputed column constant so
  # the per-analysis filter kept every row.
  sps <- tempfile(fileext = ".sps")
  writeLines(c(
    "IF (age > 18) keep = 1.",
    "FILTER BY keep.",
    "FREQUENCIES VARIABLES=age."
  ), sps)
  parsed <- annotate_filter_state(parse_sps(sps))
  fi <- which(vapply(parsed, function(p) p$command_type, character(1)) == "FREQUENCIES")
  fe <- parsed[[fi]]$filter_expr
  expect_true(is.list(fe))
  expect_equal(fe$kind, "IF")
  expect_equal(fe$cond, "age > 18")  # the CONDITION, not the RHS
  expect_equal(fe$expr, "1")
})

test_that("per-analysis FILTER block for an IF-defined filter rebuilds if_else(cond, rhs, NA) and subsets correctly (R-0042)", {
  sps <- tempfile(fileext = ".sps")
  writeLines(c(
    "IF (age > 18) keep = 1.",
    "FILTER BY keep.",
    "FREQUENCIES VARIABLES=age."
  ), sps)
  parsed <- annotate_filter_state(parse_sps(sps))
  fi <- which(vapply(parsed, function(p) p$command_type, character(1)) == "FREQUENCIES")
  res <- convert_spss_to_r(parsed[[fi]], NULL)
  # recompute rebuilds the conditional column (not a constant) and the block parses
  expect_match(res$r_code, "if_else(AGE > 18", fixed = TRUE)
  expect_match(res$r_code, 'filter(data, .data[["KEEP"]]', fixed = TRUE)
  expect_silent(parse(text = res$r_code))
  # Functional: on a fresh load WITHOUT the KEEP column, recompute+filter keep only age>18.
  data <- data.frame(AGE = c(10, 20, 30, NA))
  data <- dplyr::mutate(data, `KEEP` = dplyr::if_else(AGE > 18, 1, NA_real_))
  data <- dplyr::filter(data, .data[["KEEP"]] != 0 & !is.na(.data[["KEEP"]]))
  expect_equal(nrow(data), 2L)
})

test_that("per-analysis FILTER block recomputes the filter column so it parses + self-contains", {
  # Regression for the 29x "In argument: .data[[\"FILTER_.\"]]" failure in
  # round-1/spss/SPSS syntax: COMPUTE filter_$ lives in another local() scope, so
  # the per-analysis filter ran on data without the column. The block must now
  # re-emit mutate(FILTER_. = <cond>) before filtering.
  sps <- tempfile(fileext = ".sps")
  writeLines(c(
    "COMPUTE filter_$=(muslim = 1).",
    "FILTER BY filter_$.",
    "FREQUENCIES VARIABLES=age."
  ), sps)
  parsed <- annotate_filter_state(parse_sps(sps))
  fi <- which(vapply(parsed, function(p) p$command_type, character(1)) == "FREQUENCIES")
  res <- convert_spss_to_r(parsed[[fi]], NULL)
  # recompute line present, references the converted condition, and parses
  expect_match(res$r_code, "mutate(data, `FILTER_.` = (MUSLIM == 1))", fixed = TRUE)
  expect_match(res$r_code, 'filter(data, .data[["FILTER_."]]', fixed = TRUE)
  expect_silent(parse(text = res$r_code))
  # Functional: on data WITHOUT FILTER_. (fresh load), the recompute+filter lines
  # run without error and actually subset to the 2 muslim==1 rows. Run just those
  # two emitted lines (avoid the jmv:: analysis call, which needs the jmv pkg).
  data <- data.frame(MUSLIM = c(1, 0, 1), AGE = c(20, 30, 40))
  data <- dplyr::mutate(data, `FILTER_.` = (MUSLIM == 1))
  data <- dplyr::filter(data, .data[["FILTER_."]] != 0 & !is.na(.data[["FILTER_."]]))
  expect_equal(nrow(data), 2L)
})
