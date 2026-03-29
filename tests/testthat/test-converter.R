# test-converter.R
# Tests for SPSS expression conversion and command translation

# =============================================================================
# Expression Conversion
# =============================================================================

test_that("MEAN() converts to rowMeans(cbind())", {
  expect_match(convert_spss_expression("MEAN(a,b,c)"), "rowMeans")
})

test_that("SUM() converts to rowSums(cbind())", {
  expect_match(convert_spss_expression("SUM(a,b)"), "rowSums")
})

test_that("single = converts to == (equality)", {
  expect_match(convert_spss_expression("x=1"), "==")
})

test_that("<> converts to != (not equal)", {
  expect_match(convert_spss_expression("x<>1"), "!=")
})

test_that("AND converts to &", {
  expect_match(convert_spss_expression("x=1 AND y=2"), "&")
})

test_that("OR converts to |", {
  expect_match(convert_spss_expression("x=1 OR y=2"), "\\|")
})

test_that("ABS() is preserved", {
  expect_match(convert_spss_expression("ABS(x)"), "abs\\(")
})

test_that("SQRT() is preserved", {
  expect_match(convert_spss_expression("SQRT(x)"), "sqrt\\(")
})

test_that("Variable names are uppercased", {
  expect_match(convert_spss_expression("myvar + 1"), "MYVAR")
})

test_that("NULL expression returns NA string", {
  expect_equal(convert_spss_expression(NULL), "NA")
})

# =============================================================================
# Metadata Commands are Skipped
# =============================================================================

test_that("Metadata commands are skipped", {
  fake_sav <- list(
    data = data.frame(A = 1:5, B = 6:10),
    metadata = data.frame(name = c("A", "B"), stringsAsFactors = FALSE),
    value_labels = list(),
    n_obs = 5, n_vars = 2
  )

  skip_cmds <- c("VARIABLE LABELS", "VALUE LABELS", "FORMATS", "USE ALL")

  for (skip_cmd in skip_cmds) {
    fake_parsed <- list(
      raw = paste(skip_cmd, "test"),
      command_type = skip_cmd,
      subcommands = list(),
      variables = list(all = character()),
      options = list()
    )
    result <- convert_spss_to_r(fake_parsed, fake_sav)
    expect_true(isTRUE(result$skip), info = paste(skip_cmd, "should be skipped"))
  }
})

# =============================================================================
# RECODE Multi-Variable
# =============================================================================

test_that("RECODE multi-variable generates code for all pairs", {
  fake_sav <- list(
    data = data.frame(A = 1:5, B = 6:10),
    metadata = data.frame(name = c("A", "B"), stringsAsFactors = FALSE),
    value_labels = list(),
    n_obs = 5, n_vars = 2
  )

  fake_recode <- list(
    raw = "RECODE v1 v2 (1=2)(2=1) INTO v1r v2r",
    command_type = "RECODE",
    subcommands = list(),
    variables = list(
      source_vars = c("v1", "v2"),
      target_vars = c("v1r", "v2r"),
      source = "v1", target = "v1r",
      all = c("v1", "v2", "v1r", "v2r")
    ),
    options = list()
  )
  result <- convert_spss_to_r(fake_recode, fake_sav)
  expect_match(result$r_code, "V1R")
  expect_match(result$r_code, "V2R")
})

# =============================================================================
# Hierarchical Regression Blocks
# =============================================================================

test_that("Hierarchical regression preserves multiple blocks", {
  fake_sav <- list(
    data = data.frame(A = 1:5, B = 6:10),
    metadata = data.frame(name = c("A", "B"), stringsAsFactors = FALSE),
    value_labels = list(),
    n_obs = 5, n_vars = 2
  )

  fake_reg <- list(
    raw = "REGRESSION\n/DEPENDENT y\n/METHOD=ENTER x1 x2\n/METHOD=ENTER x3",
    command_type = "REGRESSION",
    subcommands = list(),
    variables = list(
      dependent = "y",
      independent = c("x1", "x2", "x3"),
      method_blocks = list(c("x1", "x2"), c("x3")),
      all = c("y", "x1", "x2", "x3")
    ),
    options = list()
  )
  result <- convert_spss_to_r(fake_reg, fake_sav)
  expect_match(result$r_code, "list\\(list\\(")
  expect_match(result$r_code, '"X3"')
})

# =============================================================================
# EXECUTE generates minimal code
# =============================================================================

test_that("EXECUTE generates comment-only code", {
  fake_sav <- list(
    data = data.frame(A = 1:5),
    metadata = data.frame(name = "A", stringsAsFactors = FALSE),
    value_labels = list(), n_obs = 5, n_vars = 1
  )
  fake_exec <- list(
    raw = "EXECUTE",
    command_type = "EXECUTE",
    subcommands = list(),
    variables = list(all = character()),
    options = list()
  )
  result <- convert_spss_to_r(fake_exec, fake_sav)
  expect_match(result$r_code, "EXECUTE")
  expect_true(result$is_transformation)
})
