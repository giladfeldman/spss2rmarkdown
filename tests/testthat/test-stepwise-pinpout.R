# Tests for SPSS REGRESSION /METHOD=STEPWISE|FORWARD|BACKWARD with /CRITERIA
# PIN/POUT thresholds. SPSS uses F-test entry/removal at PIN/POUT (defaults
# .05/.10); we emit olsrr::ols_step_*_p which honors p-value thresholds.

library(testthat)

fake_sav <- list(
  data = data.frame(y = 1:10, x1 = 1:10, x2 = 1:10, x3 = 1:10),
  metadata = data.frame(name = c("y", "x1", "x2", "x3"), stringsAsFactors = FALSE),
  value_labels = list(),
  n_obs = 10, n_vars = 4
)

make_reg <- function(raw, method) {
  list(
    raw = raw,
    command_type = "REGRESSION",
    subcommands = list(),
    variables = list(
      dependent = "y",
      independent = c("x1", "x2", "x3"),
      method_blocks = list(c("x1", "x2", "x3")),
      all = c("y", "x1", "x2", "x3")
    ),
    options = list(method = method)
  )
}

# -------------------- extract_stepwise_pin_pout helper --------------------

test_that("extract_stepwise_pin_pout returns SPSS defaults when /CRITERIA absent", {
  out <- extract_stepwise_pin_pout("REGRESSION /DEPENDENT y /METHOD=STEPWISE x1 x2 x3.")
  expect_equal(out$pin, 0.05)
  expect_equal(out$pout, 0.10)
})

test_that("extract_stepwise_pin_pout parses dotless SPSS form (.10 / .20)", {
  out <- extract_stepwise_pin_pout(
    "REGRESSION /CRITERIA = PIN(.10) POUT(.20) /DEPENDENT y /METHOD=STEPWISE x1 x2."
  )
  expect_equal(out$pin, 0.10)
  expect_equal(out$pout, 0.20)
})

test_that("extract_stepwise_pin_pout parses dotted form (0.025 / 0.05)", {
  out <- extract_stepwise_pin_pout(
    "REGRESSION /CRITERIA = PIN(0.025) POUT(0.05) /DEPENDENT y /METHOD=STEPWISE x1."
  )
  expect_equal(out$pin, 0.025)
  expect_equal(out$pout, 0.05)
})

test_that("extract_stepwise_pin_pout handles missing input gracefully", {
  out <- extract_stepwise_pin_pout("")
  expect_equal(out$pin, 0.05)
  expect_equal(out$pout, 0.10)
  out2 <- extract_stepwise_pin_pout(NULL)
  expect_equal(out2$pin, 0.05)
  expect_equal(out2$pout, 0.10)
})

# -------------------- /METHOD=STEPWISE -> ols_step_both_p --------------------

test_that("/METHOD=STEPWISE emits olsrr::ols_step_both_p with default thresholds", {
  parsed <- make_reg(
    "REGRESSION /DEPENDENT y /METHOD=STEPWISE x1 x2 x3.",
    "STEPWISE"
  )
  result <- convert_spss_to_r(parsed, fake_sav)
  expect_match(result$r_code, "olsrr::ols_step_both_p", fixed = TRUE)
  expect_match(result$r_code, "p_enter\\s*=\\s*0\\.05")
  expect_match(result$r_code, "p_remove\\s*=\\s*0\\.10")
  expect_true("olsrr" %in% result$packages)
  expect_false(grepl("MASS::stepAIC", result$r_code, fixed = TRUE))
})

test_that("/METHOD=STEPWISE honors /CRITERIA = PIN(.10) POUT(.20)", {
  parsed <- make_reg(
    "REGRESSION /CRITERIA = PIN(.10) POUT(.20) /DEPENDENT y /METHOD=STEPWISE x1 x2 x3.",
    "STEPWISE"
  )
  result <- convert_spss_to_r(parsed, fake_sav)
  expect_match(result$r_code, "olsrr::ols_step_both_p", fixed = TRUE)
  expect_match(result$r_code, "p_enter\\s*=\\s*0\\.10")
  expect_match(result$r_code, "p_remove\\s*=\\s*0\\.20")
})

test_that("/METHOD=STEPWISE honors /CRITERIA = PIN(0.025) POUT(0.05)", {
  parsed <- make_reg(
    "REGRESSION /CRITERIA = PIN(0.025) POUT(0.05) /DEPENDENT y /METHOD=STEPWISE x1.",
    "STEPWISE"
  )
  result <- convert_spss_to_r(parsed, fake_sav)
  expect_match(result$r_code, "p_enter\\s*=\\s*0\\.025")
  expect_match(result$r_code, "p_remove\\s*=\\s*0\\.05")
})

# -------------------- /METHOD=FORWARD -> ols_step_forward_p --------------------

test_that("/METHOD=FORWARD emits olsrr::ols_step_forward_p with PIN", {
  parsed <- make_reg(
    "REGRESSION /DEPENDENT y /METHOD=FORWARD x1 x2 x3.",
    "FORWARD"
  )
  result <- convert_spss_to_r(parsed, fake_sav)
  expect_match(result$r_code, "olsrr::ols_step_forward_p", fixed = TRUE)
  expect_match(result$r_code, "p_val\\s*=\\s*0\\.05")
  expect_true("olsrr" %in% result$packages)
})

test_that("/METHOD=FORWARD honors /CRITERIA PIN", {
  parsed <- make_reg(
    "REGRESSION /CRITERIA = PIN(.10) POUT(.20) /DEPENDENT y /METHOD=FORWARD x1 x2.",
    "FORWARD"
  )
  result <- convert_spss_to_r(parsed, fake_sav)
  expect_match(result$r_code, "olsrr::ols_step_forward_p", fixed = TRUE)
  expect_match(result$r_code, "p_val\\s*=\\s*0\\.10")
})

# -------------------- /METHOD=BACKWARD -> ols_step_backward_p --------------------

test_that("/METHOD=BACKWARD emits olsrr::ols_step_backward_p with POUT", {
  parsed <- make_reg(
    "REGRESSION /DEPENDENT y /METHOD=BACKWARD x1 x2 x3.",
    "BACKWARD"
  )
  result <- convert_spss_to_r(parsed, fake_sav)
  expect_match(result$r_code, "olsrr::ols_step_backward_p", fixed = TRUE)
  expect_match(result$r_code, "p_val\\s*=\\s*0\\.10")
  expect_true("olsrr" %in% result$packages)
})

test_that("/METHOD=BACKWARD honors /CRITERIA POUT", {
  parsed <- make_reg(
    "REGRESSION /CRITERIA = PIN(.10) POUT(.20) /DEPENDENT y /METHOD=BACKWARD x1 x2.",
    "BACKWARD"
  )
  result <- convert_spss_to_r(parsed, fake_sav)
  expect_match(result$r_code, "olsrr::ols_step_backward_p", fixed = TRUE)
  expect_match(result$r_code, "p_val\\s*=\\s*0\\.20")
})

# -------------------- generated code shape --------------------

test_that("generated stepwise code passes a real lm() fit to olsrr", {
  parsed <- make_reg(
    "REGRESSION /DEPENDENT y /METHOD=STEPWISE x1 x2 x3.",
    "STEPWISE"
  )
  result <- convert_spss_to_r(parsed, fake_sav)
  # An lm() is fit to .full and then passed into the olsrr step function.
  # Variable names get uppercased through the SPSS pipeline (Y, X1, X2, X3).
  expect_match(result$r_code, "stats::lm\\([Yy]\\s*~", perl = TRUE)
  expect_match(result$r_code, ".full", fixed = TRUE)
  expect_match(result$r_code, "olsrr::ols_step_both_p\\(\\.full", perl = TRUE)
})
