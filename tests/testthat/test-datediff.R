test_that("DATEDIFF in months -> lubridate::time_length with unit = months", {
  out <- convert_spss_expression("DATEDIFF(end_var, start_var, 'months')")
  expect_match(out, "lubridate::interval", fixed = TRUE)
  expect_match(out, "lubridate::time_length", fixed = TRUE)
  # Argument order is swapped: SPSS DATEDIFF(end, start) -> interval(start, end)
  expect_match(out, "interval\\(\\s*START_VAR\\s*,\\s*END_VAR\\s*\\)")
  expect_match(out, "unit = \"months\"", fixed = TRUE)
  expect_match(out, "as.numeric(", fixed = TRUE)
})

test_that("DATEDIFF in days -> lubridate::time_length with unit = days", {
  out <- convert_spss_expression("DATEDIFF(t2, t1, 'days')")
  expect_match(out, "interval\\(\\s*T1\\s*,\\s*T2\\s*\\)")
  expect_match(out, "unit = \"days\"", fixed = TRUE)
})

test_that("DATEDIFF in years -> lubridate::time_length with unit = years", {
  out <- convert_spss_expression("DATEDIFF(end_date, start_date, 'years')")
  expect_match(out, "interval\\(\\s*START_DATE\\s*,\\s*END_DATE\\s*\\)")
  expect_match(out, "unit = \"years\"", fixed = TRUE)
})

test_that("DATEDIFF works inside a COMPUTE-style expression with extra arithmetic", {
  out <- convert_spss_expression("DATEDIFF(d2, d1, 'days') / 30")
  expect_match(out, "lubridate::time_length", fixed = TRUE)
  expect_match(out, "/ 30", fixed = TRUE)
})

test_that("Two-argument DATEDIFF defaults to days", {
  out <- convert_spss_expression("DATEDIFF(end_var, start_var)")
  expect_match(out, "lubridate::interval", fixed = TRUE)
  expect_match(out, "unit = \"days\"", fixed = TRUE)
})
