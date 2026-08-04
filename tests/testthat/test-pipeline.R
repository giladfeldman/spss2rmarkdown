# test-pipeline.R
# End-to-end tests: parse .sps -> convert -> generate .Rmd

test_that("Full pipeline works on testdata (normality experiment)", {
  # Locate test data files
  sav_file <- system.file("testdata", "Normality-experiment-1-data.sav",
                          package = "spss2rmarkdown")
  sps_file <- system.file("testdata", "Normality-experiment-1-syntax.sps",
                          package = "spss2rmarkdown")

  # Skip if testdata not installed (e.g., during devtools::check without inst/)
  skip_if(sav_file == "", message = "Test data not found")
  skip_if(sps_file == "", message = "Test syntax not found")

  # Step 1: Parse SAV
  sav_result <- parse_sav(sav_file)
  expect_true(!is.null(sav_result))
  expect_gt(sav_result$n_obs, 0)
  expect_gt(sav_result$n_vars, 0)

  # Step 2: Parse SPS
  sps_result <- parse_sps(sps_file)
  expect_true(!is.null(sps_result))
  expect_gt(length(sps_result), 0)

  # All parsed commands should have a command_type
  for (cmd in sps_result) {
    expect_false(is.null(cmd$command_type))
  }

  # Step 3: Convert commands
  converted <- convert_all_commands(sps_result, sav_result)
  expect_true(!is.null(converted))
  expect_gt(length(converted), 0)

  # No conversion should have an error field
  errors <- sapply(converted, function(x) !is.null(x$error))
  expect_false(any(errors), info = "Some commands had conversion errors")

  # Step 4: Generate RMD
  out_dir <- tempfile("spss2r_test_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE))

  base_name <- tools::file_path_sans_ext(basename(sav_file))
  base_name <- gsub("[^a-zA-Z0-9._-]", "_", base_name)

  rmd_result <- generate_rmd(
    sav_data = sav_result,
    sav_path = sav_file,
    parsed_syntax = sps_result,
    converted_code = converted,
    output_dir = out_dir,
    options = list(include_original = TRUE),
    base_name = base_name
  )

  expect_true(!is.null(rmd_result))
  expect_true(file.exists(rmd_result$rmd_path))

  # Step 5: Check generated RMD content
  rmd_content <- readLines(rmd_result$rmd_path, warn = FALSE)
  rmd_text <- paste(rmd_content, collapse = "\n")

  # The data-leak guards below scan only the document's *renderable* content.
  # generate_rmd() injects a hidden `s2r-helpers` setup chunk (include=FALSE,
  # so knitr never emits it or its output into the rendered HTML) whose body is
  # the table-rendering helpers deparse()'d verbatim. That source legitimately
  # contains the literal token "character(0)" (`... else character(0)`), which
  # is intentional R code, not a leaked empty vector. Strip that hidden chunk
  # before scanning so the guards still catch a genuine character(0)/NULL leak
  # anywhere a value is actually printed into the report.
  helper_start <- grep("^```\\{r s2r-helpers[,}]", rmd_content)
  if (length(helper_start) == 1L) {
    fences <- grep("^```", rmd_content)
    helper_end <- fences[fences > helper_start][1]
    scan_content <- rmd_content[-(helper_start:helper_end)]
  } else {
    scan_content <- rmd_content
  }
  scan_text <- paste(scan_content, collapse = "\n")

  # Should not contain character(0) or NULL leaks
  expect_false(grepl("character\\(0\\)", scan_text),
               info = "RMD contains character(0)")
  expect_false(any(grepl("^NULL$", scan_content)),
               info = "RMD contains bare NULL")

  # Should have YAML header
  expect_match(rmd_text, "^---")

  # Should have at least one analysis or transformation section
  has_content <- grepl("## Analysis \\d+:", rmd_text) ||
                 grepl("## Transformation \\d+:", rmd_text)
  expect_true(has_content, info = "RMD should have analysis or transformation sections")
})

test_that("parse_sav generates data load code", {
  sav_file <- system.file("testdata", "Normality-experiment-1-data.sav",
                          package = "spss2rmarkdown")
  skip_if(sav_file == "", message = "Test data not found")

  sav_result <- parse_sav(sav_file)
  code <- generate_data_load_code(sav_result, "mydata.sav")

  expect_match(code, "read_sav")
  expect_match(code, "mydata.sav")
  expect_match(code, as.character(sav_result$n_vars))
})
