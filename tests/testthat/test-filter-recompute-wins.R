# A FILTER variable that the syntax COMPUTEs must be RECOMPUTED, never taken
# from a same-named column stored in the .sav.
#
# Measured 2026-09-09 against the frozen SPSS gold for
# osf_5a91c46cda91d4000fb0_sample4 (round-2-spss):
#
#   total rows in osf_wdnpx_sample4.sav ......................... 212
#   kept by the STORED ADIFILT column ........................... 178
#   kept by the syntax's own COMPUTE (c6 <= 25 and
#     r_meandomarab > 3.37) ..................................... 97
#   SPSS "N of Rows in Working Data File" (gold line 11729) ..... 97
#
# The emitted wrapper used to carry a
#   if (!"ADIFILT" %in% names(data)) data <- dplyr::mutate(...)
# guard, i.e. "recompute ONLY when no stored column exists". That is backwards
# for the ordinary SPSS idiom, in which the syntax COMPUTEs the filter variable
# and redefines it repeatedly before each FILTER BY. On the PARTIAL CORR at gold
# lines 11731-11790 it moved MEANMERGER x MEANBENEVO from
#   r = .125, p = .225, df = 94   (SPSS)   to   r = .153, p = .042, df = 174,
# i.e. across p = .05, with nothing erroring and no test going red.
#
# These tests are the acceptance criterion for that fix. The unit test below was
# watched RED against the guarded emission before the fix landed.

test_that("an emitted FILTER wrapper recomputes a syntax-computed filter var unconditionally", {
  sps <- withr::local_tempfile(fileext = ".sps")
  writeLines(c(
    "COMPUTE keep = 0.",
    "COMPUTE keep = (age <= 25).",
    "FILTER BY keep.",
    "DESCRIPTIVES VARIABLES=score.",
    "USE ALL."
  ), sps)

  cmds <- parse_sps(sps)
  k <- which(vapply(cmds, function(c)
    identical(unname(c$command_type), "DESCRIPTIVES"), logical(1)))
  expect_length(k, 1L)

  sav_info <- list(data = data.frame(AGE = numeric(), SCORE = numeric()),
                   variables = c("AGE", "SCORE", "KEEP"))
  conv <- convert_spss_to_r(cmds[[k]], sav_info)

  # The mutate() that rebuilds the filter column must be a plain, ungated
  # statement -- never the body of an `if (!<fv> %in% names(data))`, which is
  # the exact shape the defect had.
  expect_false(grepl('if (!"KEEP" %in% names(data)) data <- dplyr::mutate',
                     conv$r_code, fixed = TRUE))
  expect_true(grepl("dplyr::mutate(data, `KEEP` = (AGE <= 25))",
                    conv$r_code, fixed = TRUE))

  # And it must actually override a STALE stored column: 4 rows, the stored
  # column keeps all 4, the syntax's rule keeps 2. Run only the wrapper's own
  # statements (everything before the analysis call it wraps) and report the
  # surviving row count, so the probe measures the FILTER and not jmv.
  blk  <- parse(text = conv$r_code)[[1]]      # local({ ... })
  body <- as.list(blk[[2]])                   # `{`, stmt1, stmt2, ..., analysis
  probe <- as.call(c(body[-length(body)], quote(nrow(data))))
  data <- data.frame(AGE = c(20, 24, 30, 40),
                     SCORE = c(1, 2, 3, 4),
                     KEEP = c(1, 1, 1, 1))
  expect_equal(eval(probe, envir = list2env(list(data = data))), 2L)
})

test_that("a filter expression naming an absent variable keeps the stored column, loudly", {
  # SPSS behaves this way too: its gold for such a command records
  # `Error # 4285 ... Text: <var>` and the run continues on the stale stored
  # column. Raised during peer review, 2026-09-09.
  sps <- withr::local_tempfile(fileext = ".sps")
  writeLines(c("COMPUTE keep = (fluently = 1).",
               "FILTER BY keep.",
               "DESCRIPTIVES VARIABLES=score.",
               "USE ALL."), sps)
  cmds <- parse_sps(sps)
  k <- which(vapply(cmds, function(c)
    identical(unname(c$command_type), "DESCRIPTIVES"), logical(1)))
  sav_info <- list(data = data.frame(SCORE = numeric()),
                   variables = c("SCORE", "KEEP"))
  conv <- convert_spss_to_r(cmds[[k]], sav_info)

  blk  <- parse(text = conv$r_code)[[1]]
  body <- as.list(blk[[2]])
  probe <- as.call(c(body[-length(body)], quote(nrow(data))))

  # FLUENTLY is absent; KEEP is stored and keeps 3 of 4 rows.
  data <- data.frame(SCORE = c(1, 2, 3, 4), KEEP = c(1, 1, 1, 0))
  out <- utils::capture.output(
    n <- eval(probe, envir = list2env(list(data = data))))
  expect_equal(n, 3L)
  expect_match(paste(out, collapse = " "), "Conversion note")
  expect_match(paste(out, collapse = " "), "FLUENTLY")

  # And with no stored column to fall back on, it must fail loudly, not silently
  # analyse every row.
  bare <- data.frame(SCORE = c(1, 2, 3, 4))
  expect_error(eval(probe, envir = list2env(list(data = bare))),
               "cannot recompute")
})

test_that("a filter var the syntax never assigns still uses the stored column", {
  sps <- withr::local_tempfile(fileext = ".sps")
  writeLines(c("FILTER BY keep.",
               "DESCRIPTIVES VARIABLES=score.",
               "USE ALL."), sps)
  cmds <- parse_sps(sps)
  k <- which(vapply(cmds, function(c)
    identical(unname(c$command_type), "DESCRIPTIVES"), logical(1)))
  sav_info <- list(data = data.frame(SCORE = numeric()),
                   variables = c("SCORE", "KEEP"))
  conv <- convert_spss_to_r(cmds[[k]], sav_info)
  # No defining expression is available, so the tolerant guard is correct here.
  expect_true(grepl('if ("KEEP" %in% names(data))', conv$r_code, fixed = TRUE))
})

test_that("PARTIAL CORR under a recomputed FILTER matches SPSS with no data patch", {
  # See test-corr-with-consult.R: the corpus is not shipped with the package.
  root <- Sys.getenv("SPSS2R_GOLD_CORPUS", unset = "")
  base <- file.path(root, "round-2-spss")
  sav  <- file.path(base, "3-converted-local", "spss", "osf_wdnpx_sample4.sav")
  sps  <- file.path(base, "3-converted-local", "spss",
                    "osf_5a91c46cda91d4000fb0_sample4.sps")
  skip_if_not(nzchar(root) && file.exists(sav) && file.exists(sps),
              "gold corpus not reachable -- set SPSS2R_GOLD_CORPUS to the corpus root to run this anchor")
  skip_if_not_installed("ppcor")

  sd    <- parse_sav(sav)
  cmds  <- parse_sps(sps)
  convs <- convert_all_commands(cmds, sd)
  data  <- sd$data
  names(data) <- normalize_spss_names(names(data))
  data[] <- lapply(data, function(v)
    if (inherits(v, "haven_labelled")) as.numeric(v) else v)

  # Two-sided control: the stored column really IS stale and really IS present,
  # so a pass here cannot come from the fixture accidentally agreeing.
  expect_true("ADIFILT" %in% names(data))
  expect_equal(sum(as.numeric(data$ADIFILT) != 0 & !is.na(data$ADIFILT)), 178L)

  X <- c("MEANMERGER", "MEANINTERSEC", "MEANCOMPART", "R_MEANDOMARAB1",
         "MEANDOMARAB", "MEANDOMISR", "MEANBIISAR")
  Y <- c("MEANBENEVO", "MEANUNIVERS", "MEANSELFDIREC", "MEANSTIMUL", "MEANHEDON",
         "MEANACHIV", "MEANPOWER", "MEANSECUR", "MEANCONFORM", "MEANTRADITION")
  k <- NULL
  for (i in seq_along(cmds)) {
    v <- cmds[[i]]$variables
    if (identical(unname(cmds[[i]]$command_type), "PARTIAL CORR") &&
        identical(toupper(v$main_vars %||% ""), X) &&
        identical(toupper(v$with_vars %||% ""), Y) &&
        identical(toupper(v$controls %||% ""), "MRAT")) { k <- i; break }
  }
  skip_if(is.null(k), "target command not found in the corpus file")

  # NO data$ADIFILT patch -- the wrapper must derive the case set itself.
  #
  # This .sps redefines AdiFilt THIRTEEN times (`COMPUTE AdiFilt = 0.` followed
  # by a new condition, over and over). The wrapper must re-emit the definition
  # ACTIVE at this FILTER BY -- not the first in the file and not the last. That
  # it lands on 97/df=94/r=.125/p=.225 is the proof it picks the right one, so do
  # not "simplify" this to whichever definition is easiest to reach.
  out <- eval(parse(text = convs[[k]]$r_code), envir = list2env(list(data = data)))

  expect_equal(unique(out$N), 97)     # SPSS: N of Rows in Working Data File 97
  expect_equal(unique(out$df), 94)

  row <- out[out$Variable == "MEANMERGER" & out$With == "MEANBENEVO", ,
             drop = FALSE]
  expect_equal(round(as.numeric(row[["Partial r"]]), 3), 0.125)
  expect_equal(round(as.numeric(row[["Sig. (2-tailed)"]]), 3), 0.225)
})
