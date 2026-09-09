# A dropped MATCH FILES / ADD FILES / UPDATE must leave a LOUD trace in the report.
#
# C-0001. These commands sit in SKIP_COMMANDS, and generate_rmd drops every
# `skip = TRUE` result before building any section — so not even the explanatory
# comment reaches the .Rmd. An `ADD FILES` that should stack two sites
# (150 + 130 = 280) leaves the report computing on 150 rows: no error, no
# warning, a plausible mean from a partial sample. That is the worst defect
# class we have — a believable number from the wrong data.
#
# WHY A NOTE IS THE FIX AND NOT A FALLBACK. Measured 2026-09-09 across all 190
# corpus .sps: 12 use MATCH/ADD/UPDATE, 27 such commands, 33 non-active `/FILE=`
# operands. **Zero of the 33 exist on disk** — 9 are in-session SPSS dataset
# names (`DataSet2`, `Demos`) that were never files at all, and 24 are absolute
# paths on the researcher's own machine (`H:/temp/...`,
# `C:\Users\Laurence\AppData\Roaming\...`, `d:\jacky\...`). Two-sided controls:
# a known corpus .sav IS visible to that probe, a fabricated path is not.
#
# So implementing row-stacking would fire on nothing: the data being merged is
# genuinely unavailable, and no amount of converter work conjures it. Telling
# the researcher plainly that a merge was dropped, and naming what it wanted, is
# the honest and complete fix for every case we can actually observe.
#
# The rule must be GENERAL — driven by the set of merging commands, never by a
# file, a dataset name or a path.

.mk_conv <- function(cmd_type, order = 1) {
  list(r_code = paste0("# ", cmd_type, " (metadata command - skipped)"),
       packages = character(), analysis_type = cmd_type,
       is_transformation = FALSE, skip = TRUE, order = order)
}

.rmd_for <- function(parsed, conv) {
  gen <- getFromNamespace("generate_rmd", "spss2rmarkdown")
  out_dir <- withr::local_tempdir(.local_envir = parent.frame())
  gen(NULL, "dummy.sav", parsed, conv, output_dir = out_dir)
  paste(readLines(list.files(out_dir, pattern = "[.]Rmd$", full.names = TRUE)[1],
                  warn = FALSE), collapse = "\n")
}

test_that("a dropped MATCH FILES is named in the report, not silently vanished", {
  raw <- "MATCH FILES /FILE=*\n  /FILE='DataSet2'\n  /BY Student Nummer."
  rmd <- .rmd_for(list(list(raw = raw, command_type = "MATCH FILES")),
                  list(.mk_conv("MATCH FILES")))
  expect_match(rmd, "MATCH FILES", fixed = TRUE)
  # It must reach the reader's Conversion Notes section, not just a code comment.
  expect_match(rmd, "Conversion Notes", fixed = TRUE)
  # And it must name WHAT was not merged, or the researcher cannot judge the damage.
  expect_match(rmd, "DataSet2", fixed = TRUE)
})

test_that("ADD FILES and UPDATE are covered by the same general rule", {
  for (cmd in c("ADD FILES", "UPDATE")) {
    raw <- paste0(cmd, " /FILE=* /FILE='site2.sav'.")
    rmd <- .rmd_for(list(list(raw = raw, command_type = cmd)),
                    list(.mk_conv(cmd)))
    expect_match(rmd, cmd, fixed = TRUE)
    expect_match(rmd, "site2.sav", fixed = TRUE)
  }
})

test_that("the note says the ROW COUNT may be wrong, which is the actual danger", {
  # A researcher who reads "a command was skipped" may shrug. The report must say
  # the sample itself may be incomplete, because that is what silently changes a
  # published number.
  #
  # `expect_match(rmd, "row")` alone is NOT enough — "row" occurs elsewhere in
  # every generated .Rmd, so that assertion passed even with the note suppressed
  # (caught by mutation-testing this file). Assert the specific phrase.
  raw <- "ADD FILES /FILE=* /FILE='site2.sav'."
  rmd <- .rmd_for(list(list(raw = raw, command_type = "ADD FILES")),
                  list(.mk_conv("ADD FILES")))
  expect_match(rmd, "row count", ignore.case = TRUE)
  expect_match(rmd, "was NOT performed", fixed = TRUE)
})

test_that("TWO-SIDED CONTROL: genuine metadata commands raise NO merge note", {
  # VARIABLE LABELS is also dropped via SKIP_COMMANDS and dropping it is CORRECT.
  # A guard that warned about every skipped command would satisfy the tests above
  # while burying the real warning in noise.
  rmd <- .rmd_for(
    list(list(raw = "VARIABLE LABELS age 'Age in years'.", command_type = "VARIABLE LABELS")),
    list(.mk_conv("VARIABLE LABELS")))
  expect_false(grepl("not merged", rmd, ignore.case = TRUE))
  expect_false(grepl("merge was skipped", rmd, ignore.case = TRUE))
})

test_that("a merge inside a DEFINE macro body is still named", {
  # A merge is dropped by more than one route. SKIP_COMMANDS is the common one,
  # but a merge inside a DEFINE macro body is discarded EARLIER, at the macro
  # boundary, and never reaches `converted_code` at all — so a check driven from
  # the converted commands reports nothing for it.
  #
  # Measured on `osf-round3/2twf5/sample_analysis_macros.sps`: `match files` at
  # line 145 sits inside `define !stat_med_parallel` (lines 96–162), and that
  # macro IS invoked at line 169, so SPSS really does perform the merge. The
  # first version of this fix emitted ZERO notes for that file. Driving the
  # check from the PARSED commands catches every route.
  parsed <- list(
    list(raw = "define !mac(debug = !default(0) !tokens(1))", command_type = "DEFINE"),
    list(raw = "match files\n /file = \"H:/temp/mediator1_on_iv.sav\".",
         command_type = "MATCH FILES"),
    list(raw = "!enddefine.", command_type = "END DEFINE"),
    list(raw = "!mac debug = 1.", command_type = "MACRO CALL"))
  # Nothing survives conversion here — the macro body is dropped wholesale.
  rmd <- .rmd_for(parsed, list())
  expect_match(rmd, "MATCH FILES", fixed = TRUE)
  expect_match(rmd, "mediator1_on_iv.sav", fixed = TRUE)
})

test_that("several merge commands each get their own note", {
  parsed <- list(
    list(raw = "MATCH FILES /FILE=* /FILE='a.sav'.", command_type = "MATCH FILES"),
    list(raw = "VARIABLE LABELS age 'Age'.",         command_type = "VARIABLE LABELS"),
    list(raw = "ADD FILES /FILE=* /FILE='b.sav'.",   command_type = "ADD FILES"))
  conv <- list(.mk_conv("MATCH FILES", 1), .mk_conv("VARIABLE LABELS", 2),
               .mk_conv("ADD FILES", 3))
  rmd <- .rmd_for(parsed, conv)
  expect_match(rmd, "a.sav", fixed = TRUE)
  expect_match(rmd, "b.sav", fixed = TRUE)
})
