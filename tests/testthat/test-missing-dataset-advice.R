# "Upload it alongside the syntax" is advice the researcher cannot always follow.
#
# THE DEFECT (raised by local_43c1d703, approved for fix by the user 2026-09-10).
# When the syntax switches to a dataset we were not given, the generated report
# says: "upload it alongside the syntax to reproduce these analyses". That is
# right when the key is a FILE the researcher has on disk. It is wrong, and
# misattributes the cause, when the key was never a file at all.
#
# MEASURED under C-0001, across all 190 corpus .sps: of the 33 non-active
# `/FILE=` operands in MATCH/ADD/UPDATE commands, **zero exist on disk** -- 9 are
# in-session SPSS dataset NAMES (`DataSet2`, `Demos`, `DataSet3`, `Temp1`,
# `PreRegAnalyses2`, `ExclusionCriteria`) that were never files, and 24 are
# absolute paths on the researcher's own machine. For the first group there is
# nothing to upload: the dataset existed only inside the SPSS session, produced
# by a DATASET or merge command we could not perform. Telling that researcher to
# find a file sends them looking for something that never existed.
#
# So the advice must branch on what the key IS, and the branch must be general --
# keyed off the SHAPE of the reference, never off a filename or a corpus item.

.rmd_for <- function(parsed, conv) {
  gen <- getFromNamespace("generate_rmd", "spss2rmarkdown")
  out_dir <- withr::local_tempdir(.local_envir = parent.frame())
  gen(NULL, "dummy.sav", parsed, conv, output_dir = out_dir)
  paste(readLines(list.files(out_dir, pattern = "[.]Rmd$", full.names = TRUE)[1],
                  warn = FALSE), collapse = "\n")
}

# WHAT THIS FILE DOES AND DOES NOT PIN -- read before trusting it.
#
# The strong test is the last one: it drives `.s2r_key_looks_like_path()` itself
# over ten inputs, two-sided, and that predicate is the whole decision.
#
# The test below is DELIBERATELY WEAKER, and says so rather than pretending
# otherwise. It asserts that BOTH branches are present in the generator's
# emitted `.s2r_get()` template. It does NOT render a document that reaches
# them, because I could not build a fixture that emits `.s2r_get()` at all:
# `generate_rmd()` gates the dataset store on `length(unique(dataset_key)) > 1`
# across the converted commands, and a two-command fixture with distinct keys
# still produced an .Rmd containing `s2r_activate` and the helpers chunk but no
# `s2r_get` / `s2r_ds` body. Something further upstream is required and I did
# not find it.
#
# SO THIS IS A KNOWN GAP, not a covered case: a change that broke the emission
# while leaving both strings in the source would pass here. The better test
# renders a real multi-dataset corpus item -- `osf-paired/study08_gta94/SPSS
# Syntax statistics.jnl.sps` engages the store with 15 distinct keys -- and
# asserts on its output. Filed rather than faked.

test_that("BOTH advice branches exist in the emitted .s2r_get template", {
  # Read the TEMPLATE out of the function itself, not off disk. The first version
  # of this test did `readLines(file.path("..", "..", "R", "rmd_generator.R"))`,
  # which works under pkgload and ERRORS under `R CMD check` ("cannot open the
  # connection"): a checked package has no R/ source tree beside the tests, and
  # the check runs from a different working directory. Deparsing the installed
  # function works in both, and is closer to what actually ships.
  txt <- paste(deparse(getFromNamespace("generate_rmd", "spss2rmarkdown")),
               collapse = "\n")
  # the path branch keeps the original, correct advice...
  expect_match(txt, "upload it alongside the syntax", fixed = TRUE)
  # ...and the non-path branch says plainly there is nothing to upload.
  expect_match(txt, "never a file ", fixed = TRUE)
  expect_match(txt, "created during the SPSS session", fixed = TRUE)
  # and the branch is taken on the predicate, not on a filename
  expect_match(txt, ".s2r_key_looks_like_path(key)", fixed = TRUE)
})

test_that("the branch is decided by the SHAPE of the key, not by a filename", {
  # Pin the predicate itself, so nobody later special-cases a corpus item.
  looks_like_path <- getFromNamespace(".s2r_key_looks_like_path", "spss2rmarkdown")
  for (k in c("C:/Users/x/data.sav", "H:\\temp\\buffer4.sav", "data.sav",
              "sub/dir/file.sav", "/home/x/y.sav")) {
    expect_true(looks_like_path(k), label = paste("path:", k))
  }
  for (k in c("DataSet2", "Demos", "Temp1", "PreRegAnalyses2", "ExclusionCriteria")) {
    expect_false(looks_like_path(k), label = paste("dataset name:", k))
  }
})
