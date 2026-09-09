# `command_type` must be a PLAIN string for every command, aliased or not.
#
# Found 2026-09-09 while writing the newline-tokenisation regression above.
# extract_command_type() looks the type up in a vector keyed by REGEX:
#
#   patterns <- c("FREQUENCIES" = "FREQUENCIES", "FREQ" = "FREQUENCIES",
#                 "FRE" = "FREQUENCIES", ...)
#
# and returned `patterns[i]`, which carries that key along as a names
# attribute. Measured before the fix, for the command "Fre A B.":
#
#   str(cmd$command_type)
#    Named chr "FREQUENCIES"
#    - attr(*, "names")= chr "FRE"
#
#   identical(cmd$command_type, "FREQUENCIES")  -> FALSE
#   cmd$command_type == "FREQUENCIES"           -> TRUE
#
# The fallback path (an unrecognised command, where the type is just the first
# word) returned a BARE string, so the same field was named for the aliased
# commands and unnamed for the rest.
#
# Nothing in this repo read that name -- verified with a two-sided grep for
# `names(...command_type)` across scripts/, pkg/R/ and api/ (0 hits, while the
# control grep for `command_type` itself hit throughout). The cost is entirely
# silent: any consumer, here or downstream, that compares with identical()
# gets FALSE for exactly the aliased commands and TRUE for the rest, and
# nothing errors. This test pins the field's shape so the leak cannot return.

test_that("command_type is an unnamed string for aliased and full spellings alike", {
  ect <- getFromNamespace("extract_command_type", "spss2rmarkdown")

  # Every alias of one command resolves to the same PLAIN string.
  for (spelling in c("FRE A B.", "FREQ A B.", "FREQUENCIES A B.")) {
    ct <- ect(spelling)
    expect_null(names(ct), info = spelling)
    expect_identical(ct, "FREQUENCIES", info = spelling)
  }

  for (spelling in c("DES A.", "DESC A.", "DESCRIPTIVES A.")) {
    expect_identical(ect(spelling), "DESCRIPTIVES", info = spelling)
  }
  for (spelling in c("REL /VARIABLES=A B.", "RELIABILITY /VARIABLES=A B.")) {
    expect_identical(ect(spelling), "RELIABILITY", info = spelling)
  }

  # Multi-word commands go through the same lookup table.
  expect_identical(ect("PARTIAL CORR /VARIABLES=a b BY c."), "PARTIAL CORR")
  expect_identical(ect("SPLIT FILE BY g."), "SPLIT FILE")

  # CONTROL: the fallback path (an unrecognised command) was already unnamed
  # and must stay that way, so both paths agree on the field's shape.
  unknown <- ect("WOMBAT a b.")
  expect_null(names(unknown))
  expect_identical(unknown, "WOMBAT")
})

test_that("command_type survives identical() end to end, not just ==", {
  # The shape has to hold at the real call site, not only in the helper --
  # parse_single_command() could re-wrap it.
  sps <- file.path(withr::local_tempdir(), "types.sps")
  writeLines(c("FRE A B.", "DES A.", "WOMBAT a."), sps)
  cmds <- parse_sps(sps)
  types <- vapply(cmds, function(cc) cc$command_type, character(1))
  expect_identical(types, c("FREQUENCIES", "DESCRIPTIVES", "WOMBAT"))
  # vapply above would already fail on a stray names attribute; assert it
  # directly too so the reason a future failure appears is legible.
  for (cc in cmds) expect_null(names(cc$command_type))
})
