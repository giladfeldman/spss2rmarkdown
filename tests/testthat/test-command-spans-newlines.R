# A command's variable list continues across newlines until the "." terminator.
#
# SPSS terminates a command at the "."; newlines inside it are ordinary
# whitespace. So this, on four separate lines, is ONE FREQUENCIES over THREE
# variables:
#
#   Fre
#   TotalnumTV
#   Totalnuminternet
#   TotalnumSocial.
#
# Found 2026-09-09 while triaging the conversion corpus. Corpus item
# OSF deposit rjvq2 "Full Syntax Self-Reporting News Use In-situ and in
# Retrospect.sps", lines 346-349. Measured at the real call site before the
# fix, `parse_sps()` on that file returned, for that command:
#
#   type: FREQUENCIES
#   raw : "Fre\nTotalnumTV\nTotalnuminternet\nTotalnumSocial"
#   vars: Fre                       <-- the command KEYWORD, three variables lost
#
# and the generated report fired "variables not present in the dataset: 'FRE'".
#
# TWO-SIDED CONTROL, same file, same run: the 15 other FRE commands put their
# variables on the SAME line and all parsed correctly (e.g. "FRE Morning
# Afternoon Night" -> Morning | Afternoon | Night). So the trigger is
# specifically "the command keyword is the last token on its line", not
# FREQUENCIES parsing in general.
#
# DECISIVE PROOF THIS IS OURS, not the researcher's broken syntax: the frozen
# SPSS output for the same file (the project's ground-truth corpus, line
# 1316ff) records SPSS running it successfully --
#
#   Syntax     Fre TotalnumTV Totalnuminternet
#              TotalnumSocial.
#   Statistics   N Valid 1411   1411   1411
#              Missing    0      0      0
#
# followed by the full frequency tables (never-watched 80 = 5.7%, 9.00 = 115
# = 8.2%). The gold's only three "Error #" entries are unrelated -- a Hebrew
# word used as a command name on lines 133/144/154.
#
# The cause was `first_line <- strsplit(cmd, "\n")[[1]][1]` in the fallback
# path of extract_variables_clause() (and the matching fallbacks in the EXAMINE
# and MEANS branches): the command was sliced to its first physical line before
# the keyword was stripped, so when the keyword stood alone that line WAS the
# whole "variable list".
#
# The fix is a general tokenisation rule -- strip the keyword from the WHOLE
# command and truncate at the first "/" subcommand -- never a keyword or
# per-file special case. Measured 2026-09-09, R's default (TRE) regex engine
# makes both halves work unchanged across newlines: "\\s" matches "\n", and
# "." matches "\n" too, so `sub("\\s*/.*", "", cmd)` still truncates at a
# subcommand that appears on a later line. (In perl = TRUE mode "." does NOT
# match a newline -- so these substitutions must stay on the default engine.)

test_that("a command keyword alone on its line is not taken as the variable list", {
  clause <- getFromNamespace("extract_variables_clause", "spss2rmarkdown")

  # THE DEFECT: keyword alone on line 1, variables on the following lines.
  expect_equal(
    clause("Fre\nTotalnumTV\nTotalnuminternet\nTotalnumSocial."),
    c("TotalnumTV", "Totalnuminternet", "TotalnumSocial"))

  # TWO-SIDED CONTROL: the same command on one line already worked and must
  # keep working. A "fix" that broke this would trade one defect for a worse
  # one across the other 15 FRE commands in the same file.
  expect_equal(
    clause("Fre TotalnumTV Totalnuminternet TotalnumSocial."),
    c("TotalnumTV", "Totalnuminternet", "TotalnumSocial"))

  # The explicit VARIABLES= form is unaffected either way.
  expect_equal(clause("FREQUENCIES VARIABLES=a b c."), c("a", "b", "c"))

  # A wrapped list that starts on the keyword's line is the mixed case.
  expect_equal(clause("FREQUENCIES a b\nc d."), c("a", "b", "c", "d"))
})

test_that("a subcommand on a later line still terminates the variable list", {
  # The reason the original code sliced to the first line was to stop the
  # variable list before the subcommands. Truncating at "/" over the whole
  # command does that correctly, INCLUDING when the "/" is on a later line --
  # which the first-line slice could only ever do by accident.
  clause <- getFromNamespace("extract_variables_clause", "spss2rmarkdown")

  expect_equal(clause("Fre\nA\nB\n/FORMAT=NOTABLE\n/ORDER=ANALYSIS."), c("A", "B"))
  expect_equal(clause("Fre A B\n/FORMAT=NOTABLE."), c("A", "B"))
  expect_equal(clause("Fre\nA\nB /FORMAT=NOTABLE."), c("A", "B"))
})

test_that("EXAMINE and MEANS honour a variable list that spans newlines", {
  # The same first-line slice existed in both of these branches. Fixing only
  # the one the corpus happened to exercise would leave the identical defect
  # armed behind two other commands.
  parse_vars <- getFromNamespace("extract_variables", "spss2rmarkdown")

  ex <- parse_vars("EXAMINE\nscore\nweight\nBY group.", "EXAMINE")
  expect_equal(ex$dependent, c("score", "weight"))
  expect_equal(ex$factors, "group")

  ex2 <- parse_vars("EXAMINE\nscore\nweight.", "EXAMINE")
  expect_equal(ex2$all, c("score", "weight"))

  me <- parse_vars("MEANS\nscore\nBY\ngroup.", "MEANS")
  expect_equal(me$dependent, "score")
  expect_equal(me$factors, "group")

  # CONTROL: the same-line forms keep working.
  expect_equal(parse_vars("EXAMINE score BY group.", "EXAMINE")$dependent, "score")
  expect_equal(parse_vars("MEANS score BY group.", "MEANS")$factors, "group")
})

test_that("the corpus command that exposed this parses to its three variables", {
  # END-TO-END at the real call site: parse_sps() over a file, not a direct
  # call to the helper. A probe that reimplements the caller measures the
  # probe -- this exercises remove_comments -> normalize_whitespace ->
  # split_commands -> parse_single_command exactly as the converter does.
  sps <- file.path(withr::local_tempdir(), "spans-newlines.sps")
  writeLines(c(
    "COMPUTE TotalnumTV = 1.",
    "EXECUTE.",
    "",
    "Fre",
    "TotalnumTV",
    "Totalnuminternet",
    "TotalnumSocial.",
    "",
    "FRE Morning Afternoon Night."), sps)

  cmds <- parse_sps(sps)
  freq <- Filter(function(cc) isTRUE(cc$command_type == "FREQUENCIES"), cmds)
  expect_length(freq, 2L)

  multiline <- freq[[1]]
  expect_equal(multiline$variables$all,
               c("TotalnumTV", "Totalnuminternet", "TotalnumSocial"))
  # The keyword itself must never appear as a variable.
  expect_false("Fre" %in% multiline$variables$all)
  expect_false("FRE" %in% toupper(multiline$variables$all))

  # CONTROL: the same-line sibling in the same file.
  expect_equal(freq[[2]]$variables$all, c("Morning", "Afternoon", "Night"))
})
