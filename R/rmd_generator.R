# rmd_generator.R
# Generate R Markdown document from converted SPSS syntax

# === s2r table-rendering helpers (Part 2: real HTML tables, no mojibake) =====
# Defined here and injected verbatim into each generated .Rmd via deparse() in
# .s2r_helpers_chunk(), so the rendered document is self-contained (no package
# dependency at knit time) and works whether this file is sourced or installed.
# Canonical reference: stat2rmarkdown::render_jmv_tables. Keep the four
# converter copies (spss/stata/jamovi/jasp) in sync.

.s2r_table_to_html <- function(s) {
  if (is.null(s) || !nzchar(s)) return(NULL)
  lines <- strsplit(s, "\n", fixed = TRUE)[[1]]
  is_sep <- function(l) nchar(gsub("[[:space:]\u2500-\u257f]", "", l, perl = TRUE)) == 0L
  content <- lines[!vapply(lines, is_sep, logical(1))]
  content <- content[nchar(trimws(content)) > 0]
  if (length(content) < 2L) return(NULL)
  title <- trimws(content[[1]])
  header_line <- content[[2]]
  row_lines <- if (length(content) >= 3L) content[3:length(content)] else character(0)
  split_cols <- function(l) {
    parts <- strsplit(sub("^\\s+", "", l), "\\s{2,}", perl = TRUE)[[1]]
    parts[nchar(parts) > 0]
  }
  header <- split_cols(header_line)
  rows <- lapply(row_lines, split_cols)
  ncol_max <- max(c(length(header), vapply(rows, length, integer(1))), 0L)
  if (ncol_max < 1L) return(NULL)
  if (length(header) < ncol_max) header <- c(rep("", ncol_max - length(header)), header)
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  th <- paste0("<th>", esc(header), "</th>", collapse = "")
  body <- vapply(rows, function(r) {
    r <- c(r, rep("", ncol_max - length(r)))
    paste0("<tr>", paste0("<td>", esc(r), "</td>", collapse = ""), "</tr>")
  }, character(1))
  cap <- if (nzchar(title)) paste0("<caption>", esc(title), "</caption>") else ""
  paste0("<table class=\"s2r-table\">", cap, "<thead><tr>", th,
         "</tr></thead><tbody>", paste(body, collapse = ""), "</tbody></table>")
}

# Recurse ONLY into Group/Array containers (NOT the broad ResultsElement class --
# every jmv element incl. Table is a ResultsElement, and some leaves recurse
# infinitely). Image/plot leaves are skipped (not rendered as tables).
s2r_render_tables <- function(x, heading_level = 4, .depth = 0L) {
  if (.depth > 8L) return(invisible())
  if (inherits(x, "Table")) {
    # jmv includes structural tables (Levene's, Shapiro-Wilk, itemReliability,
    # etc.) in the results object even when the corresponding option was not
    # requested; it flags them `visible = FALSE` and its own UI hides them.
    # Rendering them anyway emits shells full of "." placeholder cells that
    # have no counterpart in jamovi's own output. Honour `$visible`: skip a
    # table only when it is *definitively* not visible (never on NA/error, to
    # stay conservative for Table-likes without the property).
    vis <- tryCatch(x$visible, error = function(e) NULL)
    if (is.logical(vis) && length(vis) == 1L && !is.na(vis) && !isTRUE(vis)) {
      return(invisible())
    }
    s <- tryCatch(x$asString(), error = function(e) NULL)
    h <- .s2r_table_to_html(s)
    if (is.null(h)) {
      if (!is.null(s) && nzchar(s)) cat("\n\n```\n", s, "\n```\n\n", sep = "")
      return(invisible())
    }
    cat("\n\n", h, "\n\n", sep = "")
    return(invisible())
  }
  if (inherits(x, "Group") || inherits(x, "Array")) {
    nms <- tryCatch(names(x), error = function(e) NULL)
    if (!is.null(nms)) for (nm in nms) {
      child <- tryCatch(x[[nm]], error = function(e) NULL)
      if (!is.null(child)) s2r_render_tables(child, heading_level, .depth + 1L)
    }
    return(invisible())
  }
  if (is.data.frame(x)) {
    cat("\n\n", knitr::kable(x, format = "html", escape = FALSE,
        table.attr = "class=\"s2r-table\""), "\n\n", sep = "")
  }
  invisible()
}

# Render a fitted model as an HTML coefficient table (modelsummary -> broom ->
# print fallback). For non-jmv (e.g. STATA fixest/lm) analyses.
s2r_render_model <- function(model) {
  if (requireNamespace("modelsummary", quietly = TRUE)) {
    out <- tryCatch(as.character(modelsummary::msummary(model, output = "html")),
                    error = function(e) NULL)
    if (!is.null(out)) { cat("\n\n", out, "\n\n", sep = ""); return(invisible()) }
  }
  if (requireNamespace("broom", quietly = TRUE)) {
    td <- tryCatch(broom::tidy(model), error = function(e) NULL)
    if (!is.null(td) && nrow(td) > 0L) {
      cat("\n\n", knitr::kable(td, format = "html", escape = FALSE,
          table.attr = "class=\"s2r-table\""), "\n\n", sep = "")
      return(invisible())
    }
  }
  print(summary(model)); invisible()
}

# Bring a jmv::linReg results object into line with what SPSS actually prints,
# for the two tables jmv decides for itself. Called by the emitted regression
# chunk, which WRAPS the jmv call in it -- see convert_regression() for why a
# wrapper and not a following statement. Returns the same object, so the
# chunk's value (which is what s2r_render_tables() renders) is unchanged.
#
# DURBIN-WATSON. SPSS reports it as a STATISTIC and prints no p-value for it.
# Primary source, read directly rather than inferred: a frozen SPSS listing
# from the project's regression test set (SPSS output created 04-SEP-2026) for
# a two-block REGRESSION carrying `/RESIDUALS DURBIN`. Its Model Summary has a
# `Durbin-Watson` column holding one number and nothing else --
#
#   Model R       R      Adjusted Std. Error   Change Statistics          Durbin-Watson
#                 Square R Square the Estimate R Sq Ch  F Ch   df1 df2 Sig
#   1     .607(b) .369   .366     5.023        .369     128.897 2  441 .000
#   2     .609(c) .370   .366     5.023        .001       1.035 1  440 .310  1.856
#
# -- the only "Sig." on that row belongs to the F Change column, not to the
# Durbin-Watson one. jmv's Durbin-Watson table adds a p that it computes by
# SIMULATION, and the generated document sets no seed, so the SAME document
# re-knitted on the SAME data gives a DIFFERENT p: four sessions have now
# reproduced that with four disjoint value sets (.042-.062, .054-.082,
# .898-.966, .864-.972), several of them straddling .05.
#
# Seeding it was the other candidate fix and is the WRONG one: SPSS prints no
# such p, so a seed would leave an invented number in the report and merely
# stop it moving -- worse than unstable, because a number that stops moving
# stops looking suspicious. The p is dropped and the statistic SPSS does print
# is kept. `durbin_p` exists so the emitted call states that decision out loud.
#
# MODEL COMPARISONS. jmv::linReg has no `modelComp` argument (checked against
# formals()); it emits that table by itself whenever there are two or more
# blocks. SPSS prints an R-square-change test only for /STATISTICS CHANGE, so
# without that keyword the table is a request nobody made.
#
# Dependency-free: this is deparsed verbatim into every generated .Rmd by
# .s2r_helpers_chunk(). Every jmv access is guarded, so an older or newer jmv
# whose results object is shaped differently degrades to "changed nothing"
# rather than killing the chunk.
s2r_spss_reg_tables <- function(res, durbin_p = FALSE, model_comp = TRUE) {
  # FAIL LOUD, NOT OPEN. Every one of these calls reaches into a jmv results
  # object whose internal shape this package does not control, and DESCRIPTION
  # puts no upper bound on the jmv version. A bare tryCatch(..., NULL) would
  # therefore publish the invented number under any jmv whose layout has moved,
  # with nothing red anywhere -- the exact silent-wrong-value failure the
  # suppression exists to prevent. So each hide is VERIFIED by reading
  # `$visible` back, and a hide that did not take prints a warning into the
  # report itself. All three consult seats (sonnet/anthropic, sol/openai,
  # grok/xai) raised this independently on 2026-09-10.
  .hidden <- function(el) {
    if (is.null(el)) return(NA)
    ok <- tryCatch({ el$setVisible(FALSE); TRUE }, error = function(e) FALSE)
    if (!isTRUE(ok)) return(FALSE)
    v <- tryCatch(el$visible, error = function(e) NULL)
    # A jmv that has no readable `$visible` cannot confirm the hide either way;
    # NA is "unknown", which is reported differently from "failed".
    if (is.null(v) || !is.logical(v) || length(v) != 1L) return(NA)
    !isTRUE(v)
  }
  .say <- function(what) {
    cat("\n\n> **Conversion note:** could not suppress ", what,
        " in the jmv output. The value shown is jmv's, not SPSS's -- ",
        "treat it as unverified.\n\n", sep = "")
  }

  models <- tryCatch(res$models, error = function(e) NULL)
  n <- tryCatch(length(models), error = function(e) 0L)
  if (!(length(n) == 1L && is.finite(n))) n <- 0L
  for (i in seq_len(n)) {
    dw <- tryCatch(models[[i]]$assump$durbin, error = function(e) NULL)
    if (is.null(dw)) next
    # SPSS prints the Durbin-Watson statistic ONCE, on the row of the FINAL
    # model -- in the frozen listing above, model 1's Durbin-Watson cell is
    # blank and only model 2 carries 1.856. jmv builds the table per block, so
    # every intermediate block publishes a statistic SPSS left empty. Raised by
    # seat `grok` (xai) and checked against that listing before acting.
    if (i < n) {
      if (isFALSE(.hidden(dw))) .say("the Durbin-Watson table for an intermediate block")
      next
    }
    if (!isTRUE(durbin_p)) {
      if (isFALSE(.hidden(tryCatch(dw$getColumn("p"), error = function(e) NULL)))) {
        .say("jmv's simulated Durbin-Watson p-value")
      }
    }
    # SPSS's Model Summary has a `Durbin-Watson` column and no autocorrelation
    # column anywhere, so jmv's `Autocorrelation` is a second number the source
    # software did not report. Deterministic, unlike the p, but still not
    # SPSS's output. Raised by seats `sol` (openai) and `grok` (xai).
    if (isFALSE(.hidden(tryCatch(dw$getColumn("autoCor"), error = function(e) NULL)))) {
      .say("jmv's Durbin-Watson autocorrelation column")
    }
  }

  if (!isTRUE(model_comp)) {
    mc <- tryCatch(res$modelComp, error = function(e) NULL)
    if (isFALSE(.hidden(mc))) .say("jmv's unrequested Model Comparisons table")
  }
  res
}

# The variable names an emitted expression actually READS.
#
# `all.vars()` alone stopped being the right answer on 2026-09-10. C-0007 makes
# convert_spss_expression() emit `.data[["T"]]` for a variable named T, because
# base R's `T` is an alias for TRUE and a bare `T` silently evaluates to TRUE
# whenever the column is absent. `all.vars()` reports that as the name `.data`:
#
#   all.vars(quote(.data[["T"]] == 1 & AGE > 2))   ->   ".data"  "AGE"
#
# `.data` is never a column, so a missing-variable check built on `all.vars()`
# is PERMANENTLY non-empty for any expression touching such a variable. The
# FILTER recompute is gated on exactly that check, so it would have been skipped
# every time and the stale stored column used instead -- silently, and with a
# conversion note blaming a variable called `.data`. That is the ADIFILT class
# of defect test-filter-recompute-wins.R exists to prevent, re-entering through
# the fix for a different one.
#
# Found by consult seat `sonnet` (anthropic), 2026-09-10, reviewing 2e80403;
# reproduced here before this helper was written.
#
# Dependency-free: deparsed into every generated .Rmd by .s2r_helpers_chunk().
.s2r_expr_vars <- function(e) {
  lit <- character()
  walk <- function(x) {
    if (is.call(x)) {
      if (length(x) == 3L && identical(x[[1]], as.name("[[")) &&
          identical(x[[2]], as.name(".data")) && is.character(x[[3]])) {
        lit <<- c(lit, as.character(x[[3]]))
      }
      for (i in seq_along(x)) {
        el <- tryCatch(x[[i]], error = function(e) NULL)
        if (!is.null(el)) walk(el)
      }
    }
    invisible()
  }
  walk(e)
  unique(c(setdiff(all.vars(e), ".data"), lit))
}

# Make a converter's r_code safe to substitute as the RHS of `.res <- <r_code>`.
#
# Several converters degrade to a comment-only stub when they cannot resolve
# their variables (e.g. "# FACTOR: No variables specified"). Substituted into
# the analysis-chunk template that becomes `.res <- # FACTOR: ...`, a comment
# is not an expression: `.res` is never bound and the following
# `s2r_render_tables(.res)` raises "object '.res' not found" -- a hard,
# user-visible "Analysis error" where a silent skip was intended.
#
# Appending an explicit `NULL` line keeps such a stub a parseable no-op
# (s2r_render_tables(NULL) renders nothing) while leaving the explanatory
# comment visible in the generated .Rmd. Real code is returned untouched.
#
# This runs at GENERATION time, so it is a package-internal helper and is not
# deparsed into the emitted .Rmd like the render-time helpers below.
.s2r_expression_safe_rcode <- function(r_code) {
  code <- paste(as.character(r_code), collapse = "\n")
  # Does the code parse to at least one expression on its own? Comments and
  # whitespace parse cleanly but yield zero expressions -- exactly the stub case.
  parsed <- tryCatch(parse(text = code), error = function(e) NULL)
  if (!is.null(parsed) && length(parsed) > 0) {
    return(code)
  }
  paste0(code, "\n  NULL")
}

# Which of an analysis's requested variables are absent from `data`?
#
# jmv reports a missing variable with a message that names NOTHING:
#
#   'names' attribute [49] must be the same length as the vector [0]
#
# where 49 is ncol(data). Reproduced 2026-09-05 against jmv 2.7.7 / R 4.4.0
# with round-3-spss uh3n8 "Syntax 7_building variable indexes.sps" +
# "Data 1_Ground file_Complete dataset all years.sav"; the two-sided control
# (the same call on a column that IS present) succeeds, so jmv is healthy and
# the defect is purely the message. Relayed verbatim by the chunk's tryCatch it
# tells the researcher nothing, and it defeats any automated comparison against
# the original SPSS output, which can only classify a failure (the researcher's
# syntax was broken vs. the conversion was) when the error names a variable that
# can be looked up in that output.
#
# So the analysis chunk pre-flights its variables and raises a NAMED error
# instead. This does NOT repair the researcher's syntax -- a missing variable
# is still an error, just a legible one.
#
# This is a RENDER-time helper, deparsed into the generated .Rmd by
# .s2r_helpers_chunk() below, so it must stay dependency-free. `data` is NULL
# on the no-.sav path, where the existing data guard already reports the cause;
# returning nothing there keeps this from firing a second, misleading error.
.s2r_missing_vars <- function(data, vars) {
  vars <- as.character(vars)
  vars <- unique(vars[!is.na(vars) & nzchar(vars)])
  if (!length(vars)) return(character(0))
  if (is.null(data) || !is.data.frame(data)) return(character(0))
  vars[!(vars %in% names(data))]
}

# Name the case-only near miss, because it means WE failed to normalise a name.
#
# The comparison above is deliberately case-SENSITIVE: the converter uppercases
# every SPSS name and the loader uppercases every column, so the two agree by
# construction. When they disagree only in case, the variable IS in the data and
# the fault is ours -- a converter path that forgot to normalise. Reported as a
# bare "not present in the dataset", that is indistinguishable from a genuinely
# absent variable. Measured cost 2026-09-09: a sibling session lost ~40 minutes
# to `variables not present in the dataset: 'meanbenevo'` when MEANBENEVO was
# right there, because `with_vars` was missing from `var_name_keys`.
#
# This CANNOT fire in a healthy report: it needs a column that matches except in
# case, which normalisation makes impossible. So it costs correct conversions
# nothing and only ever annotates one of our own defects.
#
# The suggestion is deliberately NOT quoted. An automated comparison against the
# original SPSS output extracts variable tokens from QUOTED names in a fired
# error and judges the paragraph on all of them together, so a quoted suggestion
# would inject a token that is not part of the researcher's request and could
# flip that paragraph's verdict. Unquoted, it is legible to the reader and
# invisible to the extractor.
#
# RENDER-time helper: deparsed into the generated .Rmd, so it must stay
# dependency-free.
.s2r_case_hint <- function(data, missing) {
  if (!length(missing) || is.null(data) || !is.data.frame(data)) return("")
  nms <- names(data)
  hits <- vapply(missing, function(v) {
    m <- nms[toupper(nms) == toupper(v)]
    if (length(m)) m[[1]] else NA_character_
  }, character(1))
  keep <- !is.na(hits)
  if (!any(keep)) return("")
  paste0(" (case mismatch, not a missing variable: the dataset has ",
         paste(hits[keep], collapse = ", "),
         " -- the converter failed to normalise ",
         paste(missing[keep], collapse = ", "), ")")
}

# Was this dataset key ever a FILE, or only a name inside the SPSS session?
#
# It decides what we tell a researcher whose dataset we do not have. "Upload it
# alongside the syntax" is right for a path they can go and find. It is wrong,
# and sends them looking for something that never existed, when the key was an
# in-session SPSS dataset name produced by a DATASET or merge command we could
# not perform.
#
# Measured under C-0001 across all 190 corpus .sps: of the 33 non-active `/FILE=`
# operands, ZERO exist on disk -- 9 are in-session dataset names (`DataSet2`,
# `Demos`, `Temp1`, `PreRegAnalyses2`, `ExclusionCriteria`) and 24 are absolute
# paths on the researcher's own machine. Both groups reached the same message.
#
# Keyed off the SHAPE of the reference -- a path separator, or a data-file
# extension -- never off a filename or a corpus item. A bare `data.sav` counts as
# a path because it names a file the researcher plausibly has.
#
# RENDER-time helper: deparsed into the generated .Rmd, so it must stay
# dependency-free.
.s2r_key_looks_like_path <- function(key) {
  k <- as.character(key %||% "")
  if (!nzchar(k)) return(FALSE)
  grepl("[/\\\\]", k) || grepl("\\.(sav|zsav|por|dta|csv|txt)$", k, ignore.case = TRUE)
}

# Narrow a converter's declared `variables` to the ones its emitted code
# actually references.
#
# MEASURED BEFORE THIS GUARD WAS WRITTEN, over all 190 corpus .sps files: of
# 992 analyses, 945 declare `variables`, and 48 declared names never appear in
# the emitted r_code at all -- parser artifacts from one malformed paired
# T-TEST ("T.TEST", "X.PAIRED.", "PAIRS.CRAGUN_NRNS_RG"). Pre-flighting those
# would abort analyses that currently run correctly, trading an illegible error
# for a fabricated one.
#
# Intersecting is general and strictly correct: a variable the call never
# mentions cannot be why the call failed. Word boundaries are hand-rolled
# because SPSS names contain "." (so \b would split INT04.A), and every
# non-alphanumeric character is escaped before it reaches the regex.
#
# Runs at GENERATION time, so it is package-internal and not deparsed into the
# emitted .Rmd.
.s2r_preflight_vars <- function(r_code, vars) {
  vars <- as.character(vars)
  vars <- unique(vars[!is.na(vars) & nzchar(vars)])
  if (!length(vars)) return(character(0))
  code <- paste(as.character(r_code), collapse = "\n")
  keep <- vapply(vars, function(nm) {
    esc <- gsub("([^A-Za-z0-9_])", "\\\\\\1", nm)
    grepl(paste0("(^|[^A-Za-z0-9_.])", esc, "($|[^A-Za-z0-9_.])"), code, perl = TRUE)
  }, logical(1), USE.NAMES = FALSE)
  vars[keep]
}

# Render an analysis-chunk error into a single, always-non-empty report line.
#
# The old handler did `cat("**Analysis error:**", e$message, "\n")`, which
# produced a BLANK error line for real, diagnosable failures: rlang/vctrs
# conditions carry multi-line UTF-8 messages, and conditionMessage() on some
# condition objects returns character(0), so cat() contributed nothing. A user
# then saw "Analysis error:" with no reason at all -- worse than a verbose
# message, because there is nothing to act on and the defect looks like a
# harness artifact rather than a data problem.
#
# Observed on round-3-spss "Syntax_ Exploring factor analysis...", where the
# swallowed message was the actual diagnosis: "Can't subset columns that don't
# exist. Columns `PASTI00`, `PAMA02`, ... don't exist."
#
# This is a RENDER-time helper, so it is deparsed into the generated .Rmd by
# .s2r_helpers_chunk() below and must stay dependency-free.
.s2r_format_error <- function(e) {
  msg <- tryCatch(conditionMessage(e), error = function(...) NULL)
  if (is.null(msg) || !length(msg)) msg <- tryCatch(e$message, error = function(...) NULL)
  msg <- tryCatch(as.character(msg), error = function(...) character(0))
  msg <- msg[!is.na(msg)]
  msg <- paste(msg, collapse = " ")
  # Flatten newlines so the message stays on one report line, and collapse the
  # runs of whitespace that flattening leaves behind.
  msg <- gsub("[\r\n]+", " ", msg)
  msg <- gsub("[[:space:]]+", " ", msg)
  msg <- trimws(msg)
  if (!nzchar(msg)) {
    cls <- tryCatch(paste(class(e), collapse = "/"), error = function(...) "condition")
    msg <- paste0("(no message; condition class: ", cls, ")")
  }
  msg
}

# Build the hidden setup chunk that defines the helpers inside the generated
# .Rmd, via deparse() of the live functions (no escaping, no file dependency).
.s2r_helpers_chunk <- function() {
  defs <- vapply(
    c(".s2r_table_to_html", "s2r_render_tables", "s2r_render_model",
      "s2r_spss_reg_tables",
      ".s2r_format_error", ".s2r_missing_vars", ".s2r_case_hint",
      ".s2r_key_looks_like_path",
      ".s2r_expr_vars"),
    function(nm) paste0(nm, " <- ", paste(deparse(get(nm)), collapse = "\n")),
    character(1)
  )
  paste0("```{r s2r-helpers, include=FALSE}\n",
         paste(defs, collapse = "\n"), "\n```\n")
}

#' Generate R Markdown document
#'
#' Creates a complete R Markdown document from parsed and converted SPSS
#' syntax, including data loading, transformations, and analyses.
#'
#' @param sav_data SAV data info from parse_sav()
#' @param sav_path Original SAV file path
#' @param parsed_syntax Parsed SPSS syntax
#' @param converted_code Converted R code blocks
#' @param output_dir Output directory
#' @param options Rendering options
#' @param base_name Optional base name for output files
#' @param conversion_notes Optional list of conversion notes/warnings to surface
#'   in the generated document
#' @param secondary_sav_files Optional character vector of additional .sav paths
#'   uploaded alongside the primary one. Row-aligned files are merged into
#'   `data`; a row-count mismatch is skipped with a note rather than
#'   positionally cbind-ed (which would misalign rows).
#' @return List with rmd_path and data_path
#' @examples
#' \dontrun{
#' parsed    <- parse_sps("analysis.sps")
#' sav_info  <- parse_sav("data.sav")
#' converted <- convert_all_commands(parsed, sav_info)
#' rmd <- generate_rmd(
#'   sav_data       = sav_info,
#'   sav_path       = "data.sav",
#'   parsed_syntax  = parsed,
#'   converted_code = converted,
#'   output_dir     = tempdir()
#' )
#' rmd$rmd_path
#' }
#' @export
generate_rmd <- function(sav_data, sav_path, parsed_syntax, converted_code,
                         output_dir, options = list(), base_name = NULL,
                         conversion_notes = list(), secondary_sav_files = character()) {
  include_original <- options$include_original %||% TRUE
  include_effect_sizes <- options$include_effect_sizes %||% TRUE
  table_style <- options$table_style %||% "apa7"

  # ---- Does this script run against MORE THAN ONE dataset? ----
  # annotate_dataset_state() stamped the ACTIVE .sav on every parsed command and
  # convert_all_commands() carried it here. When a script switches datasets with
  # GET FILE / DATASET ACTIVATE, running everything against a single primary
  # produces plausible numbers from the wrong wave. A single-dataset script has
  # 0 or 1 distinct key and takes exactly the old path, byte for byte.
  .ds_key_of <- function(x) {
    k <- x$dataset_key
    if (is.null(k) || length(k) != 1L || is.na(k) || !nzchar(k)) NA_character_ else as.character(k)
  }
  ds_keys_all <- vapply(converted_code, .ds_key_of, character(1))
  ds_distinct <- unique(ds_keys_all[!is.na(ds_keys_all)])
  multi_dataset <- length(ds_distinct) > 1L
  # A literal R string for one key, or NA_character_ for "the primary dataset".
  .ds_lit <- function(k) if (is.na(k)) "NA_character_" else
    paste0('"', gsub('"', '\\\\"', k), '"')


  if (is.null(base_name)) {
    base_name <- tools::file_path_sans_ext(basename(sav_path))
  }

  # ---- YAML header ----
  yaml_header <- glue::glue('---
title: "SPSS to R Conversion: {base_name}"
author: "Generated by SPSS2R"
date: "`r format(Sys.Date(), \'%B %d, %Y\')`"
output:
  html_document:
    theme: lumen
    highlight: tango
    toc: true
    toc_depth: 4
    toc_float:
      collapsed: true
      smooth_scroll: true
    code_folding: hide
    self_contained: true
    df_print: paged
---
')

  # ---- Inline style + table-rendering helpers ----
  # A self-contained CSS block (passes through pandoc as raw HTML; kept inline
  # so self_contained keeps it embedded) for readable fonts + styled tables.
  style_block <- paste0(
    "<style>\n",
    "body, .main-container { font-family: -apple-system, BlinkMacSystemFont, ",
    "'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-size: 15px; ",
    "line-height: 1.6; color: #1f2933; }\n",
    ".main-container { max-width: 960px; }\n",
    "h1,h2,h3,h4 { font-weight: 600; line-height: 1.25; margin-top: 1.6em; }\n",
    "h1 { font-size: 1.7em; } h2 { font-size: 1.4em; } h3 { font-size: 1.18em; } ",
    "h4 { font-size: 1.02em; color: #3e4c59; }\n",
    "table.s2r-table { border-collapse: collapse; margin: 0.6em 0 1.4em; ",
    "font-size: 0.92em; width: auto; }\n",
    "table.s2r-table caption { caption-side: top; text-align: left; ",
    "font-weight: 600; padding: 4px 0; color: #243b53; }\n",
    "table.s2r-table th, table.s2r-table td { padding: 6px 12px; ",
    "border: 1px solid #e4e7eb; text-align: right; }\n",
    "table.s2r-table th { background: #f5f7fa; font-weight: 600; text-align: left; }\n",
    "table.s2r-table td:first-child, table.s2r-table th:first-child { text-align: left; }\n",
    "table.s2r-table tbody tr:nth-child(even) { background: #fafbfc; }\n",
    "pre { white-space: pre-wrap; word-break: break-word; font-size: 0.85em; ",
    "background: #f5f7fa; border-radius: 6px; padding: 12px; }\n",
    "</style>\n\n"
  )

  # Inject the table-rendering helpers as a hidden setup chunk so the generated
  # .Rmd is self-contained (no package dependency at render time).
  helpers_chunk <- .s2r_helpers_chunk()

  # ---- Setup chunk ----
  setup_chunk <- '```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  error = TRUE,
  fig.width = 10,
  fig.height = 7
)

library(haven)
library(dplyr)
library(jmv)

options(jmv.progress = FALSE)

# R-0010: downgrade known jmv/R compatibility errors to info notes so a single
# incompatible analysis doesn\'t make the whole report look broken. Unknown
# errors still surface. Ported from JAMOVItoRmarkdown.
knitr::knit_hooks$set(error = function(x, options) {
  known_patterns <- c(
    "attr\\\\(data, attributeName\\\\)", "var\\\\(x\\\\) on a factor",
    "not present in the dataset", "does not exist in the data", "is not valid",
    "which are not present", "loadNamespace", "there is no package called"
  )
  is_known <- any(vapply(known_patterns, function(p) grepl(p, x, ignore.case = TRUE), logical(1)))
  if (is_known) {
    clean <- trimws(gsub("##\\\\s*", "", x))
    paste0("\\n<div class=\\"alert alert-info\\"><strong>Note:</strong> ", clean, "</div>\\n")
  } else {
    paste0("\\n<pre><code>", x, "</code></pre>\\n")
  }
})
```
'

  # ---- Data loading ----
  # Load directly from the original .sav (self-contained Rmd, no separate .rds needed).
  # Use a relative path if the .sav lives in the same directory as the Rmd.
  # If no dataset is available (sav_data is NULL), the read_sav() call would
  # reference a non-existent file (e.g., the .sps script path itself) and the
  # data-guard chunk would fire on a misleading error. Emit a no-data stub
  # instead, surfacing the cause clearly via the data-guard + Conversion Notes.
  sav_basename <- if (is.null(sav_data)) NA_character_ else basename(sav_path)
  has_sav <- !is.null(sav_data) && !is.na(sav_basename) &&
             tolower(tools::file_ext(sav_basename)) == "sav"
  data_rds_name <- paste0(base_name, ".rds")
  if (!has_sav) {
    data_section <- glue::glue('
## Data Overview

```{{r load-data}}
# No .sav dataset was paired with this script -- see the Conversion Notes
# section at the end of this document for diagnostics.
data <- NULL
```

---

')
  } else {
  # ---- Secondary .sav merge ----
  # A single .sps is sometimes paired with SEVERAL row-aligned .sav files
  # (e.g. one holding the DVs, another the questionnaire/covariates), and the
  # syntax references variables spanning them WITHOUT any DATASET ACTIVATE /
  # MATCH FILES (it assumes the researcher already merged them in-session). If
  # we load only the primary, analyses referencing the other file's variables
  # fail ("Argument 'vars' contains 'AGE' which is not present in the data").
  # When secondary .sav files are present, emit self-contained code that reads
  # each, normalizes its names, and column-binds its NEW columns onto the
  # primary -- only when the row counts match (per-subject alignment). Shared
  # columns (e.g. GROUP) are NOT overwritten; the primary wins. A row-count
  # mismatch is skipped with a note (a positional cbind would misalign rows).
  #
  # R-0047: equal row counts are NOT proof of alignment -- two files with the
  # SAME N but a different sort order pass the row-count gate and get
  # positionally cbind-ed anyway, silently misjoining every row. Before
  # binding, look for a shared ID-like variable (case-insensitive match on a
  # short allowlist of common respondent-id names, e.g. ID/CASE/SUBJECT/
  # PARTICIPANT, or any other column name present in both frames) and check
  # its values line up row-for-row; a mismatch skips the merge with a loud
  # note instead of silently misaligning. When no such variable is present at
  # all there is nothing to verify against, so the merge proceeds under the
  # documented row-order precondition (also called out in a NOTE) rather than
  # refusing to merge files that never carried an identifier to begin with.
  # NOT when the script switches datasets explicitly. Positionally cbind-ing
  # other uploaded .sav files into the primary is a guess made for scripts that
  # reference variables spanning several files with no MATCH FILES. A script
  # that says GET FILE / DATASET ACTIVATE is not guessing -- it states which
  # dataset each command runs against -- and doing both means a wave-keyed
  # analysis could succeed on a column that its own wave does not contain,
  # borrowed from a same-N sibling. Raised by the Grok 4.6 seat, 2026-09-09.
  #
  # Measured on the corpus item this feature targets, so the change is inert
  # there rather than untested: the 2015 and 2017 waves of uh3n8 have IDENTICAL
  # 59-column sets (setdiff empty in BOTH directions), so the merge could add
  # nothing, and the 2013 wave is skipped anyway on its row count (1828 vs 287).
  secondary_basenames <- if (multi_dataset) character(0) else
    unique(secondary_sav_files[nzchar(secondary_sav_files)])
  secondary_basenames <- secondary_basenames[
    tolower(tools::file_ext(secondary_basenames)) == "sav" &
    basename(secondary_basenames) != sav_basename]
  if (length(secondary_basenames) > 0) {
    vec_lit <- paste0("c(",
                      paste0('"', gsub('"', '\\\\"', basename(secondary_basenames)), '"',
                             collapse = ", "), ")")
    secondary_merge_block <- glue::glue('
# ---- Merge additional paired .sav files (row-aligned; primary wins on shared cols) ----
# NOTE: files are merged POSITIONALLY (row i of the secondary joins row i of the
# primary). This requires all paired .sav files to share the same case/row order.
# When a shared ID-like column is present its values are checked for row-for-row
# alignment before merging; without one there is no way to verify order and a
# same-N different-sort mismatch cannot be detected.
.s2r_id_pattern <- "^(ID|CASE|CASENUM|SUBJECT|SUBJID|PARTICIPANT|PARTID|RESPONDENT|RESPID)$"
for (.sec in {vec_lit}) {{
  if (!file.exists(.sec)) next
  .sd <- tryCatch(haven::read_sav(.sec), error = function(e) NULL)
  if (is.null(.sd)) next
  names(.sd) <- normalize_spss_names(names(.sd))
  .new <- setdiff(names(.sd), names(data))          # only columns not already present
  if (length(.new) == 0) next
  if (nrow(.sd) == nrow(data)) {{
    # Prefer a canonical ID-like name; fall back to any other shared column
    # name (e.g. GROUP) as a best-effort alignment check.
    .shared <- intersect(names(data), names(.sd))
    .id_col <- .shared[grepl(.s2r_id_pattern, .shared, ignore.case = TRUE)]
    if (length(.id_col) == 0) .id_col <- .shared
    .id_col <- .id_col[1]
    if (!is.na(.id_col) && length(.id_col) == 1) {{
      .lhs <- data[[.id_col]]
      .rhs <- .sd[[.id_col]]
      .mismatch <- !isTRUE(all.equal(as.character(.lhs), as.character(.rhs),
                                      check.attributes = FALSE))
      if (.mismatch) {{
        cat("**Note:** skipped merging", .sec,
            "\\u2014 row counts match (", nrow(.sd), ") but shared column \\"",
            .id_col, "\\" does not align row-for-row (different case/sort order); ",
            "a positional merge would silently misjoin cases.\\n\\n", sep = "")
        next
      }}
    }}
    data[.new] <- .sd[.new]                          # positional cbind (per-subject rows align)
  }} else {{
    cat("**Note:** skipped merging", .sec, "(", nrow(.sd), "rows vs", nrow(data),
        ") \\u2014 row counts differ, positional merge would misalign.\\n\\n")
  }}
}}
', .trim = FALSE)
  } else {
    secondary_merge_block <- ""
  }

  # The dataset store, emitted ONLY for a multi-dataset script. Empty otherwise,
  # so a normal single-dataset report is byte-identical to before.
  if (multi_dataset) {
    ds_list_comment <- paste0("#   - ", ds_distinct, collapse = "
")
    dataset_store_block <- glue::glue(paste(c(
      r"()",
      r"(# ---- Several datasets are in play; switch between them the way SPSS does ----)",
      r"(#)",
      r"(# This syntax opens more than one .sav (GET FILE / DATASET ACTIVATE) and runs)",
      r"(# different commands against different ones. Every analysis and transformation)",
      r"(# below activates the dataset that was ACTIVE at that point in the original)",
      r"(# syntax, so a command is never silently run against the wrong file.)",
      r"(#)",
      r"(# Datasets used by this script:)",
      r"({ds_list_comment})",
      r"(.s2r_ds <- new.env(parent = emptyenv()))",
      r"(.s2r_ds[[".primary"]] <- data)",
      r"(.s2r_ds[[toupper("{sav_basename}")]] <- data)",
      r"()",
      r"(# Return a named dataset, loading it on first use and caching it. A dataset the)",
      r"(# job did not receive is a hard, NAMED error rather than a silent fall back to)",
      r"(# the primary: falling back would print entirely plausible numbers computed)",
      r"(# from the wrong wave, which is the failure this block exists to remove.)",
      r"(.s2r_activate <- function(key) {{)",
      r"(  if (is.null(key) || length(key) != 1L || is.na(key) || !nzchar(key)))",
      r"(    return(.s2r_ds[[".primary"]]))",
      r"(  # DATASET COPY makes an INDEPENDENT duplicate whose contents depend on)",
      r"(  # the source's state at that point in the syntax -- which the)",
      r"(  # transformations-before-analyses emission order does not preserve. We)",
      r"(  # refuse it by name rather than aliasing the source and printing a)",
      r"(  # plausible wrong number.)",
      r"(  if (startsWith(key, "#copy-of#")) {{)",
      r"(    stop("this syntax analyses a DATASET COPY of '", sub("^#copy-of#", "", key),)",
      r"(         "', which this converter cannot reproduce faithfully; the copy and ",)",
      r"(         "the original would share one set of values here", call. = FALSE))",
      r"(  }})",
      r"(  k <- toupper(key))",
      r"(  if (!is.null(.s2r_ds[[k]])) return(.s2r_ds[[k]]))",
      r"(  # A repeated GET FILE of the same file is a FRESH read in SPSS, so each)",
      r"(  # GET gets its own instance key ("w.sav", "w.sav#2", ...). Strip the)",
      r"(  # suffix to find the file; the instances then keep separate state.)",
      r"(  key <- sub("#[0-9]+$", "", key))",
      r"(  if (!file.exists(key)) {{)",
      r"(    if (.s2r_key_looks_like_path(key)) {{)",
      r"(      stop("the syntax switches to dataset '", key, "', which was not provided ",)",
      r"(           "with this conversion; upload it alongside the syntax to reproduce ",)",
      r"(           "these analyses", call. = FALSE))",
      r"(    }} else {{)",
      r"(      stop("the syntax switches to dataset '", key, "', which was never a file ",)",
      r"(           "you can supply: it was created during the SPSS session by a command ",)",
      r"(           "this conversion could not perform, so there is nothing to upload. ",)",
      r"(           "See the Conversion Notes for the command that would have produced ",)",
      r"(           "it", call. = FALSE))",
      r"(    }})",
      r"(  }})",
      r"(  d <- haven::read_sav(key))",
      r"(  names(d) <- normalize_spss_names(names(d)))",
      r"(  d <- clean_data(d))",
      r"(  .s2r_ds[[k]] <- d)",
      r"(  d)",
      r"(}})",
      r"()",
      r"(# Write a transformed frame back to its own slot, so later commands on the SAME)",
      r"(# dataset see the new columns while the other datasets stay untouched. This is)",
      r"(# what preserves the MEANING of the original command order even though every)",
      r"(# transformation is emitted before every analysis: each wave mutates only)",
      r"(# itself, so the same variable name recomputed with a different formula per)",
      r"(# wave never collides.)",
      r"(.s2r_commit <- function(key, value) {{)",
      r"(  k <- if (is.null(key) || length(key) != 1L || is.na(key) || !nzchar(key)))",
      r"(         ".primary" else toupper(key))",
      r"(  .s2r_ds[[k]] <- value)",
      r"(  invisible(value))",
      r"(}})",
      NULL), collapse = "
"), .trim = FALSE)
  } else {
    dataset_store_block <- ""
  }

  data_section <- glue::glue('
## Data Overview

```{{r load-data}}
# Load dataset directly from the SPSS .sav (haven preserves labels and types)
data <- haven::read_sav("{sav_basename}")

# Normalize variable names to UPPERCASE (SPSS is case-insensitive).
# Non-ASCII letters -> a unique _UXXXX_ codepoint token so internationally
# named variables (e.g. Hebrew TV-aleph-1 / TV-bet-1) stay DISTINCT instead of
# collapsing to a duplicate TV.1 (which makes dplyr abort on duplicate column
# names). Must match the converter normalize_spss_names() exactly so the
# syntax references and the data columns agree.
normalize_spss_names <- function(x) {{
  if (is.null(x)) return(x)
  x <- toupper(x)
  x <- vapply(x, function(s) {{
    if (is.na(s) || !nzchar(s)) return(s)
    cps <- utf8ToInt(s)
    if (all(cps <= 127L)) return(s)
    chars <- intToUtf8(cps, multiple = TRUE)
    out <- ifelse(cps <= 127L, chars, paste0("_U", sprintf("%04X", cps), "_"))
    paste0(out, collapse = "")
  }}, character(1), USE.NAMES = FALSE)
  x <- gsub("[^A-Z0-9_]", ".", x)
  x <- ifelse(grepl("^[0-9.]", x), paste0("X", x), x)
  x
}}

names(data) <- normalize_spss_names(names(data))
{secondary_merge_block}
# Convert haven_labelled to native R types for jmv compatibility.
# Defensive guard (2026-06-06): if d is NULL / 0-col / empty, return as-is to
# avoid the closure-is-not-subsettable error in R 4.4.0 when d[] is called on
# NULL. The cwd bug fix (knit_root_dir) keeps data loading correctly so this
# branch is rarely hit, but it prevents a cascade if the load chunk ever
# fails for any other reason. See docs/handoff/2026-06-06-clean_data-defensive-wrapping-finding.md.
clean_data <- function(d) {{
  if (is.null(d) || !is.data.frame(d) || ncol(d) == 0) return(d)
  d[] <- lapply(d, function(x) {{
    if (inherits(x, "haven_labelled")) x <- tryCatch(as.numeric(x), error = function(e) x)
    if (is.logical(x)) x <- as.numeric(x)
    x
  }})
  d
}}

data <- clean_data(data)
{dataset_store_block}

cat("**Variables:**", ncol(data), "\\n\\n")
cat("**Observations:**", nrow(data), "\\n\\n")

# Warn about empty columns
na_cols <- colSums(is.na(data)) == nrow(data)
if (any(na_cols)) {{
  cat("**Warning:** The following variables are entirely NA:\\n\\n")
  cat(paste(names(data)[na_cols], collapse = ", "), "\\n\\n")
}}
```

---

')
  }

  # ---- Defensive data-load guard ----
  # Mirror Stata\'s parallel-empty rule (cf. STATA convert_use Sec 4): when no
  # dataset loaded, knitr::knit_exit() so the rendered HTML stops cleanly with
  # a single explanatory note instead of cascading per-chunk errors against
  # an empty data frame. Replaces a previous stop() that aborted render
  # entirely and produced no HTML at all.
  data_guard_chunk <- '
```{r data-guard, results="asis"}
if (!exists("data") || !is.data.frame(data) || nrow(data) == 0) {
  cat("> **\\u26a0 Data not loaded.** No dataset was paired with this script ",
      "(or all `GET FILE` / `GET DATA` / `IMPORT` references failed to resolve). ",
      "Subsequent analyses are skipped to mirror SPSS\\u2019s behavior when the ",
      "active dataset is empty. See the *Conversion Notes* section at the end ",
      "of this document for resolution diagnostics.\\n\\n", sep = "")
  knitr::knit_exit()
}
```
'

  # ---- Merge commands: name them before they are dropped ----
  # MATCH FILES / ADD FILES / UPDATE are in SKIP_COMMANDS, so the filter below
  # removes them along with genuine metadata -- and until now not even the
  # explanatory comment survived. A dropped ADD FILES that should have stacked
  # 150 + 130 rows leaves the report computing on 150: no error, no warning, a
  # plausible mean from a partial sample. That is the worst class of defect we
  # have, because nothing looks wrong.
  #
  # We do NOT attempt the merge. Measured 2026-09-09 over all 190 corpus .sps:
  # 12 use these commands, with 33 non-active `/FILE=` operands between them,
  # and ZERO of the 33 exist on disk -- 9 are in-session SPSS dataset names
  # (`DataSet2`) that were never files, and 24 are absolute paths on the
  # researcher's own machine (`H:/temp/...`). The data is genuinely unavailable,
  # so naming the loss plainly IS the fix, not a fallback for one.
  #
  # Keyed off the command type, never off a filename or dataset name, so it
  # covers every current and future merging command in the set.
  # Driven from PARSED commands, not from converted ones. A merge is dropped by
  # more than one route: SKIP_COMMANDS is the common one, but a merge inside a
  # DEFINE macro body is discarded earlier still, at the macro boundary, and
  # never reaches `converted_code` at all. Measured on
  # `osf-round3/2twf5/sample_analysis_macros.sps`: `match files` at line 145 sits
  # inside `define !stat_med_parallel` (96-162), the macro IS invoked at line 169
  # so SPSS really does perform that merge, and a converted_code-driven check
  # reported ZERO notes for it. Reading the parsed commands catches every route.
  #
  # This can over-report a merge inside a macro that is never invoked. That is
  # the safe direction -- a warning about a merge we did not perform -- and
  # detecting invocation reliably would mean expanding macros, which we do not do.
  merge_notes <- list()
  MERGE_COMMANDS <- c("MATCH FILES", "ADD FILES", "UPDATE")
  for (pc in parsed_syntax) {
    ct <- toupper(trimws(as.character(pc$command_type %||% "")))
    if (!(ct %in% MERGE_COMMANDS)) next
    raw <- as.character(pc$raw %||% "")
    # The `/FILE=` operands say WHAT was not merged. `*` is the active dataset
    # and is not a missing source, so it is dropped from the list.
    ops <- regmatches(raw, gregexpr("(?i)/\\s*FILE\\s*=\\s*('[^']*'|\"[^\"]*\"|\\S+)",
                                    raw, perl = TRUE))[[1]]
    ops <- sub("(?i)^/\\s*FILE\\s*=\\s*", "", ops, perl = TRUE)
    ops <- gsub("^['\"]|['\"]$", "", ops)
    ops <- ops[nzchar(ops) & ops != "*"]
    sources <- if (length(ops)) paste(ops, collapse = ", ") else "an unnamed source"
    merge_notes[[length(merge_notes) + 1L]] <- list(
      level = "error",
      message = paste0(
        ct, " was NOT performed: the data it merges from (", sources,
        ") is not available to this conversion, so the rows or columns it ",
        "would have added are missing. Every result below is computed on the ",
        "unmerged dataset -- the row count, and any statistic that depends on ",
        "it, may not match the original analysis."))
  }

  # ---- Separate transformations from analyses ----
  # Filter out skipped commands (metadata) and empty results
  converted_code <- converted_code[!sapply(converted_code, function(x) {
    isTRUE(x$skip) || is.null(x$r_code) || nchar(trimws(x$r_code)) == 0
  })]

  # Guard against an all-skipped script: `sapply(list(), fn)` returns a LIST,
  # and `x[list()]` raises "invalid subscript type 'list'". A .sps consisting
  # only of metadata and merge commands filters to length 0 here and crashed
  # the generator outright. The same guard already existed further down for
  # `analyses` alone; it was missing at this split. Found 2026-09-09 by the
  # C-0001 regression test, which is exactly that shape.
  if (length(converted_code) == 0) {
    transformations <- list()
    analyses <- list()
  } else {
  transformations <- converted_code[sapply(converted_code, function(x) isTRUE(x$is_transformation))]
  analyses <- converted_code[!sapply(converted_code, function(x) isTRUE(x$is_transformation))]
  }

  # Filter out pure EXECUTE from transformations (they add noise)
  transformations <- transformations[!sapply(transformations, function(x) {
    identical(trimws(x$analysis_type), "EXECUTE")
  })]

  # ---- Data transformations section ----
  # Coalesce consecutive transformation R chunks into groups so knitr only
  # pays its per-chunk overhead (~50-100 ms) once per group instead of once
  # per SPSS COMPUTE/RECODE/IF command. The worst SPSS scripts in the
  # corpus produce 4k-28k transformation chunks; ungrouped, knitr overhead
  # alone blows past any sensible adapter timeout. Each command keeps its
  # own tryCatch wrapper, so per-command error isolation is preserved.
  #
  # IMPORTANT: only the R *chunks* are coalesced. The markdown structure
  # (## Transformation N: TYPE, ### Original SPSS Syntax, ```spss```
  # source block) is emitted as STATIC markdown text outside the chunk,
  # one per transformation, exactly as before. This keeps the visual
  # hierarchy intact and -- critically -- keeps any tryCatch failure
  # rendering through default `results="markup"` so the failure text
  # appears as `<pre><code>## **Transformation N failed:** ...`, identical
  # in shape to the pre-coalesce output format. (A previous draft used
  # results="asis" + cat()'d markdown headings; that emitted the same
  # failures as `<p><strong>Transformation failed:</strong>` HTML, which
  # the heuristic verifier matches as a runtime error marker, recategorizing
  # previously-GREEN files as YELLOW with no actual behavior change.)
  #
  # echo=FALSE preserves the render-time / page-size win: the source SPSS
  # is shown above each tryCatch in the markdown, and the R code is mostly
  # boilerplate mutate()/recode() with no informational value when echoed.
  #
  # Group size is env-overridable so we can tune without recompiling.
  # Analysis chunks are intentionally NOT grouped: jmv result objects rely
  # on knitr's auto-print to render as HTML tables, which would be broken
  # by sharing a single chunk.
  transform_section <- ""
  if (length(transformations) > 0) {
    GROUP_SIZE <- suppressWarnings(as.integer(Sys.getenv("SPSS_TRANSFORMS_PER_CHUNK", "50")))
    if (is.na(GROUP_SIZE) || GROUP_SIZE < 1L) GROUP_SIZE <- 50L

    n_t <- length(transformations)
    groups <- split(seq_len(n_t), ceiling(seq_len(n_t) / GROUP_SIZE))

    # For each transformation, pre-build (1) the static markdown header +
    # source block emitted outside the chunk, and (2) the R tryCatch body
    # to be included in the group's coalesced chunk.
    md_per_transform <- vapply(seq_len(n_t), function(i) {
      conv <- transformations[[i]]
      orig_idx <- conv$order
      orig <- if (!is.null(orig_idx) && orig_idx <= length(parsed_syntax)) {
        parsed_syntax[[orig_idx]]$raw
      } else {
        ""
      }
      orig_block <- if (include_original && nchar(orig) > 0) {
        paste0("### Original SPSS Syntax\n\n```spss\n", orig, "\n```\n\n")
      } else {
        ""
      }
      paste0("\n## Transformation ", i, ": ", conv$analysis_type, "\n\n",
             orig_block)
    }, character(1))

    r_per_transform <- vapply(seq_len(n_t), function(i) {
      conv <- transformations[[i]]
      # cat() inside tryCatch's error handler -- the leading "## " in the
      # rendered HTML is added by knitr's default results="markup" mode
      # (it prefixes stdout lines), producing
      # `<pre><code>## **Transformation N failed:** ...`. This shape
      # matches the pre-coalesce output and is not flagged by the
      # heuristic verifier as a runtime error marker.
      # With several datasets in play, a transformation must run against -- and
      # write back to -- the dataset that was ACTIVE at that point in the
      # syntax. The activate/commit pair is what makes this survive the
      # transformations-then-analyses reordering below: each wave's COMPUTEs
      # mutate their OWN stored frame, so the analyses still see them. It is
      # also why the same variable name recomputed with a DIFFERENT formula per
      # wave (Syntax 7 does exactly this for risk01_prep2, intcom and risk_per)
      # does not collide.
      ds_pre <- if (multi_dataset)
        paste0("data <- .s2r_activate(", .ds_lit(.ds_key_of(conv)), ")
") else ""
      ds_post <- if (multi_dataset)
        paste0(".s2r_commit(", .ds_lit(.ds_key_of(conv)), ", data)
") else ""
      paste0(
        "tryCatch({
",
        ds_pre,
        conv$r_code,
        "
", ds_post,
        "}, error = function(e) {
",
        "  cat(\"**Transformation ", i, " failed:**\", .s2r_format_error(e), \"\\n\")\n",
        "})
"
      )
    }, character(1))

    group_blocks <- lapply(groups, function(idx) {
      group_start <- min(idx)
      group_end   <- max(idx)
      paste0(
        # Static markdown headers + source blocks for each transformation
        # in the group, in order.
        paste(md_per_transform[idx], collapse = "\n"),
        "\n",
        # Single coalesced R chunk that runs every tryCatch in the group.
        "```{r transforms-", group_start, "-to-", group_end, ", echo=FALSE}\n",
        paste(r_per_transform[idx], collapse = "\n"),
        "```\n"
      )
    })

    transform_section <- paste0(
      "\n# Data Transformations\n\n",
      "These transformations are executed first to prepare the data for analysis.\n\n",
      paste(group_blocks, collapse = "\n"),
      "\n---\n\n",
      "## Final Data Cleanup\n\n",
      "```{r final-cleanup}\n",
      "data <- clean_data(data)\n",
      "```\n\n",
      "---\n\n"
    )
  }

  # ---- Analysis sections ----
  # Filter out unsupported/error analyses that are just noise.
  # Guard against empty `analyses`: sapply(list(), fn) returns a list, and
  # `analyses[list()]` errors with "invalid subscript type 'list'". Empty in
  # means empty out -- scripts containing only transformations/metadata hit
  # this branch (e.g. all-COMPUTE/DO/END SPSS scripts).
  meaningful_analyses <- if (length(analyses) == 0) list() else analyses[!vapply(analyses, function(x) {
    grepl("^Unsupported:", x$analysis_type) ||
    grepl("^Error:", x$analysis_type) ||
    identical(trimws(x$analysis_type), "EXECUTE") ||
    identical(trimws(x$analysis_type), "Split File")
  }, logical(1))]

  # P10 fix: if no meaningful analyses, surface that explicitly so the
  # generated Rmd does not appear to silently succeed.
  empty_analyses_note <- if (length(meaningful_analyses) == 0) {
    glue::glue('
> **No analytical commands found.** This SPSS script contains only metadata,
> data-shape, or labelling commands (e.g. TITLE, VARIABLE LABELS, VALUE
> LABELS, RECODE, COMPUTE, EXECUTE). The original `.sps` file does not
> request any FREQUENCIES / DESCRIPTIVES / RELIABILITY / CORRELATIONS /
> REGRESSION / ONEWAY / etc. that the converter could translate. If the
> original analyses were run interactively in PSPP/SPSS without being
> saved to syntax, they will not appear here.

')
  } else ""

  analysis_sections <- lapply(seq_along(meaningful_analyses), function(i) {
    conv <- meaningful_analyses[[i]]
    orig_idx <- conv$order
    orig <- if (!is.null(orig_idx) && orig_idx <= length(parsed_syntax)) {
      parsed_syntax[[orig_idx]]$raw
    } else {
      ""
    }

    orig_comment <- if (include_original && nchar(orig) > 0) {
      glue::glue("
### Original SPSS Syntax
```spss
{orig}
```
")
    } else {
      ""
    }

    # The chunk template substitutes r_code as the RHS of `.res <- <r_code>`.
    # A converter that degrades to a comment-only stub (e.g.
    # "# FACTOR: No variables specified") therefore emits
    # `.res <- # FACTOR: ...` -- a comment is not an expression, so `.res` is
    # never bound and the next line raises "object '.res' not found", turning
    # an intended graceful skip into a hard user-visible Analysis error.
    # Append an explicit NULL so any comment-only stub stays a parseable no-op.
    # (s2r_render_tables(NULL) renders nothing.) Guard is at the single
    # emission point so it covers every current and future stub converter.
    res_rhs <- .s2r_expression_safe_rcode(conv$r_code)

    # Pre-flight the variables this analysis requests, so a missing one is
    # reported BY NAME instead of through jmv's "'names' attribute [ncol] must
    # be the same length as the vector [0]", which names nothing at all.
    # Emitted at this single site for EVERY analysis, from the list the
    # converter already declares -- never a per-file or per-variable case.
    # Converters that declare no variables get `character(0)` and a no-op.
    # The names are QUOTED so an automated comparison against the original SPSS
    # output can extract them: such tooling looks for quoted tokens, and an
    # unquoted list stays undecidable however readable it is to a person.
    # A converter that already performs its own presence check on these
    # variables sets `self_guards_variables` and owns the reporting; running
    # the general pre-flight as well would report one condition twice, and the
    # harder of the two reports would win.
    preflight <- if (isTRUE(conv$self_guards_variables)) {
      character(0)
    } else {
      .s2r_preflight_vars(conv$r_code, conv$variables)
    }
    vars_lit <- paste(deparse(preflight), collapse = "")

    # Run this analysis against the dataset that was ACTIVE at this point in the
    # syntax. Emitted BEFORE the missing-variable pre-flight, so the pre-flight
    # checks the right frame -- otherwise every 2015-wave analysis would be
    # pre-flighted against the 2013 columns and report the wrong names.
    ds_activate <- if (multi_dataset)
      paste0("  data <- .s2r_activate(", .ds_lit(.ds_key_of(conv)), ")
") else ""

    glue::glue('
## Analysis {i}: {conv$analysis_type}

{orig_comment}

### R Code
```{{r analysis-{i}, results=\'asis\'}}
tryCatch({{
{ds_activate}  .miss <- .s2r_missing_vars(data, {vars_lit})
  if (length(.miss) > 0) {{
    stop("variables not present in the dataset: ",
         paste0("\'", .miss, "\'", collapse = ", "),
         .s2r_case_hint(data, .miss), call. = FALSE)
  }}
  .res <- {res_rhs}
  s2r_render_tables(.res)
}}, error = function(e) {{
  cat("\\n\\n**Analysis error:**", .s2r_format_error(e), "\\n\\n")
}})
```
')
  })

  # ---- Unsupported commands summary ----
  unsupported <- if (length(analyses) == 0) list() else analyses[vapply(analyses, function(x) {
    grepl("^Unsupported:", x$analysis_type)
  }, logical(1))]

  unsupported_section <- ""
  if (length(unsupported) > 0) {
    cmds <- sapply(unsupported, function(x) x$analysis_type)
    unsupported_section <- glue::glue('
---

## Unsupported Commands

The following SPSS commands were not automatically converted and may need manual attention:

{paste("- ", cmds, collapse = "\\n")}
')
  }

  # ---- Conversion notes (dynamic, from path resolution) ----
  # The no-data guard text promises a "Conversion Notes" section with
  # diagnostics. When the caller supplied no notes (the Layer-1 adapter, or a
  # worker upload with no .sav at all), that promise used to dangle -- the
  # section was skipped entirely, leaving a cross-reference to nonexistent
  # content (caught by the 2026-08-04 Sonnet canary audit on Syntax_Hyp4).
  # Synthesize the explanatory note so the referenced section always exists
  # whenever the document points the reader at it.
  # Merge commands dropped above are reported here, ahead of the caller's own
  # notes: a silently unmerged dataset changes the numbers, so it outranks the
  # path-resolution warnings that usually fill this table.
  effective_notes <- c(merge_notes, conversion_notes)
  if (!has_sav && length(effective_notes) == 0) {
    effective_notes <- list(list(
      level = "warning",
      message = paste(
        "No .sav data file was paired with this script, so no dataset could",
        "be loaded and analytical commands were skipped. Upload the matching",
        ".sav alongside the syntax to produce numeric results.")))
  }
  dynamic_notes_section <- if (length(effective_notes) > 0) {
    rows <- vapply(effective_notes, function(n) {
      lvl <- if (is.list(n)) (n$level %||% "info") else "info"
      msg <- if (is.list(n)) (n$message %||% as.character(n)) else as.character(n)
      sprintf("| %s | %s |", lvl, gsub("\\|", "\\\\|", msg))
    }, character(1))
    paste0(
      "\n\n---\n\n## Conversion Notes\n\n",
      "These messages were emitted during conversion. ",
      "Warnings indicate path-resolution fallbacks; errors indicate genuine problems.\n\n",
      "| Level | Message |\n",
      "|-------|---------|\n",
      paste(rows, collapse = "\n"),
      "\n"
    )
  } else ""

  # ---- Footer ----
  footer <- '
---

# Session Info

```{r session-info}
sessionInfo()
```

---

*Report generated by [spss2rmarkdown](https://github.com/giladfeldman/spss2rmarkdown)*
'

  # ---- Combine all sections ----
  full_rmd <- paste(c(
    yaml_header,
    style_block,
    setup_chunk,
    helpers_chunk,
    data_section,
    data_guard_chunk,
    transform_section,
    "# Analyses\n\n",
    empty_analyses_note,
    paste(analysis_sections, collapse = "\n\n---\n\n"),
    unsupported_section,
    dynamic_notes_section,
    footer
  ), collapse = "\n")

  # Write RMD file
  rmd_path <- file.path(output_dir, paste0(base_name, ".Rmd"))
  writeLines(full_rmd, rmd_path, useBytes = TRUE)

  data_path <- file.path(output_dir, data_rds_name)

  list(
    rmd_path = rmd_path,
    data_path = data_path
  )
}
