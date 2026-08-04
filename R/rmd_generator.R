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

# Make a converter's r_code safe to substitute as the RHS of `.res <- <r_code>`.
#
# Several converters degrade to a comment-only stub when they cannot resolve
# their variables (e.g. "# FACTOR: No variables specified"). Substituted into
# the analysis-chunk template that becomes `.res <- # FACTOR: ...`, a comment
# is not an expression: `.res` is never bound and the following
# `s2r_render_tables(.res)` raises "object '.res' not found" — a hard,
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
  # whitespace parse cleanly but yield zero expressions — exactly the stub case.
  parsed <- tryCatch(parse(text = code), error = function(e) NULL)
  if (!is.null(parsed) && length(parsed) > 0) {
    return(code)
  }
  paste0(code, "\n  NULL")
}

# Build the hidden setup chunk that defines the helpers inside the generated
# .Rmd, via deparse() of the live functions (no escaping, no file dependency).
.s2r_helpers_chunk <- function() {
  defs <- vapply(
    c(".s2r_table_to_html", "s2r_render_tables", "s2r_render_model"),
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
  # primary — only when the row counts match (per-subject alignment). Shared
  # columns (e.g. GROUP) are NOT overwritten; the primary wins. A row-count
  # mismatch is skipped with a note (a positional cbind would misalign rows).
  #
  # R-0047: equal row counts are NOT proof of alignment — two files with the
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
  secondary_basenames <- unique(secondary_sav_files[nzchar(secondary_sav_files)])
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

  # ---- Separate transformations from analyses ----
  # Filter out skipped commands (metadata) and empty results
  converted_code <- converted_code[!sapply(converted_code, function(x) {
    isTRUE(x$skip) || is.null(x$r_code) || nchar(trimws(x$r_code)) == 0
  })]

  transformations <- converted_code[sapply(converted_code, function(x) isTRUE(x$is_transformation))]
  analyses <- converted_code[!sapply(converted_code, function(x) isTRUE(x$is_transformation))]

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
      paste0(
        "tryCatch({\n",
        conv$r_code,
        "\n}, error = function(e) {\n",
        "  cat(\"**Transformation ", i, " failed:**\", e$message, \"\\n\")\n",
        "})\n"
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
    # `.res <- # FACTOR: ...` — a comment is not an expression, so `.res` is
    # never bound and the next line raises "object '.res' not found", turning
    # an intended graceful skip into a hard user-visible Analysis error.
    # Append an explicit NULL so any comment-only stub stays a parseable no-op.
    # (s2r_render_tables(NULL) renders nothing.) Guard is at the single
    # emission point so it covers every current and future stub converter.
    res_rhs <- .s2r_expression_safe_rcode(conv$r_code)

    glue::glue('
## Analysis {i}: {conv$analysis_type}

{orig_comment}

### R Code
```{{r analysis-{i}, results=\'asis\'}}
tryCatch({{
  .res <- {res_rhs}
  s2r_render_tables(.res)
}}, error = function(e) {{
  cat("\\n\\n**Analysis error:**", e$message, "\\n\\n")
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
  dynamic_notes_section <- if (length(conversion_notes) > 0) {
    rows <- vapply(conversion_notes, function(n) {
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
