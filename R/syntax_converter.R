# syntax_converter.R
# Master dispatcher for SPSS -> R translation using jmv package

# Commands that are purely metadata/formatting and should be silently skipped
SKIP_COMMANDS <- c(
  "VARIABLE LABELS", "VALUE LABELS", "ADD VALUE LABELS",
  "FORMATS", "USE ALL", "TEMPORARY",
  "SAVE", "SAVE OUTFILE", "GET FILE", "GET DATA",
  "DATASET", "WRITE", "PRINT", "LIST", "DISPLAY",
  "SET", "PRESERVE", "RESTORE", "NEW FILE",
  "INPUT PROGRAM", "END INPUT PROGRAM",
  "BEGIN DATA", "END DATA",
  "DEFINE", "END DEFINE",
  "MATCH FILES", "ADD FILES", "UPDATE",
  "DO REPEAT", "END REPEAT",
  "RENAME VARIABLES", "AUTORECODE",
  "CASESTOVARS", "VARSTOCASES",
  "GRAPH", "GGRAPH", "WEIGHT"
)

# Commands that modify data and must run before analyses
DATA_COMMANDS <- c(
  "COMPUTE", "RECODE", "IF", "DO IF", "ELSE IF", "ELSE", "END IF",
  "SELECT IF", "FILTER", "SORT CASES", "DELETE VARIABLES",
  "EXECUTE", "SPLIT FILE", "COUNT", "AGGREGATE", "RANK",
  "MISSING VALUES"
)

#' Convert all parsed commands to R code
#'
#' Takes a list of parsed SPSS commands and converts each to equivalent R code.
#'
#' @param parsed_commands List of parsed commands from [parse_sps()]
#' @param sav_info SAV file info from [parse_sav()]
#' @param all_var_names Optional character vector of variable names to use when
#'   expanding SPSS variable ranges (`var1 TO var10`). When a `.sps` is paired
#'   with MULTIPLE `.sav` files, the referenced variables may live in a
#'   non-primary dataset, so the caller can pass the UNION of variable names
#'   across all paired `.sav` files here. When `NULL` (default), the names are
#'   taken from `sav_info` (the loaded primary dataset) for backward
#'   compatibility.
#' @return List of conversion results, each containing r_code, packages, and analysis_type
#' @examples
#' \dontrun{
#' parsed   <- parse_sps("analysis.sps")
#' sav_info <- parse_sav("data.sav")
#' converted <- convert_all_commands(parsed, sav_info)
#' converted[[1]]$r_code
#' converted[[1]]$analysis_type
#' }
#' @export
convert_all_commands <- function(parsed_commands, sav_info, all_var_names = NULL) {
  results <- list()

  # Resolve the reference name list used for TO-range expansion once. Prefer an
  # explicit cross-dataset union from the caller; otherwise fall back to the
  # primary dataset's own names. Normalized to match how SPSS names are rewritten
  # before interpolation (see normalize_spss_names()).
  expand_names <- normalize_spss_names(
    all_var_names %||% sav_info$metadata$name
  )

  # Two SPSS sublanguages we cannot translate to dplyr/jmv R:
  #
  # 1. MATRIX. ... END MATRIX. -- matrix-algebra dialect with `!MAT`,
  #    slicing `(:,i)`, element-wise `&*` / `&/`, etc.
  # 2. DEFINE !macro(args). ... !ENDDEFINE. -- macro definitions whose body
  #    is SPSS-template syntax, often itself containing MATRIX. blocks.
  #
  # Trying to convert these as ordinary COMPUTE produces invalid R that
  # cascades hundreds of parse errors at knit time. Mark everything inside
  # either block as Unsupported and emit the original SPSS as a comment.
  in_matrix_block <- FALSE
  in_define_block <- FALSE

  for (i in seq_along(parsed_commands)) {
    cmd <- parsed_commands[[i]]
    ct  <- toupper(trimws(unname(cmd$command_type) %||% ""))
    raw <- toupper(trimws(cmd$raw %||% ""))

    # Boundary detection (look at command type AND raw text -- parse_sps may
    # emit DEFINE / !ENDDEFINE as different command types depending on form).
    if (ct == "MATRIX") in_matrix_block <- TRUE
    if (ct == "DEFINE" || grepl("^DEFINE\\s+", raw) || grepl("^DEFINE\\s*!", raw)) {
      in_define_block <- TRUE
    }

    skip_block <- in_matrix_block || in_define_block

    if (skip_block) {
      reason <- if (in_matrix_block) "MATRIX block" else "DEFINE macro block"
      # Prefix every continuation line of cmd$raw with "# " so the multi-line
      # SPSS source becomes a multi-line R comment. Without this, continuation
      # lines leak into R's parser and produce cascade parse errors at knit
      # time (e.g. "/", ",", ")" tokens that are valid in SPSS but not in R).
      raw_commented <- paste0(
        "# ",
        gsub("\n", "\n# ", cmd$raw %||% "(empty)", fixed = TRUE)
      )
      result <- list(
        r_code = paste0(
          "# Unsupported: SPSS ", reason,
          " content (skipped to avoid invalid R).\n",
          "# Original SPSS:\n", raw_commented
        ),
        packages = character(),
        analysis_type = paste("Unsupported:", reason),
        is_transformation = TRUE
      )
    } else {
      result <- convert_spss_to_r(cmd, sav_info, all_var_names = expand_names)
    }
    result$order <- i
    # Carry the ACTIVE dataset through to the generator. annotate_dataset_state()
    # stamped it on every parsed command; without it here, a script that switches
    # datasets with GET FILE / DATASET ACTIVATE would still emit every analysis
    # against the single primary dataset. NA = "the primary dataset", which is
    # every single-dataset script and therefore the unchanged path.
    result$dataset_key <- cmd$dataset_key %||% NA_character_
    explicit_xform <- isTRUE(result$is_transformation)
    result$is_transformation <- explicit_xform || (cmd$command_type %in% DATA_COMMANDS)
    results[[i]] <- result

    if (ct == "END MATRIX") in_matrix_block <- FALSE
    if (grepl("^!ENDDEFINE", raw) || ct == "!ENDDEFINE") in_define_block <- FALSE
  }

  # Remove NULL / skip results
  results <- results[!sapply(results, is.null)]
  results
}

#' Convert a single SPSS command to R code
#'
#' Dispatches to the appropriate converter function based on command type.
#'
#' @param parsed_command A single parsed command from [parse_sps()]
#' @param sav_info SAV file info from [parse_sav()]
#' @param all_var_names Optional character vector of variable names used to
#'   expand `var1 TO var10` ranges. See [convert_all_commands()]; when `NULL`
#'   (default) the names come from `sav_info`.
#' @return List with r_code, packages, analysis_type, and optional error
#' @examples
#' \dontrun{
#' parsed   <- parse_sps("analysis.sps")
#' sav_info <- parse_sav("data.sav")
#' result   <- convert_spss_to_r(parsed[[1]], sav_info)
#' cat(result$r_code)
#' }
#' @export
convert_spss_to_r <- function(parsed_command, sav_info, all_var_names = NULL) {
  cmd_type <- parsed_command$command_type

  # Silently skip metadata/formatting commands
  if (cmd_type %in% SKIP_COMMANDS) {
    return(list(
      r_code = paste0("# ", cmd_type, " (metadata command - skipped)"),
      packages = character(),
      analysis_type = cmd_type,
      is_transformation = FALSE,
      skip = TRUE
    ))
  }

  # Normalize variable names (but NOT expression strings which contain R code)
  var_name_keys <- c(
    "all", "source", "target", "dependent", "independent",
    "groups", "factors", "covariates", "dv", "wls", "filter_var",
    "variables", "pairs", "factor", "row", "column", "y", "x", "m", "w",
    "source_vars", "target_vars", "split_var",
    "main_vars", "with_vars", "controls",
    "rank_vars", "rank_groups"
  )

  if (!is.null(parsed_command$variables)) {
    for (key in names(parsed_command$variables)) {
      val <- parsed_command$variables[[key]]
      if (is.character(val) && key %in% var_name_keys) {
        parsed_command$variables[[key]] <- normalize_spss_names(val)
      }
    }
  }

  # Expand TO syntax in variables. Pass the (optionally cross-dataset) name list
  # so a `var1 TO var10` range whose endpoints live in a non-primary .sav still
  # expands. expand_spss_variables() records any range it could NOT resolve in
  # $.unresolved_to so we never hand a literal "TO" token to jmv (which crashes
  # with "'names' attribute [k] must be the same length as the vector [0]").
  parsed_command <- expand_spss_variables(parsed_command, sav_info,
                                          all_var_names = all_var_names)

  # Guard: if a variable range could not be expanded (no available dataset
  # contains its endpoints), emit a clean Conversion Note instead of running a
  # broken analysis on a literal 'TO' token. Applies to procedure commands; the
  # converters for transformation/data commands tolerate the untouched list.
  if (isTRUE(parsed_command$.unresolved_to) && !(cmd_type %in% DATA_COMMANDS)) {
    rng <- parsed_command$.unresolved_to_ranges %||% character()
    detail <- if (length(rng)) paste0(" (", paste(rng, collapse = "; "), ")") else ""
    return(list(
      r_code = paste0(
        "# ", cmd_type, ": variable range could not be expanded", detail, ".\n",
        "# No paired dataset contains these variables, so the analysis was\n",
        "# skipped to avoid an invalid jmv call. Check that the correct .sav\n",
        "# file was uploaded alongside the syntax."),
      packages = character(),
      analysis_type = paste0(cmd_type, " (unresolved variable range)"),
      note = sprintf("%s: unresolved variable range%s", cmd_type, detail),
      skip = FALSE
    ))
  }

  # Get appropriate converter function
  converter <- switch(cmd_type,
    "DESCRIPTIVES"         = convert_descriptives,
    "FREQUENCIES"          = convert_frequencies,
    "CORRELATIONS"         = convert_correlations,
    "PARTIAL CORR"         = convert_partial_corr,
    "T-TEST"               = convert_ttest,
    "ONEWAY"               = convert_oneway,
    "GLM"                  = convert_glm,
    "UNIANOVA"             = convert_glm,
    "ANOVA"                = convert_glm,
    "MANOVA"               = convert_manova,
    "REGRESSION"           = convert_regression,
    "LOGISTIC REGRESSION"  = convert_logistic,
    "RELIABILITY"          = convert_reliability,
    "CROSSTABS"            = convert_crosstabs,
    "MEANS"                = convert_means,
    "EXAMINE"              = convert_examine,
    "COMPUTE"              = convert_compute,
    "COUNT"                = convert_count,
    "RECODE"               = convert_recode,
    "IF"                   = convert_if,
    "SELECT IF"            = convert_select_if,
    "MIXED"                = convert_mixed,
    "FACTOR"               = convert_factor,
    "NPAR TESTS"           = convert_npar_tests,
    "ROC"                  = convert_roc,
    "QUICK CLUSTER"        = convert_quick_cluster,
    "RANK"                 = convert_rank,
    "SORT CASES"           = convert_sort_cases,
    "FILTER"               = convert_filter,
    "SPLIT FILE"           = convert_split_file,
    "DELETE VARIABLES"     = convert_delete_vars,
    "EXECUTE"              = convert_execute,
    "MISSING VALUES"       = convert_missing_values,
    "AGGREGATE"            = convert_aggregate,
    # Default handler
    convert_unsupported
  )

  result <- tryCatch(
    converter(parsed_command, sav_info),
    error = function(e) {
      list(
        r_code = paste0("# Error converting: ", cmd_type, "\n# ", e$message),
        packages = character(),
        analysis_type = paste("Error:", cmd_type),
        error = e$message
      )
    }
  )

  # Per-analysis FILTER routing: if this command was annotated with a
  # filter_var by parse_sps' annotate_filter_state(), wrap the analysis
  # r_code in a local() block that locally rebinds `data` to the filtered
  # subset. This keeps the filter scoped to *this* analysis and does not
  # leak to subsequent commands. We skip transformations (which mutate
  # `data`) and skip the FILTER/USE ALL commands themselves.
  cmd_type_un <- unname(cmd_type)
  is_xform <- isTRUE(result$is_transformation) ||
              (cmd_type_un %in% DATA_COMMANDS) ||
              identical(cmd_type_un, "FILTER") ||
              identical(cmd_type_un, "USE ALL")
  if (!is_xform &&
      !is.null(parsed_command$filter_var) &&
      nzchar(parsed_command$filter_var) &&
      !is.null(result$r_code) && nzchar(result$r_code)) {
    fv <- normalize_spss_names(parsed_command$filter_var)
    indented <- paste0("  ", gsub("\n", "\n  ", result$r_code))
    # The filter column is created by an earlier `COMPUTE filter_$ = (cond)` whose
    # mutate() lives in a *different* analysis's local() scope -- so it does NOT
    # exist on the freshly-loaded `data` here. Re-emit that COMPUTE inside this
    # block (when we captured its defining expression in annotate_filter_state)
    # so the filter has a column to act on. Fall back to a tolerant guard when no
    # expression is available (older parses / IF-defined filters): only filter if
    # the column is actually present, otherwise leave data unfiltered rather than
    # erroring the whole analysis.
    #
    # The recompute is UNCONDITIONAL when we have the syntax's own definition
    # (C-0002, 2026-09-09). It used to be gated on `if (!<fv> %in% names(data))`,
    # i.e. "use the stored column when the .sav happens to ship one" -- which is
    # backwards for the ordinary SPSS idiom, where the script COMPUTEs the filter
    # variable and REDEFINES it before each FILTER BY. SPSS runs on the recomputed
    # value; a same-named column in the .sav is whatever the researcher happened to
    # save last and is stale by construction. Measured on
    # osf_5a91c46cda91d4000fb0_sample4 + osf_wdnpx_sample4.sav: the stored ADIFILT
    # kept 178 of 212 rows, the syntax's own rule kept 97, and SPSS reported 97.
    # The PARTIAL CORR under it moved from r = .125, p = .225, df = 94 (SPSS) to
    # r = .153, p = .042, df = 174 -- across p = .05, with nothing erroring.
    # See tests/testthat/test-filter-recompute-wins.R.
    #
    # A recompute can still fail legitimately: the expression may reference a
    # variable this .sav does not carry. SPSS cannot recompute in that case
    # either -- its own gold for such a command records `Error # 4285 ... Text:
    # <var>` and SPSS carries on with the stale stored column. So we do the same,
    # and say which variable was missing rather than substituting silently. The
    # test is `.s2r_expr_vars()` on the converted expression, which is R's own
    # parser answering "what does this need", not a guess. (It was a bare
    # `all.vars()` until 2026-09-10, when C-0007 started emitting
    # `.data[["T"]]` and `all.vars()` began reporting the name `.data` -- never
    # a column, so the check was permanently non-empty and the recompute never
    # ran. See .s2r_expr_vars() in rmd_generator.R.) tryCatch stays as a backstop
    # for the errors variable presence cannot predict (type mismatches, etc.).
    # Suggested during peer review on 2026-09-09; the reviewers'
    # gta94 counter-case did not hold up (FLUENTLY *is* present in that .sav, 69
    # of 103 rows, and nothing but OMS runs under that filter) but the rule they
    # proposed is better than a bare tryCatch, so it is the primary path here.
    # filter_expr is a structured record from annotate_filter_state():
    #   list(kind="COMPUTE", expr=...)            -> column = (expr)
    #   list(kind="IF", cond=..., expr=...)        -> column = if_else(cond, expr, NA)
    # The IF form must reconstruct the conditional, not just the RHS value, or the
    # recomputed column is constant and the filter is a no-op (R-0042).
    recompute_line <- ""
    fdef <- parsed_command$filter_expr
    if (is.list(fdef) && !is.null(fdef$kind)) {
      r_col <- ""
      if (identical(fdef$kind, "COMPUTE") && nzchar(fdef$expr %||% "")) {
        r_col <- tryCatch(convert_spss_expression(fdef$expr), error = function(e) "")
      } else if (identical(fdef$kind, "IF") && nzchar(fdef$cond %||% "")) {
        r_cond <- tryCatch(convert_spss_expression(fdef$cond), error = function(e) "")
        r_rhs  <- if (nzchar(fdef$expr %||% "")) {
          tryCatch(convert_spss_expression(fdef$expr), error = function(e) "")
        } else ""
        if (nzchar(r_cond)) {
          if (!nzchar(r_rhs)) r_rhs <- "1"
          r_col <- paste0("dplyr::if_else(", r_cond, ", ", r_rhs,
                          ", NA_real_)")
        }
      }
      if (nzchar(r_col)) {
        recompute_line <- paste0(
          "  # SPSS recomputes ", fv, " from the syntax before FILTER BY, so a\n",
          "  # same-named column stored in the .sav is stale and must not win.\n",
          "  # If the expression needs a variable this data does not have, SPSS\n",
          "  # could not recompute either and kept the stored column -- so do that,\n",
          "  # and name the missing variable.\n",
          "  .s2r_fmiss <- setdiff(.s2r_expr_vars(quote(", r_col, ")), names(data))\n",
          "  if (length(.s2r_fmiss) == 0L) {\n",
          "    data <- tryCatch(dplyr::mutate(data, `", fv, "` = ", r_col, "),\n",
          "                     error = function(e) { .s2r_fmiss <<- \"<recompute failed>\"; data })\n",
          "  }\n",
          "  if (length(.s2r_fmiss)) {\n",
          "    if (!\"", fv, "\" %in% names(data)) ",
          "stop(\"FILTER BY ", fv, ": cannot recompute (missing: \", ",
          "paste(.s2r_fmiss, collapse = \", \"), \") and no stored column exists\", call. = FALSE)\n",
          "    cat(\"\\n\\n> **Conversion note:** `", fv, "` could not be recomputed from the syntax \",\n",
          "        \"(missing: \", paste(.s2r_fmiss, collapse = \", \"), \"). The column stored in the .sav \",\n",
          "        \"was used instead, so the number of cases may not match the original analysis.\\n\\n\", sep = \"\")\n",
          "  }\n")
      }
    }
    filter_line <- if (nzchar(recompute_line)) {
      paste0("  data <- dplyr::filter(data, .data[[\"", fv,
             "\"]] != 0 & !is.na(.data[[\"", fv, "\"]]))\n")
    } else {
      # No recompute available: guard so a missing filter column degrades to
      # "unfiltered" instead of crashing the analysis chunk.
      paste0("  if (\"", fv, "\" %in% names(data)) ",
             "data <- dplyr::filter(data, .data[[\"", fv,
             "\"]] != 0 & !is.na(.data[[\"", fv, "\"]]))\n")
    }
    wrapped <- paste0(
      "# Per-analysis FILTER active: ", fv, " (SPSS FILTER ON BY)\n",
      "local({\n",
      recompute_line,
      filter_line,
      indented, "\n",
      "})"
    )
    result$r_code <- wrapped
    if (!"dplyr" %in% (result$packages %||% character())) {
      result$packages <- c(result$packages, "dplyr")
    }
    result$filter_var <- fv
  }

  result
}

# ==============================================================================
# JMV-BASED ANALYSIS CONVERTERS
# ==============================================================================

convert_descriptives <- function(parsed, sav_info) {
  vars <- parsed$variables$all
  if (length(vars) == 0) {
    return(list(
      r_code = "# DESCRIPTIVES: No variables specified",
      packages = character(), analysis_type = "Descriptive Statistics"
    ))
  }
  vars_str <- make_vars_str(vars)

  # SPSS /SAVE creates Z-scored variables named Z<var>. Implement the
  # side-effect by emitting a dplyr::mutate() that adds Z<var> = scale(<var>)[,1]
  # for each variable in DESCRIPTIVES VARIABLES list. SPSS truncates names to
  # 8 chars but in practice modern .sav files preserve full names; we follow
  # the simple Z<var> convention. This is critical because downstream COMPUTE
  # blocks (e.g. compute indiv_rel = MEAN(zsubrel, zq33, zq32)) reference
  # these Zscored columns. Without /SAVE side-effect they never exist.
  has_save <- isTRUE(parsed$subcommands$SAVE) ||
              grepl("/\\s*SAVE\\b", parsed$raw, ignore.case = TRUE, perl = TRUE)

  save_block <- ""
  if (has_save) {
    z_pairs <- vapply(vars, function(v) {
      v_norm <- normalize_spss_names(v)
      z_name <- paste0("Z", v_norm)
      sprintf("    `%s` = tryCatch(as.numeric(scale(.data[[\"%s\"]])[, 1]), error = function(e) NA_real_)",
              z_name, v_norm)
    }, character(1))
    save_block <- paste0(
      "# /SAVE: create Z-scored variables (SPSS DESCRIPTIVES /SAVE side-effect)\n",
      "data <- data |>\n  dplyr::mutate(\n",
      paste(z_pairs, collapse = ",\n"),
      "\n  )\n\n"
    )
  }

  jmv_call <- glue::glue('
jmv::descriptives(
  data = data,
  vars = {vars_str},
  freq = FALSE,
  mean = TRUE,
  median = TRUE,
  sd = TRUE,
  min = TRUE,
  max = TRUE,
  skew = TRUE,
  kurt = TRUE,
  missing = TRUE
)')

  if (has_save) {
    # When /SAVE is present, emit two-step block: mutate first, then descriptives.
    r_code <- paste0(save_block, jmv_call)
    list(r_code = r_code, packages = c("dplyr", "jmv"),
         analysis_type = "Descriptive Statistics (with /SAVE Z-scores)",
         variables = vars,
         is_transformation = TRUE)  # ensure mutate runs in transform section
  } else {
    list(r_code = jmv_call, packages = "jmv",
         analysis_type = "Descriptive Statistics", variables = vars)
  }
}

convert_frequencies <- function(parsed, sav_info) {
  vars <- parsed$variables$all
  if (length(vars) == 0) {
    return(list(
      r_code = "# FREQUENCIES: No variables specified",
      packages = character(), analysis_type = "Frequencies"
    ))
  }
  vars_str <- make_vars_str(vars)

  charts <- .spss_frequencies_charts(parsed$raw)
  # jmv has no pie chart, and no FITTED normal overlay. Record each request
  # rather than redrawing it as something else.
  unconv <- c(
    if (isTRUE(charts$pie)) "/PIECHART",
    if (isTRUE(charts$normal)) "/HISTOGRAM NORMAL (the fitted normal curve)"
  )
  pie_note <- if (length(unconv)) {
    paste0("\n# NOTE: ", paste(unconv, collapse = " and "),
           " requested; jmv has no equivalent, so none is drawn.")
  } else ""

  r_code <- glue::glue('
jmv::descriptives(
  data = data,
  vars = {vars_str},
  freq = TRUE,
  hist = {toupper(charts$hist)},
  dens = FALSE,
  bar = {toupper(charts$bar)}
){pie_note}')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "Frequencies", variables = vars)
}

# ---------------------------------------------------------------------------
# CORRELATIONS / PARTIAL CORR
#
# SPSS reads `/VARIABLES = A B C WITH X Y Z BY ctrl` as a RECTANGULAR request:
# the |x| * |y| cross pairs of {A,B,C} against {X,Y,Z}, optionally partialling
# out the BY variables. Verified against the frozen SPSS output for the corpus
# file osf_5a91c46cda91d4000fb0_sample4 (round-2-spss/2-ground-truth), where the
# printed tables put the x variables in rows and the y variables in columns.
# ---------------------------------------------------------------------------

# Case-deletion rule for a correlation command.
#
# SPSS writes /MISSING explicitly whenever the syntax was pasted from the
# dialogs. Measured on that corpus file: all 142 CORRELATIONS carry
# /MISSING=PAIRWISE, and the 16 PARTIAL CORR split 13 LISTWISE / 3 ANALYSIS.
# Defaults (used only when the subcommand is absent) are PAIRWISE for
# CORRELATIONS and LISTWISE for PARTIAL CORR, per the IBM syntax reference.
.s2r_corr_missing <- function(parsed, default_listwise) {
  miss <- toupper(trimws(parsed$options$missing %||% ""))
  if (!nzchar(miss)) return(default_listwise)
  if (miss %in% c("PAIRWISE", "ANALYSIS")) return(FALSE)
  if (miss == "LISTWISE") return(TRUE)
  default_listwise
}

# TRUE when the command asks for one-tailed significance.
#   CORRELATIONS /PRINT=ONETAIL ...   PARTIAL CORR /SIGNIFICANCE=ONETAIL
# SPSS's one-tailed significance is the two-tailed value halved (the direction
# is taken from the sign of the coefficient), and the column is relabelled.
# 18 of this corpus file's CORRELATIONS commands ask for it; printing the
# two-tailed p under a one-tailed request doubles every reported significance.
.s2r_corr_onetail <- function(parsed) {
  pr <- toupper(parsed$options$print %||% character())
  sg <- toupper(parsed$options$significance %||% character())
  any(pr == "ONETAIL") || any(sg == "ONETAIL")
}

# Emit the rectangular block {x} x {y} of zero-order Pearson correlations, laid
# out the way SPSS lays it out: rows = x variables x {r, p, N}, columns = y.
# jmv::corrMatrix has no `with` option -- it can only produce the full square
# matrix -- so a WITH request is served with stats::cor.test, one call per pair.
.s2r_emit_corr_block <- function(x_vars, y_vars, listwise, onetail) {
  mask <- if (listwise) {
    # /MISSING=LISTWISE drops any case incomplete on ANY variable named in the
    # command, so every cell shares one case set.
    c("  .base <- stats::complete.cases(data[, unique(c(.x, .y)), drop = FALSE])",
      "  .mask <- function(.i, .j) .base")
  } else {
    # /MISSING=PAIRWISE (the CORRELATIONS default) evaluates each pair on its
    # own complete cases, so N varies from cell to cell -- which is what the
    # frozen SPSS output shows (N = 98 / 97 / 99 across one row).
    c("  .mask <- function(.i, .j) stats::complete.cases(data[[.i]], data[[.j]])")
  }
  plab <- if (onetail) "Sig. (1-tailed)" else "Sig. (2-tailed)"
  padj <- if (onetail) "      .p <- .p / 2" else "      .p <- .p"

  paste(c(
    "local({",
    paste0("  .x <- ", paste(deparse(x_vars), collapse = "")),
    paste0("  .y <- ", paste(deparse(y_vars), collapse = "")),
    mask,
    "  .blocks <- lapply(.x, function(.i) {",
    "    .cells <- lapply(.y, function(.j) {",
    "      .ok <- .mask(.i, .j)",
    "      if (identical(.i, .j)) {",
    # SPSS prints the self-correlation as 1.000 with its significance blank --
    # a variable tested against itself is not an inferential correlation.
    "        return(c(1, NA_real_, sum(.ok)))",
    "      }",
    # Only errors are caught. A warning must NOT be turned into NA: cor.test
    # already returns NA for a zero-variance pair (measured 2026-09-09), so
    # swallowing warnings could only ever discard a good coefficient.
    "      .ct <- suppressWarnings(tryCatch(",
    "        stats::cor.test(data[[.i]][.ok], data[[.j]][.ok]),",
    "        error = function(e) NULL))",
    "      if (is.null(.ct)) return(c(NA_real_, NA_real_, sum(.ok)))",
    "      .p <- .ct$p.value",
    padj,
    "      c(round(unname(.ct$estimate), 3), round(.p, 3), sum(.ok))",
    "    })",
    "    .m <- as.data.frame(do.call(cbind, .cells), stringsAsFactors = FALSE)",
    "    names(.m) <- .y",
    "    cbind(Variable = c(.i, \"\", \"\"),",
    paste0("          Statistic = c(\"Pearson r\", \"", plab, "\", \"N\"),"),
    "          .m, stringsAsFactors = FALSE)",
    "  })",
    "  .out <- do.call(rbind, .blocks)",
    "  rownames(.out) <- NULL",
    "  .out",
    "})"
  ), collapse = "\n")
}

convert_correlations <- function(parsed, sav_info) {
  x_vars <- parsed$variables$main_vars %||% parsed$variables$all
  y_vars <- parsed$variables$with_vars %||% character()

  if (length(y_vars) == 0) {
    # No WITH: SPSS reports the square (lower-triangle) matrix over the whole
    # list, which is what jmv::corrMatrix produces.
    vars <- parsed$variables$all
    if (length(vars) < 2) {
      return(list(
        r_code = "# CORRELATIONS: Need at least 2 variables",
        packages = character(), analysis_type = "Correlation Analysis"
      ))
    }
    vars_str <- make_vars_str(vars)

    r_code <- glue::glue('
jmv::corrMatrix(
  data = data,
  vars = {vars_str},
  pearson = TRUE,
  spearman = FALSE,
  kendall = FALSE,
  sig = TRUE,
  flag = TRUE,
  ci = TRUE,
  plots = FALSE,
  plotDens = FALSE,
  plotStats = FALSE
)')

    return(list(r_code = r_code, packages = "jmv",
                analysis_type = "Correlation Analysis", variables = vars))
  }

  # WITH present: SPSS reports ONLY the rectangular block {x} x {y}. Before
  # 2026-09-09 the keyword was dropped by extract_variables_clause() and the
  # converter emitted the full square matrix -- coefficients presented as if
  # the researcher had asked for them (107 of this file's 142 commands).
  if (length(x_vars) == 0) {
    return(list(
      r_code = "# CORRELATIONS: WITH clause has no left-hand variables",
      packages = character(), analysis_type = "Correlation Analysis"
    ))
  }

  list(r_code = .s2r_emit_corr_block(x_vars, y_vars,
                                     .s2r_corr_missing(parsed, FALSE),
                                     .s2r_corr_onetail(parsed)),
       packages = character(),
       analysis_type = "Correlation Analysis",
       variables = c(x_vars, y_vars))
}

convert_partial_corr <- function(parsed, sav_info) {
  # Prefer the parser-supplied x-set / y-set / controls split (WITH- and
  # BY-aware -- see split_corr_variables_clause()). Fall back to legacy 'all'
  # parsing only if those are missing.
  main_vars <- parsed$variables$main_vars
  with_vars <- parsed$variables$with_vars %||% character()
  controls  <- parsed$variables$controls
  if (is.null(main_vars) || length(main_vars) == 0) {
    vars <- parsed$variables$all
    if (length(vars) < 2) {
      return(list(
        r_code = "# PARTIAL CORR: Need at least 2 variables",
        packages = character(), analysis_type = "Partial Correlation"
      ))
    }
    by_match <- extract_pattern(parsed$raw, "BY\\s+([^/]+)")
    if (!is.null(by_match)) {
      controls <- normalize_spss_names(trimws(strsplit(by_match, "[,[:space:]]+")[[1]]))
      main_vars <- setdiff(vars, controls)
    } else {
      main_vars <- vars[1:min(2, length(vars))]
      controls <- if (length(vars) > 2) vars[3:length(vars)] else character()
    }
  }
  if (is.null(controls)) controls <- character()

  onetail <- .s2r_corr_onetail(parsed)

  if (length(controls) == 0) {
    # No BY clause: with nothing partialled out these are ordinary zero-order
    # correlations. Route a WITH request through the SAME rectangular emitter
    # as CORRELATIONS -- falling back to jmv::corrMatrix over the union here
    # would reintroduce, for this case, exactly the over-reporting bug this
    # pair of functions exists to fix. (All three /consult seats, 2026-09-09;
    # reproduced before fixing: a 3 x 2 request emitted 10 pairs, not 6.)
    if (length(with_vars) > 0 && length(main_vars) > 0) {
      return(list(
        r_code = .s2r_emit_corr_block(main_vars, with_vars,
                                      .s2r_corr_missing(parsed, TRUE), onetail),
        packages = character(),
        analysis_type = "Partial Correlation",
        variables = c(main_vars, with_vars)))
    }
    fallback_vars <- unique(c(main_vars, with_vars))
    if (length(fallback_vars) < 2) {
      return(list(
        r_code = "# PARTIAL CORR: Need at least 2 variables",
        packages = character(), analysis_type = "Partial Correlation"
      ))
    }
    main_str <- make_vars_str(fallback_vars)
    r_code <- glue::glue('
# PARTIAL CORR with no controls -- fallback to regular correlation
jmv::corrMatrix(
  data = data,
  vars = {main_str}
)')
    return(list(r_code = r_code, packages = "jmv",
                analysis_type = "Partial Correlation",
                variables = fallback_vars))
  }

  # SPSS `/VARIABLES = A B C WITH X Y Z BY ctrl` asks for the |x| * |y| cross
  # pairs; without WITH it is every pair of the list, choose(n, 2).
  if (length(with_vars) > 0) {
    grid <- expand.grid(i = seq_along(main_vars), j = seq_along(with_vars))
    pair_list <- Map(function(i, j) c(main_vars[i], with_vars[j]), grid$i, grid$j)
  } else {
    pair_list <- list()
    if (length(main_vars) >= 2) {
      for (i in seq_len(length(main_vars) - 1)) {
        for (j in seq(i + 1, length(main_vars))) {
          pair_list <- c(pair_list, list(c(main_vars[i], main_vars[j])))
        }
      }
    }
  }
  if (length(pair_list) == 0) {
    return(list(
      r_code = "# PARTIAL CORR: Need at least 2 variables",
      packages = character(), analysis_type = "Partial Correlation"
    ))
  }

  # SPSS PARTIAL CORR has two genuinely different missing-data modes, and the
  # frozen SPSS output shows the difference directly:
  #
  #   /MISSING=LISTWISE (the default, 13 of this file's 16 commands)
  #     "Statistics are based on cases with no missing data for any variable
  #      listed." Every cell shares one case set, so every cell shares one df.
  #   /MISSING=ANALYSIS (3 commands)
  #     "Statistics for each pair ... valid data for that pair. Partial
  #      correlations are computed from zero-order correlations."
  #     df then VARIES cell to cell -- 94 and 93 in the same table
  #     (2-ground-truth/spss/osf_5a91c46cda91d4000fb0_sample4.txt:38846-38857).
  #
  # LISTWISE is served by ppcor::pcor.test on the one shared case set. ANALYSIS
  # is served by inverting the PAIRWISE zero-order correlation matrix, which is
  # what SPSS documents itself as doing; listwise-deleting each (x, y, controls)
  # triple instead is a third quantity that matches neither.
  listwise    <- .s2r_corr_missing(parsed, default_listwise = TRUE)
  pairs_lit   <- paste0("list(", paste(vapply(pair_list, function(pr)
                        paste(deparse(pr), collapse = ""), character(1)),
                        collapse = ", "), ")")
  controls_lit <- paste(deparse(controls), collapse = "")
  all_lit      <- paste(deparse(unique(c(main_vars, with_vars, controls))),
                        collapse = "")
  plab <- if (onetail) "Sig. (1-tailed)" else "Sig. (2-tailed)"
  phalf <- if (onetail) "    .p <- .p / 2" else "    .p <- .p"

  body <- if (listwise) c(
    paste0("  .all <- ", all_lit),
    "  .base <- stats::complete.cases(data[, .all, drop = FALSE])",
    "  .rows <- lapply(.pairs, function(.pr) {",
    "    .keep <- .base",
    "    if (sum(.keep) <= length(.controls) + 2L) return(NULL)",
    "    .pc <- tryCatch(",
    "      ppcor::pcor.test(",
    "        x = data[[.pr[1]]][.keep],",
    "        y = data[[.pr[2]]][.keep],",
    "        z = data[, .controls, drop = FALSE][.keep, , drop = FALSE]),",
    "      error = function(e) NULL)",
    "    if (is.null(.pc)) return(NULL)",
    "    .r <- .pc$estimate; .n <- .pc$n; .df <- .pc$n - .pc$gp - 2L",
    "    .p <- .pc$p.value",
    phalf,
    "    data.frame(Variable = .pr[1], With = .pr[2],",
    "      `Partial r` = round(.r, 3), df = .df,",
    paste0("      `", plab, "` = round(.p, 3), N = .n,"),
    "      Missing = \"LISTWISE\", check.names = FALSE, stringsAsFactors = FALSE)",
    "  })"
  ) else c(
    "  .rows <- lapply(.pairs, function(.pr) {",
    "    .vars <- unique(c(.pr, .controls))",
    # Pairwise zero-order matrix, then partial by inverting it -- SPSS's own
    # description of /MISSING=ANALYSIS.
    "    .R <- suppressWarnings(stats::cor(data[, .vars, drop = FALSE],",
    "                                      use = \"pairwise.complete.obs\"))",
    "    if (anyNA(.R)) return(NULL)",
    "    .P <- tryCatch(solve(.R), error = function(e) NULL)",
    "    if (is.null(.P)) return(NULL)",
    "    .r <- -.P[.pr[1], .pr[2]] / sqrt(.P[.pr[1], .pr[1]] * .P[.pr[2], .pr[2]])",
    # df uses the pair's own valid N, which is what makes df vary by cell.
    "    .n <- sum(stats::complete.cases(data[[.pr[1]]], data[[.pr[2]]]))",
    "    .df <- .n - length(.controls) - 2L",
    "    if (.df <= 0 || is.na(.r) || abs(.r) >= 1) return(NULL)",
    "    .t <- .r * sqrt(.df) / sqrt(1 - .r^2)",
    "    .p <- 2 * stats::pt(-abs(.t), .df)",
    phalf,
    "    data.frame(Variable = .pr[1], With = .pr[2],",
    "      `Partial r` = round(.r, 3), df = .df,",
    paste0("      `", plab, "` = round(.p, 3), N = .n,"),
    "      Missing = \"ANALYSIS\", check.names = FALSE, stringsAsFactors = FALSE)",
    "  })"
  )

  r_code <- paste(c(
    "local({",
    paste0("  .pairs <- ", pairs_lit),
    paste0("  .controls <- ", controls_lit),
    body,
    "  .out <- do.call(rbind, .rows)",
    "  if (is.null(.out)) return(NULL)",
    "  rownames(.out) <- NULL",
    "  .out",
    "})"
  ), collapse = "\n")

  list(r_code = r_code, packages = if (listwise) "ppcor" else character(),
       analysis_type = "Partial Correlation",
       variables = c(main_vars, with_vars, controls))
}

convert_ttest <- function(parsed, sav_info) {
  groups <- parsed$variables$groups
  vars <- parsed$variables$variables
  pair_pairs <- parsed$variables$pair_pairs %||% list()
  pairs <- parsed$variables$pairs

  if (!is.null(groups) && length(groups) > 0) {
    # Independent samples t-test. SPSS `T-TEST GROUPS=g(v1 v2)` compares the
    # cases with g==v1 against those with g==v2 ONLY (other levels are
    # excluded), with v1 as group 1 -- the listed order defines the sign of
    # t and the mean difference. `GROUPS=g(v)` is a cut point: cases >= v
    # form group 1, cases < v group 2. /VARIABLES may list SEVERAL DVs, each
    # analysed. (All three semantics were dropped before 2026-08-04:
    # >2-level groups hard-errored, sign flipped for descending value pairs,
    # and only the first DV was analysed.)
    group_var <- trimws(gsub("\\(.*", "", groups[1]))
    dvs <- vars
    if ((is.null(dvs) || length(dvs) == 0) && !is.null(pairs)) dvs <- pairs
    if (is.null(dvs) || length(dvs) == 0) {
      return(list(r_code = "# T-TEST: No dependent variable found",
                  packages = character(), analysis_type = "T-Test"))
    }
    vars_code <- paste0("c(", paste0('"', dvs, '"', collapse = ", "), ")")

    make_ttest_call <- function(data_arg, indent = "") {
      paste0(
        "jmv::ttestIS(\n",
        indent, "  data = ", data_arg, ",\n",
        indent, "  vars = ", vars_code, ",\n",
        indent, '  group = "', group_var, '",\n',
        indent, "  students = TRUE,\n",
        indent, "  welchs = TRUE,\n",
        indent, "  mann = FALSE,\n",
        indent, "  meanDiff = TRUE,\n",
        indent, "  ci = TRUE,\n",
        indent, "  effectSize = TRUE,\n",
        indent, "  desc = TRUE,\n",
        indent, "  plots = FALSE\n",
        indent, ")")
    }

    # Parse the GROUPS value list (captured raw by parse_single_command).
    gv_raw <- trimws(parsed$variables$group_values %||% "")
    vals <- character(0)
    vals_str <- FALSE
    if (nzchar(gv_raw)) {
      if (grepl("['\"]", gv_raw)) {
        vals_str <- TRUE
        m <- regmatches(gv_raw, gregexpr("'[^']*'|\"[^\"]*\"", gv_raw))[[1]]
        vals <- gsub("^['\"]|['\"]$", "", m)
      } else {
        vals <- strsplit(gv_raw, "[,[:space:]]+")[[1]]
        vals <- vals[nzchar(vals)]
      }
    }

    if (length(vals) >= 2) {
      v2 <- vals[1:2]
      lit <- if (vals_str) paste0('"', v2, '"') else v2
      lit_vec <- paste0("c(", paste(lit, collapse = ", "), ")")
      coerce_line <- if (vals_str) {
        paste0('  .g <- as.character(data[["', group_var, '"]])\n')
      } else {
        paste0(
          '  .g <- data[["', group_var, '"]]\n',
          "  .g <- if (is.numeric(.g)) as.numeric(.g) else",
          " suppressWarnings(as.numeric(as.character(.g)))\n")
      }
      r_code <- paste0(
        "local({\n",
        "  # GROUPS=", group_var, "(", paste(v2, collapse = " "),
        "): keep only the two listed values; first listed = group 1.\n",
        coerce_line,
        "  .keep <- !is.na(.g) & .g %in% ", lit_vec, "\n",
        "  .d <- data[.keep, , drop = FALSE]\n",
        '  .d[["', group_var, '"]] <- factor(.g[.keep], levels = ', lit_vec, ")\n",
        "  ", make_ttest_call(".d", indent = "  "), "\n",
        "})")
    } else if (length(vals) == 1 && !vals_str) {
      v <- vals[1]
      r_code <- paste0(
        "local({\n",
        "  # GROUPS=", group_var, "(", v, "): cut point \u2014 cases >= ", v,
        " form group 1, cases < ", v, " group 2.\n",
        '  .g <- data[["', group_var, '"]]\n',
        "  .g <- if (is.numeric(.g)) as.numeric(.g) else",
        " suppressWarnings(as.numeric(as.character(.g)))\n",
        "  .keep <- !is.na(.g)\n",
        "  .d <- data[.keep, , drop = FALSE]\n",
        '  .d[["', group_var, '"]] <- factor(ifelse(.g[.keep] >= ', v,
        ', ">= ', v, '", "< ', v, '"), levels = c(">= ', v, '", "< ', v, '"))\n',
        "  ", make_ttest_call(".d", indent = "  "), "\n",
        "})")
    } else {
      # No usable value list (bare GROUPS=g, or a quoted single value):
      # pass through unchanged -- the grouping variable must already be
      # 2-level, exactly as before this fix. Unverifiable by any corpus GT,
      # so the conservative behavior is preserved.
      r_code <- make_ttest_call("data")
    }
    analysis_type <- "Independent Samples T-Test"
  } else if (length(pair_pairs) > 0) {
    # Paired samples t-test(s) - one or many pairs from PAIRS [WITH] [(PAIRED)]
    norm_pairs <- lapply(pair_pairs, function(p) {
      list(i1 = normalize_spss_names(p$i1), i2 = normalize_spss_names(p$i2))
    })
    pair_strs <- vapply(norm_pairs, function(p) {
      sprintf('list(i1 = "%s", i2 = "%s")', p$i1, p$i2)
    }, character(1))
    pairs_list_code <- paste0("list(\n    ", paste(pair_strs, collapse = ",\n    "), "\n  )")

    r_code <- glue::glue('
jmv::ttestPS(
  data = data,
  pairs = {pairs_list_code},
  students = TRUE,
  wilcoxon = FALSE,
  meanDiff = TRUE,
  ci = TRUE,
  effectSize = TRUE,
  desc = TRUE
)')
    analysis_type <- "Paired Samples T-Test"
  } else if (!is.null(pairs) && length(pairs) >= 2) {
    # Fallback path (older parsed structure)
    r_code <- glue::glue('
jmv::ttestPS(
  data = data,
  pairs = list(list(i1 = "{pairs[1]}", i2 = "{pairs[2]}")),
  students = TRUE,
  wilcoxon = FALSE,
  meanDiff = TRUE,
  ci = TRUE,
  effectSize = TRUE,
  desc = TRUE
)')
    analysis_type <- "Paired Samples T-Test"
  } else {
    dv <- vars[1]
    if (is.null(dv)) {
      return(list(r_code = "# T-TEST: No dependent variable found",
                  packages = character(), analysis_type = "T-Test"))
    }
    r_code <- glue::glue('
jmv::ttestOneS(
  data = data,
  vars = c("{dv}"),
  testValue = 0,
  students = TRUE,
  wilcoxon = FALSE,
  meanDiff = TRUE,
  ci = TRUE,
  effectSize = TRUE,
  desc = TRUE
)')
    analysis_type <- "One-Sample T-Test"
  }

  list(r_code = r_code, packages = "jmv",
       analysis_type = analysis_type, variables = c(groups, vars, pairs))
}

convert_oneway <- function(parsed, sav_info) {
  dv <- parsed$variables$dependent[1]
  fct <- parsed$variables$factor
  if (is.null(dv) || is.null(fct)) {
    return(list(r_code = "# ONEWAY: Missing DV or factor",
                packages = character(), analysis_type = "One-Way ANOVA"))
  }

  # NB: jmv::anovaOneW has no `effectSize` argument (unlike jmv::ANOVA). Passing
  # it throws "unused argument (effectSize = TRUE)" and kills the whole analysis.
  r_code <- glue::glue('
jmv::anovaOneW(
  data = data,
  deps = c("{dv}"),
  group = "{fct}",
  welchs = TRUE,
  fishers = TRUE,
  desc = TRUE,
  descPlot = TRUE,
  phMethod = "tukey",
  phTest = TRUE
)')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "One-Way ANOVA", variables = c(dv, fct))
}

convert_glm <- function(parsed, sav_info) {
  dv <- parsed$variables$dependent
  factors <- parsed$variables$factors %||% character()
  covariates <- parsed$variables$covariates %||% character()
  ws_factors <- parsed$variables$ws_factors %||% list()

  if (is.null(dv) || length(dv) == 0) {
    return(list(r_code = "# GLM/UNIANOVA: Missing dependent variable",
                packages = character(), analysis_type = "ANOVA"))
  }

  # ---- Repeated-measures ANOVA branch (SPSS GLM with WSFACTOR) ----
  if (length(ws_factors) > 0) {
    dvs <- normalize_spss_names(dv)
    # Validate: product of within-factor levels should match number of DVs
    prod_levels <- prod(vapply(ws_factors, function(f) f$n, integer(1)))
    if (prod_levels != length(dvs)) {
      return(list(
        r_code = sprintf(
          "# GLM/WSFACTOR mismatch: %d DVs but WSFACTOR levels imply %d cells\n# DVs: %s\n# WSFACTORs: %s",
          length(dvs), prod_levels,
          paste(dvs, collapse = ", "),
          paste(vapply(ws_factors, function(f) sprintf("%s(%d)", f$name, f$n), character(1)),
                collapse = ", ")
        ),
        packages = character(), analysis_type = "Repeated-Measures ANOVA"))
    }

    # Build rm list: factor names + level labels ("L1", "L2", ...)
    rm_parts <- vapply(ws_factors, function(f) {
      lvls <- paste0('"L', seq_len(f$n), '"', collapse = ", ")
      sprintf('list(label = "%s", levels = c(%s))', f$name, lvls)
    }, character(1))
    rm_str <- paste0("list(\n    ", paste(rm_parts, collapse = ",\n    "), "\n  )")

    # Build rmCells list. SPSS convention: the LAST WSFACTOR varies fastest in DV order.
    n_factors <- length(ws_factors)
    sizes <- vapply(ws_factors, function(f) f$n, integer(1))
    # Mixed-radix decomposition with rightmost factor as least-significant digit.
    cell_for_index <- function(i0) {
      # i0 is 0-indexed; returns integer vector of level indices (1-based) per factor
      digits <- integer(n_factors)
      remaining <- i0
      for (k in seq(n_factors, 1)) {
        digits[k] <- (remaining %% sizes[k]) + 1L
        remaining <- remaining %/% sizes[k]
      }
      digits
    }
    rmCells_parts <- character(length(dvs))
    for (i in seq_along(dvs)) {
      lvl_idx <- cell_for_index(i - 1L)
      cell_labels <- paste0('"L', lvl_idx, '"', collapse = ", ")
      rmCells_parts[i] <- sprintf('list(measure = "%s", cell = c(%s))', dvs[i], cell_labels)
    }
    rmCells_str <- paste0("list(\n    ", paste(rmCells_parts, collapse = ",\n    "), "\n  )")

    # emMeans: marginal means for each main effect AND for the full interaction.
    factor_names <- vapply(ws_factors, function(f) f$name, character(1))
    em_parts <- vapply(factor_names, function(fn) sprintf('c("%s")', fn), character(1))
    em_parts <- c(em_parts, paste0("c(",
                                   paste0('"', factor_names, '"', collapse = ", "),
                                   ")"))
    em_str <- paste0("list(\n    ", paste(em_parts, collapse = ",\n    "), "\n  )")

    # Guard: the within-subjects cell measures (DVs) must exist in the data. When
    # a GLM references variables that were computed in an external/upstream SPSS
    # session and are absent here (SPSS itself errors "undefined variable" in
    # that case), jmv::anovaRM fails deep inside with a cryptic
    # "'names' attribute [N] must be the same length as the vector [0]". Emit a
    # clear note listing the missing measures and skip, instead of the cascade.
    dvs_vec <- paste0("c(", paste0('"', dvs, '"', collapse = ", "), ")")
    r_code <- glue::glue('
.rm_measures <- {dvs_vec}
.rm_missing <- setdiff(.rm_measures, names(data))
if (length(.rm_missing) > 0) {{
  cat("**Analysis skipped:** repeated-measures ANOVA references variable(s) not present in the data:",
      paste(.rm_missing, collapse = ", "),
      "\\u2014 these are typically computed in an upstream syntax/session not included here.\\n\\n")
}} else {{
jmv::anovaRM(
  data = data,
  rm = {rm_str},
  rmCells = {rmCells_str},
  rmTerms = ~ {paste(factor_names, collapse = " * ")},
  effectSize = c("partEta"),
  emMeans = {em_str}
)
}}')

    # This converter performs its own presence check above and deliberately
    # reports a SOFT "Analysis skipped:" note rather than an error, so the
    # generator's general pre-flight must stand down here -- otherwise the same
    # condition is reported twice, and the harder of the two wins (measured
    # 2026-09-05: round-3-spss/"Analyses" went GREEN -> YELLOW purely from the
    # double guard). One condition, one report.
    return(list(r_code = r_code, packages = "jmv",
                analysis_type = "Repeated-Measures ANOVA",
                variables = dvs, self_guards_variables = TRUE))
  }

  # ---- Between-subjects factorial ANOVA / ANCOVA (existing behaviour) ----
  # When multiple DVs are listed without WSFACTOR, fall back to the first DV.
  if (length(dv) > 1) {
    dv <- dv[1]
  }

  is_ancova <- length(covariates) > 0

  if (is_ancova) {
    covs_str <- make_vars_str(covariates)

    if (length(factors) > 0) {
      factors_str <- make_vars_str(factors)
      r_code <- glue::glue('
jmv::ancova(
  data = data,
  dep = "{dv}",
  factors = {factors_str},
  covs = {covs_str},
  effectSize = c("eta", "partEta"),
  homo = TRUE,
  postHoc = {factors_str},
  postHocCorr = c("tukey"),
  emMeans = ~ {paste(factors, collapse = " + ")},
  emmPlots = TRUE
)')
    } else {
      r_code <- glue::glue('
jmv::ancova(
  data = data,
  dep = "{dv}",
  covs = {covs_str},
  effectSize = c("eta", "partEta"),
  homo = TRUE
)')
    }
    analysis_type <- "ANCOVA"
  } else {
    factors_str <- make_vars_str(factors)

    if (length(factors) > 0) {
      r_code <- glue::glue('
jmv::ANOVA(
  data = data,
  dep = "{dv}",
  factors = {factors_str},
  effectSize = c("eta", "partEta"),
  homo = TRUE,
  postHoc = {factors_str},
  postHocCorr = c("tukey"),
  emMeans = ~ {paste(factors, collapse = " + ")},
  emmPlots = TRUE
)')
    } else {
      # No factors found -- run as one-sample descriptives instead
      r_code <- glue::glue('
jmv::descriptives(
  data = data,
  vars = "{dv}",
  mean = TRUE,
  sd = TRUE,
  median = TRUE,
  min = TRUE,
  max = TRUE
)')
    }
    analysis_type <- "Factorial ANOVA"
  }

  list(r_code = r_code, packages = "jmv",
       analysis_type = analysis_type, variables = c(dv, factors, covariates))
}

convert_manova <- function(parsed, sav_info) {
  dvs <- parsed$variables$dependent
  factors <- parsed$variables$factors
  if (is.null(dvs) || is.null(factors)) {
    return(list(r_code = "# MANOVA: Missing DVs or factors",
                packages = character(), analysis_type = "MANOVA"))
  }

  deps_str <- make_vars_str(dvs)
  factors_str <- make_vars_str(factors)

  r_code <- glue::glue('
jmv::mancova(
  data = data,
  deps = {deps_str},
  factors = {factors_str},
  multivar = c("pillai", "wilks", "hotel", "roy"),
  boxM = TRUE,
  shapiro = TRUE
)')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "MANOVA", variables = c(dvs, factors))
}

#' Extract PIN/POUT thresholds from a SPSS REGRESSION /CRITERIA subcommand
#'
#' SPSS REGRESSION /CRITERIA = PIN(0.05) POUT(0.10) sets the F-test
#' entry/removal p-value thresholds for stepwise selection. Defaults
#' follow SPSS: PIN = 0.05, POUT = 0.10.
#'
#' @param cmd_raw Raw command text (single string).
#' @return list(pin = numeric, pout = numeric).
#' @keywords internal
extract_stepwise_pin_pout <- function(cmd_raw) {
  pin  <- 0.05
  pout <- 0.10
  if (is.null(cmd_raw) || !nzchar(cmd_raw)) return(list(pin = pin, pout = pout))

  pin_match <- regmatches(cmd_raw, regexec(
    "PIN\\s*\\(\\s*\\.?(\\d+(?:\\.\\d+)?)\\s*\\)",
    cmd_raw, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(pin_match) >= 2) {
    val <- pin_match[2]
    if (!grepl("\\.", val)) val <- paste0("0.", val)  # SPSS allows .05 form
    v <- suppressWarnings(as.numeric(val))
    if (!is.na(v)) pin <- v
  }

  pout_match <- regmatches(cmd_raw, regexec(
    "POUT\\s*\\(\\s*\\.?(\\d+(?:\\.\\d+)?)\\s*\\)",
    cmd_raw, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(pout_match) >= 2) {
    val <- pout_match[2]
    if (!grepl("\\.", val)) val <- paste0("0.", val)
    v <- suppressWarnings(as.numeric(val))
    if (!is.na(v)) pout <- v
  }

  list(pin = pin, pout = pout)
}

# Which jmv::linReg options did this REGRESSION command actually ASK for?
#
# Until 2026-09-09 convert_regression() emitted seventeen options TRUE from a
# fixed template, whatever the syntax said. The worst was `durbin = TRUE`: jmv
# computes the Durbin-Watson p by SIMULATION and the generated .Rmd sets no
# seed, so the same document re-knitted on the same data printed a different p.
# Measured over six identical calls, N = 210: autocorrelation 0.1300234 and DW
# 1.731849 identical every time, p = .042 .050 .042 .034 .062 .044 -- five
# distinct values STRADDLING .05. In a report whose purpose is reproducibility
# that is the most serious defect class there is.
#
# And it was never requested. Two-sided control on the round-2 `sample4` pair:
# `/RESIDUALS` 0 and `DURBIN` 0 in the .sps, `Durbin` 0 in the frozen SPSS gold,
# against `REGRESSION` 203 in the .sps and `Model Summary` 256 in the gold -- so
# the zeros are real zeros, and we were inventing 203 autocorrelation tables.
#
# The rule is the project's rule: WE CONVERT THE SYNTAX. An option is on when
# the command asks for it, keyed off the SPSS subcommand that governs it, never
# off a file or a variable.
#
# Always on, because SPSS prints them without being asked: R, R Square and
# Adjusted R Square (Model Summary), the ANOVA table (in /STATISTICS DEFAULTS),
# and the standardized Beta (always in the Coefficients table). AIC, BIC and
# RMSE appear in NO SPSS REGRESSION table, so they are off unless /STATISTICS
# ALL. Options are emitted explicitly as FALSE rather than omitted, so the
# generated report records the decision instead of hiding it.
# Read an SPSS keyword only WITHIN its own subcommand, so `/STATISTICS ... COLLIN`
# is not satisfied by the word COLLIN sitting under `/RESIDUALS`, and a variable
# that happens to be named DURBIN or HISTOGRAM does not switch a test or a plot
# on. Shared by the regression option map and the plot gates below.
# A slash inside a QUOTED STRING is not a subcommand delimiter. SPSS reads
# `REGRESSION SELECT sex EQ '/RESIDUALS NORMPROB'` as a SELECT comparison value;
# we read it as a /RESIDUALS subcommand and fabricated a residual normal-
# probability plot the report then presented as SPSS output. That is the same
# fabrication class as the defect this whole gate exists for.
# Found by consult seat `sol`, 2026-09-09; reproduced here against a control
# (the identical command without the literal gave norm = FALSE) before the fix.
# Both quote styles are handled; a literal becomes a single blank.
.spss_mask_literals <- function(txt) {
  txt <- gsub("'[^']*'", " ", txt, perl = TRUE)
  gsub('"[^"]*"', " ", txt, perl = TRUE)
}

.spss_sub_text <- function(raw) {
  .spss_mask_literals(toupper(paste(as.character(raw %||% ""), collapse = " ")))
}

# SPSS lets a subcommand or keyword be truncated to its shortest unambiguous
# form, so `/HIST` really does draw a histogram and `/PLOT NPPL` really does draw
# a normal probability plot. Matching only the full word converts those with the
# plot OFF -- silently removing a chart from a published report, which is the
# direction that costs the most.
#
# Found by consult seat `sonnet`, 2026-09-09, reviewing 90bde10. Measured
# exposure inside the commands actually gated: ZERO truncations in the 287-file
# corpus (FREQUENCIES carried /HISTOGRAM 32, /PIECHART 4, /BARCHART 2; EXAMINE's
# /PLOT carried BOXPLOT 137, HISTOGRAM 77, STEMLEAF 64, NPPLOT 46, NONE 2). The
# full forms are the control that makes that zero a real zero, so this is a
# latent defect. Fixed regardless: a prefix match here can only turn a plot ON.
#
# Three characters is the floor -- below every documented SPSS minimum for these
# keywords, and short enough that an ambiguous "/HI" is still refused.
.spss_kw_pattern <- function(word, min_prefix = 3L) {
  n <- nchar(word)
  if (n <= min_prefix) return(word)
  paste0("(?:", paste(substr(rep(word, n - min_prefix + 1L), 1L,
                             seq.int(n, min_prefix)), collapse = "|"), ")")
}

.spss_sub_has <- function(txt, sub, kw) {
  # `/\\s*` here, not `/`: SPSS permits blanks around a slash, and until
  # 2026-09-09 `.spss_has_sub` accepted `/ PLOT` while this function could not
  # SLICE it -- so the subcommand was found and then read as empty, and every
  # EXAMINE plot switched off. Found by consult seat `sol`; reproduced against
  # the no-blank control, which gave hist = TRUE for the same command.
  m <- regmatches(txt, gregexpr(paste0("/\\s*", .spss_kw_pattern(sub), "[^/]*"),
                                txt, perl = TRUE))[[1]]
  if (!length(m)) return(FALSE)
  # A parenthesised token is a user-supplied NAME, not a keyword: in
  # `/SAVE=PRED(COOK)` the COOK is the name of the saved predicted-value column,
  # and reading it as the COOK statistic put a Cook's distance table in the
  # report that SPSS never produced. Also `sol`, 2026-09-09, reproduced against
  # a genuine `/SAVE=COOK` control. Statistic keywords always sit OUTSIDE the
  # parentheses, so dropping their contents cannot lose a real request.
  m <- gsub("\\([^)]*\\)", " ", m, perl = TRUE)
  any(grepl(paste0("\\b", .spss_kw_pattern(kw), "\\b"), m, perl = TRUE))
}

.spss_has_sub <- function(txt, sub) {
  grepl(paste0("/\\s*", .spss_kw_pattern(sub), "\\b"), txt, perl = TRUE)
}

# FREQUENCIES charts. SPSS draws none unless asked: measured over the 287-file
# .sps corpus, 462 FREQUENCIES blocks carried 30 /HISTOGRAM, 2 /BARCHART and
# 4 /PIECHART. jmv::descriptives has no `pie` argument (checked against
# formals()), so a pie chart is recorded as unconverted rather than silently
# redrawn as a bar.
.spss_frequencies_charts <- function(raw) {
  txt <- .spss_sub_text(raw)
  list(
    hist = .spss_has_sub(txt, "HISTOGRAM"),
    bar  = .spss_has_sub(txt, "BARCHART"),
    pie  = .spss_has_sub(txt, "PIECHART"),
    # SPSS's NORMAL superimposes a curve FITTED from the mean and SD. jmv's
    # `dens` is a KERNEL density, a different estimator -- substituting it would
    # be pretending, so the request is recorded and `dens` stays FALSE. Found by
    # consult seat `sol`, 2026-09-09.
    normal = .spss_sub_has(txt, "HISTOGRAM", "NORMAL")
  )
}

# EXAMINE plots. Unlike CORRELATIONS and T-TEST this command really does have a
# /PLOT subcommand, and its SPSS default is not empty: with /PLOT absent SPSS
# prints BOXPLOT and STEMLEAF. jmv has no stem-and-leaf, so `box` alone carries
# the default. SPSS has no density curve anywhere in EXAMINE, so `dens` is only
# ever FALSE.
.spss_examine_plots <- function(raw) {
  txt <- .spss_sub_text(raw)
  if (!.spss_has_sub(txt, "PLOT")) {
    # The default is BOXPLOT *and* STEMLEAF. We keep the boxplot; the
    # stem-and-leaf has no jmv equivalent, so it is recorded rather than
    # dropped. Found by consult seat `sol`, 2026-09-09.
    return(list(hist = FALSE, dens = FALSE, box = TRUE, qq = FALSE,
                unconverted = "STEMLEAF (SPSS default)"))
  }
  if (.spss_sub_has(txt, "PLOT", "NONE")) {
    return(list(hist = FALSE, dens = FALSE, box = FALSE, qq = FALSE))
  }
  all_p <- .spss_sub_has(txt, "PLOT", "ALL")
  list(
    hist = all_p || .spss_sub_has(txt, "PLOT", "HISTOGRAM"),
    dens = FALSE,
    box  = all_p || .spss_sub_has(txt, "PLOT", "BOXPLOT"),
    qq   = all_p || .spss_sub_has(txt, "PLOT", "NPPLOT"),
    # jmv draws neither of these. They are RECORDED rather than dropped, and
    # never approximated by a different plot. STEMLEAF appears 64 times inside
    # EXAMINE's /PLOT in the 287-file corpus, so this is an active omission and
    # not a hypothetical one. Found by consult seat `sonnet`, 2026-09-09.
    unconverted = c(
      if (all_p || .spss_sub_has(txt, "PLOT", "STEMLEAF")) "STEMLEAF",
      if (all_p || .spss_sub_has(txt, "PLOT", "SPREADLEVEL")) "SPREADLEVEL",
      # SPSS NPPLOT draws a normal AND a detrended Q-Q plot;
      # jmv::descriptives(qq = TRUE) draws one ordinary Q-Q via stat_qq, so the
      # detrended one has no equivalent. Found by consult seat `sol`, 2026-09-09.
      if (all_p || .spss_sub_has(txt, "PLOT", "NPPLOT")) "the detrended Q-Q plot"
    )
  )
}

# The confidence LEVEL a `CI(n)` keyword asks for, as a percentage.
#
# `.spss_sub_has()` strips parenthesised text before matching keywords -- it has
# to, or `/SAVE=PRED(COOK)` reads as a request for Cook's distance. That makes
# `CI(90)` and `CI(99)` indistinguishable from `CI(95)`, so both jmv's `ciWidth`
# and `ciWidthOR` went unemitted and jmv's 95% default was published as though
# it were the interval SPSS reported. A wrong interval on a real coefficient,
# with nothing erroring. Raised independently by consult seats `sol` (openai)
# and `grok` (xai), 2026-09-10.
#
# LATENT IN THIS CORPUS, and said out loud rather than left implied: every one
# of the 448 `CI(n)` keywords across the 190 .sps asks for 95% -- 242 `CI(95)`,
# 160 `CI(.95)`, 46 `CI(.9500)`. SPSS accepts the level as either a percentage
# or a proportion, so all three forms are normalised here; a value at or below 1
# is read as a proportion. The width is emitted explicitly even when it is 95,
# so the generated report RECORDS the level instead of inheriting a default.
.spss_ci_level <- function(txt, sub, default = 95) {
  m <- regmatches(txt, gregexpr(paste0("/\\s*", .spss_kw_pattern(sub), "[^/]*"),
                                txt, perl = TRUE))[[1]]
  if (!length(m)) return(default)
  hit <- regmatches(m, regexpr("\\bCI\\s*\\(\\s*([0-9.]+)\\s*\\)", m, perl = TRUE))
  hit <- hit[nzchar(hit)]
  if (!length(hit)) return(default)
  v <- suppressWarnings(as.numeric(sub(".*\\(\\s*([0-9.]+)\\s*\\).*", "\\1", hit[[1]])))
  if (is.na(v) || v <= 0) return(default)
  if (v <= 1) v <- v * 100
  if (v >= 100) return(default)
  round(v, 4)
}

.spss_regression_options <- function(raw) {
  txt <- .spss_sub_text(raw)
  sub_has <- function(sub, kw) .spss_sub_has(txt, sub, kw)
  stat_all <- sub_has("STATISTICS", "ALL")
  res_all  <- sub_has("RESIDUALS", "ALL") || sub_has("RESIDUALS", "DEFAULTS")
  list(
    aic      = stat_all,
    bic      = stat_all,
    rmse     = stat_all,
    ci       = stat_all || sub_has("STATISTICS", "CI"),
    # SPSS prints no confidence interval for the STANDARDIZED coefficient, so
    # this one needs an explicit ALL and is not implied by CI.
    ciStdEst = stat_all,
    collin   = stat_all || sub_has("STATISTICS", "COLLIN") || sub_has("STATISTICS", "TOL"),
    # Cook's distance reaches SPSS output through /SAVE, not /STATISTICS.
    cooks    = sub_has("SAVE", "COOK"),
    durbin   = sub_has("RESIDUALS", "DURBIN"),
    norm     = res_all || sub_has("RESIDUALS", "HISTOGRAM") || sub_has("RESIDUALS", "NORMPROB"),
    qqPlot   = res_all || sub_has("RESIDUALS", "NORMPROB"),
    resPlots = grepl("/SCATTERPLOT", txt, fixed = TRUE),
    # The two REQUESTED-BUT-NOT-EMITTED gates (C-0011). Every gate above this
    # line answers "did SPSS ask for this?" in the REMOVING direction only;
    # these two answer it in the other direction, which nothing checked until
    # now. Corpus incidence, measured over all 190 .sps: CHANGE appears on a
    # /STATISTICS line in 27 files (324 occurrences), ZPP in 14 files; control
    # COLLIN, which IS gated above, appears in 15.
    #
    # CHANGE -> jmv's Model Comparisons table. There is NO `modelComp` argument
    # to jmv::linReg (checked against formals(); the 35 names do not include
    # it) -- jmv emits that table automatically and marks it visible only when
    # there are two or more blocks. So this flag does not switch the table on;
    # it decides whether a table jmv produced UNASKED is allowed to reach the
    # report. Without /STATISTICS CHANGE, SPSS prints one Model Summary row per
    # model and no change test at all, so an unrequested Model Comparisons
    # table is the same invention class as the Durbin-Watson one.
    change   = stat_all || sub_has("STATISTICS", "CHANGE"),
    # ZPP -> zero-order / partial / part correlations. jmv::linReg has no
    # equivalent option at all, so this flag only drives a note. Never
    # approximate it with a different correlation.
    zpp      = stat_all || sub_has("STATISTICS", "ZPP"),
    ci_level = .spss_ci_level(txt, "STATISTICS"),

    # The rest of the reverse-direction sweep. Every REGRESSION subcommand
    # keyword the 190-file corpus actually uses was enumerated (998 REGRESSION
    # blocks) and each one is either mapped onto a jmv option above or recorded
    # here as having none. Nothing is approximated by a different statistic.
    #
    #   /STATISTICS  ANOVA 670  COEFF 670  OUTS 670  R 670  CHANGE 324  CI 183
    #                ZPP 85  TOL 53  COLLIN 49  BCOV 48  DEFAULT 3
    #   /RESIDUALS   HISTOGRAM 56  NORMPROB 56  DURBIN 7      (all mapped)
    #   /SAVE        COOK 52  LEVER 34  SRESID 15  MAHAL 12  ZRESID 11 ...
    #   /SCATTERPLOT 57   /DESCRIPTIVES 122   /CASEWISE 6   /PARTIALPLOT 3
    #   /MISSING     LISTWISE 651  PAIRWISE 24
    #   /NOORIGIN    669  (jmv's default; /ORIGIN 0 uses)
    #
    # OUTS and /DESCRIPTIVES are the two that were assumed harmless and are
    # not: a frozen SPSS listing for a `/STATISTICS COEFF OUTS ... /DESCRIPTIVES
    # MEAN STDDEV CORR SIG N` command prints an `Excluded Variables` table and a
    # `Descriptive Statistics` table, and jmv::linReg produces neither.
    # `Excluded Variables` appears in 7 of the 131 frozen listings.
    outs     = stat_all || sub_has("STATISTICS", "OUTS"),
    bcov     = stat_all || sub_has("STATISTICS", "BCOV"),
    descrip  = .spss_has_sub(txt, "DESCRIPTIVES"),
    casewise = .spss_has_sub(txt, "CASEWISE"),
    partplot = .spss_has_sub(txt, "PARTIALPLOT"),
    # jmv::linReg deletes listwise and offers no alternative, so a PAIRWISE
    # request changes the N behind every coefficient. That is a NUMBER
    # difference, not a missing table, which is why it is called out by name.
    pairwise = sub_has("MISSING", "PAIRWISE"),
    # /SAVE writes new columns (COO_1, ZRE_1, ...) into the SPSS dataset.
    # jmv's cooks/mahal are "output variables" in ITS dataset, never in ours,
    # so nothing downstream can reference a saved column.
    saves    = .spss_has_sub(txt, "SAVE")
  )
}

convert_regression <- function(parsed, sav_info) {
  dv <- parsed$variables$dependent
  ivs <- parsed$variables$independent
  method_blocks <- parsed$variables$method_blocks

  if (is.null(dv)) {
    return(list(r_code = "# REGRESSION: Missing dependent variable",
                packages = character(), analysis_type = "Linear Regression"))
  }
  if (length(ivs) == 0) {
    return(list(r_code = "# REGRESSION: No independent variables found",
                packages = character(), analysis_type = "Linear Regression"))
  }

  # Detect stepwise / forward / backward method. SPSS /METHOD=STEPWISE uses
  # F-test entry/removal thresholds (PIN, POUT). We emit olsrr::ols_step_*_p
  # which implements the same p-value-driven selection. SPSS defaults are
  # PIN=0.05, POUT=0.10; honour /CRITERIA=PIN(.x) POUT(.y) when present.
  method <- toupper(parsed$options$method %||% "")
  if (method %in% c("STEPWISE", "FORWARD", "BACKWARD")) {
    crit <- extract_stepwise_pin_pout(parsed$raw %||% "")
    pin  <- crit$pin
    pout <- crit$pout
    rhs_full <- paste(ivs, collapse = " + ")
    iv_names <- paste(paste0('"', ivs, '"'), collapse = ", ")

    step_call <- switch(method,
      "STEPWISE" = sprintf("olsrr::ols_step_both_p(.full, p_enter = %s, p_remove = %s)",
                           format(pin, nsmall = 2), format(pout, nsmall = 2)),
      "FORWARD"  = sprintf("olsrr::ols_step_forward_p(.full, p_val = %s)",
                           format(pin, nsmall = 2)),
      "BACKWARD" = sprintf("olsrr::ols_step_backward_p(.full, p_val = %s)",
                           format(pout, nsmall = 2))
    )

    r_code <- glue::glue('local({{
  .dat <- data[, c("{dv}", {iv_names})]
  .dat <- .dat[stats::complete.cases(.dat), ]
  .full <- stats::lm({dv} ~ {rhs_full}, data = .dat)
  .step <- {step_call}
  cat("\\n**N analysed (listwise):**", nrow(.dat), "\\n\\n")
  cat("\\n## {method} selection (PIN={pin}, POUT={pout}) -- final model\\n")
  print(.step)
  .step
}})')

    return(list(r_code = r_code, packages = "olsrr",
                analysis_type = paste0("Linear Regression (", method, ")"),
                variables = c(dv, ivs)))
  }

  covs_str <- make_vars_str(ivs)

  # Build blocks: if multiple METHOD subcommands, preserve hierarchical structure
  if (!is.null(method_blocks) && length(method_blocks) > 1) {
    blocks_parts <- sapply(method_blocks, function(block) {
      block_norm <- normalize_spss_names(block)
      paste0("list(", paste0('"', block_norm, '"', collapse = ", "), ")")
    })
    blocks_str <- paste0("list(", paste(blocks_parts, collapse = ", "), ")")
  } else {
    blocks_str <- paste0("list(list(", paste0('"', ivs, '"', collapse = ", "), "))")
  }

  # Surface the analysed N (listwise over DV + predictors) alongside the jmv
  # tables: SPSS's ANOVA df only lets a reader RECONSTRUCT N; printing it
  # directly lets them verify missing-data handling matched SPSS.
  ivs_vec <- paste0('"', ivs, '"', collapse = ", ")

  # See .spss_regression_options() above for why each of these is gated and why
  # six of them are not. The order is fixed so the emitted call always ends on
  # `resPlots`, which keeps the jmv call the LAST expression in the chunk --
  # s2r_render_tables() renders the chunk's value, and anything after the call
  # silently produces no tables at all.
  .ropt <- .spss_regression_options(parsed$raw)
  .opts <- c(r = TRUE, r2 = TRUE, r2Adj = TRUE,
             aic = .ropt$aic, bic = .ropt$bic, rmse = .ropt$rmse,
             modelTest = TRUE, anova = TRUE,
             ci = .ropt$ci, stdEst = TRUE, ciStdEst = .ropt$ciStdEst,
             collin = .ropt$collin, cooks = .ropt$cooks, durbin = .ropt$durbin,
             norm = .ropt$norm, qqPlot = .ropt$qqPlot, resPlots = .ropt$resPlots)
  opts_str <- paste0("  ", names(.opts), " = ",
                     ifelse(.opts, "TRUE", "FALSE"), collapse = ",\n")
  # The LEVEL, not just the switch -- see .spss_ci_level(). Emitted whenever an
  # interval is emitted, so `/STATISTICS CI(90)` cannot be published as jmv's
  # 95% default.
  if (.ropt$ci || .ropt$ciStdEst) {
    opts_str <- paste0(opts_str, ",\n  ciWidth = ", .ropt$ci_level)
    if (.ropt$ciStdEst) {
      opts_str <- paste0(opts_str, ",\n  ciWidthStdEst = ", .ropt$ci_level)
    }
  }

  # Notes for what SPSS asked for that jmv cannot give back identically. These
  # are R comments and the analysis chunks are echoed (echo = TRUE in
  # generate_rmd), so they reach the READER of the report, not just the file.
  n_blocks <- if (!is.null(method_blocks)) length(method_blocks) else 1L
  reg_notes <- character()
  if (.ropt$durbin) {
    reg_notes <- c(reg_notes,
      "  # NOTE [SPSS]: /RESIDUALS DURBIN requested. SPSS prints the Durbin-Watson",
      "  # STATISTIC alone -- its Model Summary carries no p-value for it. jmv",
      "  # computes one by SIMULATION and this document sets no seed, so that p",
      "  # changes on every re-knit. It is suppressed; the statistic is reported.")
  }
  if (.ropt$change) {
    reg_notes <- c(reg_notes,
      if (n_blocks > 1L) c(
        "  # NOTE [SPSS]: /STATISTICS CHANGE requested. jmv's Model Comparisons",
        "  # table tests each block against the PREVIOUS one. SPSS additionally",
        "  # prints a change row for the FIRST block against the intercept-only",
        "  # model; jmv produces no such row, so it is absent here.")
      else c(
        "  # NOTE [SPSS]: /STATISTICS CHANGE requested, but this REGRESSION has a",
        "  # single /METHOD block. SPSS would print its change against the",
        "  # intercept-only model; jmv emits no comparison for a single block, so",
        "  # the R-square change table has no equivalent and is not reported."))
  }
  # One grouped note for everything SPSS was asked to print that jmv::linReg
  # has no option for (checked against its 35 formals). Recorded, never
  # approximated by a different statistic, and never dropped in silence.
  unavailable <- c(
    if (.ropt$zpp)      "/STATISTICS ZPP (zero-order, partial and part correlations)",
    # OUTS sits on 670 of the corpus's 679 /STATISTICS lines, and for a single
    # ENTER block there is nothing to exclude, so SPSS prints no such table
    # either and a note would be pure noise. It is only a real omission where
    # SPSS would have had excluded variables to list -- more than one block.
    # (A STEPWISE/FORWARD/BACKWARD method also excludes variables, but those
    # return above on the olsrr path and never reach this code.)
    if (.ropt$outs && n_blocks > 1L)
                        "/STATISTICS OUTS (the Excluded Variables table)",
    if (.ropt$bcov)     "/STATISTICS BCOV (the coefficient covariance matrix)",
    if (.ropt$descrip)  "/DESCRIPTIVES (the Descriptive Statistics and Correlations tables)",
    if (.ropt$casewise) "/CASEWISE (the Casewise Diagnostics table)",
    if (.ropt$partplot) "/PARTIALPLOT (partial regression plots)",
    if (.ropt$saves)    "/SAVE (SPSS writes the saved diagnostics back as new columns; this report does not, so later syntax cannot reference them)"
  )
  if (length(unavailable)) {
    reg_notes <- c(reg_notes,
      "  # NOTE [SPSS]: the syntax also asked for --",
      paste0("  #   ", unavailable),
      "  # jmv::linReg has no option for these, so they are reported as absent",
      "  # rather than replaced by a different statistic.")
  }
  if (.ropt$pairwise) {
    reg_notes <- c(reg_notes,
      "  # WARNING [SPSS]: /MISSING PAIRWISE requested. jmv::linReg deletes",
      "  # cases LISTWISE and offers no alternative, so every coefficient below",
      "  # is estimated on the listwise N printed above, not on SPSS's pairwise",
      "  # Ns. The numbers can differ from the SPSS output.")
  }
  notes_str <- if (length(reg_notes)) paste0(paste(reg_notes, collapse = "\n"), "\n") else ""

  # s2r_spss_reg_tables() WRAPS the jmv call rather than following it. The
  # chunk's VALUE is what s2r_render_tables() renders, so a statement placed
  # after the call would render no tables at all; a wrapper that returns the
  # same results object leaves that value unchanged.
  #
  # Both arguments are always emitted, TRUE or FALSE, so the generated report
  # RECORDS the decision instead of hiding it.
  r_code <- glue::glue('
{{
  cat("**N analysed (listwise):**",
      sum(stats::complete.cases(data[, c("{dv}", {ivs_vec}), drop = FALSE])),
      "\\n\\n")
{notes_str}  s2r_spss_reg_tables(
  jmv::linReg(
  data = data,
  dep = "{dv}",
  covs = {covs_str},
  blocks = {blocks_str},
  refLevels = list(),
{opts_str}
),
  durbin_p = FALSE,
  model_comp = {ifelse(.ropt$change, "TRUE", "FALSE")}
)
}}')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "Linear Regression", variables = c(dv, ivs))
}

# LOGISTIC REGRESSION /PRINT (C-0009). Same fixed-template defect C-0008 fixed
# for linear regression: fourteen jmv::logRegBin options were hard-coded TRUE
# regardless of the command, so an ROC curve, an AUC, AIC, BIC and a McFadden
# R-square were reported for every logistic model whether SPSS produces them or
# not. It ranked below C-0008 for one measured reason -- none of these is
# computed by simulation, so unlike the Durbin-Watson p they were WRONG TO
# INVENT but STABLE.
#
# The mapping below is read off SPSS's own output, not off its documentation.
# Primary sources: the five frozen SPSS listings in the project's test set that
# contain a LOGISTIC REGRESSION `Variables in the Equation` table.
#
# WHAT SPSS PRINTS WITH NO /PRINT AT ALL (measured on a bare `LOGISTIC
# REGRESSION VARIABLES <dv> /METHOD=ENTER <iv> /CRITERIA=...` whose listing
# carries no /PRINT subcommand at all): Omnibus Tests of Model Coefficients;
# Model Summary carrying `-2 Log likelihood`, `Cox & Snell R Square` and
# `Nagelkerke R Square`; Classification Table; Variables in the Equation with
# B, S.E., Wald, df, Sig., Exp(B). Those are the always-on options.
#
# WHAT NO SPSS LOGISTIC REGRESSION PRINTS, two-sided over all 131 frozen SPSS
# listings: `AUC` 0 files, `Area Under the Curve` 0, `ROC Curve` 0, `Akaike` 0
# -- against `Nagelkerke` 5, `Cox & Snell` 5 and `Hosmer` 3 as the presence
# controls that make those zeros real zeros. The one `McFadden` listing is PLUM
# (ordinal regression) and the one `BIC` listing is MIXED, neither of them this
# command. So aic, bic, auc, rocPlot and the r2mf entry of pseudoR2 are off for
# good.
#
# WHAT /PRINT=CI(n) BUYS, and nothing else does -- the gate's two-sided control
# over those five listings:
#
#   /PRINT              Exp(B)   C.I.for EXP(B)
#   GOODFIT CI(95)          2          1
#   GOODFIT CI(95)          2          1
#   CI(95)                 48          8
#   summary GOODFIT         6          0
#   (no /PRINT)             2          0
#
# The interval SPSS adds is for EXP(B) -- the odds ratio -- never for B, so
# `ciOR` is gated on CI and `ci` (jmv's interval around the log-odds estimate)
# is off outright.
#
# `omni` is jmv's per-predictor Omnibus LIKELIHOOD-RATIO test. SPSS tests
# predictors with WALD (in Variables in the Equation) and reserves its own
# "Omnibus Tests of Model Coefficients" for the Step/Block/Model rows, which is
# jmv's `modelTest`. A per-predictor LR table is a third test SPSS never
# printed, so it is off.
.spss_logistic_options <- function(raw) {
  txt <- .spss_sub_text(raw)
  sub_has <- function(kw) .spss_sub_has(txt, "PRINT", kw)
  print_all <- sub_has("ALL")
  list(
    ci_or   = print_all || sub_has("CI"),
    ci_level = .spss_ci_level(txt, "PRINT"),
    # Requested-but-unavailable. Recorded, never approximated with a
    # different statistic -- the reverse-direction check C-0011 exists for.
    goodfit = print_all || sub_has("GOODFIT"),
    corr    = print_all || sub_has("CORR"),
    iter    = print_all || sub_has("ITER"),
    summary = print_all || sub_has("SUMMARY")
  )
}

convert_logistic <- function(parsed, sav_info) {
  dv <- parsed$variables$dependent
  facs <- setdiff(parsed$variables$factors %||% character(), dv)
  ivs <- parsed$variables$independent %||% parsed$variables$all
  ivs <- setdiff(ivs, c(dv, facs))
  if (is.null(dv) || !nzchar(dv)) {
    return(list(r_code = "# LOGISTIC REGRESSION: Missing DV",
                packages = character(), analysis_type = "Logistic Regression"))
  }
  if (length(ivs) + length(facs) == 0) {
    return(list(r_code = paste0(
      "# LOGISTIC REGRESSION: no covariates found for ", dv, ".\n",
      "# SPSS takes them from a WITH clause or from /METHOD; neither named a\n",
      "# variable here, so no model is fitted rather than an empty one."),
      packages = character(), analysis_type = "Logistic Regression"))
  }

  covs_str <- make_vars_str(ivs)
  .lopt <- .spss_logistic_options(parsed$raw)

  # Emitted explicitly TRUE or FALSE, never omitted, so the generated report
  # RECORDS each decision rather than hiding it behind a jmv default.
  .opts <- c(modelTest = TRUE, dev = TRUE,
             aic = FALSE, bic = FALSE,
             omni = FALSE,
             ci = FALSE, OR = TRUE, ciOR = .lopt$ci_or,
             class = TRUE, acc = TRUE, spec = TRUE, sens = TRUE,
             auc = FALSE, rocPlot = FALSE)
  opts_str <- paste0("  ", names(.opts), " = ",
                     ifelse(.opts, "TRUE", "FALSE"), collapse = ",\n")
  # SPSS's `/PRINT CI(n)` sets the LEVEL of the Exp(B) interval. Without this
  # jmv's 95% default was published whatever the syntax asked for. See
  # .spss_ci_level().
  if (.lopt$ci_or) {
    opts_str <- paste0(opts_str, ",\n  ciWidthOR = ", .lopt$ci_level)
  }

  missing_kw <- c(
    if (.lopt$goodfit) "GOODFIT (the Hosmer-Lemeshow test and its contingency table)",
    if (.lopt$corr)    "CORR (the correlation matrix of the parameter estimates)",
    if (.lopt$iter)    "ITER (the iteration history)",
    if (.lopt$summary) "SUMMARY (the step summary)"
  )
  log_notes <- if (length(missing_kw)) paste0(
    "# NOTE [SPSS]: /PRINT asked for ", paste(missing_kw, collapse = "; "),
    ".\n# jmv::logRegBin has no such option -- checked against formals() -- so",
    "\n# it is omitted rather than replaced by a different statistic.\n") else ""

  # An interaction term reaches `covs` as a name no column can match, so the
  # parser separates them out. Report them: dropping a term the researcher
  # asked for without saying so is the silent-omission failure this project
  # exists to avoid.
  if (length(parsed$variables$interactions %||% character())) {
    log_notes <- paste0(log_notes,
      "# NOTE [SPSS]: /METHOD included the interaction term(s) ",
      paste(parsed$variables$interactions, collapse = ", "),
      ".\n# jmv::logRegBin takes variable names only, so the model below is the",
      "\n# MAIN-EFFECTS model and the interaction is NOT in it.\n")
  }

  # A contrast type other than Indicator or Simple codes the levels
  # differently, which changes the coefficients. jmv's `factors` always uses
  # indicator (dummy) coding against the first level, so say so rather than
  # let a Deviation or Helmert contrast pass as if it had been honoured.
  ctypes <- setdiff(toupper(parsed$variables$contrast_types %||% character()),
                    c("INDICATOR", "SIMPLE", ""))
  if (length(ctypes)) {
    log_notes <- paste0(log_notes,
      "# NOTE [SPSS]: /CONTRAST asked for ", paste(ctypes, collapse = ", "),
      " coding.\n# jmv::logRegBin codes factors as INDICATOR against the first",
      " level, so the\n# contrast coefficients below are not the ones SPSS would print.\n")
  }
  # SPSS's stepwise methods fit a REDUCED model. jmv::logRegBin has no
  # equivalent (convert_regression() reaches olsrr for linear stepwise; there
  # is no logistic counterpart in this stack), so forcing every covariate in
  # would publish different coefficients under the researcher's method name.
  # 0 corpus blocks use it, against 114 for /METHOD=ENTER as the control.
  nonenter <- setdiff(toupper(parsed$variables$methods %||% character()),
                      c("ENTER", ""))
  if (length(nonenter)) {
    log_notes <- paste0(log_notes,
      "# WARNING [SPSS]: /METHOD=", paste(nonenter, collapse = "/"),
      " is a STEPWISE method -- SPSS drops\n# predictors that do not meet its",
      " entry/removal criteria. jmv::logRegBin has no\n# stepwise equivalent, so",
      " the model below FORCES EVERY PREDICTOR IN and its\n# coefficients are",
      " not the ones SPSS reported.\n")
  }
  # /SELECT restricts SPSS to a subgroup for this command only.
  if (length(parsed$variables$select_clause %||% character())) {
    log_notes <- paste0(log_notes,
      "# WARNING [SPSS]: `", parsed$variables$select_clause,
      "` restricts this analysis to a\n# subgroup. It is not applied here, so the",
      " model below is fitted on ALL cases.\n")
  }

  facs_line <- if (length(facs)) {
    paste0("\n  factors = ", make_vars_str(facs), ",")
  } else ""
  covs_line <- if (length(ivs)) {
    paste0("\n  covs = ", covs_str, ",")
  } else ""
  block_vars <- c(ivs, facs)

  r_code <- glue::glue('
{log_notes}jmv::logRegBin(
  data = data,
  dep = "{dv}",{covs_line}{facs_line}
  blocks = list(list({paste0(\'"\', block_vars, \'"\', collapse = ", ")})),
  refLevels = list(),
  pseudoR2 = c("r2cs", "r2n"),
{opts_str}
)')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "Logistic Regression",
       variables = c(dv, ivs, facs))
}

convert_reliability <- function(parsed, sav_info) {
  vars <- parsed$variables$all
  if (length(vars) == 0) {
    return(list(r_code = "# RELIABILITY: No variables specified",
                packages = character(), analysis_type = "Reliability Analysis"))
  }
  vars_str <- make_vars_str(vars)

  # No omega: SPSS RELIABILITY (/MODEL=ALPHA) computes Cronbach's alpha,
  # scale statistics, and item-total statistics -- it never computes
  # McDonald's omega. Emitting omegaScale/omegaItems fabricated scale- and
  # item-level statistics the source software's output does not contain
  # (caught by the 2026-08-04 Sonnet canary audit on sample4).
  r_code <- glue::glue('
jmv::reliability(
  data = data,
  vars = {vars_str},
  alphaScale = TRUE,
  meanScale = TRUE,
  sdScale = TRUE,
  corPlot = TRUE,
  alphaItems = TRUE,
  meanItems = TRUE,
  sdItems = TRUE,
  itemRestCor = TRUE
)')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "Reliability Analysis", variables = vars)
}

convert_crosstabs <- function(parsed, sav_info) {
  row_var <- parsed$variables$row
  col_var <- parsed$variables$column
  if (is.null(row_var) || is.null(col_var)) {
    return(list(r_code = "# CROSSTABS: Missing row or column variable",
                packages = character(), analysis_type = "Chi-Square / Crosstabulation"))
  }

  r_code <- glue::glue('
jmv::contTables(
  data = data,
  rows = "{row_var}",
  cols = "{col_var}",
  obs = TRUE,
  exp = TRUE,
  pcRow = TRUE,
  pcCol = TRUE,
  pcTot = TRUE,
  chiSq = TRUE,
  chiSqCorr = TRUE,
  likeRat = TRUE,
  fisher = TRUE,
  contCoef = TRUE,
  phiCra = TRUE,
  odds = TRUE,
  relRisk = TRUE,
  ci = TRUE,
  gamma = TRUE,
  taub = TRUE
)')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "Chi-Square / Crosstabulation", variables = c(row_var, col_var))
}

convert_means <- function(parsed, sav_info) {
  vars <- parsed$variables$all
  if (length(vars) == 0) {
    return(list(r_code = "# MEANS: No variables specified",
                packages = character(), analysis_type = "Means"))
  }
  vars_str <- make_vars_str(vars)

  r_code <- glue::glue('
jmv::descriptives(
  data = data,
  vars = {vars_str},
  mean = TRUE,
  sd = TRUE,
  se = TRUE,
  ci = TRUE,
  min = TRUE,
  max = TRUE
)')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "Means", variables = vars)
}

convert_examine <- function(parsed, sav_info) {
  # SPSS EXAMINE VARIABLES = <dvs> BY <factor> requests descriptives of the
  # dependent variables split by the grouping factor. Use the dependent list
  # for `vars` (falling back to `all` for older parses that lack it) and map
  # any grouping factors to jmv::descriptives' `splitBy`. Passing the factor
  # in `vars` would (wrongly) compute descriptives of the factor itself.
  dvs <- parsed$variables$dependent %||% parsed$variables$all
  factors <- parsed$variables$factors %||% character()
  if (length(dvs) == 0) {
    return(list(r_code = "# EXAMINE: No variables specified",
                packages = character(), analysis_type = "Explore"))
  }
  vars_str <- make_vars_str(dvs)
  ex_plots <- .spss_examine_plots(parsed$raw)
  ex_note <- if (length(ex_plots$unconverted)) {
    paste0("\n# NOTE: /PLOT ", paste(ex_plots$unconverted, collapse = " "),
           " requested; jmv has no such plot, so none is drawn.")
  } else ""
  split_line <- if (length(factors) > 0) {
    paste0("\n  splitBy = ", make_vars_str(factors), ",")
  } else ""

  r_code <- glue::glue('
jmv::descriptives(
  data = data,
  vars = {vars_str},{split_line}
  mean = TRUE,
  median = TRUE,
  sd = TRUE,
  se = TRUE,
  ci = TRUE,
  iqr = TRUE,
  range = TRUE,
  skew = TRUE,
  kurt = TRUE,
  sw = TRUE,
  hist = {toupper(ex_plots$hist)},
  dens = {toupper(ex_plots$dens)},
  box = {toupper(ex_plots$box)},
  qq = {toupper(ex_plots$qq)}
){ex_note}')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "Explore", variables = c(dvs, factors))
}

#' Convert AGGREGATE (MODE = ADDVARIABLES) -> grouped dplyr::mutate
#'
#' Handles the common "add aggregate columns back onto the active dataset" form:
#'   AGGREGATE /OUTFILE = * MODE = ADDVARIABLES /BREAK = g1 g2
#'     /newvar1 = MEAN(var) /newvar2 = SUM(var2) ...
#' A `/BREAK` with variables becomes group_by(); an empty/absent BREAK is a
#' whole-sample aggregate repeated down every row. Aggregate functions are
#' mapped to their R equivalents. Previously AGGREGATE was emitted as an
#' "Unsupported command" comment, so the aggregate columns were never created
#' and any downstream `COMPUTE x = var - var_mean` produced all-NA, which then
#' made jmv analyses fail (e.g. "Argument 'covs' contains" for a mean-centered
#' predictor that resolved to an empty column).
#' @param parsed Parsed SPSS command object
#' @param sav_info SAV file information from parse_sav()
#' @keywords internal
convert_aggregate <- function(parsed, sav_info) {
  raw <- parsed$raw
  # Helper: capture group 1 of a perl regex (so `\n` in a bracket class means a
  # newline, not the literal letters "\" and "n" -- the latter would truncate a
  # variable list at the first "n", e.g. "gender" -> "ge").
  perl_cap <- function(pat, s) {
    m <- regmatches(s, regexec(pat, s, ignore.case = TRUE, perl = TRUE))[[1]]
    if (length(m) >= 2) trimws(m[2]) else NULL
  }
  # OUTFILE that is not the active dataset (a real file) isn't an add-variables
  # aggregate; we cannot reproduce a separate aggregated file, so pass through.
  outfile <- perl_cap("/OUTFILE\\s*=\\s*([^/\\r\\n]+)", raw)
  is_addvars <- grepl("ADDVARIABLES", raw, ignore.case = TRUE) ||
    (!is.null(outfile) && grepl("^\\s*\\*", outfile))
  if (!is.null(outfile) && !grepl("\\*", outfile) && !is_addvars) {
    return(convert_unsupported(parsed, sav_info))
  }

  # BREAK variables (optional; may be empty).
  break_clause <- perl_cap("/BREAK\\s*=\\s*([^/\\r\\n]*)", raw)
  break_vars <- character()
  if (!is.null(break_clause) && nzchar(trimws(break_clause))) {
    break_vars <- trimws(strsplit(trimws(break_clause), "[,[:space:]]+")[[1]])
    break_vars <- normalize_spss_names(break_vars[nchar(break_vars) > 0])
  }

  # Aggregate assignments: /newvar = FUNC(args). Scan every "/name = FUNC(...)".
  assigns <- regmatches(raw, gregexpr(
    "/\\s*([A-Za-z_][\\w.]*)\\s*=\\s*([A-Za-z]+)\\s*\\(([^)]*)\\)",
    raw, perl = TRUE))[[1]]
  # SPSS aggregate-function -> R expression builder (operating on column `col`).
  agg_expr <- function(fun, col) {
    fun <- toupper(fun)
    switch(fun,
      "MEAN"   = sprintf("mean(%s, na.rm = TRUE)", col),
      "SUM"    = sprintf("sum(%s, na.rm = TRUE)", col),
      "SD"     = sprintf("sd(%s, na.rm = TRUE)", col),
      "MIN"    = sprintf("min(%s, na.rm = TRUE)", col),
      "MAX"    = sprintf("max(%s, na.rm = TRUE)", col),
      "MEDIAN" = sprintf("median(%s, na.rm = TRUE)", col),
      "N"      = sprintf("sum(!is.na(%s))", col),
      "NU"     = sprintf("sum(!is.na(%s))", col),
      "FIRST"  = sprintf("dplyr::first(%s)", col),
      "LAST"   = sprintf("dplyr::last(%s)", col),
      "SD"     = sprintf("sd(%s, na.rm = TRUE)", col),
      NULL)
  }
  mutate_lines <- character()
  for (a in assigns) {
    m <- regmatches(a, regexec(
      "/\\s*([A-Za-z_][\\w.]*)\\s*=\\s*([A-Za-z]+)\\s*\\(([^)]*)\\)", a, perl = TRUE))[[1]]
    if (length(m) < 4) next
    tgt <- normalize_spss_names(trimws(m[2]))
    fun <- trimws(m[3])
    arg <- normalize_spss_names(trimws(m[4]))
    if (!is_valid_r_ident(tgt) || !nzchar(arg)) next
    ex <- agg_expr(fun, arg)
    if (is.null(ex)) next  # unsupported aggregate function -> skip this pair
    mutate_lines <- c(mutate_lines, sprintf("    `%s` = %s", tgt, ex))
  }

  if (length(mutate_lines) == 0) {
    return(convert_unsupported(parsed, sav_info))
  }

  mutate_body <- paste(mutate_lines, collapse = ",\n")
  if (length(break_vars) > 0) {
    grp <- paste(sprintf("`%s`", break_vars), collapse = ", ")
    r_code <- glue::glue(
      "data <- data |>\n  dplyr::group_by({grp}) |>\n  dplyr::mutate(\n{mutate_body}\n  ) |>\n  dplyr::ungroup()")
  } else {
    r_code <- glue::glue(
      "data <- data |>\n  dplyr::mutate(\n{mutate_body}\n  )")
  }

  list(r_code = r_code, packages = "dplyr",
       analysis_type = "Data Transformation (AGGREGATE)",
       is_transformation = TRUE)
}

convert_factor <- function(parsed, sav_info) {
  vars <- parsed$variables$all
  if (length(vars) == 0) {
    return(list(r_code = "# FACTOR: No variables specified",
                packages = character(), analysis_type = "Factor Analysis"))
  }
  vars_str <- make_vars_str(vars)

  # Engine: psych, NOT jmv::efa.
  #
  # jmv::efa is non-deterministically broken in the worker environment: the
  # byte-identical call in fresh isolated R processes failed 8/8 in one batch
  # with "'names' attribute [N] must be the same length as the vector [0]" and
  # succeeded 4/4 in another. It is not data-determined -- missing variables,
  # full-frame vs subset, haven_labelled columns, and CPU contention were each
  # tested and refuted. psych is already a declared dependency, is
  # deterministic, and reproduces SPSS EXACTLY (verified 2026-08-04 against
  # SPSS ground truth on Driver_Data.sav: KMO .833, Bartlett chi-square
  # 905.322 df 28, and all 8 communalities identical to 3 dp -- see
  # tests/testthat/test-factor-psych-engine.R).
  #
  # SPSS FACTOR defaults mirrored here: /MISSING PAIRWISE -> pairwise
  # correlations; /CRITERIA MINEIGEN(1) -> retain eigenvalue > 1;
  # /EXTRACTION PC -> principal components; /ROTATION VARIMAX.
  r_code <- glue::glue('
local({{
  .vars <- {vars_str}
  .x <- as.data.frame(lapply(data[, .vars, drop = FALSE], as.numeric))
  .R <- stats::cor(.x, use = "pairwise.complete.obs")
  .n <- sum(stats::complete.cases(.x))
  .nf <- max(1L, sum(eigen(.R)$values > 1))
  .pc <- psych::principal(.R, nfactors = .nf, rotate = "varimax")
  list(
    kmo          = psych::KMO(.R),
    bartlett     = psych::cortest.bartlett(.R, n = .n),
    n            = .n,
    nfactors     = .nf,
    communality  = .pc$communality,
    loadings     = unclass(.pc$loadings),
    eigenvalues  = eigen(.R)$values
  )
}})')

  list(r_code = r_code, packages = "psych",
       analysis_type = "Factor Analysis", variables = vars)
}

convert_npar_tests <- function(parsed, sav_info) {
  npar_type <- parsed$variables$npar_type

  if (is.null(npar_type)) {
    return(list(
      r_code = paste0("# NPAR TESTS: Could not determine subtest type\n# ", parsed$raw),
      packages = character(), analysis_type = "Non-Parametric Tests"
    ))
  }

  switch(npar_type,
    "MANN_WHITNEY" = {
      dv <- parsed$variables$dependent
      fct <- parsed$variables$factor
      if (is.null(dv) || is.null(fct)) {
        return(list(r_code = "# NPAR /M-W: Missing DV or factor",
                    packages = character(), analysis_type = "Mann-Whitney U"))
      }
      r_code <- glue::glue('
jmv::ttestIS(
  data = data,
  vars = c("{dv}"),
  group = "{fct}",
  students = FALSE,
  welchs = FALSE,
  mann = TRUE,
  meanDiff = TRUE,
  effectSize = TRUE,
  desc = TRUE
)')
      list(r_code = r_code, packages = "jmv",
           analysis_type = "Mann-Whitney U Test", variables = c(dv, fct))
    },

    "WILCOXON" = {
      v1 <- parsed$variables$var1
      v2 <- parsed$variables$var2
      if (is.null(v1) || is.null(v2)) {
        return(list(r_code = "# NPAR /WILCOXON: Missing paired variables",
                    packages = character(), analysis_type = "Wilcoxon Signed-Rank"))
      }
      r_code <- glue::glue('
jmv::ttestPS(
  data = data,
  pairs = list(list(i1 = "{v1}", i2 = "{v2}")),
  students = FALSE,
  wilcoxon = TRUE,
  meanDiff = TRUE,
  effectSize = TRUE,
  desc = TRUE
)')
      list(r_code = r_code, packages = "jmv",
           analysis_type = "Wilcoxon Signed-Rank Test", variables = c(v1, v2))
    },

    "KRUSKAL_WALLIS" = {
      dv <- parsed$variables$dependent
      fct <- parsed$variables$factor
      if (is.null(dv) || is.null(fct)) {
        return(list(r_code = "# NPAR /K-W: Missing DV or factor",
                    packages = character(), analysis_type = "Kruskal-Wallis"))
      }
      r_code <- glue::glue('
jmv::anovaOneW(
  data = data,
  deps = c("{dv}"),
  group = "{fct}",
  fishers = FALSE,
  welchs = FALSE,
  kruskal = TRUE,
  desc = TRUE,
  descPlot = TRUE
)')
      list(r_code = r_code, packages = "jmv",
           analysis_type = "Kruskal-Wallis Test", variables = c(dv, fct))
    },

    "CHISQUARE" = {
      vars <- parsed$variables$all
      if (length(vars) == 0) {
        return(list(r_code = "# NPAR /CHISQUARE: No variable specified",
                    packages = character(), analysis_type = "Chi-Square Goodness of Fit"))
      }
      var_str <- vars[1]
      r_code <- glue::glue('
# Chi-Square Goodness of Fit Test
chisq_result <- chisq.test(table(data[["{var_str}"]]))
chisq_result')
      list(r_code = r_code, packages = character(),
           analysis_type = "Chi-Square Goodness of Fit", variables = vars)
    },

    "KOLMOGOROV_SMIRNOV" = {
      vars <- parsed$variables$all
      if (length(vars) == 0) {
        return(list(r_code = "# NPAR /K-S: No variable specified",
                    packages = character(), analysis_type = "Kolmogorov-Smirnov"))
      }
      var_str <- vars[1]
      r_code <- glue::glue('
# One-Sample Kolmogorov-Smirnov Test (vs Normal distribution)
x <- data[["{var_str}"]][!is.na(data[["{var_str}"]])]
ks_result <- ks.test(x, "pnorm", mean(x), sd(x))
ks_result')
      list(r_code = r_code, packages = character(),
           analysis_type = "Kolmogorov-Smirnov Test", variables = vars)
    },

    "BINOMIAL" = {
      vars <- parsed$variables$all
      prop <- parsed$variables$test_prop %||% 0.5
      if (length(vars) == 0) {
        return(list(r_code = "# NPAR /BINOMIAL: No variable specified",
                    packages = character(), analysis_type = "Binomial Test"))
      }
      var_str <- vars[1]
      r_code <- glue::glue('
# Binomial Test
tbl <- table(data[["{var_str}"]][!is.na(data[["{var_str}"]])])
binom_result <- binom.test(tbl[1], sum(tbl), p = {prop})
binom_result')
      list(r_code = r_code, packages = character(),
           analysis_type = "Binomial Test", variables = vars)
    },

    "FRIEDMAN" = {
      vars <- parsed$variables$all
      if (length(vars) < 2) {
        return(list(r_code = "# NPAR /FRIEDMAN: Need at least 2 variables",
                    packages = character(), analysis_type = "Friedman Test"))
      }
      vars_str <- paste0('"', vars, '"', collapse = ", ")
      r_code <- glue::glue('
# Friedman Rank Sum Test
friedman_data <- as.matrix(data[, c({vars_str})])
friedman_data <- friedman_data[complete.cases(friedman_data), ]
friedman_result <- friedman.test(friedman_data)
friedman_result')
      list(r_code = r_code, packages = character(),
           analysis_type = "Friedman Test", variables = vars)
    },

    "SIGN" = {
      v1 <- parsed$variables$var1
      v2 <- parsed$variables$var2
      if (is.null(v1) || is.null(v2)) {
        return(list(r_code = "# NPAR /SIGN: Missing paired variables",
                    packages = character(), analysis_type = "Sign Test"))
      }
      r_code <- glue::glue('
# Sign Test
diffs <- data[["{v1}"]] - data[["{v2}"]]
diffs <- diffs[!is.na(diffs) & diffs != 0]
n_pos <- sum(diffs > 0)
n_total <- length(diffs)
sign_result <- binom.test(n_pos, n_total, p = 0.5)
cat("Sign Test\\n")
cat(sprintf("  Positive differences: %d\\n", n_pos))
cat(sprintf("  Negative differences: %d\\n", n_total - n_pos))
cat(sprintf("  Exact Sig. (2-tailed): %.4f\\n", sign_result$p.value))')
      list(r_code = r_code, packages = character(),
           analysis_type = "Sign Test", variables = c(v1, v2))
    },

    "RUNS" = {
      vars <- parsed$variables$all
      if (length(vars) == 0) {
        return(list(r_code = "# NPAR /RUNS: No variable specified",
                    packages = character(), analysis_type = "Runs Test"))
      }
      var_str <- vars[1]
      r_code <- glue::glue('
# Runs Test (around mean)
x <- data[["{var_str}"]][!is.na(data[["{var_str}"]])]
med <- mean(x)
binary <- x > med
runs <- sum(diff(as.numeric(binary)) != 0) + 1
n1 <- sum(binary); n2 <- sum(!binary)
exp_runs <- 1 + (2 * n1 * n2) / (n1 + n2)
var_runs <- (2 * n1 * n2 * (2 * n1 * n2 - n1 - n2)) / ((n1 + n2)^2 * (n1 + n2 - 1))
z <- (runs - exp_runs) / sqrt(var_runs)
p <- 2 * pnorm(-abs(z))
cat(sprintf("Runs Test\\n  Test Value (Mean): %.2f\\n  Runs: %d\\n  Z: %.3f\\n  Sig.: %.4f\\n", med, runs, z, p))')
      list(r_code = r_code, packages = character(),
           analysis_type = "Runs Test", variables = vars)
    },

    "MEDIAN" = {
      dv <- parsed$variables$dependent
      fct <- parsed$variables$factor
      if (is.null(dv) || is.null(fct)) {
        return(list(r_code = "# NPAR /MEDIAN: Missing DV or factor",
                    packages = character(), analysis_type = "Median Test"))
      }
      r_code <- glue::glue('
# Median Test
x <- data[["{dv}"]]; g <- data[["{fct}"]]
complete <- !is.na(x) & !is.na(g)
x <- x[complete]; g <- g[complete]
med <- median(x)
above <- x > med
tbl <- table(above, g)
median_result <- chisq.test(tbl, correct = FALSE)
cat("Median Test\\n")
cat(sprintf("  Grand Median: %.2f\\n", med))
median_result')
      list(r_code = r_code, packages = character(),
           analysis_type = "Median Test", variables = c(dv, fct))
    },

    # Default fallback
    {
      list(
        r_code = paste0("# NPAR TESTS: Unrecognized subtest type '", npar_type, "'\n# ", parsed$raw),
        packages = character(), analysis_type = "Non-Parametric Tests"
      )
    }
  )
}

convert_roc <- function(parsed, sav_info) {
  test_var <- parsed$variables$test_var
  state_var <- parsed$variables$state_var
  pos_val <- parsed$variables$positive_value %||% "1"

  if (is.null(test_var) || is.null(state_var)) {
    return(list(r_code = "# ROC: Missing test or state variable",
                packages = character(), analysis_type = "ROC Analysis"))
  }

  r_code <- glue::glue('
# ROC Curve Analysis
x <- data[["{test_var}"]]; state <- data[["{state_var}"]]
complete <- !is.na(x) & !is.na(state)
x <- x[complete]; state <- state[complete]
positive <- as.numeric(state) == {pos_val}

# Calculate AUC using Wilcoxon statistic
n_pos <- sum(positive); n_neg <- sum(!positive)
if (n_pos > 0 && n_neg > 0) {{
  U <- wilcox.test(x[positive], x[!positive])$statistic
  auc <- U / (n_pos * n_neg)
  cat(sprintf("ROC Analysis\\n  Variable: {test_var}\\n  AUC: %.4f\\n  N (positive): %d\\n  N (negative): %d\\n", auc, n_pos, n_neg))
  # Plot ROC curve
  thresholds <- sort(unique(c(-Inf, x, Inf)))
  sens <- sapply(thresholds, function(t) mean(x[positive] >= t))
  spec <- sapply(thresholds, function(t) mean(x[!positive] < t))
  plot(1 - spec, sens, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity",
       main = "ROC Curve", col = "blue", lwd = 2)
  abline(0, 1, lty = 2, col = "gray")
}}')

  list(r_code = r_code, packages = character(),
       analysis_type = "ROC Analysis", variables = c(test_var, state_var))
}

convert_quick_cluster <- function(parsed, sav_info) {
  vars <- parsed$variables$all
  k <- parsed$variables$n_clusters %||% 3

  if (length(vars) == 0) {
    return(list(r_code = "# QUICK CLUSTER: No variables specified",
                packages = character(), analysis_type = "K-Means Cluster"))
  }

  vars_str <- paste0('"', vars, '"', collapse = ", ")

  r_code <- glue::glue('
# K-Means Cluster Analysis
cluster_vars <- c({vars_str})
cluster_data <- data[, cluster_vars]
cluster_data <- cluster_data[complete.cases(cluster_data), ]
cluster_data_scaled <- scale(cluster_data)
set.seed(42)
km <- kmeans(cluster_data_scaled, centers = {k}, nstart = 25)
cat("K-Means Cluster Analysis\\n")
cat(sprintf("  Number of clusters: %d\\n", {k}))
cat("\\nFinal Cluster Centers (standardized):\\n")
print(round(km$centers, 2))
cat("\\nCluster Sizes:\\n")
print(km$size)')

  list(r_code = r_code, packages = character(),
       analysis_type = "K-Means Cluster Analysis", variables = vars)
}

convert_rank <- function(parsed, sav_info) {
  vars <- parsed$variables$rank_vars
  if (is.null(vars) || length(vars) == 0) vars <- parsed$variables$all
  if (length(vars) == 0) vars <- extract_variables_clause(parsed$raw)
  vars <- normalize_spss_names(vars)

  if (length(vars) == 0) {
    return(list(r_code = "# RANK: No variables specified",
                packages = character(), analysis_type = "Rank Cases",
                is_transformation = TRUE))
  }

  groups <- normalize_spss_names(parsed$variables$rank_groups %||% character())
  direction <- parsed$variables$rank_direction %||% "A"
  desc_wrap <- function(v) if (toupper(direction) == "D") sprintf("dplyr::desc(%s)", v) else v

  if (length(groups) > 0) {
    # RANK var BY group (A) -> arrange + group_by + mutate(rank())
    group_str <- paste(groups, collapse = ", ")
    arrange_args <- paste(c(groups, sapply(vars, desc_wrap)), collapse = ", ")
    rank_exprs <- vapply(vars, function(v) {
      sprintf("R%s = rank(%s, na.last = 'keep')", v, v)
    }, character(1))
    mutate_args <- paste(rank_exprs, collapse = ",\n    ")
    r_code <- glue::glue('# RANK {paste(vars, collapse=" ")} BY {paste(groups, collapse=" ")} ({direction})
data <- data |>
  dplyr::arrange({arrange_args}) |>
  dplyr::group_by({group_str}) |>
  dplyr::mutate(
    {mutate_args}
  ) |>
  dplyr::ungroup()')
  } else {
    # Simple case: RANK var [(A)|(D)]
    rank_exprs <- vapply(vars, function(v) {
      x <- if (toupper(direction) == "D") sprintf("-data[['%s']]", v) else sprintf("data[['%s']]", v)
      sprintf("data[['R%s']] <- rank(%s, na.last = 'keep')", v, x)
    }, character(1))
    r_code <- paste(c(sprintf("# RANK: Create rank variables (direction=%s)", direction),
                      rank_exprs), collapse = "\n")
  }

  list(r_code = r_code, packages = if (length(groups) > 0) "dplyr" else character(),
       analysis_type = "Rank Cases", variables = c(vars, groups),
       is_transformation = TRUE)
}

convert_mixed <- function(parsed, sav_info) {
  r_code <- '
# Mixed / Multilevel Model
# Note: jmv does not have a mixed model function. Use lme4:
# library(lme4)
# model <- lmer(dv ~ fixed_effects + (1 | random_factor), data = data)
# summary(model)

message("Mixed models require lme4 - see example above")'

  list(r_code = r_code, packages = "lme4",
       analysis_type = "Mixed Model", variables = parsed$variables$all)
}

# ==============================================================================
# DATA TRANSFORMATION CONVERTERS
# ==============================================================================

convert_compute <- function(parsed, sav_info) {
  target <- parsed$variables$target
  expr <- parsed$variables$expression

  if (is.null(target) || is.null(expr)) {
    return(list(
      r_code = paste0("# COMPUTE: Could not parse - ", parsed$raw),
      packages = "dplyr", analysis_type = "Data Transformation (COMPUTE)",
      is_transformation = TRUE
    ))
  }

  # Expand SPSS TO ranges (e.g., "A11 TO A18") using variable order from SAV
  expr <- expand_spss_to_ranges(expr, sav_info)

  target <- normalize_spss_names(target)
  # R-0009: guard against a target that fails to normalize to a valid R
  # identifier (e.g. an unexpanded macro that collapses to "") -- emit a NOTE
  # instead of unparseable `dplyr::mutate( = ...)`.
  if (!is_valid_r_ident(target)) {
    return(spss_invalid_ident_note("COMPUTE", parsed$raw,
                                   "Data Transformation (COMPUTE)"))
  }
  r_expr <- convert_spss_expression(expr)

  # R-0096: $CASENUM -> dplyr::row_number() is only correct if this COMPUTE
  # runs on data still in original SPSS case order (no prior SELECT IF /
  # FILTER / SORT CASES / SPLIT FILE in the same .sps). convert_spss_expression
  # has no visibility into surrounding commands to verify that, so surface a
  # visible caveat here rather than silently trusting the substitution --
  # consistent with this codebase's rule that risky/unverifiable translations
  # must be flagged, never silently assumed correct.
  casenum_note <- if (grepl("\\$CASENUM\\b", expr, ignore.case = TRUE, perl = TRUE)) {
    paste0(
      "# NOTE [SPSS]: $CASENUM translated to dplyr::row_number(), which is ",
      "only equivalent to SPSS's case number if no SELECT IF/FILTER/SORT ",
      "CASES/SPLIT FILE precedes this line -- verify against the original ",
      "case order if any of those appear earlier in this syntax file.\n"
    )
  } else {
    ""
  }

  r_code <- paste0(casenum_note, glue::glue("
data <- data |>
  dplyr::mutate({target} = {r_expr})"))

  list(r_code = r_code, packages = "dplyr",
       analysis_type = "Data Transformation (COMPUTE)",
       variables = target, is_transformation = TRUE)
}

#' Convert COUNT command -> rowSums of logical checks
#' @param parsed Parsed SPSS command object
#' @param sav_info SAV file information from parse_sav()
#' @keywords internal
#' Build a match-predicate factory from an SPSS COUNT criterion
#'
#' Returns a function that, given an R expression for a column, produces the
#' R predicate testing whether that column matches the SPSS criterion.
#' A criterion is a comma/space separated list of TERMS, each of which is a
#' single value, a range, or the MISSING/SYSMIS keyword; a value matches when
#' it satisfies ANY term. Supported forms:
#'   \code{1}                single value    -> \code{x == 1}
#'   \code{1,2,3}            value list      -> \code{x \%in\% c(1, 2, 3)}
#'   \code{1 THRU 3}         inclusive range -> \code{x >= 1 & x <= 3}
#'   \code{LO THRU 3} / \code{1 THRU HI}     -> open-ended range
#'   \code{MISSING} / \code{SYSMIS}          -> \code{is.na(x)}
#'   \code{MISSING, LO THRU -1}              -> mixed list of the above
#' SPSS accepts \code{THR}/\code{THRU}/\code{THROUGH} as spellings of the
#' range keyword. An unrecognised term falls back to equality against its
#' literal text so behaviour is never silently dropped.
#' @param count_value Raw text from inside the COUNT parentheses.
#' @keywords internal
.s2r_count_match_expr <- function(count_value) {
  crit <- trimws(as.character(count_value)[1])
  thru_re <- "\\b(THRU|THR|THROUGH)\\b"

  # Split the criterion into terms. Commas always separate; bare whitespace
  # separates too, EXCEPT inside a "<lo> THRU <hi>" range, so protect ranges
  # first by rewriting their internal spaces to a sentinel.
  protected <- gsub(paste0("([^,[:space:]]+)\\s+", thru_re, "\\s+([^,[:space:]]+)"),
                    "\\1\001\\2\001\\3", crit, ignore.case = TRUE, perl = TRUE)
  terms <- trimws(strsplit(protected, "[,[:space:]]+")[[1]])
  terms <- terms[nzchar(terms)]

  # Build one predicate per term; a value matches if ANY term matches.
  preds <- lapply(terms, function(term) {
    parts <- strsplit(term, "\001", fixed = TRUE)[[1]]
    if (length(parts) == 3) {           # a protected range
      lo <- trimws(parts[1]); hi <- trimws(parts[3])
      lo_open <- grepl("^(LO|LOWEST)$", lo, ignore.case = TRUE)
      hi_open <- grepl("^(HI|HIGHEST)$", hi, ignore.case = TRUE)
      return(function(x) {
        bounds <- c(if (!lo_open) paste0(x, " >= ", lo),
                    if (!hi_open) paste0(x, " <= ", hi))
        if (!length(bounds)) paste0("!is.na(", x, ")")
        else paste0("(", paste(bounds, collapse = " & "), ")")
      })
    }
    if (grepl("^(MISSING|SYSMIS)$", term, ignore.case = TRUE)) {
      return(function(x) paste0("is.na(", x, ")"))
    }
    function(x) paste0(x, " == ", term)
  })

  if (!length(preds)) return(function(x) paste0(x, " == ", crit)) # keep literal

  # A pure list of plain values reads better (and shorter) as %in%.
  plain <- vapply(terms, function(t)
    !grepl("\001", t, fixed = TRUE) &&
    !grepl("^(MISSING|SYSMIS)$", t, ignore.case = TRUE), logical(1))
  if (length(terms) > 1 && all(plain)) {
    return(function(x) paste0(x, " %in% c(", paste(terms, collapse = ", "), ")"))
  }
  if (length(preds) == 1) return(preds[[1]])

  function(x) paste0("(", paste(vapply(preds, function(f) f(x), character(1)),
                                collapse = " | "), ")")
}

convert_count <- function(parsed, sav_info) {
  target <- parsed$variables$target
  varlist_raw <- parsed$variables$varlist_raw
  count_value <- parsed$variables$count_value

  if (is.null(target) || is.null(varlist_raw)) {
    return(list(
      r_code = paste0("# COUNT: Could not parse - ", parsed$raw),
      packages = "dplyr", analysis_type = "Data Transformation (COUNT)",
      is_transformation = TRUE
    ))
  }

  target <- normalize_spss_names(target)

  # Parse variable list (may contain TO)
  raw_vars <- trimws(strsplit(varlist_raw, "[,[:space:]]+")[[1]])
  raw_vars <- raw_vars[nchar(raw_vars) > 0]

  # Expand TO syntax
  all_names <- normalize_spss_names(sav_info$metadata$name)
  norm_vars <- normalize_spss_names(raw_vars)

  expanded <- expand_to_syntax(norm_vars, all_names)

  # Build the per-variable match test. `count_value` is the RAW text inside the
  # parentheses, which SPSS allows to be more than a single value:
  #   (1)          single value
  #   (1,2,3)      value LIST  -- match ANY of them
  #   (1 THRU 3)   inclusive RANGE
  # Interpolating that text straight into `== {count_value}` produced
  # `== 1,2,3` / `== 1 THRU 3`, neither of which is valid R. knitr then failed
  # the whole chunk, losing EVERY COUNT in the file rather than just this one.
  match_expr <- .s2r_count_match_expr(count_value)

  # SPSS COUNT never yields missing: a value that is missing simply does not
  # match, and the count stays defined. Guard each test so NA contributes 0
  # rather than propagating NA through the sum. The one exception is a
  # criterion that deliberately counts missings (MISSING / SYSMIS) -- there a
  # `!is.na(x) &` guard would cancel the very thing being counted, so wrap in
  # isTRUE-style NA coercion instead of excluding NA rows.
  counts_missing <- grepl("\\b(MISSING|SYSMIS)\\b", count_value, ignore.case = TRUE)
  checks <- vapply(expanded, function(v) {
    x <- paste0("data[['", v, "']]")
    if (counts_missing) {
      # NA-safe truthiness: NA in the predicate becomes FALSE (contributes 0).
      paste0("(!is.na(", match_expr(x), ") & ", match_expr(x), ")")
    } else {
      paste0("(!is.na(", x, ") & ", match_expr(x), ")")
    }
  }, character(1))
  checks <- paste(checks, collapse = " + ")

  r_code <- glue::glue("
data[['{target}']] <- {checks}")

  list(r_code = r_code, packages = character(),
       analysis_type = "Data Transformation (COUNT)",
       variables = target, is_transformation = TRUE)
}

#' Convert RECODE command - handles multiple source->target pairs
#' @param parsed Parsed SPSS command object
#' @param sav_info SAV file information from parse_sav()
#' @keywords internal
convert_recode <- function(parsed, sav_info) {
  source_vars <- parsed$variables$source_vars
  target_vars <- parsed$variables$target_vars
  if (is.null(source_vars) || length(source_vars) == 0) {
    source_vars <- c(parsed$variables$source)
  }
  if (is.null(target_vars) || length(target_vars) == 0) {
    target_vars <- source_vars  # recode in place
  }

  source_vars <- normalize_spss_names(source_vars)
  target_vars <- normalize_spss_names(target_vars)

  # R-0009: if any source/target normalizes to an invalid R identifier, emit a
  # NOTE rather than broken `mutate( = dplyr::case_when(...))`.
  if (!all(vapply(c(source_vars, target_vars), is_valid_r_ident, logical(1)))) {
    return(spss_invalid_ident_note(
      "RECODE", parsed$raw, "Data Transformation (RECODE)",
      extra = list(variables = unique(c(source_vars, target_vars)))))
  }

  # Ensure same length (truncate to shorter)
  n <- min(length(source_vars), length(target_vars))
  source_vars <- source_vars[1:n]
  target_vars <- target_vars[1:n]

  recode_rules <- extract_recode_rules(parsed$raw)

  # Generate recode for each source->target pair
  chunks <- sapply(seq_len(n), function(i) {
    src <- source_vars[i]
    tgt <- target_vars[i]

    case_when_clauses <- vapply(recode_rules, function(rule) {
      old_val <- rule$old
      new_val <- rule$new

      if (toupper(new_val) %in% c("MISSING", "SYSMIS")) new_val <- "NA"
      # COPY as an output value keeps the source value (ELSE=COPY, range=COPY).
      if (toupper(new_val) == "COPY") new_val <- src

      if (toupper(old_val) == "ELSE") {
        paste0("TRUE ~ ", new_val)
      } else {
        cond <- recode_input_condition(old_val, src)
        if (is.na(cond)) NA_character_ else paste0(cond, " ~ ", new_val)
      }
    }, character(1))

    # An unparseable input spec must fail loudly, not emit invalid R.
    if (any(is.na(case_when_clauses))) {
      bad <- recode_rules[[which(is.na(case_when_clauses))[1]]]
      stop("RECODE input spec could not be parsed: (", bad$old, "=", bad$new, ")")
    }

    clauses_str <- paste(case_when_clauses, collapse = ",\n    ")

    # Add default fallback if no ELSE rule. SPSS semantics: unmatched values
    # keep whatever the TARGET held before the RECODE -- for an in-place
    # recode that is the source value; for INTO with a NEW target that is
    # system-missing (values are NOT copied from the source); for INTO with an
    # EXISTING target its prior values are preserved. Initializing a missing
    # target to NA and falling back to `TRUE ~ tgt` reproduces all three.
    has_else <- any(sapply(recode_rules, function(r) toupper(r$old) == "ELSE"))
    if (!has_else) {
      clauses_str <- paste0(clauses_str, ",\n    TRUE ~ ", tgt)
    }

    # paste0, not glue: glue() strips the trailing newline, which glued this
    # line onto the next statement as `<- NAdata <- ...` -- valid R that
    # right-associatively assigned the mutate() RESULT into the target column.
    init_line <- if (src == tgt) "" else {
      paste0('if (!"', tgt, '" %in% names(data)) data[["', tgt, '"]] <- NA\n')
    }

    paste0(init_line, glue::glue("data <- data |>
  dplyr::mutate({tgt} = dplyr::case_when(
    {clauses_str}
  ))"))
  })

  r_code <- paste(chunks, collapse = "\n\n")

  list(r_code = r_code, packages = "dplyr",
       analysis_type = "Data Transformation (RECODE)",
       variables = unique(c(source_vars, target_vars)),
       is_transformation = TRUE)
}

#' Convert IF command -> dplyr::mutate with if_else
#' @param parsed Parsed SPSS command object
#' @param sav_info SAV file information from parse_sav()
#' @keywords internal
convert_if <- function(parsed, sav_info) {
  condition <- parsed$variables$condition
  target <- parsed$variables$target
  expr <- parsed$variables$expression

  if (!is.null(condition) && !is.null(target) && !is.null(expr)) {
    target <- normalize_spss_names(target)
    # R-0009: a degenerate target would emit `mutate( = ...)` and `data[['']]`.
    if (!is_valid_r_ident(target)) {
      return(spss_invalid_ident_note("IF", parsed$raw,
                                     "Data Transformation (IF)"))
    }
    r_condition <- convert_spss_expression(condition)
    r_expr <- convert_spss_expression(expr)

    # R-0096: same $CASENUM -> dplyr::row_number() caveat as convert_compute()
    # -- only correct if no SELECT IF/FILTER/SORT CASES/SPLIT FILE precedes
    # this IF in the same .sps. Surface it rather than silently trust it.
    casenum_note <- if (grepl("\\$CASENUM\\b", paste(condition, expr), ignore.case = TRUE, perl = TRUE)) {
      paste0(
        "# NOTE [SPSS]: $CASENUM translated to dplyr::row_number(), which is ",
        "only equivalent to SPSS's case number if no SELECT IF/FILTER/SORT ",
        "CASES/SPLIT FILE precedes this line -- verify against the original ",
        "case order if any of those appear earlier in this syntax file.\n"
      )
    } else {
      ""
    }

    # If target already exists, use if_else to preserve existing values.
    # missing = {target}: SPSS semantics -- when the IF condition is MISSING the
    # assignment is not made and the case keeps its prior value. Without it,
    # if_else() writes NA wherever the condition is NA (e.g. user-missing codes
    # nulled by read_sav), destroying values shipped in the .sav ("Math is
    # cool" MissingData t-test, 2026-08-04).
    # The PRESERVED-VALUE argument is a READ of the target column, so it carries
    # the same hazard C-0007 fixed inside convert_spss_expression(): a target
    # called T or F is base R's TRUE/FALSE, and a bare `T` there would read as
    # the logical whenever the column is absent instead of erroring. The line
    # above pre-creates the column, so the mask normally shadows it -- but
    # "normally" is exactly the assumption that made the original defect
    # silent. `.data[[...]]` removes the assumption. The mutate TARGET stays a
    # bare name because that is an assignment, not a read. Raised by consult
    # seat `grok` (xai), 2026-09-10.
    keep <- if (target %in% c("T", "F")) paste0('.data[["', target, '"]]') else target
    r_code <- paste0(casenum_note, glue::glue("
if (!'{target}' %in% names(data)) data[['{target}']] <- NA
data <- data |>
  dplyr::mutate({target} = dplyr::if_else({r_condition}, {r_expr}, {keep}, missing = {keep}))"))
  } else {
    r_code <- glue::glue("
# IF command could not be fully parsed:
# {parsed$raw}")
  }

  list(r_code = r_code, packages = "dplyr",
       analysis_type = "Data Transformation (IF)",
       is_transformation = TRUE)
}

#' Convert SELECT IF -> dplyr::filter
#' @param parsed Parsed SPSS command object
#' @param sav_info SAV file information from parse_sav()
#' @keywords internal
convert_select_if <- function(parsed, sav_info) {
  condition <- parsed$variables$condition
  if (is.null(condition)) {
    return(list(
      r_code = paste0("# SELECT IF: Could not parse - ", parsed$raw),
      packages = "dplyr", analysis_type = "Select Cases",
      is_transformation = TRUE
    ))
  }

  r_condition <- convert_spss_expression(condition)

  r_code <- glue::glue("
data <- data |>
  dplyr::filter({r_condition})")

  list(r_code = r_code, packages = "dplyr",
       analysis_type = "Select Cases", is_transformation = TRUE)
}

convert_sort_cases <- function(parsed, sav_info) {
  vars_match <- sub("^SORT\\s+CASES\\s+(BY\\s+)?", "", parsed$raw, ignore.case = TRUE)
  vars <- strsplit(vars_match, "[,[:space:]]+")[[1]]
  vars <- vars[vars != ""]
  vars <- normalize_spss_names(vars)

  vars_expr <- sapply(vars, function(v) {
    if (grepl("\\(D\\)", v, ignore.case = TRUE)) {
      v_clean <- sub("\\(D\\)", "", v, ignore.case = TRUE)
      paste0("dplyr::desc(", v_clean, ")")
    } else {
      sub("\\(A\\)", "", v, ignore.case = TRUE)
    }
  })

  r_code <- glue::glue('
data <- data |>
  dplyr::arrange({paste(vars_expr, collapse = ", ")})')

  list(r_code = r_code, packages = "dplyr",
       analysis_type = "Sort Cases", is_transformation = TRUE)
}

convert_filter <- function(parsed, sav_info) {
  # FILTER ON / FILTER OFF / FILTER BY are tracked per-analysis at parse time
  # by annotate_filter_state(); each downstream procedure receives a
  # `filter_var` annotation and wraps its r_code in a local() block.
  # The FILTER command itself emits only a comment, so the row filter is
  # never applied globally to `data`. This is the correct SPSS semantics --
  # FILTER changes which cases are included for analyses, not the dataset.
  if (isTRUE(parsed$variables$filter_off)) {
    r_code <- "# FILTER OFF (subsequent analyses will use the unfiltered data)"
  } else {
    var <- parsed$variables$filter_var
    if (is.null(var) || !nzchar(var)) {
      r_code <- paste0("# FILTER: Could not parse - ", parsed$raw)
    } else {
      var_normalized <- normalize_spss_names(var)
      r_code <- paste0(
        "# FILTER ON BY ", var_normalized,
        " (subsequent analyses will run on data with ",
        var_normalized, " != 0 & !is.na(", var_normalized, "))"
      )
    }
  }

  list(r_code = r_code, packages = character(),
       analysis_type = "Filter Cases (per-analysis)",
       is_transformation = TRUE)
}

convert_split_file <- function(parsed, sav_info) {
  if (isTRUE(parsed$variables$split_off)) {
    r_code <- '# SPLIT FILE OFF -- subsequent analyses apply to the full dataset
data <- dplyr::ungroup(data)'
    pkgs <- "dplyr"
  } else {
    var <- parsed$variables$split_var
    if (!is.null(var)) {
      var <- normalize_spss_names(var)
      r_code <- glue::glue('# SPLIT FILE BY {var}
# Subsequent jmv/dplyr summaries that respect dplyr groups will run per-group.
# Note: jmv functions consume the data argument as-is; downstream analyses see
# a grouped tibble. Emit SPLIT FILE OFF to ungroup.
data <- dplyr::group_by(data, {var})')
      pkgs <- "dplyr"
    } else {
      r_code <- paste0("# SPLIT FILE: Could not parse - ", parsed$raw)
      pkgs <- character()
    }
  }

  list(r_code = r_code, packages = pkgs,
       analysis_type = "Split File", is_transformation = TRUE)
}

convert_delete_vars <- function(parsed, sav_info) {
  vars_match <- sub("^DELETE\\s+VARIABLES\\s+", "", parsed$raw, ignore.case = TRUE)
  vars <- strsplit(vars_match, "[,[:space:]]+")[[1]]
  vars <- vars[vars != ""]
  vars_normalized <- normalize_spss_names(vars)

  vars_quoted <- paste(paste0('"', vars_normalized, '"'), collapse = ", ")

  r_code <- glue::glue('
data <- data |>
  dplyr::select(-dplyr::any_of(c({vars_quoted})))')

  list(r_code = r_code, packages = "dplyr",
       analysis_type = "Delete Variables", is_transformation = TRUE)
}

convert_execute <- function(parsed, sav_info) {
  list(
    r_code = "# EXECUTE (Implicit in R)",
    packages = character(),
    analysis_type = "EXECUTE",
    is_transformation = TRUE
  )
}

#' Convert MISSING VALUES command -> dplyr::mutate replacing flagged values with NA
#'
#' SPSS syntax: `MISSING VALUES var1 var2 (-9, -8, -7).` declares that the
#' listed numeric values represent missing for the given variables. In R we
#' replace those values with NA so downstream analyses ignore them.
#' @keywords internal
convert_missing_values <- function(parsed, sav_info) {
  body <- sub("^MISSING\\s+VALUES?\\s+", "", parsed$raw, ignore.case = TRUE, perl = TRUE)
  body <- trimws(body)

  # Pattern: each "spec" is `var1 var2 ... (val[, val, ...])` possibly several
  # specs separated by '/' (rare but allowed in SPSS).
  spec_strings <- trimws(strsplit(body, "/", fixed = TRUE)[[1]])
  spec_strings <- spec_strings[nchar(spec_strings) > 0]

  mutate_lines <- character()
  affected <- character()

  for (spec in spec_strings) {
    paren_match <- regexec("\\(([^)]*)\\)", spec)[[1]]
    if (paren_match[1] == -1) next
    starts <- as.integer(paren_match)
    lengths <- attr(paren_match, "match.length")
    var_part <- substr(spec, 1, starts[1] - 1)
    val_part <- substr(spec, starts[2], starts[2] + lengths[2] - 1)

    raw_vars <- trimws(strsplit(trimws(var_part), "[,[:space:]]+")[[1]])
    raw_vars <- raw_vars[nchar(raw_vars) > 0]
    if (length(raw_vars) == 0) next
    var_names <- normalize_spss_names(raw_vars)
    affected <- c(affected, var_names)

    # Expand "lo THRU hi" if present in the values
    val_clean <- trimws(val_part)
    # Split on commas or whitespace (SPSS allows both)
    raw_vals <- trimws(strsplit(val_clean, "[,[:space:]]+")[[1]])
    raw_vals <- raw_vals[nchar(raw_vals) > 0]

    # Build a vector spec for R. We support plain numeric values; THRU range
    # values are translated to a range condition.
    has_thru <- any(toupper(raw_vals) == "THRU")
    if (has_thru) {
      # Find lo and hi. Only one THRU expected.
      thru_pos <- which(toupper(raw_vals) == "THRU")[1]
      if (thru_pos > 1 && thru_pos < length(raw_vals)) {
        lo <- raw_vals[thru_pos - 1]
        hi <- raw_vals[thru_pos + 1]
        if (toupper(lo) %in% c("LO", "LOWEST")) {
          cond <- function(v) sprintf(".data[[\"%s\"]] <= %s", v, hi)
        } else if (toupper(hi) %in% c("HI", "HIGHEST")) {
          cond <- function(v) sprintf(".data[[\"%s\"]] >= %s", v, lo)
        } else {
          cond <- function(v) sprintf("(.data[[\"%s\"]] >= %s & .data[[\"%s\"]] <= %s)", v, lo, v, hi)
        }
        for (vn in var_names) {
          mutate_lines <- c(
            mutate_lines,
            sprintf("    `%s` = ifelse(%s, NA, .data[[\"%s\"]])",
                    vn, cond(vn), vn))
        }
      }
    } else {
      # Plain enumerated values; quote strings that aren't numeric
      val_quoted <- vapply(raw_vals, function(v) {
        if (grepl("^-?[0-9.]+$", v)) v else paste0('"', v, '"')
      }, character(1))
      val_vec <- paste(val_quoted, collapse = ", ")
      for (vn in var_names) {
        mutate_lines <- c(
          mutate_lines,
          sprintf("    `%s` = ifelse(.data[[\"%s\"]] %%in%% c(%s), NA, .data[[\"%s\"]])",
                  vn, vn, val_vec, vn))
      }
    }
  }

  if (length(mutate_lines) == 0) {
    return(list(
      r_code = paste0("# MISSING VALUES: Could not parse - ", parsed$raw),
      packages = "dplyr", analysis_type = "Missing Values",
      is_transformation = TRUE
    ))
  }

  r_code <- paste0(
    "data <- data |>\n  dplyr::mutate(\n",
    paste(mutate_lines, collapse = ",\n"),
    "\n  )"
  )

  list(r_code = r_code, packages = "dplyr",
       analysis_type = "Missing Values",
       variables = unique(affected),
       is_transformation = TRUE)
}

convert_unsupported <- function(parsed, sav_info) {
  r_code <- glue::glue('
# UNSUPPORTED COMMAND: {parsed$command_type}
# Original SPSS syntax:
# {gsub("\\n", "\\n# ", parsed$raw)}
# This command requires manual conversion.')

  list(r_code = r_code, packages = character(),
       analysis_type = paste("Unsupported:", parsed$command_type),
       variables = parsed$variables$all, unsupported = TRUE)
}

# ==============================================================================
# EXPRESSION CONVERSION HELPERS
# ==============================================================================

#' Convert SPSS expression to R expression
#'
#' Translates SPSS syntax expressions (operators, functions, variable names)
#' Expand SPSS TO ranges using variable order from SAV file
#'
#' Converts "A11 TO A18" to "A11, A12, A13, A14, A15, A16, A17, A18"
#' by looking up variable positions in the SAV data dictionary.
#'
#' @param expr SPSS expression string
#' @param sav_info SAV file information from parse_sav()
#' @return Expression with TO ranges expanded
#' @keywords internal
expand_spss_to_ranges <- function(expr, sav_info) {
  if (is.null(expr) || !grepl("\\bTO\\b", expr, ignore.case = TRUE)) return(expr)

  # Get variable names from SAV (preserving order)
  all_vars <- NULL
  if (!is.null(sav_info) && !is.null(sav_info$data)) {
    all_vars <- names(sav_info$data)
  } else if (!is.null(sav_info) && !is.null(sav_info$variables)) {
    all_vars <- sav_info$variables
  }

  if (is.null(all_vars) || length(all_vars) == 0) {
    # No variable list (e.g. multi-.sav script, or vars COMPUTE-created at
    # runtime). Expand each range lexically when it is a numeric-sibling family;
    # otherwise drop the bare TO to a comma as a best effort.
    result <- expr
    while (grepl("(\\w+)\\s+TO\\s+(\\w+)", result, ignore.case = TRUE)) {
      m <- regmatches(result, regexpr("(\\w+)\\s+TO\\s+(\\w+)", result, ignore.case = TRUE))
      parts <- strsplit(m, "\\s+TO\\s+", perl = TRUE)[[1]]
      lexical <- .expand_to_numeric_lexical(parts[1], parts[2])
      expanded <- if (!is.null(lexical)) paste(lexical, collapse = ", ") else paste(parts[1], parts[2], sep = ", ")
      result <- sub("(\\w+)\\s+TO\\s+(\\w+)", expanded, result, ignore.case = TRUE)
    }
    return(result)
  }

  all_vars_upper <- toupper(all_vars)

  # Find and expand each "var1 TO var2" pattern
  expand_one <- function(match, from_var, to_var) {
    from_idx <- match(toupper(from_var), all_vars_upper)
    to_idx <- match(toupper(to_var), all_vars_upper)
    if (is.na(from_idx) || is.na(to_idx) || from_idx > to_idx) {
      # Not contiguous in the dictionary -- try a lexical numeric expansion
      # before falling back to dropping the middle of the range.
      lexical <- .expand_to_numeric_lexical(from_var, to_var)
      if (!is.null(lexical)) return(paste(lexical, collapse = ", "))
      return(paste(from_var, to_var, sep = ", "))
    }
    paste(all_vars[from_idx:to_idx], collapse = ", ")
  }

  # Replace all "word TO word" patterns
  result <- gsub("(\\w+)\\s+TO\\s+(\\w+)", "\\1 TO \\2", expr, ignore.case = TRUE)
  while (grepl("(\\w+)\\s+TO\\s+(\\w+)", result, ignore.case = TRUE)) {
    m <- regmatches(result, regexpr("(\\w+)\\s+TO\\s+(\\w+)", result, ignore.case = TRUE))
    parts <- strsplit(m, "\\s+TO\\s+", perl = TRUE)[[1]]
    expanded <- expand_one(m, parts[1], parts[2])
    result <- sub("(\\w+)\\s+TO\\s+(\\w+)", expanded, result, ignore.case = TRUE)
  }

  result
}

#' to equivalent R expressions.
#'
#' @param expr SPSS expression string
#' @return R expression string
#' @examples
#' convert_spss_expression("MEAN(var1, var2, var3)")
#' convert_spss_expression("MISSING(age)")
#' convert_spss_expression("x = 1 AND y <> 0")
#' @export
convert_spss_expression <- function(expr) {
  if (is.null(expr)) return("NA")

  r_expr <- expr

  # SPSS system variable $SYSMIS is the system-missing CONSTANT (a value, not a
  # function). `COMPUTE x = $SYSMIS.` initializes x to missing. Passed through
  # verbatim it leaks a bare `$` into the generated R (`mutate(x = $SYSMIS)` ->
  # "unexpected '$'"), breaking the whole chunk and every downstream `IF` that
  # references x (round-3-spss Study2/3/4_MainAnalyses: `COMPUTE GROUP_new =
  # $SYSMIS` then `IF (GROUP=1) GROUP_new = 2`). Translate to NA. Do this FIRST,
  # before the `$` can confuse later transforms. (The SYSMIS(x)/MISSING(x)
  # function forms are handled separately below.)
  r_expr <- gsub("\\$SYSMIS\\b", "NA", r_expr, ignore.case = TRUE, perl = TRUE)

  # R-0096: $SYSMIS was not the only bare-$ SPSS system variable that could hit
  # this expression translator -- $CASENUM (the per-case sequential row number,
  # e.g. `COMPUTE ID = $CASENUM.` or `SELECT IF ($CASENUM <= 100)`) is common in
  # real syntax and leaks the same bare `$` into generated R the same way
  # $SYSMIS used to. It has a faithful, deterministic R equivalent:
  # dplyr::row_number(). Translate it here, before other transforms can see the
  # `$`, exactly like the $SYSMIS rule above.
  #
  # Deliberately NOT extended to every SPSS system variable:
  #   - $DATE / $DATE11 / $JDATE / $TIME represent the date/time the SPSS job
  #     happened to run, not a reproducible data value. Faking a value (e.g.
  #     Sys.Date()) would inject non-deterministic output into a report whose
  #     whole purpose is reproducibility, and mapping them to NA (like SYSMIS)
  #     would misrepresent a timestamp as missing data. Left unhandled; still
  #     bare-$ if encountered (rare in COMPUTE/IF/RECODE -- these are mostly
  #     used for report/title stamping, not data transformation).
  #   - $LENGTH / $WIDTH are page-formatting settings for printed output, not
  #     data values; they are not expected to appear in a data expression.
  #   - $TDATE / $DATE14 are not real SPSS system variables (verified against
  #     the IBM SPSS Statistics System Variables reference); nothing to handle.
  r_expr <- gsub("\\$CASENUM\\b", "dplyr::row_number()", r_expr, ignore.case = TRUE, perl = TRUE)

  # DATEDIFF(end, start, 'unit') -> lubridate::time_length(interval(start, end), unit)
  # SPSS units: years, quarters, months, weeks, days, hours, minutes, seconds.
  # lubridate::time_length() expects singular forms. We normalize plural -> singular
  # and pass through any unit verbatim (lubridate handles many forms).
  r_expr <- gsub(
    "\\bDATEDIFF\\s*\\(\\s*([^,]+?)\\s*,\\s*([^,]+?)\\s*,\\s*['\"]([A-Za-z]+)['\"]\\s*\\)",
    "as.numeric(lubridate::time_length(lubridate::interval(\\2, \\1), unit = \"\\L\\3\"))",
    r_expr, ignore.case = TRUE, perl = TRUE
  )
  # Two-argument DATEDIFF(end, start) defaults to days in SPSS
  r_expr <- gsub(
    "\\bDATEDIFF\\s*\\(\\s*([^,]+?)\\s*,\\s*([^,)]+?)\\s*\\)",
    "as.numeric(lubridate::time_length(lubridate::interval(\\2, \\1), unit = \"days\"))",
    r_expr, ignore.case = TRUE, perl = TRUE
  )


  # ---- Protect string literals from every rewrite below (measured 2026-09-09) ----
  #
  # Every rule after this point is a blind `gsub` over the whole expression, and
  # the worst of them is the "normalise variable names to UPPERCASE" pass near
  # the end. None of them knew what a quoted string was, so they rewrote the
  # INSIDE of one:
  #
  #   trialcode NE 'AppAvoidTraining'  ->  TRIALCODE != 'APPAVOIDTRAINING'
  #   cond = 'Not Applicable'          ->  COND == '! APPLICABLE'
  #   cond = 'Netherlands OR Belgium'  ->  COND == 'NETHERLANDS | BELGIUM'
  #
  # SPSS string comparison is CASE-SENSITIVE, so the first of those matches no
  # row at all and the `SELECT IF` keeps every case SPSS dropped: no error, no
  # warning, a plausible mean over the wrong sample. 25 of the 190 corpus .sps
  # contain at least one altered literal. (None can DEMONSTRATE a wrong number,
  # because the only two that ship data are source defects -- gta94's
  # `Fluently` is undefined in its .sav and 3apxv's DataQuest.sav has no `ID`
  # column -- but the live service converts the researcher's own data, where
  # the variable does exist.)
  #
  # Masked here, restored at the very end. The token is already upper case, has
  # no `$`/`#`/`@`, and is not one of the function names restored below, so it
  # passes through every intervening rule untouched. DATEDIFF is lifted above
  # this point because it is the one rule that must READ a literal (its quoted
  # unit argument).
  .s2r_lit_m <- gregexpr("'[^']*'|\"[^\"]*\"", r_expr, perl = TRUE)
  .s2r_lits <- regmatches(r_expr, .s2r_lit_m)[[1]]
  if (length(.s2r_lits)) {
    regmatches(r_expr, .s2r_lit_m) <-
      list(paste0("S2RSTRLIT", seq_along(.s2r_lits), "ZZ"))
  }


  # Handle SPSS not-equal operators BEFORE equality conversion
  # (otherwise ~= becomes ~== and <> becomes <>= etc.)
  r_expr <- gsub("~=", "!=", r_expr)
  r_expr <- gsub("<>", "!=", r_expr)

  # SPSS word-form relational operators (NE/EQ/LT/GT/LE/GE).
  # Must run BEFORE the single-`=` -> `==` rule below so we don't double-convert
  # the EQ replacement. Word-bounded, case-insensitive -- SPSS accepts mixed case.
  r_expr <- gsub("\\bNE\\b", "!=", r_expr, ignore.case = TRUE, perl = TRUE)
  r_expr <- gsub("\\bLE\\b", "<=", r_expr, ignore.case = TRUE, perl = TRUE)
  r_expr <- gsub("\\bGE\\b", ">=", r_expr, ignore.case = TRUE, perl = TRUE)
  r_expr <- gsub("\\bLT\\b", "<", r_expr, ignore.case = TRUE, perl = TRUE)
  r_expr <- gsub("\\bGT\\b", ">", r_expr, ignore.case = TRUE, perl = TRUE)
  r_expr <- gsub("\\bEQ\\b", "==", r_expr, ignore.case = TRUE, perl = TRUE)

  # Handle SPSS equality (single = to ==, but not <= >= !=)
  r_expr <- gsub("(?<!<|>|!|=)=(?!=)", "==", r_expr, perl = TRUE)

  # Handle SPSS power operator '**' -> R '^' BEFORE other transforms
  # (must come before any single-char '*' interpretation)
  r_expr <- gsub("\\*\\*", "^", r_expr)

  # Handle ~MISSING(x) and ~SYSMIS(x) -> !is.na(x) BEFORE the general
  # MISSING/SYSMIS rewrite (otherwise the general rule fires first and
  # converts to ~is.na, leaving the ~ in place).
  r_expr <- gsub("~\\s*MISSING\\s*\\(([^)]+)\\)", "!is.na(\\1)", r_expr, ignore.case = TRUE, perl = TRUE)
  r_expr <- gsub("~\\s*SYSMIS\\s*\\(([^)]+)\\)", "!is.na(\\1)", r_expr, ignore.case = TRUE, perl = TRUE)

  # NMISS(a, b, ...) -> rowSums(is.na(cbind(a, b, ...)))
  r_expr <- gsub("\\bNMISS\\s*\\(([^)]+)\\)", "rowSums(is.na(cbind(\\1)))", r_expr, ignore.case = TRUE)
  # NVALID(a, b, ...) -> rowSums(!is.na(cbind(a, b, ...)))
  r_expr <- gsub("\\bNVALID\\s*\\(([^)]+)\\)", "rowSums(!is.na(cbind(\\1)))", r_expr, ignore.case = TRUE)

  # Handle MISSING() -> is.na()
  r_expr <- gsub("\\bMISSING\\s*\\(([^)]+)\\)", "is.na(\\1)", r_expr, ignore.case = TRUE)
  r_expr <- gsub("\\bSYSMIS\\s*\\(([^)]+)\\)", "is.na(\\1)", r_expr, ignore.case = TRUE)

  # RANGE(x, lo, hi) -> dplyr::between(x, lo, hi)  (only the 3-arg form)
  r_expr <- gsub("\\bRANGE\\s*\\(\\s*([^,]+),\\s*([^,]+),\\s*([^)]+)\\)",
                 "dplyr::between(\\1, \\2, \\3)", r_expr, ignore.case = TRUE)

  # ANY(x, v1, v2, ...) -> (x %in% c(v1, v2, ...))
  r_expr <- gsub("\\bANY\\s*\\(\\s*([^,]+),\\s*([^)]+)\\)",
                 "(\\1 %in% c(\\2))", r_expr, ignore.case = TRUE)

  # MEAN.k(a, b, c) -> rowMeans with row-validity gate (SPSS requires k non-missing)
  # Replace BEFORE generic MEAN(..) so the dotted form is handled first.
  r_expr <- gsub("MEAN\\.([0-9]+)\\s*\\(([^)]+)\\)",
                 "ifelse(rowSums(!is.na(cbind(\\2))) >= \\1, rowMeans(cbind(\\2), na.rm = TRUE), NA_real_)",
                 r_expr, ignore.case = TRUE)
  # SUM.k(a, b, c) -> rowSums with row-validity gate
  r_expr <- gsub("SUM\\.([0-9]+)\\s*\\(([^)]+)\\)",
                 "ifelse(rowSums(!is.na(cbind(\\2))) >= \\1, rowSums(cbind(\\2), na.rm = TRUE), NA_real_)",
                 r_expr, ignore.case = TRUE)

  # MEAN(a,b,c) -> rowMeans(cbind(a,b,c), na.rm=TRUE)
  r_expr <- gsub("\\bMEAN\\s*\\(([^)]+)\\)", "rowMeans(cbind(\\1), na.rm = TRUE)", r_expr, ignore.case = TRUE)

  # SUM(...) -> rowSums(cbind(...), na.rm=TRUE)
  r_expr <- gsub("\\bSUM\\s*\\(([^)]+)\\)", "rowSums(cbind(\\1), na.rm = TRUE)", r_expr, ignore.case = TRUE)

  # MAX(a, b, c) and MIN(a, b, c) -> pmax / pmin (rowwise)
  r_expr <- gsub("\\bMAX\\s*\\(([^)]+)\\)", "pmax(\\1, na.rm = TRUE)", r_expr, ignore.case = TRUE)
  r_expr <- gsub("\\bMIN\\s*\\(([^)]+)\\)", "pmin(\\1, na.rm = TRUE)", r_expr, ignore.case = TRUE)

  # Math functions
  r_expr <- gsub("\\bABS\\s*\\(", "abs(", r_expr, ignore.case = TRUE)
  r_expr <- gsub("\\bSQRT\\s*\\(", "sqrt(", r_expr, ignore.case = TRUE)
  r_expr <- gsub("\\bLN\\s*\\(", "log(", r_expr, ignore.case = TRUE)
  r_expr <- gsub("\\bLG10\\s*\\(", "log10(", r_expr, ignore.case = TRUE)
  r_expr <- gsub("\\bEXP\\s*\\(", "exp(", r_expr, ignore.case = TRUE)
  r_expr <- gsub("\\bRND\\s*\\(", "round(", r_expr, ignore.case = TRUE)
  r_expr <- gsub("\\bTRUNC\\s*\\(", "trunc(", r_expr, ignore.case = TRUE)
  r_expr <- gsub("\\bMOD\\s*\\(", "%%", r_expr, ignore.case = TRUE)

  # Logical operators
  r_expr <- gsub("\\bAND\\b", "&", r_expr, ignore.case = TRUE)
  r_expr <- gsub("\\bOR\\b", "|", r_expr, ignore.case = TRUE)
  r_expr <- gsub("\\bNOT\\b", "!", r_expr, ignore.case = TRUE)

  # SPSS uses ~ as NOT in some contexts
  # But only when followed by ( or a word boundary, not in variable names
  r_expr <- gsub("~\\(", "!(", r_expr)

  # Normalize variable names to UPPERCASE (but protect R functions and literals)
  r_expr <- gsub("\\b([A-Za-z_][A-Za-z0-9_.]*[A-Za-z0-9_])\\b", "\\U\\1", r_expr, perl = TRUE)

  # C-0007: single-character variable names. The pattern above needs TWO OR
  # MORE characters -- `[A-Za-z_]` then any run then `[A-Za-z0-9_]` -- so `a`
  # stayed lower-case while `aa` became `AA`. The loader upper-cases every
  # column, so `a` matched nothing and the transformation errored.
  #
  #   convert_spss_expression("(a = 1 AND b = 2)")   -> "(a == 1 & b == 2)"
  #   convert_spss_expression("(aa = 1 AND bb = 2)") -> "(AA == 1 & BB == 2)"
  #
  # A real corpus file does exactly this: `if (x = -6 and sweep = 1) x =
  # INTCMC04.` six times over. The assignment TARGET is normalised by
  # convert_if(), so the emitted line reads `mutate(X = if_else(x == -6 & SWEEP
  # == 1, INTCMC04, X))` -- upper-case on one side of the same variable and
  # lower-case on the other. It fails LOUD, which is why this ranked P2.
  #
  # This pass runs where it does -- AFTER every R-emitting rule -- so it can see
  # tokens this function itself produced, and the guard is the lookahead for an
  # opening parenthesis: a single letter followed by `(` is a CALL, which is how
  # the `c` of the `c(...)` that ANY() emits survives. The `%in%` operator needs
  # no guard (both its letters are adjacent to another letter, so neither
  # matches) and neither does an exponent like `1e-5` (the `e` is preceded by a
  # digit). Two-sided controls for all four are in
  # test-expression-single-char-names.R.
  r_expr <- gsub("(?<![A-Za-z0-9_.$])([a-z])(?![A-Za-z0-9_.]|\\s*\\()",
                 "\\U\\1", r_expr, perl = TRUE)

  # ...except that TWO of the twenty-six upper-case names R already means
  # something by: `T` and `F` are base R's aliases for TRUE and FALSE. Every
  # expression this function produces is placed inside dplyr::mutate(),
  # dplyr::filter() or dplyr::if_else(), where a COLUMN called T shadows the
  # base value -- so `T` is right while the column exists and silently becomes
  # TRUE the moment it does not. That would convert a loud missing-variable
  # error into a condition that is true for every row, which is the one
  # direction this project must never trade toward. `.data[["T"]]` keeps the
  # column reference and restores the loud error ("Column `T` not found").
  #
  # This also repairs the case that predates the line above: a researcher whose
  # SPSS variable was already written `T` in upper case has always been emitted
  # as a bare `T`. `\bT\b` cannot fire inside TRUE or FALSE (the boundary
  # fails), and nothing else in this function emits a bare T or F.
  r_expr <- gsub("\\bT\\b", '.data[["T"]]', r_expr, perl = TRUE)
  r_expr <- gsub("\\bF\\b", '.data[["F"]]', r_expr, perl = TRUE)

  # Restore R function names to proper case
  # DATEDIFF -> lubridate fixups (function names + unit strings) must lowercase
  r_expr <- gsub("\\bAS\\.NUMERIC\\b", "as.numeric", r_expr)
  r_expr <- gsub("\\bLUBRIDATE::TIME_LENGTH\\b", "lubridate::time_length", r_expr)
  r_expr <- gsub("\\bLUBRIDATE::INTERVAL\\b", "lubridate::interval", r_expr)
  # DATEDIFF now runs ABOVE the string-literal mask (it is the one rule that has
  # to READ a literal -- its quoted unit), so its emitted `unit = "years"` is
  # itself carried through the rules below: the literal is protected, but the
  # argument NAME is upper-cased and its `=` is doubled. Restore both, the same
  # way the lubridate function names above are restored.
  #
  # The old post-hoc `tolower()` sweep that used to live here is gone: the unit
  # is now lower-cased at translation time with PCRE `\L`, because with literals
  # protected there is no longer an upper-cased unit for a sweep to find.
  # The trailing quote is NOT part of the match: at this point the unit literal
  # has been replaced by a bare mask token (the mask consumes the quotes too),
  # so `UNIT == "` would match nothing. Anchored on the preceding comma so it
  # cannot fire on a researcher's own variable called UNIT.
  r_expr <- gsub(", UNIT == ", ", unit = ", r_expr, fixed = TRUE)
  r_expr <- gsub("\\bROWMEANS\\b", "rowMeans", r_expr)
  r_expr <- gsub("\\bROWSUMS\\b", "rowSums", r_expr)
  r_expr <- gsub("\\bCBIND\\b", "cbind", r_expr)
  # R-0096: $CASENUM -> dplyr::row_number() must survive the blanket uppercase
  # pass above (same treatment as ROWMEANS/ROWSUMS/CBIND).
  r_expr <- gsub("\\bROW_NUMBER\\b", "row_number", r_expr)
  r_expr <- gsub("\\bPMAX\\b", "pmax", r_expr)
  r_expr <- gsub("\\bPMIN\\b", "pmin", r_expr)
  r_expr <- gsub("\\bIFELSE\\b", "ifelse", r_expr)
  r_expr <- gsub("\\bNA_REAL_\\b", "NA_real_", r_expr)
  r_expr <- gsub("\\bBETWEEN\\b", "between", r_expr)
  r_expr <- gsub("\\bNA\\.RM\\b", "na.rm", r_expr)
  r_expr <- gsub("\\bIS\\.NA\\b", "is.na", r_expr)
  r_expr <- gsub("\\bTRUE\\b", "TRUE", r_expr)
  r_expr <- gsub("\\bFALSE\\b", "FALSE", r_expr)
  r_expr <- gsub("\\bNA\\b", "NA", r_expr)
  r_expr <- gsub("\\bABS\\b", "abs", r_expr)
  r_expr <- gsub("\\bSQRT\\b", "sqrt", r_expr)
  r_expr <- gsub("\\bLOG\\b", "log", r_expr)
  r_expr <- gsub("\\bLOG10\\b", "log10", r_expr)
  r_expr <- gsub("\\bEXP\\b", "exp", r_expr)
  r_expr <- gsub("\\bROUND\\b", "round", r_expr)
  r_expr <- gsub("\\bTRUNC\\b", "trunc", r_expr)
  r_expr <- gsub("\\bDPLYR\\b", "dplyr", r_expr)
  r_expr <- gsub("\\bDATA\\b", "data", r_expr)
  # Restore lowercase %in% (the outer normalization upper-cases bare words)
  r_expr <- gsub("%IN%", "%in%", r_expr)
  r_expr <- gsub("%%", "%%", r_expr)  # MOD operator stays as %%

  # SPSS allows `$`, `#`, `@` in variable names (e.g. `GOV$_COMP1`,
  # `#scratch_var`, `@active`). The data-load chunk applies
  # `normalize_spss_names` which replaces these with `.`, so the data frame
  # has columns like `GOV._COMP1`. Mirror that substitution in emitted
  # expressions: without it, R's parser breaks on `$` (it's the subset
  # operator) and even when it doesn't break, the variable name doesn't
  # match the column name in `data`. Surfaces 190 parse errors in the
  # round-2 outlier with a single change.
  #
  # Applied last so all SPSS-specific tokens (e.g. EXP, MISSING) have
  # already been rewritten -- this ONLY rewrites variable identifiers.
  # Inside string literals stays untouched (we look for letter+$/#/@+letter
  # boundaries, which don't appear in normal R code we've already emitted).
  r_expr <- gsub(
    "([A-Za-z_][A-Za-z0-9_]*)[$#@]([A-Za-z0-9_])",
    "\\1.\\2", r_expr, perl = TRUE
  )
  # Repeat once more in case names had multiple specials (e.g. `A$B$C`).
  r_expr <- gsub(
    "([A-Za-z_][A-Za-z0-9_.]*)[$#@]([A-Za-z0-9_])",
    "\\1.\\2", r_expr, perl = TRUE
  )

  # Restore the protected string literals, byte for byte. Done last so no rule
  # above can see -- and therefore cannot rewrite -- their contents. `fixed`
  # matching, and the trailing `ZZ` keeps token 1 from matching inside token 10.
  if (length(.s2r_lits)) {
    for (.i in seq_along(.s2r_lits)) {
      r_expr <- gsub(paste0("S2RSTRLIT", .i, "ZZ"), .s2r_lits[[.i]],
                     r_expr, fixed = TRUE)
    }
  }

  r_expr
}

#' Split a string on delimiter chars, ignoring delimiters inside quotes
#'
#' SPSS string literals ('a b', "1,2") may contain the very characters RECODE
#' specs are split on (comma, space, =). A naive strsplit silently shreds them
#' into garbled-but-valid R (e.g. `src %in% c('a, b')` -- one wrong string)
#' that never matches: wrong numbers with no error. This walker only splits
#' outside quotes.
#'
#' @param x Character string to split.
#' @param delims Character vector of single-character delimiters.
#' @return Character vector of trimmed, non-empty fields.
#' @keywords internal
split_outside_quotes <- function(x, delims) {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  out <- character()
  buf <- character()
  q <- ""
  for (ch in chars) {
    if (q != "") {
      buf <- c(buf, ch)
      if (ch == q) q <- ""
    } else if (ch == "'" || ch == "\"") {
      q <- ch
      buf <- c(buf, ch)
    } else if (ch %in% delims) {
      out <- c(out, paste0(buf, collapse = ""))
      buf <- character()
    } else {
      buf <- c(buf, ch)
    }
  }
  out <- c(out, paste0(buf, collapse = ""))
  out <- trimws(out)
  out[nchar(out) > 0]
}

#' Build an R condition for one RECODE rule's input spec
#'
#' SPSS allows the left side of a rule to be a comma- or space-separated LIST
#' of discrete values, THRU ranges, and the keywords MISSING/SYSMIS, e.g.
#' `(1,2,3=1)`, `(1 thru 3, 5=9)`, `(LO THRU 0=0)`. Keywords are
#' case-insensitive. Quoted string values may contain commas/spaces
#' (`('a b'='x')`); tokenization is quote-aware. Returns a single R logical
#' expression on `src`, or NA_character_ when the spec cannot be parsed
#' (callers must fail loudly -- emitting invalid R like `X == 1,2,3` silently
#' breaks the whole report).
#'
#' @param spec Character string: the input (left) side of one RECODE rule.
#' @param src Character string: the source variable name to test.
#' @return Single R logical expression, or NA_character_ if unparseable.
#' @keywords internal
recode_input_condition <- function(spec, src) {
  tokens <- split_outside_quotes(spec, ",")
  if (length(tokens) == 0) return(NA_character_)

  other_conds <- character()
  values <- character()
  for (tok in tokens) {
    # Walk the (quote-aware) space-separated words treating THRU as an INFIX
    # operator, so extra discrete values sharing one token parse too:
    # `(1 THRU 3 5=9)` is range 1-3 PLUS value 5, not `src <= 3 5` (invalid R,
    # caught by Codex cross-review 2026-08-04). Keywords are case-insensitive
    # (the previous implementation matched "thru" case-insensitively but split
    # case-SENSITIVELY, leaving hi = NA and crashing with "missing value where
    # TRUE/FALSE needed"). A quoted literal like 'a thru b' stays one word, so
    # its inner THRU is never treated as the operator.
    words <- split_outside_quotes(tok, c(" ", "\t", "\n"))
    i <- 1
    while (i <= length(words)) {
      w <- words[i]
      if (i + 2 <= length(words) && toupper(words[i + 1]) == "THRU") {
        lo <- w
        hi <- words[i + 2]
        if (toupper(lo) %in% c("LO", "LOWEST")) {
          other_conds <- c(other_conds, paste0(src, " <= ", hi))
        } else if (toupper(hi) %in% c("HI", "HIGHEST")) {
          other_conds <- c(other_conds, paste0(src, " >= ", lo))
        } else {
          other_conds <- c(other_conds, paste0(src, " >= ", lo, " & ", src, " <= ", hi))
        }
        i <- i + 3
      } else if (toupper(w) == "THRU") {
        # Dangling THRU without both bounds -- fail loudly, never emit R.
        return(NA_character_)
      } else if (toupper(w) %in% c("MISSING", "SYSMIS", "NA")) {
        # Limitation: SPSS MISSING (user+system) vs SYSMIS (system only) are
        # distinct, but this pipeline converts user-missing codes to NA at
        # import/MISSING VALUES time, so both collapse to is.na() here.
        other_conds <- c(other_conds, paste0("is.na(", src, ")"))
        i <- i + 1
      } else {
        values <- c(values, w)
        i <- i + 1
      }
    }
  }

  val_cond <- if (length(values) == 1) {
    paste0(src, " == ", values)
  } else if (length(values) > 1) {
    paste0(src, " %in% c(", paste(values, collapse = ", "), ")")
  } else {
    character()
  }
  conds <- c(val_cond, other_conds)
  if (length(conds) == 0) return(NA_character_)
  if (length(conds) == 1) return(conds)
  paste(paste0("(", conds, ")"), collapse = " | ")
}

#' Extract recode rules from a RECODE command
#'
#' @param cmd Character string of SPSS command text.
#' @return List of `list(old = , new = )` rule pairs.
#' @keywords internal
extract_recode_rules <- function(cmd) {
  # Find all parenthesized rules `(old=new)` with a quote-aware scan: a `)`,
  # `(`, or `=` inside an SPSS string literal ('a=b', "x)y") is data, not
  # rule syntax. A naive regex/strsplit shredded such rules into
  # garbled-but-valid R that silently never matched.
  chars <- strsplit(cmd, "", fixed = TRUE)[[1]]
  rules <- list()
  q <- ""
  in_rule <- FALSE
  buf <- character()
  for (ch in chars) {
    if (q != "") {
      if (in_rule) buf <- c(buf, ch)
      if (ch == q) q <- ""
    } else if (ch == "'" || ch == "\"") {
      q <- ch
      if (in_rule) buf <- c(buf, ch)
    } else if (!in_rule && ch == "(") {
      in_rule <- TRUE
      buf <- character()
    } else if (in_rule && ch == ")") {
      in_rule <- FALSE
      parts <- split_outside_quotes(paste0(buf, collapse = ""), "=")
      if (length(parts) == 2) {
        rules[[length(rules) + 1]] <- list(old = parts[1], new = parts[2])
      }
    } else if (in_rule) {
      buf <- c(buf, ch)
    }
  }
  rules
}

# ==============================================================================
# VARIABLE NAME HELPERS
# ==============================================================================

#' Normalize SPSS variable names to R-safe uppercase names
#'
#' ASCII punctuation (spaces, parens, hyphens, ...) collapses to "." as before.
#' NON-ASCII letters (Hebrew/Cyrillic/accented, common in international .sav
#' files) are mapped to a deterministic per-codepoint token "_UXXXX_" instead of
#' a bare "." -- otherwise distinct names that differ ONLY by a non-ASCII letter
#' (e.g. Hebrew `TV<aleph>1` / `TV<bet>1` / `TV<gimel>1`) would all collapse to
#' the same `TV.1`, producing DUPLICATE column names. dplyr then aborts every
#' downstream transform with "Can't transform a data frame with duplicate
#' names", silently failing the whole report. The token keeps the mapping
#' injective AND identical whether applied to a .sav's columns (at render time)
#' or to a syntax variable reference (at conversion time), so the two stay
#' consistent. ASCII-only names are unaffected.
#'
#' @param x Character vector of variable names
#' @return Normalized variable names
normalize_spss_names <- function(x) {
  if (is.null(x)) return(x)
  x <- toupper(x)
  # Map each non-ASCII char to a unique "_UXXXX_" codepoint token (keeps
  # internationally-named variables distinct instead of collapsing to ".").
  x <- vapply(x, function(s) {
    if (is.na(s) || !grepl("[^-]", s)) return(s)
    chars <- strsplit(s, "", useBytes = FALSE)[[1]]
    out <- vapply(chars, function(ch) {
      cp <- utf8ToInt(ch)
      if (length(cp) == 1 && cp <= 127L) ch
      else paste0("_U", sprintf("%04X", cp), "_")
    }, character(1))
    paste0(out, collapse = "")
  }, character(1), USE.NAMES = FALSE)
  # ASCII punctuation -> "." (unchanged historical behavior). The "_UXXXX_"
  # tokens above are pure [A-Z0-9_] so they survive this pass intact.
  x <- gsub("[^A-Z0-9_]", ".", x)
  x <- ifelse(grepl("^[0-9.]", x), paste0("X", x), x)
  x
}
# (normalize_spss_names: the "_UXXXX_" codepoint tokens above use UPPERCASE hex
# so they are pure [A-Z0-9_] and pass through the punctuation gsub unchanged,
# keeping the non-ASCII -> token mapping injective.)

#' Validate a generated R identifier (cross-project rec R-0009)
#'
#' Ported verbatim from STATA2Rmarkdown's `is_valid_r_ident()`. SPSS variable
#' names are normalized by [normalize_spss_names()] before being interpolated
#' into generated R as bare identifiers (e.g. the LHS of `dplyr::mutate()`).
#' Almost every non-empty input normalizes to a valid name, but a degenerate
#' target (an unexpanded macro that collapses to "", a multi-element vector,
#' etc.) would emit unparseable R like `mutate( = expr)`. Callers gate the
#' target through this predicate and fall back to a NOTE when it is invalid.
#'
#' @param x A candidate identifier.
#' @return TRUE iff `x` is a single non-empty syntactically-valid R name.
#' @keywords internal
is_valid_r_ident <- function(x) {
  !is.null(x) && length(x) == 1 && nzchar(x) && grepl("^[A-Za-z_][A-Za-z0-9_.]*$", x)
}

#' Build the "not a static R identifier" NOTE fallback (cross-project rec R-0009)
#'
#' Shared by the data-transformation converters (COMPUTE / IF / RECODE) so an
#' invalid generated identifier becomes a single visible comment rather than
#' broken R that silently corrupts the knitted report.
#'
#' @param command SPSS command name shown in the NOTE (e.g. "COMPUTE").
#' @param raw The original SPSS line, preserved for the reader.
#' @param analysis_type analysis_type tag for the returned converter result.
#' @param extra Additional named elements merged into the result list().
#' @keywords internal
spss_invalid_ident_note <- function(command, raw, analysis_type, extra = list()) {
  raw_str <- if (is.null(raw)) "" else as.character(raw)
  base <- list(
    r_code = paste0("# NOTE [SPSS]: ", command,
                    " target is not a static R identifier (likely an unexpanded ",
                    "macro). Original: ", gsub("[\r\n]+", " ", raw_str)),
    packages = "dplyr",
    analysis_type = analysis_type,
    is_transformation = TRUE
  )
  modifyList(base, extra)
}

#' Build a c('VAR1', 'VAR2', ...) string for jmv arguments
#' @param vars Character vector of variable names
#' @keywords internal
make_vars_str <- function(vars) {
  paste0("c(", paste0("'", vars, "'", collapse = ", "), ")")
}

#' Expand SPSS TO syntax in variable lists
#'
#' Expands variable ranges like VAR1 TO VAR5 into the full list of
#' variables between them in the dataset.
#'
#' @param parsed_command A parsed command object
#' @param sav_info SAV file info from [parse_sav()]
#' @param all_var_names Optional character vector of variable names to expand
#'   `TO` ranges against, used when the names are not in the .sav dictionary
#'   (e.g. COMPUTE-created at runtime, or several paired .sav files).
#' @return The parsed command with TO syntax expanded
#' @examples
#' \dontrun{
#' parsed   <- parse_sps("analysis.sps")
#' sav_info <- parse_sav("data.sav")
#' # Expand TO syntax (e.g., item1 TO item10)
#' parsed <- lapply(parsed, expand_spss_variables, sav_info = sav_info)
#' }
#' @export
expand_spss_variables <- function(parsed_command, sav_info, all_var_names = NULL) {
  # Reference name list for TO-range expansion. Prefer an explicit cross-dataset
  # union supplied by the caller (so a `var1 TO varN` whose members live in a
  # NON-primary paired .sav still resolves); otherwise fall back to the primary
  # dataset's own names. expand_to_syntax() additionally applies a lexical
  # numeric-sibling expansion when a name is in NO .sav at all (e.g. created by
  # an earlier COMPUTE), so passing NULL/empty here is safe.
  all_names <- if (!is.null(all_var_names)) {
    all_var_names
  } else {
    normalize_spss_names(sav_info$metadata$name)
  }

  if (!is.null(parsed_command$variables)) {
    # REGRESSION stores its per-/METHOD predictor lists at
    # $variables$method_blocks as a LIST OF character vectors, and
    # make_regression_code() emits them straight into jmv::linReg(blocks = ...).
    # The is.character() branch below skips a list, so those blocks kept the
    # literal "TO" token and jmv died with an opaque "object 'XVE8' not found"
    # while SPSS itself expands the range and reports the full model. Recurse
    # one level into a list of character vectors so blocks expand too.
    parsed_command$variables <- lapply(parsed_command$variables, function(x) {
      if (is.character(x)) {
        expand_to_syntax(x, all_names)
      } else if (is.list(x) && length(x) &&
                 all(vapply(x, is.character, logical(1)))) {
        lapply(x, function(b) expand_to_syntax(b, all_names))
      } else {
        x
      }
    })

    # A literal "TO" surviving in a variable vector means neither the dataset
    # dictionary nor the lexical numeric-sibling fallback could expand that range
    # (e.g. `alpha TO omega` with non-numeric, non-resident endpoints). Flag it so
    # the caller emits a clean Conversion Note instead of handing the bare "TO"
    # token to jmv (which would crash). Reconstruct the offending "A TO B" pairs
    # for the note.
    # Flatten one level so a nested block list (REGRESSION method_blocks) is
    # checked too -- an unresolvable range there must raise the flag as well,
    # or the caller hands a bare "TO" token to jmv.
    to_check <- list()
    for (x in parsed_command$variables) {
      if (is.character(x)) {
        to_check[[length(to_check) + 1L]] <- x
      } else if (is.list(x)) {
        for (b in x) if (is.character(b)) to_check[[length(to_check) + 1L]] <- b
      }
    }
    leftover <- vapply(to_check, function(x) {
      any(toupper(x) == "TO")
    }, logical(1))
    if (any(leftover)) {
      ranges <- character(0)
      for (x in to_check[leftover]) {
        pos <- which(toupper(x) == "TO")
        pos <- pos[pos > 1 & pos < length(x)]
        ranges <- c(ranges, sprintf("%s TO %s", x[pos - 1], x[pos + 1]))
      }
      parsed_command$.unresolved_to <- TRUE
      parsed_command$.unresolved_to_ranges <- unique(ranges)
    }
  }

  parsed_command
}

#' Lexically expand a numeric-sibling SPSS TO range without a SAV dictionary
#'
#' SPSS's `var1 TO varN` most commonly enumerates a family of names that share
#' a common prefix/suffix and differ only by a trailing integer field
#' (`item1 TO item10`, `risk01_prep2 TO risk07_prep2`, `Q1a TO Q5a`). When the
#' .sav dictionary lookup fails -- because the items were COMPUTE-created at
#' runtime (not in any .sav), or the script is paired with multiple .sav and the
#' primary one doesn't hold these vars -- we can still reproduce SPSS's intent by
#' expanding that integer field lexically. This is correct for the dominant
#' contiguous-numeric idiom and, crucially, stops the literal keyword `'TO'` from
#' leaking into the generated jmv/dplyr call (which crashes with
#' `"'names' attribute [N] must be the same length as the vector [0]"`).
#'
#' Returns the expanded character vector (preserving the source token case and
#' zero-padding width), or `NULL` when the two endpoints are not a single
#' varying integer field (caller then falls back to its own behaviour).
#'
#' @param from_var Left endpoint token (e.g. "item1")
#' @param to_var Right endpoint token (e.g. "item10")
#' @keywords internal
.expand_to_numeric_lexical <- function(from_var, to_var) {
  if (is.null(from_var) || is.null(to_var)) return(NULL)
  if (length(from_var) != 1 || length(to_var) != 1) return(NULL)
  if (!nzchar(from_var) || !nzchar(to_var)) return(NULL)

  a <- from_var; b <- to_var
  ac <- strsplit(a, "", fixed = TRUE)[[1]]
  bc <- strsplit(b, "", fixed = TRUE)[[1]]

  # Longest common (case-insensitive) PREFIX, then back it up off any trailing
  # digits so the varying integer field is never split (item1/item10 would
  # otherwise share the prefix "item1").
  pre_len <- 0L
  max_pre <- min(length(ac), length(bc))
  while (pre_len < max_pre && toupper(ac[pre_len + 1L]) == toupper(bc[pre_len + 1L])) {
    pre_len <- pre_len + 1L
  }
  while (pre_len > 0L && grepl("[0-9]", ac[pre_len])) pre_len <- pre_len - 1L

  ra <- substr(a, pre_len + 1L, nchar(a))
  rb <- substr(b, pre_len + 1L, nchar(b))
  rac <- strsplit(ra, "", fixed = TRUE)[[1]]
  rbc <- strsplit(rb, "", fixed = TRUE)[[1]]

  # Longest common SUFFIX of the residuals (digits allowed -- a constant trailing
  # field like "_prep2" legitimately ends in a digit), backed off any leading
  # digits so it cannot eat into the varying integer field.
  suf_len <- 0L
  max_suf <- min(length(rac), length(rbc))
  while (suf_len < max_suf &&
         toupper(rac[length(rac) - suf_len]) == toupper(rbc[length(rbc) - suf_len])) {
    suf_len <- suf_len + 1L
  }
  while (suf_len > 0L && grepl("[0-9]", rac[length(rac) - suf_len + 1L])) suf_len <- suf_len - 1L

  prefix <- substr(a, 1L, pre_len)                       # source-cased prefix
  suffix <- if (suf_len > 0L) substr(ra, nchar(ra) - suf_len + 1L, nchar(ra)) else ""
  n_from <- substr(ra, 1L, nchar(ra) - suf_len)
  n_to   <- substr(rb, 1L, nchar(rb) - suf_len)

  # Both middles must be pure (non-empty) integer fields.
  if (!grepl("^[0-9]+$", n_from) || !grepl("^[0-9]+$", n_to)) return(NULL)

  i_from <- suppressWarnings(as.integer(n_from))
  i_to   <- suppressWarnings(as.integer(n_to))
  if (is.na(i_from) || is.na(i_to) || i_from > i_to) return(NULL)
  # Guard against pathological ranges (a malformed parse should never blow up
  # the converter by trying to materialise millions of names).
  if (i_to - i_from > 10000L) return(NULL)

  # Preserve zero-padding when either endpoint is zero-padded.
  width <- if (grepl("^0[0-9]", n_from) || grepl("^0[0-9]", n_to)) {
    max(nchar(n_from), nchar(n_to))
  } else {
    0L
  }
  nums <- if (width > 0L) {
    formatC(i_from:i_to, width = width, flag = "0")
  } else {
    as.character(i_from:i_to)
  }
  paste0(prefix, nums, suffix)
}

#' Expand TO in a vector of variable names using a reference list
#' @param vars Character vector of variable names
#' @param all_names Character vector of all dataset variable names
#' @keywords internal
expand_to_syntax <- function(vars, all_names) {
  if (length(vars) < 3) return(vars)

  to_indices <- which(toupper(vars) == "TO")
  if (length(to_indices) == 0) return(vars)

  # SPSS is case-insensitive: match range endpoints against the dictionary
  # without regard to case (the cross-dataset union may carry a different casing
  # than the syntax token). When matched, emit the dictionary's own spelling.
  all_names_upper <- toupper(all_names)

  new_vars <- character()
  last_idx <- 1

  for (i in seq_along(to_indices)) {
    to_pos <- to_indices[i]
    if (to_pos == 1 || to_pos == length(vars)) next

    start_var <- vars[to_pos - 1]
    end_var <- vars[to_pos + 1]

    start_idx <- match(toupper(start_var), all_names_upper)
    end_idx <- match(toupper(end_var), all_names_upper)

    if (to_pos > last_idx + 1) {
      new_vars <- c(new_vars, vars[last_idx:(to_pos - 2)])
    }

    if (!is.na(start_idx) && !is.na(end_idx)) {
      direction <- if (start_idx <= end_idx) 1 else -1
      seq_idxs <- seq(start_idx, end_idx, by = direction)
      new_vars <- c(new_vars, all_names[seq_idxs])
    } else {
      # SAV dictionary lookup failed (computed vars not in any .sav, or a
      # multi-.sav script whose primary file lacks these names). Try a lexical
      # numeric-sibling expansion before giving up -- leaving the literal "TO" in
      # the var list crashes the downstream jmv call.
      lexical <- .expand_to_numeric_lexical(start_var, end_var)
      if (!is.null(lexical)) {
        new_vars <- c(new_vars, lexical)
      } else {
        new_vars <- c(new_vars, start_var, "TO", end_var)
      }
    }

    last_idx <- to_pos + 2
  }

  if (last_idx <= length(vars)) {
    new_vars <- c(new_vars, vars[last_idx:length(vars)])
  }

  new_vars
}
