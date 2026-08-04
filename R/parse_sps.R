# parse_sps.R
# Parse SPSS .SPS syntax files

#' Find the ")" matching the "(" at `open_pos`, ignoring parentheses inside
#' single- or double-quoted string literals.
#'
#' Quote-aware (cross-project rec R-0008, ported from STATA2Rmarkdown's
#' find_unquoted_word): an SPSS condition like `IF (q1 = "yes (maybe)") ...`
#' must not be truncated at the ")" inside the string literal, which the old
#' lazy `\\((.+?)\\)` regex did.
#'
#' @param s A single string.
#' @param open_pos 1-based index of the opening "(".
#' @return 1-based index of the matching ")", or 0 if not balanced.
#' @noRd
find_matching_paren <- function(s, open_pos) {
  chars <- strsplit(s, "")[[1]]
  n <- length(chars)
  if (open_pos < 1 || open_pos > n || chars[open_pos] != "(") return(0L)
  depth <- 0L
  quote_ch <- ""
  for (i in open_pos:n) {
    ch <- chars[i]
    if (nzchar(quote_ch)) {
      if (ch == quote_ch) quote_ch <- ""
    } else if (ch == '"' || ch == "'") {
      quote_ch <- ch
    } else if (ch == "(") {
      depth <- depth + 1L
    } else if (ch == ")") {
      depth <- depth - 1L
      if (depth == 0L) return(i)
    }
  }
  0L
}

#' Parse an SPSS syntax file
#'
#' Reads and parses an SPSS .sps syntax file into a list of structured
#' command objects, each containing the command type, subcommands,
#' variables, and options.
#'
#' @param sps_path Path to the .SPS file
#' @return List of parsed commands
#' @examples
#' \dontrun{
#' # Parse an SPSS syntax file
#' parsed <- parse_sps("analysis.sps")
#' length(parsed)
#' parsed[[1]]$command_type
#' parsed[[1]]$raw
#' }
#' @export
parse_sps <- function(sps_path) {
  raw <- readLines(sps_path, warn = FALSE, encoding = "UTF-8")
  raw <- paste(raw, collapse = "\n")

  cleaned <- remove_comments(raw)
  cleaned <- normalize_whitespace(cleaned)

  commands <- split_commands(cleaned)

  parsed <- lapply(commands, parse_single_command)

  # Filter out empty/NULL commands
  parsed <- parsed[!sapply(parsed, is.null)]

  # Annotate per-analysis FILTER state. SPSS FILTER ON BY <var> activates a
  # row filter that applies to all subsequent procedures until FILTER OFF or
  # USE ALL. We walk the commands sequentially, maintain filter state, and
  # stamp $filter_var on each command so the converter can emit a per-analysis
  # filter wrap (instead of a global mutate that would persist across analyses).
  parsed <- annotate_filter_state(parsed)

  parsed
}

#' Build an alias map from a parsed .sps script
#'
#' SPSS legacy scripts often use 8-character-truncated variable names. The
#' canonical (full) name is sometimes recoverable from the syntax itself
#' through:
#'
#'   * `RECODE old INTO new` — both names appear together,
#'   * `VARIABLE LABELS varname 'Long form'` — a label hints at the full name
#'     (we record the literal varname rather than guessing),
#'   * `RENAME VARIABLES (old=new)` — explicit rename pair.
#'
#' We collect every name that appears in the script and return a unique,
#' upper-cased character vector. Downstream code uses [resolve_truncated_name]
#' to match a referenced name back to a `.sav` variable when the script form
#' is truncated.
#'
#' @param parsed List of parsed commands from [parse_sps()].
#' @return Character vector of all variable names referenced in the script
#'   (uppercased, unique).
#' @export
build_alias_map <- function(parsed) {
  if (length(parsed) == 0) return(character())

  collected <- character()
  for (cmd in parsed) {
    if (is.null(cmd)) next
    ct <- unname(cmd$command_type)

    # RECODE: source_vars and target_vars
    if (identical(ct, "RECODE")) {
      collected <- c(collected,
                     cmd$variables$source_vars %||% character(),
                     cmd$variables$target_vars %||% character())
    }

    # COMPUTE newvar = expr — record both the target and any identifier tokens
    # that appear in the right-hand expression. The expression often references
    # source variables that may be 8-char-truncated or never appear elsewhere
    # in the script. We use a permissive identifier regex (letters, digits,
    # underscore, dot — matching SPSS naming) and discard SPSS function names
    # the user never declares (MEAN, SUM, SQRT, etc.).
    if (identical(ct, "COMPUTE")) {
      tgt <- cmd$variables$target %||% character()
      collected <- c(collected, tgt)
      expr <- cmd$variables$expression %||% ""
      if (nzchar(expr)) {
        ids <- regmatches(expr, gregexpr(
          "[A-Za-z_][A-Za-z0-9_.]*", expr, perl = TRUE))[[1]]
        # Filter out SPSS built-in function names so we don't pollute the alias
        # map with non-variable identifiers.
        spss_funcs <- c("MEAN","SUM","SQRT","ABS","LOG","LN","EXP","MIN","MAX",
                        "MEDIAN","MODE","SD","VARIANCE","SE","CDFNORM","PROBIT",
                        "TRUNC","RND","MOD","LAG","LAG1","CASE","SYSMIS","MISSING",
                        "VALUE","NMISS","NVALID","ANY","RANGE","COUNT","UNIFORM",
                        "NORMAL","RV","LOWER","UPPER","CHAR","CONCAT","INDEX",
                        "SUBSTR","LENGTH","IF","NUMBER","STRING","DATE","TIME",
                        "YRMODA","XDATE","RECORD","NCDF","TO","BY","WITH","AND","OR","NOT")
        ids <- ids[!toupper(ids) %in% spss_funcs]
        collected <- c(collected, ids)
      }
    }

    # IF (cond) target = expr — same treatment as COMPUTE for target/expression
    if (identical(ct, "IF")) {
      tgt <- cmd$variables$target %||% character()
      collected <- c(collected, tgt)
    }

    # RENAME VARIABLES (old=new) — parse from raw if present
    if (identical(ct, "RENAME VARIABLES")) {
      pairs <- regmatches(cmd$raw,
        gregexpr("\\(\\s*([A-Za-z_][A-Za-z0-9_.]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_.]*)\\s*\\)",
                 cmd$raw, perl = TRUE))[[1]]
      for (p in pairs) {
        m <- regmatches(p, regexec(
          "\\(\\s*([A-Za-z_][A-Za-z0-9_.]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_.]*)\\s*\\)",
          p, perl = TRUE))[[1]]
        if (length(m) >= 3) collected <- c(collected, m[2], m[3])
      }
    }

    # VARIABLE LABELS varname 'Long' — record the varname (a short alias
    # candidate). VARIABLE LABELS is a SKIP_COMMAND for conversion but we still
    # see the parsed object here.
    if (identical(ct, "VARIABLE LABELS")) {
      body <- sub("^VARIABLE\\s+LABELS?\\s+", "", cmd$raw, ignore.case = TRUE)
      # Pull every "<word> 'label'" pair
      hits <- regmatches(body, gregexpr(
        "([A-Za-z_][A-Za-z0-9_.]*)\\s*['\"][^'\"]*['\"]",
        body, perl = TRUE))[[1]]
      for (h in hits) {
        nm <- regmatches(h, regexec("([A-Za-z_][A-Za-z0-9_.]*)", h, perl = TRUE))[[1]][2]
        if (nzchar(nm)) collected <- c(collected, nm)
      }
    }

    # Generic: also collect anything in $variables that looks like a name
    if (!is.null(cmd$variables) && is.list(cmd$variables)) {
      for (v in cmd$variables) {
        if (is.character(v)) collected <- c(collected, v)
      }
    }
  }

  collected <- toupper(collected)
  collected <- collected[nzchar(collected)]
  unique(collected)
}

#' Resolve a (possibly 8-char-truncated) SPSS name to a canonical .sav name
#'
#' SPSS legacy scripts may use 8-character-truncated forms of longer
#' canonical variable names (for example, `ADRESS` truncated from
#' `ADDRESS_FULL`). When a referenced name does not exist in the .sav
#' variable list, we try to recover the canonical form by:
#'
#'   1. Exact (case-insensitive) match in `sav_vars` — return it.
#'   2. 8-char prefix match: find names in `sav_vars` whose first 8
#'      characters match the queried name's first 8 characters. If
#'      exactly one match, return it. (Multiple matches: ambiguous,
#'      return the queried name unchanged.)
#'   3. Fall back to `alias_map` lookup (informational; not yet
#'      authoritative on its own).
#'   4. Otherwise return the queried name unchanged.
#'
#' Names are compared case-insensitively; the returned canonical name
#' preserves the casing in `sav_vars`.
#'
#' @param name Character: the name as it appears in the script.
#' @param sav_vars Character vector of canonical names from the .sav
#'   metadata.
#' @param alias_map Optional character vector of additional candidate
#'   names from [build_alias_map()].
#' @return The canonical name if resolved, otherwise the input `name`.
#' @export
resolve_truncated_name <- function(name, sav_vars, alias_map = character()) {
  if (is.null(name) || !nzchar(name)) return(name)
  if (length(sav_vars) == 0) return(name)

  name_u <- toupper(name)
  sav_u  <- toupper(sav_vars)

  # 1. Exact (case-insensitive) match
  exact <- which(sav_u == name_u)
  if (length(exact) >= 1) return(sav_vars[exact[1]])

  # 2. 8-char prefix match (only meaningful for names of length 8)
  if (nchar(name_u) == 8L) {
    # Find sav names whose first 8 chars equal the queried name (and
    # whose full length is > 8, i.e., the candidate truly is a longer
    # form that would have been truncated).
    cand_idx <- which(substr(sav_u, 1L, 8L) == name_u & nchar(sav_u) > 8L)
    if (length(cand_idx) == 1L) return(sav_vars[cand_idx])
    # If ambiguous, fall through to alias_map / unchanged.
  }

  # 3. Alias map fallback (informational only — only useful if alias_map
  #    happens to contain a longer form that matches a sav variable)
  if (length(alias_map) > 0 && nchar(name_u) == 8L) {
    alias_u <- toupper(alias_map)
    cand_alias <- alias_map[substr(alias_u, 1L, 8L) == name_u & nchar(alias_u) > 8L]
    if (length(cand_alias) >= 1) {
      # Prefer one that also appears in sav_vars
      hit <- intersect(toupper(cand_alias), sav_u)
      if (length(hit) == 1) return(sav_vars[match(hit, sav_u)])
    }
  }

  # 4. Underscore-variant match: SPSS users sometimes write the same variable
  #    with or without underscores (e.g. script RGAFFL vs sav RG_AFFL).
  #    Strip underscores on both sides and look for a unique exact match.
  name_nu <- gsub("_", "", name_u, fixed = TRUE)
  if (nzchar(name_nu)) {
    sav_nu <- gsub("_", "", sav_u, fixed = TRUE)
    nu_idx <- which(sav_nu == name_nu)
    if (length(nu_idx) == 1L) return(sav_vars[nu_idx])
  }

  # 5. Unchanged
  name
}

#' Annotate parsed commands with per-analysis FILTER state
#'
#' Walks parsed commands in order, tracking the active SPSS FILTER (FILTER ON
#' BY <var>) so each subsequent procedure carries `$filter_var` until a
#' `FILTER OFF` or `USE ALL` clears it.
#'
#' @param parsed List of parsed commands (output of `parse_single_command()`).
#' @return The same list with each command annotated with `$filter_var`
#'   (character or NULL).
#' @keywords internal
annotate_filter_state <- function(parsed) {
  filter_state <- NULL  # NULL = no filter; character = active var name
  filter_expr  <- NULL  # SPSS-syntax defining expression of the active filter var

  # Map of filter-variable name -> its most recent defining SPSS expression,
  # captured from `COMPUTE <var> = <expr>` / `IF (..) <var> = <expr>`. SPSS's
  # `COMPUTE filter_$ = (cond)` + `FILTER BY filter_$` pattern computes the filter
  # column once, but our generated per-analysis filter runs on a freshly-loaded
  # data frame where that column was never computed — so we must re-emit the
  # COMPUTE inside each filtered analysis. Stashing the defining expression here
  # lets the converter do that. (Case-insensitive keys: SPSS names are.)
  var_defs <- list()

  for (i in seq_along(parsed)) {
    cmd <- parsed[[i]]
    if (is.null(cmd)) next

    # extract_command_type() returns a *named* character; unname for comparison.
    ct <- unname(cmd$command_type)

    # Record COMPUTE/IF variable definitions so a later FILTER BY can recover the
    # filter variable's expression. The two SPSS idioms differ and must NOT be
    # conflated:
    #   COMPUTE keep = (age > 18).   -> the *expression* (age > 18) IS the filter rule
    #   IF (age > 18) keep = 1.      -> the filter rule is the *condition* (age > 18),
    #                                   NOT the RHS value (1). Capturing the RHS made
    #                                   the recomputed column a constant, so the
    #                                   per-analysis filter kept every row (R-0042).
    # Store a structured record so the converter can rebuild the right column.
    if (identical(ct, "COMPUTE")) {
      tgt <- cmd$variables$target %||% ""
      ex  <- cmd$variables$expression %||% ""
      if (nzchar(tgt) && nzchar(ex)) {
        var_defs[[toupper(tgt)]] <- list(kind = "COMPUTE", expr = ex)
      }
    } else if (identical(ct, "IF")) {
      tgt  <- cmd$variables$target %||% ""
      cond <- cmd$variables$condition %||% ""
      ex   <- cmd$variables$expression %||% ""
      if (nzchar(tgt) && nzchar(cond)) {
        var_defs[[toupper(tgt)]] <- list(kind = "IF", cond = cond, expr = ex)
      }
    }

    if (identical(ct, "FILTER")) {
      if (isTRUE(cmd$variables$filter_off)) {
        filter_state <- NULL
        filter_expr  <- NULL
      } else if (!is.null(cmd$variables$filter_var) &&
                 nzchar(cmd$variables$filter_var)) {
        filter_state <- cmd$variables$filter_var
        filter_expr  <- var_defs[[toupper(filter_state)]] %||% NULL
      }
      # FILTER commands themselves never carry filter_var — they only set state.
      cmd$filter_var  <- NULL
      cmd$filter_expr <- NULL
    } else if (identical(ct, "USE ALL")) {
      filter_state <- NULL
      filter_expr  <- NULL
      cmd$filter_var  <- NULL
      cmd$filter_expr <- NULL
    } else {
      cmd$filter_var  <- filter_state
      cmd$filter_expr <- filter_expr
    }

    parsed[[i]] <- cmd
  }

  parsed
}

#' Remove SPSS comments from syntax
#' @param syntax Character string of SPSS syntax text
#' @keywords internal
remove_comments <- function(syntax) {
  # Remove block comments /* ... */ (possibly spanning lines)
  syntax <- gsub("/\\*.*?\\*/", "", syntax, perl = TRUE)

  lines <- strsplit(syntax, "\n")[[1]]

  # SPSS comment semantics: a line whose first token is `*` (or `COMMENT`)
  # begins a comment ONLY when a new command is starting. A `*` that appears
  # while a command is still open is the multiplication operator (e.g. a
  # continuation line `  * 2.` of a COMPUTE) and MUST be kept.
  #
  # The previous implementation was stateless and used a `^\*\s*[0-9]` guard to
  # avoid eating expression continuation lines, but that guard also preserved
  # standalone year-style comments such as `*2013` / `* 2017 - no EFA`. Those
  # have no `.` terminator, so split_commands() then glued them onto the
  # following command (e.g. `*2013\nGET\nFILE=...`), corrupting its command_type
  # to `*2013` and hiding the GET FILE data reference. Tracking command-open
  # state lets us blank the comment line (correct) without consuming the real
  # command that follows it, and still protect genuine `*`-as-operator
  # continuation lines.
  #
  # Each comment line is treated as self-contained — we blank that single line
  # only (the original well-tested behavior). We do NOT swallow following lines
  # up to a `.`: a bare `*comment` here is followed by the next command, not by
  # comment continuation text, and consuming to the next period would eat that
  # command.
  at_command_start <- TRUE   # does the current line begin a new command?
  out <- character(length(lines))

  for (i in seq_along(lines)) {
    line    <- lines[i]
    trimmed <- trimws(line)

    if (nchar(trimmed) == 0) {
      out[i] <- line
      # A blank line ends any pending continuation; the next non-blank line
      # starts fresh.
      at_command_start <- TRUE
      next
    }

    is_comment_line <- at_command_start &&
      (startsWith(trimmed, "*") || grepl("^COMMENT(\\s|$)", toupper(trimmed)))

    if (is_comment_line) {
      out[i] <- ""
      # The comment occupied this line; a new command still starts next.
      at_command_start <- TRUE
    } else {
      out[i] <- line
      # The next line begins a new command iff this content line terminated one
      # with a period.
      at_command_start <- grepl("\\.\\s*$", trimmed)
    }
  }

  paste(out, collapse = "\n")
}

#' Normalize whitespace in syntax
#' @param syntax Character string of SPSS syntax text
#' @keywords internal
normalize_whitespace <- function(syntax) {
  syntax <- gsub("[ \t]+", " ", syntax)
  lines <- strsplit(syntax, "\n")[[1]]
  lines <- trimws(lines)
  paste(lines, collapse = "\n")
}

#' Split syntax into individual commands
#'
#' SPSS commands are terminated by a period at the end of a line.
#' We must not split on periods inside string literals or numbers.
#' @param syntax Character string of SPSS syntax text
#' @keywords internal
split_commands <- function(syntax) {
  lines <- strsplit(syntax, "\n")[[1]]

  commands <- character()
  current <- ""

  for (line in lines) {
    # Check if line ends with a period (the SPSS command terminator)
    # But not periods inside quoted strings
    stripped <- trimws(line)
    if (nchar(stripped) == 0) next

    current <- if (nchar(current) == 0) stripped else paste(current, stripped, sep = "\n")

    # A command ends when the line ends with a period (possibly with trailing space)
    # We check after stripping: does it end with '.'?
    if (grepl("\\.$", stripped)) {
      # Remove the trailing period
      current <- sub("\\.$", "", current)
      current <- trimws(current)
      if (nchar(current) > 0) {
        commands <- c(commands, current)
      }
      current <- ""
    }
  }

  # Leftover (command without trailing period - common in some files)
  current <- trimws(current)
  if (nchar(current) > 0) {
    commands <- c(commands, current)
  }

  commands
}

#' Parse a single SPSS command
#' @param cmd Character string of SPSS command text
#' @keywords internal
parse_single_command <- function(cmd) {
  if (is.null(cmd) || nchar(trimws(cmd)) == 0) {
    return(NULL)
  }

  command_type <- extract_command_type(cmd)

  if (is.null(command_type)) {
    return(NULL)
  }

  subcommands <- extract_subcommands(cmd)
  variables <- extract_variables(cmd, command_type)
  options <- extract_options(cmd, command_type)

  list(
    raw = cmd,
    command_type = command_type,
    subcommands = subcommands,
    variables = variables,
    options = options
  )
}

#' Extract the main command type
#'
#' Recognizes full and abbreviated SPSS command names.
#' @param cmd Character string of SPSS command text
#' @keywords internal
extract_command_type <- function(cmd) {
  # Known SPSS commands - order matters: longer/multi-word matches first
  patterns <- c(
    "GET\\s+FILE"              = "GET FILE",
    "GET\\s+DATA"              = "GET DATA",
    "SAVE\\s+OUTFILE"          = "SAVE OUTFILE",
    "LOGISTIC\\s+REGRESSION"   = "LOGISTIC REGRESSION",
    "PARTIAL\\s+CORR"          = "PARTIAL CORR",
    "NONPAR\\s+CORR"           = "NONPAR CORR",
    "NPAR\\s+TESTS?"           = "NPAR TESTS",
    "SORT\\s+CASES"            = "SORT CASES",
    "SPLIT\\s+FILE"            = "SPLIT FILE",
    "SEL(ECT)?\\s+IF"          = "SELECT IF",
    "DELETE\\s+VARIABLES"      = "DELETE VARIABLES",
    "VAR(IABLE)?\\s+LAB(ELS?)?" = "VARIABLE LABELS",
    "VAL(UE)?\\s+LAB(ELS?)?"   = "VALUE LABELS",
    "ADD\\s+VALUE\\s+LABELS?"  = "ADD VALUE LABELS",
    "MISSING\\s+VALUES?"       = "MISSING VALUES",
    "DO\\s+IF"                 = "DO IF",
    "ELSE\\s+IF"               = "ELSE IF",
    "END\\s+IF"                = "END IF",
    "DO\\s+REPEAT"             = "DO REPEAT",
    "END\\s+REPEAT"            = "END REPEAT",
    "T-TEST"                   = "T-TEST",
    "UNIANOVA"                 = "UNIANOVA",
    "ONEWAY"                   = "ONEWAY",
    "GLM"                      = "GLM",
    "MANOVA"                   = "MANOVA",
    "ANOVA"                    = "ANOVA",
    "REGRESSION"               = "REGRESSION",
    "CORRELATIONS?"            = "CORRELATIONS",
    "CROSSTABS?"               = "CROSSTABS",
    "FREQUENCIES"              = "FREQUENCIES",
    "FREQ"                     = "FREQUENCIES",
    "FRE"                      = "FREQUENCIES",
    "DESCRIPTIVES"             = "DESCRIPTIVES",
    "DESC"                     = "DESCRIPTIVES",
    "DES"                      = "DESCRIPTIVES",
    "RELIABILITY"              = "RELIABILITY",
    "RELI"                     = "RELIABILITY",
    "REL"                      = "RELIABILITY",
    "FACTOR"                   = "FACTOR",
    "CLUSTER"                  = "CLUSTER",
    "QUICK\\s+CLUSTER"         = "QUICK CLUSTER",
    "ROC"                      = "ROC",
    "MEANS"                    = "MEANS",
    "EXAMINE"                  = "EXAMINE",
    "EXAM"                     = "EXAMINE",
    "EXA"                      = "EXAMINE",
    "GRAPH"                    = "GRAPH",
    "GGRAPH"                   = "GGRAPH",
    "MIXED"                    = "MIXED",
    "GENLIN"                   = "GENLIN",
    "COMPUTE"                  = "COMPUTE",
    "COUNT"                    = "COUNT",
    "RECODE"                   = "RECODE",
    "IF\\b"                    = "IF",
    "EXECUTE"                  = "EXECUTE",
    "EXEC"                     = "EXECUTE",
    "EXE"                      = "EXECUTE",
    "DATASET"                  = "DATASET",
    "FILTER"                   = "FILTER",
    "WEIGHT"                   = "WEIGHT",
    "AGGREGATE"                = "AGGREGATE",
    "PROCESS"                  = "PROCESS",
    "USE\\s+ALL"               = "USE ALL",
    "FORMATS?"                 = "FORMATS",
    "ELSE"                     = "ELSE",
    "TEMPORARY"                = "TEMPORARY",
    "SAVE"                     = "SAVE",
    "WRITE"                    = "WRITE",
    "PRINT"                    = "PRINT",
    "LIST"                     = "LIST",
    "DISPLAY"                  = "DISPLAY",
    "SET"                      = "SET",
    "PRESERVE"                 = "PRESERVE",
    "RESTORE"                  = "RESTORE",
    "NEW\\s+FILE"              = "NEW FILE",
    "INPUT\\s+PROGRAM"         = "INPUT PROGRAM",
    "END\\s+INPUT\\s+PROGRAM"  = "END INPUT PROGRAM",
    "BEGIN\\s+DATA"            = "BEGIN DATA",
    "END\\s+DATA"              = "END DATA",
    "DEFINE"                   = "DEFINE",
    "END\\s+DEFINE"            = "END DEFINE",
    "MATCH\\s+FILES"           = "MATCH FILES",
    "ADD\\s+FILES"             = "ADD FILES",
    "UPDATE"                   = "UPDATE",
    "RENAME\\s+VARIABLES?"     = "RENAME VARIABLES",
    "AUTORECODE"               = "AUTORECODE",
    "RANK"                     = "RANK",
    "CASESTOVARS"              = "CASESTOVARS",
    "VARSTOCASES"              = "VARSTOCASES"
  )

  # Multi-word commands frequently span lines, e.g.
  #   GET
  #     FILE='data.sav'.
  # The pattern table uses `\s+` between words, which matches a newline too, but
  # the historical probe only looked at the FIRST physical line ("GET"), so the
  # second keyword ("FILE") was invisible and the command mis-typed as bare
  # "GET". Probe a whitespace-normalized prefix (newlines -> spaces) so the
  # multi-word patterns can see the full leading clause regardless of wrapping.
  probe <- toupper(trimws(gsub("[[:space:]]+", " ", cmd)))

  for (i in seq_along(patterns)) {
    pattern <- names(patterns)[i]
    # Boundary after the keyword: a negative lookahead for a word character.
    # This treats whitespace, EOL, the "." command terminator (e.g. "EXE." /
    # "EXECUTE."), AND the value delimiters that immediately follow a keyword in
    # real SPSS (`=`, `(`, `/`, quotes) all as boundaries — so
    # `GET FILE='data.sav'` matches `GET\s+FILE` even though `=` (not a space)
    # follows `FILE`. The old `(\\s|\\.|$)` required whitespace/period/EOL and so
    # missed `FILE=` / `GET DATA/...`, mis-typing the command. perl=TRUE for the
    # lookahead. `GETX` still won't match `GET` (X is a word char).
    if (grepl(paste0("^", pattern, "(?![A-Za-z0-9_])"), probe,
              ignore.case = TRUE, perl = TRUE)) {
      return(patterns[i])
    }
  }

  # Default: first word (strip a trailing "." command terminator, e.g. "EXE.")
  first_word <- strsplit(probe, " ")[[1]][1]
  first_word <- sub("\\.$", "", first_word)
  if (nchar(first_word) > 0) {
    return(toupper(first_word))
  }

  NULL
}

#' Extract subcommands from a command
#'
#' Subcommands start with / and contain key=value or just key
#' @param cmd Character string of SPSS command text
#' @keywords internal
extract_subcommands <- function(cmd) {
  # Split by / that appears at line start or after whitespace (not inside strings)
  parts <- strsplit(cmd, "(?<=\\s)/|(?<=^)/|\\n\\s*/", perl = TRUE)[[1]]

  if (length(parts) <= 1) {
    # Try simpler split for inline subcommands
    parts <- strsplit(cmd, "\\s+/")[[1]]
    if (length(parts) <= 1) return(list())
  }

  # First part is the main command, rest are subcommands
  subcommands <- parts[-1]

  result <- list()
  for (sub in subcommands) {
    sub <- trimws(sub)
    # Extract subcommand name (first word before = or space)
    sub_name <- toupper(trimws(sub("^(\\w+).*", "\\1", sub)))
    sub_value <- trimws(sub("^\\w+\\s*=?\\s*", "", sub))
    if (nchar(sub_value) == 0) sub_value <- TRUE
    result[[sub_name]] <- sub_value
  }

  result
}

#' Parse a SPSS GLM /WSFACTOR specification into within-subjects factor specs
#'
#' SPSS lists each within-subjects factor as a name followed by an integer
#' level count and an optional contrast keyword, e.g.
#' `recall 2 Polynomial task 2 Polynomial`. The contrast keyword (Polynomial,
#' Deviation, Simple, Difference, Helmert, Repeated, Special) only affects the
#' contrast coding, not the cell structure, so it is parsed and discarded here.
#'
#' @param spec Character string: the text after `/WSFACTOR=` up to the next
#'   subcommand.
#' @return A list of factor specs, each `list(name = <chr>, n = <int>)`, in the
#'   order listed (which is the order SPSS uses to map DVs to cells: the last
#'   factor varies fastest).
#' @keywords internal
parse_wsfactor_spec <- function(spec) {
  spec <- trimws(spec)
  if (!nzchar(spec)) return(list())
  # Tokenize into identifiers and integers, in order. Contrast keywords are
  # identifiers and are skipped because they are not immediately followed by a
  # level count (the parser only emits a factor when a name is followed by a
  # number).
  toks <- regmatches(spec, gregexpr(
    "[A-Za-z_][A-Za-z0-9_.]*|[0-9]+", spec, perl = TRUE))[[1]]
  factors <- list()
  i <- 1L
  n <- length(toks)
  while (i <= n) {
    name <- toks[i]
    # A factor name must be a non-numeric token immediately followed by an
    # integer level count. Anything else (a stray contrast keyword) is skipped.
    if (!grepl("^[0-9]+$", name) && i < n && grepl("^[0-9]+$", toks[i + 1L])) {
      factors[[length(factors) + 1L]] <- list(
        name = name,
        n = as.integer(toks[i + 1L])
      )
      i <- i + 2L
    } else {
      i <- i + 1L
    }
  }
  factors
}

#' Extract variables from a command
#' @param cmd Character string of SPSS command text
#' @param command_type Character string of SPSS command type
#' @keywords internal
extract_variables <- function(cmd, command_type) {
  result <- list(all = character())

  if (command_type %in% c("DESCRIPTIVES", "FREQUENCIES", "RELIABILITY")) {
    vars <- extract_variables_clause(cmd)
    result$all <- vars
    result$analysis <- vars

  } else if (command_type == "CORRELATIONS") {
    vars <- extract_variables_clause(cmd)
    result$all <- vars
    result$correlate <- vars

  } else if (command_type == "PARTIAL CORR") {
    # PARTIAL CORR /VARIABLES= y1 y2 BY z1 z2  (BY introduces controls)
    vars_clause <- extract_pattern(cmd, "/VARIABLES\\s*=\\s*([^/]+)")
    if (is.null(vars_clause)) {
      # Fallback: take everything after the command name up to first '/' or newline
      vars_clause <- sub("^PARTIAL\\s+CORR\\s+", "", cmd, ignore.case = TRUE)
      vars_clause <- sub("\\s*/.*", "", vars_clause)
    }
    if (!is.null(vars_clause) && nchar(trimws(vars_clause)) > 0) {
      vars_clause <- trimws(vars_clause)
      if (grepl("\\bBY\\b", vars_clause, ignore.case = TRUE)) {
        sides <- strsplit(vars_clause, "(?i)\\bBY\\b", perl = TRUE)[[1]]
        main <- trimws(strsplit(trimws(sides[1]), "[,[:space:]]+")[[1]])
        controls <- trimws(strsplit(trimws(sides[2]), "[,[:space:]]+")[[1]])
        main <- main[nchar(main) > 0]
        controls <- controls[nchar(controls) > 0]
        result$main_vars <- main
        result$controls <- controls
        result$all <- c(main, controls)
      } else {
        toks <- trimws(strsplit(vars_clause, "[,[:space:]]+")[[1]])
        toks <- toks[nchar(toks) > 0]
        result$main_vars <- toks
        result$controls <- character()
        result$all <- toks
      }
    }

  } else if (command_type == "T-TEST") {
    result$groups <- extract_pattern(cmd, "GROUPS\\s*=\\s*([^(/]+)")
    result$variables <- extract_variables_clause(cmd)
    pairs_clause <- extract_pattern(cmd, "PAIRS\\s*=\\s*([^/]+)")
    result$pairs <- pairs_clause

    # Build pair_pairs list and paired_keyword flag from the PAIRS= clause.
    # Use word-boundary tokenization rather than splitting on any '.' (a
    # period mid-string would have produced literal "T.TEST"-style tokens).
    # SPSS forms supported:
    #   PAIRS = X1 X2 WITH X3 X4 (PAIRED)         -> pairs (X1,X3),(X2,X4)
    #   PAIRS = X1 WITH X2                        -> pairs (X1,X2)
    #   PAIRS = X1 X2 X3 X4                       -> consecutive (X1,X2),(X3,X4)
    result$paired_keyword <- FALSE
    result$pair_pairs <- list()
    if (!is.null(pairs_clause) && nzchar(trimws(pairs_clause))) {
      pc <- pairs_clause
      # detect (PAIRED)
      if (grepl("\\(\\s*PAIRED\\s*\\)", pc, ignore.case = TRUE)) {
        result$paired_keyword <- TRUE
        pc <- sub("\\(\\s*PAIRED\\s*\\)", "", pc, ignore.case = TRUE)
      }
      pc <- trimws(pc)

      # Split into LHS / RHS on case-insensitive word-boundary "WITH"
      if (grepl("\\bWITH\\b", pc, ignore.case = TRUE, perl = TRUE)) {
        sides <- strsplit(pc, "(?i)\\bWITH\\b", perl = TRUE)[[1]]
        lhs <- trimws(sides[1])
        rhs <- if (length(sides) >= 2) trimws(sides[2]) else ""
        # Tokenize variable names: word characters only, ignore commas/spaces
        lhs_toks <- regmatches(lhs, gregexpr("[A-Za-z_][A-Za-z0-9_.]*", lhs, perl = TRUE))[[1]]
        rhs_toks <- regmatches(rhs, gregexpr("[A-Za-z_][A-Za-z0-9_.]*", rhs, perl = TRUE))[[1]]
        if (length(lhs_toks) > 0 && length(rhs_toks) > 0) {
          # SPSS T-TEST PAIRS = a b WITH c d  -> elementwise (a,c),(b,d).
          # When sides differ in length, recycle the shorter side (SPSS docs).
          n <- max(length(lhs_toks), length(rhs_toks))
          lhs_toks <- rep_len(lhs_toks, n)
          rhs_toks <- rep_len(rhs_toks, n)
          result$pair_pairs <- lapply(seq_len(n), function(k) {
            list(i1 = lhs_toks[k], i2 = rhs_toks[k])
          })
        }
      } else {
        # No WITH: pair consecutive variables (a,b),(c,d),...
        toks <- regmatches(pc, gregexpr("[A-Za-z_][A-Za-z0-9_.]*", pc, perl = TRUE))[[1]]
        if (length(toks) >= 2 && length(toks) %% 2 == 0) {
          result$pair_pairs <- lapply(seq(1, length(toks), by = 2), function(k) {
            list(i1 = toks[k], i2 = toks[k + 1])
          })
        }
      }
    }

    result$all <- c(result$groups, result$variables, result$pairs)

  } else if (command_type == "ONEWAY") {
    # ONEWAY dv1 [dv2 ...] BY factor [/CONTRAST=... /POLYNOMIAL=... ...]
    # Work off the command up to the first "/" subcommand (and only the first
    # line's worth of "<dvs> BY <factor>") so multi-line /CONTRAST specs are not
    # swallowed. NOTE: `\w`/`\s` are NOT valid inside a bracket expression in R's
    # default (TRE) regex — `[\\w\\s,]` there matches the literal characters
    # \\, w, s, comma, so the previous pattern never matched a real DV name and
    # ONEWAY silently produced an empty var list (-> a broken `.res <- # ONEWAY:
    # Missing DV or factor` stub and an "object '.res' not found" cascade). Use
    # perl = TRUE so the classes work.
    head <- sub("\\s*/.*", "", cmd)                 # drop subcommands
    head <- sub("^ONEWAY\\s+", "", head, ignore.case = TRUE)
    head <- sub("\\.\\s*$", "", trimws(head))
    by_match <- regmatches(head, regexec("^(.+?)\\s+BY\\s+([\\w.]+)", head,
                                         ignore.case = TRUE, perl = TRUE))[[1]]
    if (length(by_match) >= 3) {
      result$dependent <- trimws(strsplit(trimws(by_match[2]), "[,[:space:]]+")[[1]])
      result$dependent <- result$dependent[nchar(result$dependent) > 0]
      result$factor <- trimws(by_match[3])
      result$all <- c(result$dependent, result$factor)
    }

  } else if (command_type %in% c("GLM", "UNIANOVA", "ANOVA")) {
    # Match the leading command, capture DV(s) up to BY, the first "/"
    # subcommand, or end of the command line. Both legacy ANOVA (multiple DVs
    # before BY) and repeated-measures GLM (multiple DVs each mapping to a
    # WSFACTOR cell, no BY) list several DVs here. The capture class excludes
    # "/" and newlines so a multi-line command's "/WSFACTOR=..." subcommand on
    # the next line is not swallowed into the DV list.
    head_pat <- "^(?:GLM|UNIANOVA|ANOVA)\\s+([^/\\n]+?)(?:\\s+BY\\s|\\s*/|\\s*$)"
    head_match <- regmatches(cmd, regexec(head_pat, cmd, ignore.case = TRUE,
                                          perl = TRUE))[[1]]
    if (length(head_match) >= 2) {
      dv_tokens <- trimws(strsplit(trimws(head_match[2]), "[,[:space:]]+")[[1]])
      dv_tokens <- dv_tokens[nchar(dv_tokens) > 0]
      # Preserve ALL dependent variables. The repeated-measures branch in
      # convert_glm() needs every DV (one per within-subjects cell); the
      # between-subjects branch already collapses to the first DV itself.
      result$dependent <- dv_tokens
    }
    # Within-subjects factors: /WSFACTOR=name1 levels1 [contrast1] name2 ...
    # SPSS lists each factor as a name, an integer level count, and an optional
    # contrast keyword (Polynomial, Deviation, Simple, Helmert, ...). The
    # presence of any WSFACTOR is what flags a repeated-measures GLM so that
    # convert_glm() routes to jmv::anovaRM rather than a between-subjects model.
    wsf_match <- regmatches(cmd, regexec(
      "/WSFACTOR\\s*=\\s*([^/]+)", cmd, ignore.case = TRUE, perl = TRUE))[[1]]
    if (length(wsf_match) >= 2) {
      result$ws_factors <- parse_wsfactor_spec(wsf_match[2])
    }
    # Capture factors after BY up to next "/" or " WITH " or end-of-string.
    # Prior regex used [^/WITH] which (with ignore.case) excluded individual
    # letters W/I/T/H — silently truncating any factor name containing them.
    by_match <- regmatches(cmd, regexec(
      "BY\\s+(.+?)(?:\\s+WITH\\s|\\s*/|$)",
      cmd, ignore.case = TRUE))[[1]]
    if (length(by_match) >= 2) {
      factors_raw <- trimws(by_match[2])
      # Strip any (min,max) ranges sometimes attached to legacy ANOVA factors.
      factors_clean <- gsub("\\([^)]*\\)", "", factors_raw)
      result$factors <- trimws(strsplit(factors_clean, "[,[:space:]]+")[[1]])
      result$factors <- result$factors[nchar(result$factors) > 0]
    }
    with_match <- regmatches(cmd, regexec(
      "\\bWITH\\s+(.+?)(?:\\s*/|$)",
      cmd, ignore.case = TRUE))[[1]]
    if (length(with_match) >= 2) {
      result$covariates <- trimws(strsplit(trimws(with_match[2]), "[,[:space:]]+")[[1]])
      result$covariates <- result$covariates[nchar(result$covariates) > 0]
    }
    result$all <- c(result$dependent, result$factors, result$covariates)

  } else if (command_type == "REGRESSION") {
    result$dependent <- extract_pattern(cmd, "/DEPENDENT\\s*=?\\s*(\\w+)")

    # Capture ALL /METHOD subcommands with their variables
    matches <- gregexpr("/METHOD\\s*=\\s*\\w+\\s+([^/]+)", cmd, ignore.case = TRUE)
    match_data <- regmatches(cmd, matches)[[1]]

    method_blocks <- list()
    independent_vars <- character()
    if (length(match_data) > 0) {
      for (m in match_data) {
        vars_part <- sub("/METHOD\\s*=\\s*\\w+\\s+", "", m, ignore.case = TRUE)
        vars <- trimws(strsplit(vars_part, "[,[:space:]]+")[[1]])
        vars <- vars[nchar(vars) > 0]
        method_blocks[[length(method_blocks) + 1]] <- vars
        independent_vars <- c(independent_vars, vars)
      }
    }

    result$independent <- unique(independent_vars)
    result$method_blocks <- method_blocks
    result$all <- c(result$dependent, result$independent)

  } else if (command_type == "CROSSTABS") {
    by_match <- regmatches(cmd, regexec("(\\w+)\\s+BY\\s+(\\w+)", cmd, ignore.case = TRUE))[[1]]
    if (length(by_match) >= 3) {
      result$row <- trimws(by_match[2])
      result$column <- trimws(by_match[3])
      result$all <- c(result$row, result$column)
    }

  } else if (command_type == "COMPUTE") {
    compute_match <- regmatches(cmd, regexec("COMPUTE\\s+(\\S+?)\\s*=\\s*(.+)", cmd, ignore.case = TRUE))[[1]]
    if (length(compute_match) >= 3) {
      result$target <- trimws(compute_match[2])
      result$expression <- trimws(compute_match[3])
      result$all <- result$target
    }

  } else if (command_type == "COUNT") {
    # COUNT target = varlist (value)
    count_match <- regmatches(cmd, regexec(
      "COUNT\\s+(\\w+)\\s*=\\s*(.+?)\\s*\\(([^)]+)\\)",
      cmd, ignore.case = TRUE))[[1]]
    if (length(count_match) >= 4) {
      result$target <- trimws(count_match[2])
      result$varlist_raw <- trimws(count_match[3])
      result$count_value <- trimws(count_match[4])
      result$all <- result$target
    }

  } else if (command_type == "RECODE") {
    # Parse the full RECODE command: RECODE var1 var2 ... (rules) INTO target1 target2 ...
    recode_body <- sub("^RECODE\\s+", "", cmd, ignore.case = TRUE)

    # Extract everything before the first parenthesis = source variables
    before_parens <- sub("\\s*\\(.*", "", recode_body)
    source_vars <- trimws(strsplit(before_parens, "[,[:space:]]+")[[1]])
    source_vars <- source_vars[nchar(source_vars) > 0]

    # Extract INTO targets (if present)
    into_match <- regmatches(recode_body, regexec("INTO\\s+(.+)$", recode_body, ignore.case = TRUE))[[1]]
    target_vars <- character()
    if (length(into_match) >= 2) {
      target_vars <- trimws(strsplit(trimws(into_match[2]), "[,[:space:]]+")[[1]])
      target_vars <- target_vars[nchar(target_vars) > 0]
    }

    result$source_vars <- source_vars
    result$target_vars <- target_vars
    result$source <- source_vars[1]
    result$target <- if (length(target_vars) > 0) target_vars[1] else source_vars[1]
    result$all <- unique(c(source_vars, target_vars))

  } else if (command_type == "IF") {
    # IF (condition) target = expression
    # Quote/paren-aware (R-0008): find the ")" matching the first "(" while
    # ignoring parens inside quoted string literals, so a condition like
    # `q1 = "yes (maybe)"` is not truncated at the embedded ")".
    open_pos <- regexpr("\\(", cmd)[1]
    close_pos <- if (open_pos > 0) find_matching_paren(cmd, open_pos) else 0L
    if (open_pos > 0 && close_pos > open_pos) {
      result$condition <- trimws(substr(cmd, open_pos + 1L, close_pos - 1L))
      rest <- trimws(substr(cmd, close_pos + 1L, nchar(cmd)))
      assign_match <- regmatches(rest, regexec(
        "^(\\w+)\\s*=\\s*(.+)$", rest))[[1]]
      if (length(assign_match) >= 3) {
        result$target <- trimws(assign_match[2])
        result$expression <- trimws(assign_match[3])
        result$all <- result$target
      }
    }

  } else if (command_type == "SELECT IF") {
    # SELECT IF (condition)
    sel_match <- regmatches(cmd, regexec(
      "SELECT\\s+IF\\s*\\(?(.+?)\\)?$",
      cmd, ignore.case = TRUE))[[1]]
    if (length(sel_match) >= 2) {
      result$condition <- trimws(sel_match[2])
    }
    result$all <- character()

  } else if (command_type == "FILTER") {
    # FILTER BY var or FILTER OFF
    filter_body <- sub("^FILTER\\s+", "", cmd, ignore.case = TRUE)
    if (grepl("^OFF", toupper(trimws(filter_body)))) {
      result$filter_off <- TRUE
    } else {
      var <- sub("^BY\\s+", "", filter_body, ignore.case = TRUE)
      result$filter_var <- trimws(var)
    }
    result$all <- character()

  } else if (command_type == "SPLIT FILE") {
    split_body <- sub("^SPLIT\\s+FILE\\s+", "", cmd, ignore.case = TRUE)
    if (grepl("^OFF", toupper(trimws(split_body)))) {
      result$split_off <- TRUE
    } else {
      # SPLIT FILE LAYERED BY var or SPLIT FILE BY var
      var_match <- extract_pattern(split_body, "BY\\s+(\\w+)")
      result$split_var <- var_match
    }
    result$all <- character()

  } else if (command_type == "MANOVA") {
    # MANOVA dv1 dv2 ... BY factor(min,max) [factor2(min,max) ...]
    by_match <- regmatches(cmd, regexec(
      "MANOVA\\s+(.+?)\\s+BY\\s+(.+)", cmd, ignore.case = TRUE))[[1]]
    if (length(by_match) >= 3) {
      result$dependent <- trimws(strsplit(trimws(by_match[2]), "[,[:space:]]+")[[1]])
      # Factors may have (min,max) ranges attached
      factors_raw <- trimws(by_match[3])
      factors_clean <- gsub("\\([^)]*\\)", "", factors_raw)
      result$factors <- trimws(strsplit(factors_clean, "[,[:space:]]+")[[1]])
      result$factors <- result$factors[nchar(result$factors) > 0]
      result$all <- c(result$dependent, result$factors)
    }

  } else if (command_type == "NPAR TESTS") {
    # Detect which subtest and extract variables accordingly
    cmd_upper <- toupper(cmd)
    if (grepl("/CHISQUARE", cmd_upper)) {
      result$npar_type <- "CHISQUARE"
      vars <- extract_pattern(cmd, "/CHISQUARE\\s*=\\s*([^/]+)")
      if (!is.null(vars)) result$all <- trimws(strsplit(vars, "[,[:space:]]+")[[1]])
    } else if (grepl("/K-W", cmd_upper)) {
      result$npar_type <- "KRUSKAL_WALLIS"
      dv_by <- regmatches(cmd, regexec("/K-W\\s*=\\s*(\\w+)\\s+BY\\s+(\\w+)", cmd, ignore.case = TRUE))[[1]]
      if (length(dv_by) >= 3) {
        result$dependent <- trimws(dv_by[2])
        result$factor <- trimws(dv_by[3])
        result$all <- c(result$dependent, result$factor)
      }
    } else if (grepl("/M-W", cmd_upper)) {
      result$npar_type <- "MANN_WHITNEY"
      dv_by <- regmatches(cmd, regexec("/M-W\\s*=\\s*(\\w+)\\s+BY\\s+(\\w+)", cmd, ignore.case = TRUE))[[1]]
      if (length(dv_by) >= 3) {
        result$dependent <- trimws(dv_by[2])
        result$factor <- trimws(dv_by[3])
        result$all <- c(result$dependent, result$factor)
      }
    } else if (grepl("/WILCOXON", cmd_upper)) {
      result$npar_type <- "WILCOXON"
      pairs <- extract_pattern(cmd, "/WILCOXON\\s*=\\s*(\\w+)\\s+WITH\\s+(\\w+)")
      var1 <- extract_pattern(cmd, "/WILCOXON\\s*=\\s*(\\w+)")
      if (!is.null(pairs)) {
        m <- regmatches(cmd, regexec("/WILCOXON\\s*=\\s*(\\w+)\\s+WITH\\s+(\\w+)", cmd, ignore.case = TRUE))[[1]]
        result$var1 <- trimws(m[2]); result$var2 <- trimws(m[3])
      }
      result$all <- c(result$var1, result$var2)
    } else if (grepl("/SIGN", cmd_upper)) {
      result$npar_type <- "SIGN"
      m <- regmatches(cmd, regexec("/SIGN\\s*=\\s*(\\w+)\\s+WITH\\s+(\\w+)", cmd, ignore.case = TRUE))[[1]]
      if (length(m) >= 3) { result$var1 <- trimws(m[2]); result$var2 <- trimws(m[3]) }
      result$all <- c(result$var1, result$var2)
    } else if (grepl("/FRIEDMAN", cmd_upper)) {
      result$npar_type <- "FRIEDMAN"
      vars <- extract_pattern(cmd, "/FRIEDMAN\\s*=\\s*([^/]+)")
      if (!is.null(vars)) result$all <- trimws(strsplit(vars, "[,[:space:]]+")[[1]])
    } else if (grepl("/RUNS", cmd_upper)) {
      result$npar_type <- "RUNS"
      vars <- extract_pattern(cmd, "/RUNS\\s*(?:\\([^)]*\\))?\\s*=\\s*(\\w+)")
      if (!is.null(vars)) result$all <- trimws(vars)
    } else if (grepl("/BINOMIAL", cmd_upper)) {
      result$npar_type <- "BINOMIAL"
      # Extract test proportion if specified: /BINOMIAL(.5)=var
      prop_match <- extract_pattern(cmd, "/BINOMIAL\\s*\\(([^)]+)\\)")
      if (!is.null(prop_match)) result$test_prop <- as.numeric(prop_match)
      vars <- extract_pattern(cmd, "/BINOMIAL\\s*(?:\\([^)]*\\))?\\s*=\\s*(\\w+)")
      if (!is.null(vars)) result$all <- trimws(vars)
    } else if (grepl("/K-S", cmd_upper)) {
      result$npar_type <- "KOLMOGOROV_SMIRNOV"
      # /K-S(NORMAL)=var
      dist <- extract_pattern(cmd, "/K-S\\s*\\(([^)]+)\\)")
      if (!is.null(dist)) result$distribution <- toupper(trimws(dist))
      vars <- extract_pattern(cmd, "/K-S\\s*(?:\\([^)]*\\))?\\s*=\\s*(\\w+)")
      if (!is.null(vars)) result$all <- trimws(vars)
    } else if (grepl("/MEDIAN", cmd_upper)) {
      result$npar_type <- "MEDIAN"
      dv_by <- regmatches(cmd, regexec("/MEDIAN\\s*=\\s*(\\w+)\\s+BY\\s+(\\w+)", cmd, ignore.case = TRUE))[[1]]
      if (length(dv_by) >= 3) {
        result$dependent <- trimws(dv_by[2])
        result$factor <- trimws(dv_by[3])
        result$all <- c(result$dependent, result$factor)
      }
    } else {
      # Generic fallback - try to get variables
      vars <- extract_variables_clause(cmd)
      result$all <- vars
    }

  } else if (command_type == "ROC") {
    # ROC testvar BY statevar(value) [/PLOT=CURVE]
    m <- regmatches(cmd, regexec("ROC\\s+(\\w+)\\s+BY\\s+(\\w+)", cmd, ignore.case = TRUE))[[1]]
    if (length(m) >= 3) {
      result$test_var <- trimws(m[2])
      result$state_var <- trimws(m[3])
      # Extract positive value
      val_match <- regmatches(cmd, regexec("BY\\s+\\w+\\s*\\(([^)]+)\\)", cmd, ignore.case = TRUE))[[1]]
      if (length(val_match) >= 2) result$positive_value <- trimws(val_match[2])
      result$all <- c(result$test_var, result$state_var)
    }

  } else if (command_type == "QUICK CLUSTER") {
    # QUICK CLUSTER var1 var2 ... /CRITERIA=CLUSTER(k)
    vars_part <- sub("^QUICK\\s+CLUSTER\\s+", "", cmd, ignore.case = TRUE)
    vars_part <- sub("\\s*/.*", "", vars_part)
    result$all <- trimws(strsplit(vars_part, "[,[:space:]]+")[[1]])
    result$all <- result$all[nchar(result$all) > 0]
    k_match <- extract_pattern(cmd, "CLUSTER\\s*\\(\\s*(\\d+)\\s*\\)")
    if (!is.null(k_match)) result$n_clusters <- as.integer(k_match)

  } else if (command_type == "PROCESS") {
    result$y <- extract_pattern(cmd, "y\\s*=\\s*(\\w+)")
    result$x <- extract_pattern(cmd, "x\\s*=\\s*(\\w+)")
    result$m <- extract_pattern(cmd, "m\\s*=\\s*(\\w+)")
    result$w <- extract_pattern(cmd, "w\\s*=\\s*(\\w+)")
    result$all <- c(result$y, result$x, result$m, result$w)

  } else if (command_type == "RANK") {
    # RANK [VARIABLES=] var1 var2 ... [BY group1 group2] [(A)|(D)] [ /options ]
    body <- sub("^RANK\\s+(VARIABLES\\s*=\\s*)?", "", cmd, ignore.case = TRUE)
    # Strip subcommands (everything after the first /)
    body <- sub("\\s*/.*", "", body)
    body <- trimws(body)

    # Capture trailing direction flag (A) or (D); default ascending
    direction <- "A"
    dir_match <- regmatches(body, regexec("\\(\\s*([AD])\\s*\\)\\s*$", body, ignore.case = TRUE))[[1]]
    if (length(dir_match) >= 2) {
      direction <- toupper(trimws(dir_match[2]))
      body <- sub("\\(\\s*[AD]\\s*\\)\\s*$", "", body, ignore.case = TRUE)
      body <- trimws(body)
    }
    result$rank_direction <- direction

    # Split on BY (case-insensitive)
    if (grepl("\\bBY\\b", body, ignore.case = TRUE)) {
      sides <- strsplit(body, "(?i)\\bBY\\b", perl = TRUE)[[1]]
      vars <- trimws(strsplit(trimws(sides[1]), "[,[:space:]]+")[[1]])
      groups <- trimws(strsplit(trimws(sides[2]), "[,[:space:]]+")[[1]])
      vars <- vars[nchar(vars) > 0]
      groups <- groups[nchar(groups) > 0]
      result$rank_groups <- groups
    } else {
      vars <- trimws(strsplit(body, "[,[:space:]]+")[[1]])
      vars <- vars[nchar(vars) > 0]
      result$rank_groups <- character()
    }
    result$all <- c(vars, result$rank_groups)
    result$rank_vars <- vars

  } else if (command_type == "EXAMINE") {
    # EXAMINE VARIABLES = dv1 dv2 ... [BY factor1 [BY factor2 ...]] [/subcmds]
    # The variables before the first BY are the dependent/analysis variables;
    # any variables after a BY are grouping factors. Previously EXAMINE had no
    # branch here, so `all` stayed empty and convert_examine emitted
    # "# EXAMINE: No variables specified", which then produced an invalid
    # `.res <- # EXAMINE: ...` assignment and an "object '.res' not found"
    # cascade over every downstream chunk that reused `.res`.
    body <- extract_pattern(cmd, "VARIABLES\\s*=?\\s*([^/]+)")
    if (is.null(body)) {
      first_line <- strsplit(cmd, "\n")[[1]][1]
      body <- sub("^EXAMINE\\s+", "", first_line, ignore.case = TRUE)
      body <- sub("\\s*/.*", "", body)
    }
    body <- sub("\\.\\s*$", "", trimws(body))
    if (grepl("\\bBY\\b", body, ignore.case = TRUE)) {
      sides <- strsplit(body, "(?i)\\bBY\\b", perl = TRUE)[[1]]
      dvs <- trimws(strsplit(trimws(sides[1]), "[,[:space:]]+")[[1]])
      factors <- if (length(sides) > 1) {
        trimws(strsplit(trimws(paste(sides[-1], collapse = " ")), "[,[:space:]]+")[[1]])
      } else character()
      dvs <- dvs[nchar(dvs) > 0]
      factors <- factors[nchar(factors) > 0]
      result$dependent <- dvs
      result$factors <- factors
      result$all <- c(dvs, factors)
    } else {
      vars <- extract_variables_clause(cmd)
      result$dependent <- vars
      result$factors <- character()
      result$all <- vars
    }
  }

  # Clean up NULL values
  result$all <- unique(result$all[!is.na(result$all) & nchar(result$all) > 0])

  result
}

#' Extract the VARIABLES= clause
#' @param cmd Character string of SPSS command text
#' @keywords internal
extract_variables_clause <- function(cmd) {
  # Try VARIABLES= first
  vars_match <- extract_pattern(cmd, "VARIABLES\\s*=?\\s*([^/]+)")

  if (is.null(vars_match)) {
    # Try after command name
    first_line <- strsplit(cmd, "\n")[[1]][1]
    vars_match <- sub("^\\w+\\s+", "", first_line)
    vars_match <- sub("\\s*/.*", "", vars_match)
  }

  if (is.null(vars_match) || nchar(trimws(vars_match)) == 0) {
    return(character())
  }

  # Drop the trailing "." command terminator so the last variable name does not
  # carry it (e.g. "FRE a b c." -> "a","b","c", not "a","b","c.").
  vars_match <- sub("\\.\\s*$", "", trimws(vars_match))

  vars <- strsplit(trimws(vars_match), "[,[:space:]]+")[[1]]

  # Handle TO syntax marker (actual expansion happens later)
  vars <- vars[!toupper(vars) %in% c("BY", "WITH", "ALL")]
  vars <- vars[nzchar(vars)]

  trimws(vars)
}

#' Extract a pattern match from command
#' @param cmd Character string of SPSS command text
#' @param pattern Regex pattern to extract
#' @keywords internal
extract_pattern <- function(cmd, pattern) {
  match <- regmatches(cmd, regexec(pattern, cmd, ignore.case = TRUE))[[1]]
  if (length(match) >= 2) {
    return(trimws(match[2]))
  }
  NULL
}

#' Extract options from a command
#' @param cmd Character string of SPSS command text
#' @param command_type Character string of SPSS command type
#' @keywords internal
extract_options <- function(cmd, command_type) {
  options <- list()

  if (grepl("/STATISTICS", cmd, ignore.case = TRUE)) {
    stats <- extract_pattern(cmd, "/STATISTICS\\s*=?\\s*([^/]+)")
    if (!is.null(stats)) {
      options$statistics <- toupper(trimws(strsplit(stats, "[,[:space:]]+")[[1]]))
    }
  }

  if (grepl("/METHOD", cmd, ignore.case = TRUE)) {
    method <- extract_pattern(cmd, "/METHOD\\s*=\\s*(\\w+)")
    options$method <- toupper(method)
  }

  if (grepl("/PRINT", cmd, ignore.case = TRUE)) {
    print_opts <- extract_pattern(cmd, "/PRINT\\s*=?\\s*([^/]+)")
    if (!is.null(print_opts)) {
      options$print <- toupper(trimws(strsplit(print_opts, "[,[:space:]]+")[[1]]))
    }
  }

  if (grepl("/POSTHOC", cmd, ignore.case = TRUE)) {
    posthoc <- extract_pattern(cmd, "/POSTHOC\\s*=?\\s*([^/]+)")
    options$posthoc <- posthoc
  }

  if (grepl("/PLOT", cmd, ignore.case = TRUE)) {
    options$plot <- TRUE
  }

  if (grepl("/MISSING", cmd, ignore.case = TRUE)) {
    missing_opt <- extract_pattern(cmd, "/MISSING\\s*=?\\s*(\\w+)")
    if (!is.null(missing_opt)) {
      options$missing <- toupper(missing_opt)
    }
  }

  if (grepl("/SAVE", cmd, ignore.case = TRUE)) {
    options$save <- TRUE
  }

  # PROCESS macro options
  if (command_type == "PROCESS") {
    model <- extract_pattern(cmd, "model\\s*=\\s*(\\d+)")
    if (!is.null(model)) options$model <- as.integer(model)

    boot <- extract_pattern(cmd, "boot\\s*=\\s*(\\d+)")
    if (!is.null(boot)) options$boot <- as.integer(boot)
  }

  options
}
