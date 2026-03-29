# parse_sps.R
# Parse SPSS .SPS syntax files

#' Parse an SPSS syntax file
#'
#' Reads and parses an SPSS .sps syntax file into a list of structured
#' command objects, each containing the command type, subcommands,
#' variables, and options.
#'
#' @param sps_path Path to the .SPS file
#' @return List of parsed commands
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

  parsed
}

#' Remove SPSS comments from syntax
remove_comments <- function(syntax) {
  # Remove block comments /* ... */ (possibly spanning lines)
  syntax <- gsub("/\\*.*?\\*/", "", syntax, perl = TRUE)

  lines <- strsplit(syntax, "\n")[[1]]
  lines <- sapply(lines, function(line) {
    trimmed <- trimws(line)
    # Lines starting with * are comments (but not *inside* expressions)
    if (startsWith(trimmed, "*") && !grepl("^\\*\\s*[0-9]", trimmed)) {
      ""
    } else if (grepl("^COMMENT\\s", toupper(trimmed))) {
      ""
    } else {
      line
    }
  })

  paste(lines, collapse = "\n")
}

#' Normalize whitespace in syntax
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
    "SELECT\\s+IF"             = "SELECT IF",
    "DELETE\\s+VARIABLES"      = "DELETE VARIABLES",
    "VARIABLE\\s+LABELS?"      = "VARIABLE LABELS",
    "VALUE\\s+LABELS?"         = "VALUE LABELS",
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
    "REGRESSION"               = "REGRESSION",
    "CORRELATIONS?"            = "CORRELATIONS",
    "CROSSTABS?"               = "CROSSTABS",
    "FREQUENCIES"              = "FREQUENCIES",
    "FREQ"                     = "FREQUENCIES",
    "DESCRIPTIVES"             = "DESCRIPTIVES",
    "RELIABILITY"              = "RELIABILITY",
    "FACTOR"                   = "FACTOR",
    "CLUSTER"                  = "CLUSTER",
    "QUICK\\s+CLUSTER"         = "QUICK CLUSTER",
    "ROC"                      = "ROC",
    "MEANS"                    = "MEANS",
    "EXAMINE"                  = "EXAMINE",
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

  first_line <- strsplit(cmd, "\n")[[1]][1]
  first_line <- toupper(trimws(first_line))

  for (i in seq_along(patterns)) {
    pattern <- names(patterns)[i]
    if (grepl(paste0("^", pattern, "(\\s|$)"), first_line, ignore.case = TRUE)) {
      return(patterns[i])
    }
  }

  # Default: first word
  first_word <- strsplit(first_line, "\\s+")[[1]][1]
  if (nchar(first_word) > 0) {
    return(toupper(first_word))
  }

  NULL
}

#' Extract subcommands from a command
#'
#' Subcommands start with / and contain key=value or just key
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

#' Extract variables from a command
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

  } else if (command_type == "T-TEST") {
    result$groups <- extract_pattern(cmd, "GROUPS\\s*=\\s*([^(/]+)")
    result$variables <- extract_variables_clause(cmd)
    result$pairs <- extract_pattern(cmd, "PAIRS\\s*=\\s*([^/]+)")
    result$all <- c(result$groups, result$variables, result$pairs)

  } else if (command_type == "ONEWAY") {
    by_match <- regmatches(cmd, regexec("([\\w\\s,]+)\\s+BY\\s+(\\w+)", cmd, ignore.case = TRUE))[[1]]
    if (length(by_match) >= 3) {
      result$dependent <- trimws(strsplit(by_match[2], "[,[:space:]]+")[[1]])
      result$factor <- trimws(by_match[3])
      result$all <- c(result$dependent, result$factor)
    }

  } else if (command_type %in% c("GLM", "UNIANOVA")) {
    result$dependent <- extract_pattern(cmd, "^(?:GLM|UNIANOVA)\\s+(\\w+)")
    by_match <- extract_pattern(cmd, "BY\\s+([^/WITH]+)")
    if (!is.null(by_match)) {
      result$factors <- trimws(strsplit(by_match, "[,[:space:]]+")[[1]])
    }
    with_match <- extract_pattern(cmd, "WITH\\s+([^/]+)")
    if (!is.null(with_match)) {
      result$covariates <- trimws(strsplit(with_match, "[,[:space:]]+")[[1]])
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
    compute_match <- regmatches(cmd, regexec("COMPUTE\\s+(\\w+)\\s*=\\s*(.+)", cmd, ignore.case = TRUE))[[1]]
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
    if_match <- regmatches(cmd, regexec(
      "IF\\s*\\((.+?)\\)\\s*(\\w+)\\s*=\\s*(.+)",
      cmd, ignore.case = TRUE))[[1]]
    if (length(if_match) >= 4) {
      result$condition <- trimws(if_match[2])
      result$target <- trimws(if_match[3])
      result$expression <- trimws(if_match[4])
      result$all <- result$target
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
  }

  # Clean up NULL values
  result$all <- unique(result$all[!is.na(result$all) & nchar(result$all) > 0])

  result
}

#' Extract the VARIABLES= clause
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

  vars <- strsplit(trimws(vars_match), "[,[:space:]]+")[[1]]

  # Handle TO syntax marker (actual expansion happens later)
  vars <- vars[!toupper(vars) %in% c("BY", "WITH", "ALL")]

  trimws(vars)
}

#' Extract a pattern match from command
extract_pattern <- function(cmd, pattern) {
  match <- regmatches(cmd, regexec(pattern, cmd, ignore.case = TRUE))[[1]]
  if (length(match) >= 2) {
    return(trimws(match[2]))
  }
  NULL
}

#' Extract options from a command
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
