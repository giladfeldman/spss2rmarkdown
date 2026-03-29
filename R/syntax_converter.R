# syntax_converter.R
# Master dispatcher for SPSS -> R translation using jmv package

# Commands that are purely metadata/formatting and should be silently skipped
SKIP_COMMANDS <- c(
  "VARIABLE LABELS", "VALUE LABELS", "ADD VALUE LABELS",
  "MISSING VALUES", "FORMATS", "USE ALL", "TEMPORARY",
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
  "EXECUTE", "SPLIT FILE", "COUNT", "AGGREGATE", "RANK"
)

#' Convert all parsed commands to R code
#'
#' Takes a list of parsed SPSS commands and converts each to equivalent R code.
#'
#' @param parsed_commands List of parsed commands from [parse_sps()]
#' @param sav_info SAV file info from [parse_sav()]
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
convert_all_commands <- function(parsed_commands, sav_info) {
  results <- list()

  for (i in seq_along(parsed_commands)) {
    cmd <- parsed_commands[[i]]
    result <- convert_spss_to_r(cmd, sav_info)
    result$order <- i
    result$is_transformation <- cmd$command_type %in% DATA_COMMANDS
    results[[i]] <- result
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
#' @return List with r_code, packages, analysis_type, and optional error
#' @examples
#' \dontrun{
#' parsed   <- parse_sps("analysis.sps")
#' sav_info <- parse_sav("data.sav")
#' result   <- convert_spss_to_r(parsed[[1]], sav_info)
#' cat(result$r_code)
#' }
#' @export
convert_spss_to_r <- function(parsed_command, sav_info) {
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
    "source_vars", "target_vars", "split_var"
  )

  if (!is.null(parsed_command$variables)) {
    for (key in names(parsed_command$variables)) {
      val <- parsed_command$variables[[key]]
      if (is.character(val) && key %in% var_name_keys) {
        parsed_command$variables[[key]] <- normalize_spss_names(val)
      }
    }
  }

  # Expand TO syntax in variables
  parsed_command <- expand_spss_variables(parsed_command, sav_info)

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
    # Default handler
    convert_unsupported
  )

  tryCatch(
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

  r_code <- glue::glue('
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

  list(r_code = r_code, packages = "jmv",
       analysis_type = "Descriptive Statistics", variables = vars)
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

  r_code <- glue::glue('
jmv::descriptives(
  data = data,
  vars = {vars_str},
  freq = TRUE,
  hist = TRUE,
  bar = TRUE
)')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "Frequencies", variables = vars)
}

convert_correlations <- function(parsed, sav_info) {
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
  plots = TRUE,
  plotDens = TRUE,
  plotStats = TRUE
)')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "Correlation Analysis", variables = vars)
}

convert_partial_corr <- function(parsed, sav_info) {
  vars <- parsed$variables$all
  if (length(vars) < 2) {
    return(list(
      r_code = "# PARTIAL CORR: Need at least 2 variables",
      packages = character(), analysis_type = "Partial Correlation"
    ))
  }

  # First variables are to correlate, variables after BY or WITH are controls
  by_match <- extract_pattern(parsed$raw, "BY\\s+([^/]+)")
  if (!is.null(by_match)) {
    controls <- normalize_spss_names(trimws(strsplit(by_match, "[,[:space:]]+")[[1]]))
    main_vars <- setdiff(vars, controls)
  } else {
    main_vars <- vars[1:min(2, length(vars))]
    controls <- if (length(vars) > 2) vars[3:length(vars)] else character()
  }

  main_str <- make_vars_str(main_vars)
  controls_str <- if (length(controls) > 0) make_vars_str(controls) else "NULL"

  r_code <- glue::glue('
jmv::corrPart(
  data = data,
  vars = {main_str},
  controls = {controls_str}
)')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "Partial Correlation", variables = vars)
}

convert_ttest <- function(parsed, sav_info) {
  groups <- parsed$variables$groups
  vars <- parsed$variables$variables
  pairs <- parsed$variables$pairs

  if (!is.null(groups) && length(groups) > 0) {
    # Independent samples t-test
    group_var <- gsub("\\(.*", "", groups[1])
    dv <- vars[1] %||% pairs[1]
    if (is.null(dv)) {
      return(list(r_code = "# T-TEST: No dependent variable found",
                  packages = character(), analysis_type = "T-Test"))
    }

    r_code <- glue::glue('
jmv::ttestIS(
  data = data,
  vars = c("{dv}"),
  group = "{group_var}",
  students = TRUE,
  welchs = TRUE,
  mann = FALSE,
  meanDiff = TRUE,
  ci = TRUE,
  effectSize = TRUE,
  desc = TRUE,
  plots = TRUE
)')
    analysis_type <- "Independent Samples T-Test"
  } else if (!is.null(pairs) && length(pairs) >= 2) {
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
  phTest = TRUE,
  effectSize = TRUE
)')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "One-Way ANOVA", variables = c(dv, fct))
}

convert_glm <- function(parsed, sav_info) {
  dv <- parsed$variables$dependent
  factors <- parsed$variables$factors %||% character()
  covariates <- parsed$variables$covariates %||% character()

  if (is.null(dv)) {
    return(list(r_code = "# GLM/UNIANOVA: Missing dependent variable",
                packages = character(), analysis_type = "ANOVA"))
  }

  is_ancova <- length(covariates) > 0

  if (is_ancova) {
    factors_str <- make_vars_str(factors)
    covs_str <- make_vars_str(covariates)

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
    analysis_type <- "ANCOVA"
  } else {
    factors_str <- make_vars_str(factors)

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

  r_code <- glue::glue('
jmv::linReg(
  data = data,
  dep = "{dv}",
  covs = {covs_str},
  blocks = {blocks_str},
  refLevels = list(),
  r = TRUE,
  r2 = TRUE,
  r2Adj = TRUE,
  aic = TRUE,
  bic = TRUE,
  rmse = TRUE,
  modelTest = TRUE,
  anova = TRUE,
  ci = TRUE,
  stdEst = TRUE,
  ciStdEst = TRUE,
  collin = TRUE,
  cooks = TRUE,
  durbin = TRUE,
  norm = TRUE,
  qqPlot = TRUE,
  resPlots = TRUE
)')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "Linear Regression", variables = c(dv, ivs))
}

convert_logistic <- function(parsed, sav_info) {
  dv <- parsed$variables$dependent
  ivs <- parsed$variables$independent %||% parsed$variables$all
  if (is.null(dv)) {
    return(list(r_code = "# LOGISTIC REGRESSION: Missing DV",
                packages = character(), analysis_type = "Logistic Regression"))
  }

  covs_str <- make_vars_str(ivs)

  r_code <- glue::glue('
jmv::logRegBin(
  data = data,
  dep = "{dv}",
  covs = {covs_str},
  blocks = list(list({paste0(\'"\', ivs, \'"\', collapse = ", ")})),
  refLevels = list(),
  modelTest = TRUE,
  dev = TRUE,
  aic = TRUE,
  bic = TRUE,
  pseudoR2 = c("r2mf", "r2cs", "r2n"),
  omni = TRUE,
  ci = TRUE,
  OR = TRUE,
  ciOR = TRUE,
  class = TRUE,
  acc = TRUE,
  spec = TRUE,
  sens = TRUE,
  auc = TRUE,
  rocPlot = TRUE
)')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "Logistic Regression", variables = c(dv, ivs))
}

convert_reliability <- function(parsed, sav_info) {
  vars <- parsed$variables$all
  if (length(vars) == 0) {
    return(list(r_code = "# RELIABILITY: No variables specified",
                packages = character(), analysis_type = "Reliability Analysis"))
  }
  vars_str <- make_vars_str(vars)

  r_code <- glue::glue('
jmv::reliability(
  data = data,
  vars = {vars_str},
  alphaScale = TRUE,
  omegaScale = TRUE,
  meanScale = TRUE,
  sdScale = TRUE,
  corPlot = TRUE,
  alphaItems = TRUE,
  omegaItems = TRUE,
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
  taub = TRUE,
  barPlot = TRUE
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
  vars <- parsed$variables$all
  if (length(vars) == 0) {
    return(list(r_code = "# EXAMINE: No variables specified",
                packages = character(), analysis_type = "Explore"))
  }
  vars_str <- make_vars_str(vars)

  r_code <- glue::glue('
jmv::descriptives(
  data = data,
  vars = {vars_str},
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
  hist = TRUE,
  dens = TRUE,
  box = TRUE,
  qq = TRUE
)')

  list(r_code = r_code, packages = "jmv",
       analysis_type = "Explore", variables = vars)
}

convert_factor <- function(parsed, sav_info) {
  vars <- parsed$variables$all
  if (length(vars) == 0) {
    return(list(r_code = "# FACTOR: No variables specified",
                packages = character(), analysis_type = "Factor Analysis"))
  }
  vars_str <- make_vars_str(vars)

  r_code <- glue::glue('
jmv::efa(
  data = data,
  vars = {vars_str},
  nFactorMethod = "parallel",
  extraction = "pa",
  rotation = "varimax",
  hideLoadings = 0.3,
  sortLoadings = TRUE,
  screePlot = TRUE,
  eigen = TRUE,
  factorCor = TRUE,
  factorSummary = TRUE,
  kmo = TRUE,
  bartlett = TRUE
)')

  list(r_code = r_code, packages = "jmv",
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
  vars <- parsed$variables$all
  if (length(vars) == 0) vars <- extract_variables_clause(parsed$raw)
  vars <- normalize_spss_names(vars)

  if (length(vars) == 0) {
    return(list(r_code = "# RANK: No variables specified",
                packages = character(), analysis_type = "Rank Cases",
                is_transformation = TRUE))
  }

  rank_exprs <- sapply(vars, function(v) {
    glue::glue("data[['R{v}']] <- rank(data[['{v}']], na.last = 'keep')")
  })

  r_code <- paste(c("# RANK: Create rank variables", rank_exprs), collapse = "\n")

  list(r_code = r_code, packages = character(),
       analysis_type = "Rank Cases", variables = vars,
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

  target <- normalize_spss_names(target)
  r_expr <- convert_spss_expression(expr)

  r_code <- glue::glue("
data <- data |>
  dplyr::mutate({target} = {r_expr})")

  list(r_code = r_code, packages = "dplyr",
       analysis_type = "Data Transformation (COMPUTE)",
       variables = target, is_transformation = TRUE)
}

#' Convert COUNT command -> rowSums of logical checks
#' @param parsed Parsed SPSS command object
#' @param sav_info SAV file information from parse_sav()
#' @keywords internal
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

  # Build the rowSums expression
  checks <- paste0("(data[['", expanded, "']] == ", count_value, ")", collapse = " + ")

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

  # Ensure same length (truncate to shorter)
  n <- min(length(source_vars), length(target_vars))
  source_vars <- source_vars[1:n]
  target_vars <- target_vars[1:n]

  recode_rules <- extract_recode_rules(parsed$raw)

  # Generate recode for each source->target pair
  chunks <- sapply(seq_len(n), function(i) {
    src <- source_vars[i]
    tgt <- target_vars[i]

    case_when_clauses <- sapply(recode_rules, function(rule) {
      old_val <- rule$old
      new_val <- rule$new

      if (toupper(old_val) %in% c("MISSING", "SYSMIS")) old_val <- "NA"
      if (toupper(new_val) %in% c("MISSING", "SYSMIS")) new_val <- "NA"

      if (old_val == "NA") {
        paste0("is.na(", src, ") ~ ", new_val)
      } else if (grepl("\\s+THRU\\s+", old_val, ignore.case = TRUE)) {
        bounds <- trimws(strsplit(old_val, "\\s+THRU\\s+", perl = TRUE)[[1]])
        lo <- bounds[1]
        hi <- bounds[2]
        if (toupper(lo) == "LO" || toupper(lo) == "LOWEST") {
          paste0(src, " <= ", hi, " ~ ", new_val)
        } else if (toupper(hi) == "HI" || toupper(hi) == "HIGHEST") {
          paste0(src, " >= ", lo, " ~ ", new_val)
        } else {
          paste0(src, " >= ", lo, " & ", src, " <= ", hi, " ~ ", new_val)
        }
      } else if (toupper(old_val) == "ELSE") {
        paste0("TRUE ~ ", new_val)
      } else {
        paste0(src, " == ", old_val, " ~ ", new_val)
      }
    })

    clauses_str <- paste(case_when_clauses, collapse = ",\n    ")

    # Add default fallback if no ELSE rule
    has_else <- any(sapply(recode_rules, function(r) toupper(r$old) == "ELSE"))
    if (!has_else) {
      if (src == tgt) {
        clauses_str <- paste0(clauses_str, ",\n    TRUE ~ ", src)
      } else {
        clauses_str <- paste0(clauses_str, ",\n    TRUE ~ ", src)
      }
    }

    glue::glue("data <- data |>
  dplyr::mutate({tgt} = dplyr::case_when(
    {clauses_str}
  ))")
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
    r_condition <- convert_spss_expression(condition)
    r_expr <- convert_spss_expression(expr)

    # If target already exists, use if_else to preserve existing values
    r_code <- glue::glue("
if (!'{target}' %in% names(data)) data[['{target}']] <- NA
data <- data |>
  dplyr::mutate({target} = dplyr::if_else({r_condition}, {r_expr}, {target}))")
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
  if (isTRUE(parsed$variables$filter_off)) {
    # FILTER OFF - not easily reversible without storing original data
    # We'll add a comment noting this
    r_code <- '# FILTER OFF - restoring full dataset
# Note: If data was filtered earlier, the filter is now removed.
# (Original data should be reloaded if a prior FILTER BY was applied)'
  } else {
    var <- parsed$variables$filter_var
    if (is.null(var)) {
      return(list(
        r_code = paste0("# FILTER: Could not parse - ", parsed$raw),
        packages = "dplyr", analysis_type = "Filter Cases",
        is_transformation = TRUE
      ))
    }
    var_normalized <- normalize_spss_names(var)

    r_code <- glue::glue("
data <- data |>
  dplyr::filter(`{var_normalized}` == 1 | `{var_normalized}` == TRUE)")
  }

  list(r_code = r_code, packages = "dplyr",
       analysis_type = "Filter Cases", is_transformation = TRUE)
}

convert_split_file <- function(parsed, sav_info) {
  if (isTRUE(parsed$variables$split_off)) {
    r_code <- '# SPLIT FILE OFF
# Subsequent analyses apply to the full dataset (no grouping)'
  } else {
    var <- parsed$variables$split_var
    if (!is.null(var)) {
      var <- normalize_spss_names(var)
      r_code <- glue::glue('# SPLIT FILE BY {var}
# Note: In SPSS, SPLIT FILE runs subsequent analyses separately per group.
# In R/jmv, use the splitBy argument in analysis functions, or filter manually.
# Grouping variable: {var}')
    } else {
      r_code <- paste0("# SPLIT FILE: Could not parse - ", parsed$raw)
    }
  }

  list(r_code = r_code, packages = character(),
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

  # Handle SPSS equality (single = to ==, but not <= >= !=)
  r_expr <- gsub("(?<!<|>|!|=)=(?!=)", "==", r_expr, perl = TRUE)

  # Handle SPSS <> (not equal) -> !=
  r_expr <- gsub("<>", "!=", r_expr)

  # Handle ~= (not equal) -> !=
  r_expr <- gsub("~=", "!=", r_expr)

  # Handle MISSING() -> is.na()
  r_expr <- gsub("\\bMISSING\\s*\\(([^)]+)\\)", "is.na(\\1)", r_expr, ignore.case = TRUE)
  r_expr <- gsub("\\bSYSMIS\\s*\\(([^)]+)\\)", "is.na(\\1)", r_expr, ignore.case = TRUE)
  r_expr <- gsub("~MISSING\\s*\\(", "!is.na(", r_expr, ignore.case = TRUE)
  r_expr <- gsub("~missing\\s*\\(", "!is.na(", r_expr, ignore.case = TRUE)

  # MEAN(a,b,c) -> rowMeans(cbind(a,b,c), na.rm=TRUE)
  r_expr <- gsub("MEAN\\s*\\(([^)]+)\\)", "rowMeans(cbind(\\1), na.rm = TRUE)", r_expr, ignore.case = TRUE)

  # SUM(...) -> rowSums(cbind(...), na.rm=TRUE)
  r_expr <- gsub("SUM\\s*\\(([^)]+)\\)", "rowSums(cbind(\\1), na.rm = TRUE)", r_expr, ignore.case = TRUE)

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
  # Also handle single-char variable names
  # (the above pattern requires 2+ chars due to the character class at end)

  # Restore R function names to proper case
  r_expr <- gsub("\\bROWMEANS\\b", "rowMeans", r_expr)
  r_expr <- gsub("\\bROWSUMS\\b", "rowSums", r_expr)
  r_expr <- gsub("\\bCBIND\\b", "cbind", r_expr)
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

  r_expr
}

#' Extract recode rules from RECODE command
#' @param cmd Character string of SPSS command text
#' @keywords internal
extract_recode_rules <- function(cmd) {
  # Find all parenthesized rules: (old=new)
  matches <- gregexpr("\\(([^)]+)\\)", cmd)[[1]]
  if (matches[1] == -1) return(list())

  rules <- list()
  for (i in seq_along(matches)) {
    rule_str <- substr(cmd, matches[i] + 1, matches[i] + attr(matches, "match.length")[i] - 2)
    parts <- strsplit(rule_str, "=")[[1]]
    if (length(parts) == 2) {
      rules[[length(rules) + 1]] <- list(old = trimws(parts[1]), new = trimws(parts[2]))
    }
  }
  rules
}

# ==============================================================================
# VARIABLE NAME HELPERS
# ==============================================================================

#' Normalize SPSS variable names to R-safe uppercase names
#'
#' @param x Character vector of variable names
#' @return Normalized variable names
normalize_spss_names <- function(x) {
  if (is.null(x)) return(x)
  x <- toupper(x)
  x <- gsub("[^A-Z0-9_]", ".", x)
  x <- ifelse(grepl("^[0-9.]", x), paste0("X", x), x)
  x
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
#' @return The parsed command with TO syntax expanded
#' @examples
#' \dontrun{
#' parsed   <- parse_sps("analysis.sps")
#' sav_info <- parse_sav("data.sav")
#' # Expand TO syntax (e.g., item1 TO item10)
#' parsed <- lapply(parsed, expand_spss_variables, sav_info = sav_info)
#' }
#' @export
expand_spss_variables <- function(parsed_command, sav_info) {
  all_names <- normalize_spss_names(sav_info$metadata$name)
  if (is.null(all_names)) return(parsed_command)

  if (!is.null(parsed_command$variables)) {
    parsed_command$variables <- lapply(parsed_command$variables, function(x) {
      if (is.character(x)) expand_to_syntax(x, all_names) else x
    })
  }

  parsed_command
}

#' Expand TO in a vector of variable names using a reference list
#' @param vars Character vector of variable names
#' @param all_names Character vector of all dataset variable names
#' @keywords internal
expand_to_syntax <- function(vars, all_names) {
  if (length(vars) < 3) return(vars)

  to_indices <- which(toupper(vars) == "TO")
  if (length(to_indices) == 0) return(vars)

  new_vars <- character()
  last_idx <- 1

  for (i in seq_along(to_indices)) {
    to_pos <- to_indices[i]
    if (to_pos == 1 || to_pos == length(vars)) next

    start_var <- vars[to_pos - 1]
    end_var <- vars[to_pos + 1]

    start_idx <- match(start_var, all_names)
    end_idx <- match(end_var, all_names)

    if (to_pos > last_idx + 1) {
      new_vars <- c(new_vars, vars[last_idx:(to_pos - 2)])
    }

    if (!is.na(start_idx) && !is.na(end_idx)) {
      direction <- if (start_idx <= end_idx) 1 else -1
      seq_idxs <- seq(start_idx, end_idx, by = direction)
      new_vars <- c(new_vars, all_names[seq_idxs])
    } else {
      new_vars <- c(new_vars, start_var, "TO", end_var)
    }

    last_idx <- to_pos + 2
  }

  if (last_idx <= length(vars)) {
    new_vars <- c(new_vars, vars[last_idx:length(vars)])
  }

  new_vars
}
