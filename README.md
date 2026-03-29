# spss2rmarkdown

Convert SPSS `.sps` syntax files and `.sav` data files to reproducible
R Markdown reports using the **jmv** package ecosystem.

## Installation

Install from GitHub:

```r
# install.packages("remotes")
remotes::install_github("giladfeldman/spss2rmarkdown", subdir = "pkg/spss2rmarkdown")
```

## Quick Start

```r
library(spss2rmarkdown)

# Step-by-step pipeline:
# 1. Parse the .sps syntax file
parsed <- parse_sps("analysis.sps")

# 2. Parse the .sav data file
sav_info <- parse_sav("data.sav")

# 3. Convert SPSS commands to R code
converted <- convert_all_commands(parsed, sav_info)

# 4. Generate the R Markdown report
rmd <- generate_rmd(
  sav_data       = sav_info,
  sav_path       = "data.sav",
  parsed_syntax  = parsed,
  converted_code = converted,
  output_dir     = "output"
)
```

## Features

- **60+ SPSS commands** supported: `DESCRIPTIVES`, `FREQUENCIES`,
  `CORRELATIONS`, `T-TEST`, `ONEWAY`, `GLM`, `UNIANOVA`, `REGRESSION`,
  `LOGISTIC REGRESSION`, `CROSSTABS`, `RELIABILITY`, `FACTOR`, `NPAR TESTS`,
  `MANOVA`, `MIXED`, `PROCESS`, and more.
- **Data manipulation**: `COMPUTE`, `RECODE`, `IF`, `SELECT IF`, `FILTER`,
  `SPLIT FILE`, `SORT CASES`, `COUNT`, `AGGREGATE`.
- **Expression converter**: SPSS operators and functions translated to R
  equivalents (`MEAN()` -> `rowMeans()`, `MISSING()` -> `is.na()`,
  `AND`/`OR` -> `&`/`|`, etc.).
- **TO syntax expansion**: variable ranges like `VAR1 TO VAR5` resolved
  against the actual dataset.
- **Self-contained output**: generated `.Rmd` includes original SPSS syntax
  as foldable code blocks for easy comparison.
- **APA 7 helpers**: publication-ready tables and ggplot2 themes included.

## Supported SPSS Commands

### Statistical Analyses

| SPSS Command | R Package |
|---|---|
| DESCRIPTIVES / FREQUENCIES | jmv::descriptives |
| CORRELATIONS / PARTIAL CORR | jmv::corrMatrix |
| T-TEST (independent, paired, one-sample) | jmv::ttestIS / ttestPS / ttestOneS |
| ONEWAY | jmv::anovaOneW |
| GLM / UNIANOVA | jmv::ANOVA |
| REGRESSION | jmv::linReg |
| LOGISTIC REGRESSION | jmv::logRegBin |
| CROSSTABS | jmv::contTables |
| RELIABILITY | jmv::reliability |
| FACTOR | jmv::efa |
| NPAR TESTS | jmv (non-parametric family) |
| MANOVA | stats::manova |
| MIXED | afex::mixed |
| PROCESS | mediation / custom |

### Data Manipulation

`COMPUTE`, `RECODE`, `IF`, `DO IF`, `SELECT IF`, `FILTER`, `SORT CASES`,
`SPLIT FILE`, `DELETE VARIABLES`, `COUNT`, `AGGREGATE`, `RANK`.

## License

MIT
