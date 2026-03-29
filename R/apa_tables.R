# apa_tables.R
# APA 7-style table generation helpers
# These complement jmv output with additional formatting options

#' Generate APA-style HTML table wrapper with copy button
#'
#' @param table_html HTML table content
#' @param caption Table caption
#' @return HTML string with copy button
apa_table_wrapper <- function(table_html, caption = "") {
  glue::glue('
<div class="apa-table-container" style="margin: 20px 0;">
  <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;">
    <em style="font-size: 14px;">{caption}</em>
    <button onclick="copyTableToClipboard(this)"
            style="padding: 5px 10px; cursor: pointer; background: #4CAF50; color: white; border: none; border-radius: 4px;">
      Copy to Clipboard
    </button>
  </div>
  {table_html}
</div>

<script>
function copyTableToClipboard(btn) {{
  const container = btn.closest(".apa-table-container");
  const table = container.querySelector("table");
  if (table) {{
    const range = document.createRange();
    range.selectNode(table);
    window.getSelection().removeAllRanges();
    window.getSelection().addRange(range);
    document.execCommand("copy");
    window.getSelection().removeAllRanges();
    btn.textContent = "Copied!";
    setTimeout(() => {{ btn.textContent = "Copy to Clipboard"; }}, 2000);
  }}
}}
</script>
')
}

#' Format p-value for APA style
#'
#' @param p P-value
#' @return Formatted string
format_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < .001) return("< .001")
  sprintf("%.3f", p)
}

#' Format effect size for APA style
#'
#' @param es Effect size value
#' @param ci_lower Lower CI bound
#' @param ci_upper Upper CI bound
#' @param es_name Name of effect size (e.g., "d", "eta-squared")
#' @return Formatted string
format_effect_size <- function(es, ci_lower = NULL, ci_upper = NULL, es_name = "d") {
  es_str <- sprintf("%.2f", es)

  if (!is.null(ci_lower) && !is.null(ci_upper)) {
    ci_str <- sprintf("95%% CI [%.2f, %.2f]", ci_lower, ci_upper)
    return(paste0(es_name, " = ", es_str, ", ", ci_str))
  }

  paste0(es_name, " = ", es_str)
}

#' Create APA-style descriptives table
#'
#' @param data Data frame
#' @param vars Variable names
#' @return HTML table
apa_descriptives_table <- function(data, vars) {
  results <- data.frame(
    Variable = vars,
    M = sapply(vars, function(v) mean(data[[v]], na.rm = TRUE)),
    SD = sapply(vars, function(v) sd(data[[v]], na.rm = TRUE)),
    Min = sapply(vars, function(v) min(data[[v]], na.rm = TRUE)),
    Max = sapply(vars, function(v) max(data[[v]], na.rm = TRUE)),
    n = sapply(vars, function(v) sum(!is.na(data[[v]])))
  )

  knitr::kable(results,
               format = "html",
               digits = 2,
               caption = "Descriptive Statistics") |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "condensed"),
      full_width = FALSE
    )
}
