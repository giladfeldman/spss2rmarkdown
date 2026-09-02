# spss2rmarkdown 0.3.0

Twelve commits of fixes since 0.2.0 (2026-08-04). Several changed *values* or
suppressed output entirely rather than raising an error, so any script using the
affected commands should be re-run.

## Numeric correctness

* **`TO` ranges inside `REGRESSION /METHOD` blocks are expanded.** REGRESSION
  stores its per-`/METHOD` predictor lists as a *list* of character vectors, and
  the expander only handled bare character vectors — so `/METHOD=ENTER session1
  to session9` reached `jmv::linReg(blocks = ...)` with the literal token `to`
  still in it. jmv then died with an opaque "object not found" and the entire
  hierarchical regression rendered **no statistics at all**, while SPSS reports
  the full model. The unresolved-range guard missed it for the same reason.
* **`RELIABILITY` no longer fabricates McDonald's omega.** SPSS `MODEL=ALPHA`
  does not compute omega; a value was being emitted anyway.
* **T-TEST `GROUPS=g(v1 v2)` semantics.** The listed values were not used to
  subset (a hard error on any group variable with more than two levels), their
  order was ignored (a sign-flip risk), and every dependent variable after the
  first was dropped.
* **Multi-`.sav` pairing is resolved by variable coverage**, not by filename
  order. Uploads without an explicit `GET FILE` were auto-paired with the
  alphabetically first `.sav`, which silently analysed the wrong dataset.
* `FACTOR` is converted with `psych` instead of the non-deterministic
  `jmv::efa`, which returned different loadings across runs.
* `COUNT` criteria are parsed as term *lists*, handling `MISSING`, mixed forms,
  and `THRU` ranges; previously these emitted invalid R.
* `FACTOR` / `MEANS` variable extraction fixed; comment-only stubs guarded.

## Robustness

* Analysis and transformation errors **never render blank** — a failed command
  now shows what failed instead of producing an empty section.
* `$CASENUM` translates to `dplyr::row_number()`, and is flagged as unsafe when
  a `SELECT IF` / `FILTER` / `SORT CASES` / `SPLIT FILE` precedes it, since the
  equivalence only holds without those.
* Documentation and dependency declarations corrected (`utils::modifyList`
  declared, `olsrr` moved to Suggests, roxygen blocks repaired).

## Conversion coverage

* `$CASENUM`, SPSS's per-case sequential row number, translates to
  `dplyr::row_number()` (previously only `$SYSMIS` among bare-`$` system
  variables was handled; a bare `$CASENUM` leaked through and broke the
  generated R the same way `$SYSMIS` used to, R-0096). The substitution emits
  a `# NOTE [SPSS]` caveat comment above the generated line, because
  `row_number()` only matches true SPSS `$CASENUM` semantics when no
  `SELECT IF`/`FILTER`/`SORT CASES`/`SPLIT FILE` precedes the reference in the
  same syntax file — the converter cannot verify that automatically, so it
  flags it for manual review instead of silently trusting it. `$DATE`,
  `$DATE11`, `$JDATE`, `$TIME` (run-timestamp system variables, not
  reproducible data values) and `$LENGTH`, `$WIDTH` (output-formatting
  settings) are intentionally still unhandled.

# spss2rmarkdown 0.2.0

This release publishes ~4 months of fixes developed since 0.1.0 (45 commits).
Highlights, grouped by area.

## RECODE correctness

Several of these produced *wrong values* rather than errors, so any script
using the affected forms should be re-run.

* `RECODE` now accepts lowercase `thru` and lowercase `lo`/`hi` range
  keywords. Previously the keyword was *detected* case-insensitively but the
  bounds were *split* case-sensitively, leaving the upper bound `NA` and
  aborting the whole command with "missing value where TRUE/FALSE needed".
* Comma- and space-separated value lists — `(1,2,3=1)`, `(1 2 3=1)` — now emit
  `%in%` conditions. They previously emitted invalid R (`X == 1,2,3`).
* A range and discrete values may share one spec: `(1 THRU 3 5=9)`,
  `(LO THRU 0 99=1)`. `THRU` is handled as an infix operator.
* `COPY` is honoured as a rule output, both as `ELSE=COPY` and on any
  individual rule. It previously emitted a bare `COPY` symbol that failed at
  run time.
* `RECODE ... INTO` now follows SPSS's missing-value semantics: with no `ELSE`,
  unmatched cases leave a *new* target system-missing (they are no longer
  copied from the source), while an *existing* target keeps its prior values.
  In-place `RECODE` still leaves unmatched values unchanged.
* Quoted string values containing commas, spaces, or `=` — `('a b'='x')`,
  `('1,2'='y')`, `('a=b'='x')` — are tokenized quote-aware. They were
  previously shredded into conditions that silently never matched.
* An unparseable rule now fails loudly instead of emitting invalid R.

## Other fixes

* `UNIANOVA`/`GLM` factor lists after `BY` are no longer truncated at the first
  `w`/`i`/`t`/`h` character. The old `[^/WITH]` pattern was a negated
  *character class*, not a negated word, so factors such as `condition`,
  `treatment`, `weight`, `height`, and `time` were mangled or dropped.
* `MISSING VALUES` is translated as a real transformation (including
  `LO THRU`, `(9, 99, 999)`, and `LOWEST THRU` forms) rather than skipped as
  metadata, so user-missing codes no longer survive as literal numbers.
* `MEAN.n` / `SUM.n` and the rest of the `.n` family expand to a
  minimum-valid-count gate, not a bare `rowMeans(na.rm = TRUE)`.
* Linear regression reports the analysed N after listwise deletion.
* GLM `/WSFACTOR` routes repeated-measures designs to `jmv::anovaRM`.
* `/METHOD=STEPWISE` honours `PIN`/`POUT` via `olsrr`.
* Quote-aware `IF` condition parsing; `$SYSMIS` translates to `NA`; word-form
  relational operators (`NE`, `EQ`, `LT`, `GT`, `LE`, `GE`).
* `var1 TO varN` resolves across multiple paired `.sav` files; 3-letter command
  abbreviations; non-ASCII variable names de-duplicate injectively.
* Per-analysis `FILTER` routing replaces a global mutate, so `FILTER BY` no
  longer leaks across analyses.
* Analysis tables render as real HTML tables (fixes `ââ` mojibake from Unicode
  box-drawing), with a floating TOC and code folding.
* `DATEDIFF` converts via `lubridate::time_length`.

# spss2rmarkdown 0.1.2

* Validate every generated R identifier through `is_valid_r_ident()` before
  interpolating it as a bare name into generated R (cross-project rec R-0009,
  ported from STATA2Rmarkdown). COMPUTE / IF / RECODE now emit a
  `# NOTE [SPSS]: ...` comment instead of unparseable `dplyr::mutate( = ...)`
  when a target fails to normalize to a valid identifier (e.g. an unexpanded
  macro that collapses to "").

# spss2rmarkdown 0.1.1

* Quote-aware SPSS `IF` condition parsing: an in-string `") word ="` inside a
  quoted literal no longer mis-splits the condition and target (cross-project
  rec R-0008, ported from STATA2Rmarkdown's `find_unquoted_word`).

# spss2rmarkdown 0.1.0

* Initial release
* Parse SPSS .sps syntax files with 60+ command support
* Parse SPSS .sav data files with full metadata extraction
* Convert SPSS commands to R using the jmv package
* Expression converter for SPSS -> R syntax (operators, functions, missing values)
* SPSS TO syntax expansion for variable ranges
* Generate reproducible R Markdown reports with original SPSS syntax preserved
* APA 7-style table and plot helpers
