# apa_plots.R
# APA 7-style plot themes and helpers

#' APA theme for ggplot2
#'
#' @param base_size Base font size
#' @param base_family Base font family
#' @return ggplot2 theme
theme_apa <- function(base_size = 12, base_family = "serif") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Title
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = base_size + 2),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = base_size),

      # Axes
      axis.title = ggplot2::element_text(face = "bold", size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1, color = "black"),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),
      axis.ticks = ggplot2::element_line(color = "black"),

      # Panel
      panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, color = "black", linewidth = 0.5),
      panel.background = ggplot2::element_rect(fill = "white"),

      # Legend
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold", size = base_size),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.key = ggplot2::element_rect(fill = "white"),

      # Strip (for facets)
      strip.text = ggplot2::element_text(face = "bold", size = base_size),
      strip.background = ggplot2::element_rect(fill = "gray95", color = "black")
    )
}

#' Create means comparison bar plot with error bars
#'
#' @param data Data frame
#' @param dv Dependent variable name
#' @param group Grouping variable name
#' @param ci Confidence interval level (default 0.95)
#' @return ggplot object
plot_means_comparison <- function(data, dv, group, ci = 0.95) {
  summary_data <- data |>
    dplyr::group_by(.data[[group]]) |>
    dplyr::summarise(
      mean = mean(.data[[dv]], na.rm = TRUE),
      sd = sd(.data[[dv]], na.rm = TRUE),
      n = dplyr::n(),
      se = sd / sqrt(n),
      ci_width = qt(1 - (1 - ci) / 2, n - 1) * se,
      .groups = "drop"
    )

  ggplot2::ggplot(summary_data, ggplot2::aes(x = .data[[group]], y = mean)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.7, width = 0.6) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean - ci_width, ymax = mean + ci_width),
      width = 0.2, linewidth = 0.8
    ) +
    ggplot2::labs(
      y = dv,
      x = group,
      caption = paste0(ci * 100, "% Confidence Intervals")
    ) +
    theme_apa()
}

#' Create interaction plot for factorial designs
#'
#' @param data Data frame
#' @param dv Dependent variable name
#' @param factor1 First factor (x-axis)
#' @param factor2 Second factor (lines)
#' @param ci Confidence interval level
#' @return ggplot object
plot_interaction <- function(data, dv, factor1, factor2, ci = 0.95) {
  summary_data <- data |>
    dplyr::group_by(.data[[factor1]], .data[[factor2]]) |>
    dplyr::summarise(
      mean = mean(.data[[dv]], na.rm = TRUE),
      sd = sd(.data[[dv]], na.rm = TRUE),
      n = dplyr::n(),
      se = sd / sqrt(n),
      ci_width = qt(1 - (1 - ci) / 2, n - 1) * se,
      .groups = "drop"
    )

  ggplot2::ggplot(summary_data,
                  ggplot2::aes(x = .data[[factor1]], y = mean,
                              color = .data[[factor2]], group = .data[[factor2]])) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean - ci_width, ymax = mean + ci_width),
      width = 0.1, linewidth = 0.7
    ) +
    ggplot2::labs(
      y = dv,
      x = factor1,
      color = factor2,
      caption = paste0(ci * 100, "% Confidence Intervals")
    ) +
    ggplot2::scale_color_brewer(palette = "Set2") +
    theme_apa()
}
