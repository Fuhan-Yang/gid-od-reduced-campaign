#' Generic horizontal grouped bar chart, used for all strategy
#' comparison plots. Facets by country, so it works equally well for
#' a single-country summary or a combined multi-country summary.
#'
#' @param df Dataframe (typically a province-level summary)
#' @param cols Named vector: new_label = old_column_name
#' @param ylab Y-axis label
#' @param caption Plot caption
#' @return a ggplot object
make_barplot <- function(df, cols, title, ylab,position = position,
                                  caption = "CDC Internal Analysis | Pre-decisional Draft") {
  n_countries <- dplyr::n_distinct(df$place.admin.0)

  p <- df %>%
    dplyr::rename(!!!cols) %>%
    tidyr::pivot_longer(cols = names(cols), names_to = title, values_to = "number") %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = place.admin.1, y = number, fill = data[[title]]), position = position) +
    ggplot2::xlab("State") +
    ggplot2::ylab(ylab) +
    ggplot2::labs(caption = caption) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  # only facet if there's more than one country in the data
  if (n_countries > 1) {
    p <- p + ggplot2::facet_wrap(place.admin.0 ~ ., nrow = 2, scales = "free")
  }
  p
}

#' Generate and save the full standard set of strategy comparison plots.
#' Works for a single-country summary or a combined multi-country one.
#'
#' @param summary Province-level summary dataframe
#' @param output_dir Directory to save PNGs into
save_all_plots <- function(summary, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  p1 <- make_barplot(
    summary,
    cols = c("0 dose received before strategies" = "all_0_dose_prov_pre",
             "1 or 2 doses received before strategies" = "all_other_dose_prov_pre"),
    title = "Unprotected before strategies",
    ylab = "Number of Population",
    position = "stack"
  )
  ggplot2::ggsave(file.path(output_dir, "Histogram of unprotected.png"), p1, width = 10, height = 8)

  # p2 <- make_strategy_barplot(
  #   summary,
  #   cols = c("Ideal Scope" = "fully_protected_ideal_post",
  #            "Reduced Scope" = "fully_protected_reduced_post"),
  #   ylab = "Number of population that are fully protected after strategies"
  # )
  # ggplot2::ggsave(file.path(output_dir, "Histogram of fully_protected.png"), p2, width = 10, height = 8)
  #
  # p3 <- make_strategy_barplot(
  #   summary,
  #   cols = c("Ideal Scope" = "partially_protected_ideal_post",
  #            "Reduced Scope" = "partially_protected_reduced_post"),
  #   ylab = "Number of population that are partially protected after strategies"
  # )
  # ggplot2::ggsave(file.path(output_dir, "Histogram of partially_protected.png"), p3, width = 10, height = 8)
  #
  # p4 <- make_strategy_barplot(
  #   summary,
  #   cols = c("Ideal Scope" = "case_ideal_post",
  #            "Reduced Scope" = "case_reduced_post"),
  #   ylab = "Number of AFP cases that could have after strategies"
  # )
  # ggplot2::ggsave(file.path(output_dir, "Histogram of case.png"), p4, width = 10, height = 8)
  #
  # p5 <- make_strategy_barplot(
  #   summary,
  #   cols = c("Ideal Scope" = "case_partially_protected_ideal_post",
  #            "Reduced Scope" = "case_partially_protected_reduced_post"),
  #   ylab = "Number of AFP cases that could have from the partially-protected children"
  # )
  # ggplot2::ggsave(file.path(output_dir, "Histogram of partially-protected case.png"), p5, width = 10, height = 8)

  # invisible(list(strategies = p1, fully_protected = p2, partially_protected = p3,
  #                case = p4, partially_protected_case = p5))
}
