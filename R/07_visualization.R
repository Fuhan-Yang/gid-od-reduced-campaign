library(flextable)
library(ggplot2)

#' Generic horizontal grouped bar chart, used for all strategy
#' comparison plots. Facets by country, so it works equally well for
#' a single-country summary or a combined multi-country summary.
#'
#' @param df Dataframe (typically a province-level summary)
#' @param cols Named vector: new_label = old_column_name
#' @param ylab Y-axis label
#' @param caption Plot caption
#' @return a ggplot object

make_barplot <- function(data,
                        country,
                        out_dir,
                        width = 8.5,
                        height = 5,
                        save = TRUE) {

  caption_txt <- "CDC Internal Analysis | Pre-decisional Draft"

  # Optional filter to one country (place.admin.0)
  if (!is.null(country)) {
    data <- data %>% filter(place.admin.0 == country)
  }

  # ---- Plot 1: unprotected before campaigns ----
  ymax <- max(data$all_0_dose_prov_pre + data$all_other_dose_prov_pre, na.rm = T)
  p_before <- data %>%
    rename(`0 dose received before campaigns` = "all_0_dose_prov_pre",
         `1 or 2 doses received before campaigns` = "all_other_dose_prov_pre") %>%
    tidyr::pivot_longer(cols = c("0 dose received before campaigns",
                                "1 or 2 doses received before campaigns"),
                        names_to = "Vulnerable\nbefore campaigns", values_to = "number") %>%
    ggplot() +
    geom_col(aes(x = place.admin.1, y = number, fill = `Vulnerable\nbefore campaigns`), position = "stack") +
    scale_y_continuous(breaks = round(seq(0, ymax * 1.1, length.out = 8)),
                       labels = round(seq(0, ymax * 1.1, length.out = 8)/1e5)) +
    xlab("State") +
    ylab("Number of children vulnerable (\u00d7 100,000)") +
    # labs(caption = "CDC Internal Analysis | Pre-decisional Draft") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")


  # ---- Plot 2: unprotected after campaigns ----
  ymax <- max(data$unprotected_reduced_post - data$unprotected_ideal_post, na.rm = T)
  p_after <- data %>%
    mutate(difference = unprotected_reduced_post - unprotected_ideal_post) %>%
    ggplot() +
    geom_col(aes(x = place.admin.1, y = difference)) +
    scale_y_continuous(breaks = round(seq(0, ymax * 1.1, length.out = 8)),
                      labels = round(seq(0, ymax * 1.1, length.out = 8)/1e5)) +
    # facet_wrap(place.admin.0 ~ ., nrow = 2, scales = "free") +
    xlab("State") +
    ylab("Number of children that remain vulnerable after campaigns (\u00d7 100,000)") +
    # labs(caption = "CDC Internal Analysis | Pre-decisional Draft") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")

  # ---- Save ----
  if (save) {
    ggsave(file.path(out_dir,
                     paste0("Histogram of unprotected before campaigns ", country, ".png")),
           plot = p_before, width = width, height = height)
    ggsave(file.path(out_dir,
                     paste0("Histogram of unprotected after campaigns ", country, ".png")),
           plot = p_after, width = width, height = height)
  }
}

make_table <- function(data, out_dir, country) {

  country_df <- data %>%
    select(-u5pop) %>%
    group_by(place.admin.0) %>%
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = T))) %>%
    ungroup() %>%
    mutate(protected_ideal_post = partially_protected_ideal_post + fully_protected_ideal_post,
          protected_reduced_post = partially_protected_reduced_post + fully_protected_reduced_post) %>%
    select(place.admin.0, all_unprotected_prov_pre, all_0_dose_prov_pre,
          all_other_dose_prov_pre,
          unprotected_ideal_post, protected_ideal_post, unprotected_reduced_post,
          protected_reduced_post)
  
  flextable(country_df) %>%
  set_header_labels(
    place.admin.0 = "Country",
    all_unprotected_prov_pre = "Total vulnerable",
    all_0_dose_prov_pre = "0 doses",
    all_other_dose_prov_pre = "1 or 2 doses",
    unprotected_ideal_post = "Vulnerable",
    protected_ideal_post = "Protected", 
    unprotected_reduced_post = "Vulnerable",
    protected_reduced_post = "Protected"
  ) %>%
  add_header_row(
    values = c("", "Before campaigns",
               "After campaigns: Ideal scope", "After campaigns: Reduced scope"),
    colwidths = c(1, 3, 2, 2)
  ) %>%
  colformat_num(j = 1:8, big.mark = ",", digits = 0) %>%
  theme_booktabs() %>%                       # white background, horizontal rules only
  bg(bg = "white", part = "all") %>%
  bold(part = "header") %>%
  align(align = "center", part = "header") %>%
  align(j = 1:8, align = "right", part = "body") %>%
  merge_v(j = "place.admin.0") %>%
  vline(j = c(1, 4, 6), border = officer::fp_border(color = "grey70"), part = "all") %>%
  hline(border = officer::fp_border(color = "grey70"), part = "all") %>%
  valign(j = "place.admin.0", valign = "top") %>%
  fontsize(size = 9, part = "all") %>%
  add_footer_lines("CDC Internal Analysis | Pre-decisional Draft") %>%
  italic(part = "footer") %>%
  autofit() -> ft

  flextable::save_as_image(ft, path = file.path(out_dir,
                     paste0("campaigns ", country, ".png")))

}

#' Generate and save the full standard set of strategy comparison plots.
#' Works for a single-country summary or a combined multi-country one.
#'
#' @param summary Province-level summary dataframe
#' @param output_dir Directory to save PNGs into
save_all_plots <- function(summary, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  country <- unique(summary$place.admin.0)

  if(length(country) != 1) {
    stop("Only one country should be plotted!")
  }

  p1 <- make_barplot(summary, out_dir = output_dir, country = country)
  p2 <- make_table(data = summary, out_dir = output_dir, country = country)

  invisible(list(p1, p2))
  
}
