library(dplyr)
library(ggplot2)
library(openxlsx)
library(sf)
library(yaml)
library(purrr)
library(flextable)

source("functions.R")  # your existing helpers: geo_data_remap, get_p_unprotected,
# calculate_protected, s_to_c, datebreak_to_year_factor

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

config <- yaml::read_yaml("config.yaml")

dir.create(config$paths$output_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Load shared inputs ONCE (country-independent, expensive) ----
inputs      <- load_raw_inputs(config)
raw_data    <- inputs$raw_data
global_dist <- inputs$global_dist
scope_all   <- prepare_scope(config$paths$scope_csv)  # all countries; filtered per-run

# ---- Run pipeline per country ----
results <- purrr::map(
  config$countries,
  run_country_pipeline,
  raw_data = raw_data,
  global_dist = global_dist,
  scope_all = scope_all,
  config = config
) %>% purrr::set_names(config$countries)

# ---- Report success/failure ----
succeeded <- purrr::keep(results, ~ .x$success)
failed    <- purrr::keep(results, ~ !.x$success)

message("\n=== Pipeline summary ===")
message(length(succeeded), "/", length(results), " countries succeeded.")
if (length(failed) > 0) {
  for (f in failed) message("  FAILED: ", f$country, " -- ", f$error)
}

# ---- Combine successful countries' summaries for cross-country plots ----
if (length(succeeded) > 0) {
  combined_summary <- purrr::map_dfr(succeeded, "summary")
  combined_full    <- purrr::map_dfr(succeeded, "full")
  combined_underest <- purrr::map_dfr(succeeded, "underestimated_pop")

  combined_dir <- file.path(config$paths$output_dir, "combined")
  dir.create(combined_dir, recursive = TRUE, showWarnings = FALSE)

  openxlsx::write.xlsx(combined_full,    file.path(combined_dir, "strategies_full.xlsx"), rowNames = FALSE)
  openxlsx::write.xlsx(combined_summary, file.path(combined_dir, "strategies.xlsx"), rowNames = FALSE)
  write.csv(combined_underest, file.path(combined_dir, "underestimated_province_pop.csv"), row.names = FALSE)

  save_all_plots(combined_summary, combined_dir)
  message("Combined outputs written to: ", combined_dir)
}

message("Pipeline complete.")
