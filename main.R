library(dplyr)
source("functions.R")  # your existing helpers: geo_data_remap, get_p_unprotected,
# calculate_protected, s_to_c, datebreak_to_year_factor

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

config <- yaml::read_yaml("config.yaml")

dir.create(config$paths$output_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Load shared inputs ONCE (country-independent, expensive) ----
inputs      <- load_raw_inputs(config)
raw_data    <- inputs$raw_data
global_dist <- inputs$global_dist
scope_all   <- prepare_scope(config$paths$scope_csv, config$countries, config$admin1_name_fixes)  # all countries; filtered per-run

# ---- Run pipeline per country ----
purrr::map(
  config$countries,
  run_country_pipeline,
  raw_data = raw_data,
  global_dist = global_dist,
  scope_all = scope_all,
  config = config
)

