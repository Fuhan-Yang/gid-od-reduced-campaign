#' Load raw polio data and shapefile.
#'
#'
#' @param config configuration file to specify the data
#' @param force_refresh If TRUE, ignore cache and re-pull from source
#' @return list(raw_data = ..., global_dist = ...)
load_raw_inputs <- function(config, force_refresh = FALSE) {

    message("Pulling raw.data from source (this may take a while)...")
    raw_data <- sirfunctions::get_all_polio_data(
      size = "small",
      data_folder   = config$paths$data_folder,
      polis_folder  = config$paths$polis_folder
    )

    message("Loading global.dist from source...")
    global_dist <- sirfunctions::load_clean_dist_sp(
      fp = config$paths$global_dist_rds,
      type = "long"
    )

  list(raw_data = raw_data, global_dist = global_dist)
}
