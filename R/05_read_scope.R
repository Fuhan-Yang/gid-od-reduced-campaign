#' Clean strategy "scope" data (ideal/reduced target populations per
#' state), optionally filtered to ONE country.
#'
#' Scope data for all countries typically lives in a single CSV, so this
#' reads once and filters -- read this outside the per-country loop if
#' you want to avoid re-reading the CSV for every country.
#'
#' @param scope_csv_path Path to scope.csv
#' @param country Optional single country name to filter to
#' @return dataframe: place.admin.0, place.admin.1, ideal_population, reduced_population
prepare_scope <- function(scope_csv_path, country = NULL) {
  scope <- read.csv(scope_csv_path) %>%
    dplyr::mutate(
      place.admin.1 = toupper(state),
      ideal_population   = dplyr::if_else(is.na(target_population), 0, target_population),
      reduced_population = dplyr::if_else(is.na(reduced_population), 0, reduced_population)
    ) %>%
    dplyr::rename(place.admin.0 = country) %>%
    dplyr::select(place.admin.0, place.admin.1, ideal_population, reduced_population)

  if (!is.null(country)) {
    scope <- scope %>% dplyr::filter(place.admin.0 == country)
  }
  scope
}
