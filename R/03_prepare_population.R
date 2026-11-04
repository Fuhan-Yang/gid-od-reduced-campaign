library(sf)
#' Clean district-level population data for ONE country: rename,
#' harmonize admin-2 names, filter to years, compute birth cohort.
#'
#' @param dist_pop_raw raw.data$dist.pop
#' @param country Single country name
#' @param birthyears Character vector of birth years
#' @param name_fixes Named list/vector: old admin2 name -> new admin2 name
#' @return dataframe with place.admin.0/1/2, year, birth_pop
prepare_district_population <- function(dist_pop_raw, country, birthyears, name_fixes) {
  dist_pop_raw %>%
    dplyr::rename(place.admin.0 = ctry, place.admin.1 = prov, place.admin.2 = dist) %>%
    dplyr::mutate(year = as.character(year), 
                  place.admin.1 = apply_name_fixes(place.admin.1, name_fixes), 
                  place.admin.2 = apply_name_fixes(place.admin.2, name_fixes)) %>%
    dplyr::mutate(place.admin.1 = if_else(place.admin.1 == "OROMIYA", "OROMIA", place.admin.1)) %>%
    dplyr::select(place.admin.0, place.admin.1, place.admin.2, year, u5pop) %>%
    dplyr::filter(place.admin.0 == country, year %in% birthyears) %>%
    dplyr::mutate(birth_pop = u5pop / 5) %>%
    dplyr::select(place.admin.0, place.admin.1, place.admin.2, year, birth_pop)
}

#' Interpolate district population onto the target-year map, then
#' recombine with the target year's own (non-interpolated) data.
#'
#' @param dist_pop Output of prepare_district_population()
#' @param shapefile Output of prepare_shapefile() (same country)
#' @param target_year Character, e.g. "2026"
#' @param crs Numeric EPSG code for interpolation
#' @return dataframe (geometry dropped): place.admin.0/1/2, year, birth_pop
remap_district_population <- function(dist_pop, shapefile, target_year, crs) {
  if (nrow(dist_pop) == 0) {
    warning("remap_district_population(): dist_pop has 0 rows -- check filters.")
    return(dist_pop %>% dplyr::mutate(year = as.character(year)))
  }

  interpolated <- geo_data_remap(
    data_to_interpolate = dist_pop,
    shapefile = shapefile,
    target_year = target_year,
    data_col_name = "birth_pop",
    crs = crs,
    geo_id = "GUID",
    admin_level = "2",
    na.action = "NA is 0"
  )

  interpolated %>%
    dplyr::bind_rows(dist_pop %>% dplyr::filter(year == target_year)) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(place.admin.0, place.admin.1, place.admin.2, year, birth_pop)
}

#' Clean province-level population for ONE country (used for strategy
#' cost denominators).
#'
#' @param prov_pop_raw raw.data$prov.pop
#' @param country Single country name
#' @param target_year Character, e.g. "2026"
#' @return dataframe: place.admin.0, place.admin.1, u5pop
prepare_province_population <- function(prov_pop_raw, country, target_year) {
  prov_pop_raw %>%
    dplyr::rename(place.admin.0 = ctry, place.admin.1 = prov) %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::filter(place.admin.0 == country, year == target_year) %>%
    dplyr::select(place.admin.0, place.admin.1, u5pop)
}
