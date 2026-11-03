library(sf)
#' Filter and reshape the global district shapefile to ONE country and
#' the birth-year cohorts under analysis.
#'
#' @param global_dist Output of sirfunctions::load_clean_dist_sp()
#' @param country Single country name (upper case)
#' @param birthyears Character vector of birth years to retain
#' @return sf dataframe with place.admin.0/1/2, year, AREA_SQM, GUID
prepare_shapefile <- function(global_dist, country, birthyears) {
  global_dist %>%
    dplyr::filter(ADM0_NAME == country, active.year.01 %in% birthyears) %>%
    dplyr::rename(
      place.admin.0 = ADM0_NAME,
      place.admin.1 = ADM1_NAME,
      place.admin.2 = ADM2_NAME
    ) %>%
    dplyr::mutate(year = as.character(active.year.01)) %>%
    dplyr::select(place.admin.0, place.admin.1, place.admin.2, year, AREA_SQM, GUID)
}
