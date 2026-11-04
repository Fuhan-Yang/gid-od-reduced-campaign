library(sf)
#' Build district-year SIA round counts for ONE country.
#'
#' @param raw_sia raw.data$sia
#' @param country Single country name
#' @param date_start,date_end Date range of eligible activities
#' @param cohort_birthdays Vector of Dates used to bucket activities into years
#' @param sia_filters list(age_group=, vaccine_type=, status=)
#' @param name_fixes Named vector: old admin2 name -> new admin2 name
#' @return dataframe: place.admin.2, year, place.admin.0, place.admin.1, n_sia_per_year
prepare_sia_counts <- function(raw_sia, country, date_start, date_end,
                               cohort_birthdays, sia_filters, admin1_name_fixes, admin2_name_fixes) {
  raw_sia %>%
    dplyr::mutate(activity.start.date = as.Date(activity.start.date)) %>%
    dplyr::filter(
      place.admin.0 == country,
      activity.start.date >= date_start & activity.start.date < date_end,
      age.group == sia_filters$age_group,
      vaccine.type == sia_filters$vaccine_type,
      status == sia_filters$status
    ) %>%
    dplyr::mutate(year = datebreak_to_year_factor(activity.start.date, cohort_birthdays)) %>%
    dplyr::mutate(
      place.admin.1 = apply_name_fixes(place.admin.1, admin1_name_fixes),
      place.admin.2 = apply_name_fixes(place.admin.2, admin2_name_fixes)
    ) %>%
    dplyr::select(sia.code, place.admin.0, activity.start.date, place.admin.1, place.admin.2, year) %>%
    dplyr::distinct() %>%
    dplyr::group_by(place.admin.2, year) %>%
    dplyr::reframe(
      place.admin.0 = place.admin.0[1],
      place.admin.1 = place.admin.1[1],
      n_sia_per_year = dplyr::n_distinct(sia.code)
    )
}

#' Interpolate SIA counts onto the target-year map and recombine.
#'
#' @param sia_count Output of prepare_sia_counts()
#' @param shapefile Output of prepare_shapefile() (same country)
#' @param target_year Character, e.g. "2026"
#' @param crs Numeric EPSG code
#' @return dataframe (geometry dropped): place.admin.0/1/2, year, n_sia_per_year
remap_sia_counts <- function(sia_count, shapefile, target_year, crs) {
  if (nrow(sia_count) == 0) {
    warning("remap_sia_counts(): sia_count has 0 rows -- check filters.")
    return(sia_count %>% dplyr::mutate(year = as.character(year)))
  }

  interpolated <- geo_data_remap(
    data_to_interpolate = sia_count,
    shapefile = shapefile,
    target_year = target_year,
    data_col_name = "n_sia_per_year",
    crs = crs,
    geo_id = "GUID",
    admin_level = "2",
    na.action = "NA is 0"
  )

  interpolated %>%
    dplyr::bind_rows(sia_count %>% dplyr::filter(year == target_year)) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(place.admin.0, place.admin.1, place.admin.2, year, n_sia_per_year)
}
