### functions ###
library(sf)
library(areal)

# get the probability of eligible for protection: the one that has been exposed to n SIA with n_sia_cumu have been given since born
get_sia_coverage <- function(n, n_sia_cumu, p_missing, type = "independent") {

  if(type == "independent") {
    dbinom(n, n_sia_cumu, (1 - p_missing))
  } else if(type == "same") {
    p_missing
  }

}

# convert birthdays to year-factor
# split the sia date into intervals with datebreak
datebreak_to_year_factor <- function(date, datebreak) {
  factor(findInterval(date, datebreak),
         levels = c(1:(length(datebreak) - 1)),
         labels = format(datebreak[-1], "%Y")
  )
}

# the strategies will implement proportional to the n_0,1,2 children, get the proportion #
calculate_protected <- function(p_missing, strategy_pop, prov_pop, type = "independent"){

  if(type == "independent") {
    (1-p_missing)^2*strategy_pop * strategy_pop/prov_pop
  } else if(type == "same") {
    (1-p_missing)*strategy_pop * strategy_pop/prov_pop
  }

}

### unprotected to case ###
s_to_c <- function(s, attack_rate, ci_r) {
  s*attack_rate*ci_r
}

### data prep for geo interpolation ###
geo_data_prep <- function(data_to_interpolate, shapefile,data_col_name,
                          geo_id,admin_level, na.action) {

  if(admin_level == "0") {
    admin_cols <- "place.admin.0"
  } else if (admin_level == "1") {
    admin_cols <- c("place.admin.0", "place.admin.1")
  } else if (admin_level == "2") {
    admin_cols <- c("place.admin.0", "place.admin.1", "place.admin.2")
  } else {
    stop("Wrong admin_level.Only valid numbers are 0,1,2.")
  }

  if(all(!admin_cols %in% colnames(shapefile))) {
    stop(paste(admin_cols, "is missing in shapefile."))
  }

  if(all(!admin_cols %in% colnames(data_to_interpolate))) {
    stop(paste(admin_cols, "is missing in data."))
  }

  if(!("year" %in% colnames(data_to_interpolate))| !("year" %in% colnames(shapefile))) {

    stop("`year` must exist for both data and shapefile.")
  }

  if(!geo_id %in% colnames(shapefile)) {
    stop(paste(geo_id, "is missing in shapefile."))
  }

  shapefile %>%
    left_join(data_to_interpolate, by = c(admin_cols, "year")) -> data_shape

  ## there could be NA for the regions that don't have the available data,
  ## one could assume the value in this region is 0
  if(na.action == "NA is 0") {
    data_shape[is.na(data_shape[,data_col_name]), data_col_name] = 0
  }

  return(data_shape)

}

### interpolate the data by year ###
geo_data_remap <- function(data_to_interpolate, shapefile, target_year,
                           data_col_name, crs, geo_id = "GUID", admin_level = "2",
                           na.action = "NA is 0") {

  data_shape <- geo_data_prep(data_to_interpolate, shapefile, data_col_name,
                              geo_id, admin_level,
                              na.action)

  years <- as.character(unique(data_shape$year))
  years <- years[-which(years == as.character(target_year))]

  target <- shapefile %>%
    filter(year == target_year) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = crs)

  remapped_data <- data.frame()

  for(yr in years) {

    source <- data_shape %>% filter(year == yr)
    inter <- geo_interpolate(source, target, data_col_name, crs, geo_id) %>%
      mutate(year = yr)

    remapped_data <- rbind(remapped_data,inter)
  }

  return(remapped_data)

}

### geospatial interpolation ###
geo_interpolate <- function(source, target, data_col_name, crs, geo_id) {

  source %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = crs) %>%
    sf::st_make_valid() -> source

  target %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = crs) %>%
    sf::st_make_valid() -> target

  if(!ar_validate(source, target, varList = data_col_name)) {
    stop("Validation failed. Run ar_validate() with verbose = T to diagnose.")
  }

  aw_interpolate(target, tid = !!rlang::sym(geo_id), source, sid = !!rlang::sym(geo_id),
                 weight = "sum", output = "sf", extensive = data_col_name)
}

apply_name_fixes <- function(x, name_fixes) {
  if (is.null(name_fixes) || length(name_fixes) == 0) return(x)
  dplyr::replace_values(
    as.character(x),
    from = names(name_fixes),
    to   = unlist(name_fixes, use.names = FALSE)
  )
}
