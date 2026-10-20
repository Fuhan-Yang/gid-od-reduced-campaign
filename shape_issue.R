rm(list=ls())
library(dplyr)
library(ggplot2)
library(sf)

global.dist <-
  sirfunctions::load_clean_dist_sp(
    fp = file.path(
      "GID/PEB/SIR",
      polis_folder="Sandbox/POLIS_fy",
      "misc",
      "global.dist.rds"
    ),
    type = "long"
  )


raw.data <- sirfunctions::get_all_polio_data(
    size = "small", data_folder = "GID/PEB/SIR/Data",
    polis_folder = "GID/PEB/SIR/POLIS"
)

##### process #####
date_start <- as.Date("2021-11-01")
date_end <- as.Date("2026-11-01")

# change <- read.csv("changes.csv")
# change %>%
#   mutate(new_start_date = as.Date(new_start_date)) %>%
#   filter(new_start_date > date_start) -> change

countries <- "ETHIOPIA"
birthyears <- as.character(c(2022:2026))


#### population by districts within 2022-2026
raw.data$dist.pop %>%
  rename(place.admin.0 = ctry, place.admin.1 = prov, place.admin.2 = dist) %>%
  mutate(year = as.character(year)) %>%
  mutate(place.admin.2 = case_when(
    place.admin.2 == "MAYO BELWA" ~ "MAYO-BELWA",
    place.admin.2 == "TAFAWA BALEWA" ~ "TAFAWA-BALEWA",
    place.admin.2 == "DANGE SHUNI" ~ "DANGE-SHUNI",
    place.admin.2 == "WAG HAMRA" ~ "WAGHIMERA",
    .default = place.admin.2
  )) %>%
  # fix inconsistency between sia and pop data
  select("place.admin.0", "place.admin.1", "place.admin.2", "year", "u5pop") %>%
  filter(place.admin.0 %in% countries, year %in% birthyears) %>%
  tidyr::complete(place.admin.2, year) %>%
  mutate(birth_pop = u5pop / 5) %>%
  select(place.admin.0, place.admin.1, place.admin.2, year, birth_pop) -> eth_dist_pop


###### shapefile in the same period #####

# use active.year.01 to align with year in dist_pop!
eth_shape <- global.dist %>%
  filter(ADM0_NAME == "ETHIOPIA", active.year.01 %in% birthyears) %>%
  select(ADM0_NAME, ADM1_NAME, ADM2_NAME, yr.st, yr.end, active.year.01, AREA_SQM) %>%
  rename(place.admin.0 = ADM0_NAME, place.admin.1 = ADM1_NAME, place.admin.2 = ADM2_NAME,
         year = active.year.01) %>%
  mutate(year = as.character(year))


eth_dist_pop %>%
  left_join(eth_shape, by = c("place.admin.0", "place.admin.1", "place.admin.2", "year")) %>%
  filter(!is.na(birth_pop)) -> eth_dist_pop_shape


eth_crs <- 32637

prep_districts <- function(gdf, year, province_col, district_col, crs = NULL) {

  if (is.null(crs)) {
    crs <- st_crs(gdf)   # just use whatever this dataset already has
  }

  gdf %>%
    st_transform(crs) %>%
    st_make_valid() %>%
    mutate(
      # district_uid = paste(year, province_col, district_col, sep = "_"))
      district_uid = paste(.data[[year]], .data[[province_col]], .data[[district_col]], sep = "-"))
    # select(district_uid, year, province = all_of(province_col), district = all_of(district_col), geometry)
}

build_crosswalk <- function(df1, df2) {

  inter <- st_intersection(
    df1 %>% select(id_y1 = district_uid, AREA_SQM, Shape),
    df2 %>% select(id_y2 = district_uid, AREA_SQM, Shape)
  )

  inter %>%
    mutate(overlap_area = as.numeric(st_area(Shape))) %>%
    st_drop_geometry() %>%
    filter(overlap_area > 0) %>%
    mutate(
      frac_of_y1 = overlap_area / AREA_SQM,
      frac_of_y2 = overlap_area / AREA_SQM
    )
}


# NOTE for issue: lazy substitute for the argument name?
eth_shape2 <- prep_districts(eth_shape, "yr.st",
                             "place.admin.1", "place.admin.2",
                             crs = eth_crs)

all_cw = data.frame()
startyears = unique(eth_shape2$yr.st)

for(i in 1:length(startyears)) {

  df1 <- eth_shape2 %>% filter(yr.st == startyears[i])
  df2 <- eth_shape2 %>% filter(yr.st == startyears[i+1])

  cw <- build_crosswalk(df1, df2)
  all_cw <- rbind(all_cw, cw)

}

all_cw %>%
  mutate(f_diff = frac_of_y2 - frac_of_y1) -> all_cw







t = st_area(eth_shape$Shape)

# prov_pop <- raw.data$prov.pop
#
# prov_pop %>%
#   rename(place.admin.0 = ctry, place.admin.1 = prov) %>%
#   mutate(year = as.character(year)) %>%
#   filter(year == "2026") %>%
#   select(place.admin.0, place.admin.1, u5pop, -year) -> prov_pop
