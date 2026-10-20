rm(list=ls())
library(dplyr)

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
eth <- global.dist %>%
  filter(ADM0_NAME == "ETHIOPIA", yr.st >=2018)

eth_2026 <- global.dist %>%
  filter(ADM0_NAME == "ETHIOPIA", yr.st == 2026) %>%
  select(ADM0_NAME, ADM1_NAME, ADM2_NAME, yr.st)

eth_2025 <- global.dist %>%
  filter(ADM0_NAME == "ETHIOPIA", yr.st == 2025) %>%
  select(ADM0_NAME, ADM1_NAME, ADM2_NAME, yr.st)





date_start <- as.Date("2021-11-01")
date_end <- as.Date("2026-11-01")

change <- read.csv("changes.csv")
change %>%
  mutate(new_start_date = as.Date(new_start_date)) %>%
  filter(new_start_date > date_start) -> change


countries <- "ETHIOPIA"
birthyears <- as.character(c(2022:2026))


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
  select(place.admin.0, place.admin.1, place.admin.2, year, birth_pop) -> dist_pop


eth_dist_pop <- dist_pop %>%
  filter(year == "2026", !is.na(birth_pop))


prov_pop <- raw.data$prov.pop

prov_pop %>%
  rename(place.admin.0 = ctry, place.admin.1 = prov) %>%
  mutate(year = as.character(year)) %>%
  filter(year == "2026") %>%
  select(place.admin.0, place.admin.1, u5pop, -year) -> prov_pop
