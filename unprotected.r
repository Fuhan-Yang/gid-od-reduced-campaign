rm(list = ls())
library(dplyr)
library(ggplot2)
library(openxlsx)
source("functions.R")

## Regions to analyze ##
### countries
countries <- c(
    "NIGERIA",
    # "CHAD",
    "ETHIOPIA"
    # "NIGER",
    # "CENTRAL AFRICAN REPUBLIC"
)


#### Import the data ###
raw.data <- sirfunctions::get_all_polio_data(
    size = "small", data_folder = "GID/PEB/SIR/Data",
    polis_folder = "GID/PEB/SIR/POLIS"
)
# save(raw.data, file = "raw.data.Rdata")

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

# load("raw.data.Rdata")
# load("global.dist.Rdata")

##### Config:  params #########

### population that have born during this period are eligible for protection
date_start <- as.Date("2021-11-01")
date_end <- as.Date("2026-11-01")

attack_rate <- 0.3963697
ci_r <- 1 / 2000

### coverage per SIA round
p_missing <- 0.3

### age brackets given period ###
cohort_birthdays <- seq(date_start, date_end, "1 year")
birthyears <- as.character(c(2022:2026)) # these are the years when the to-be-protect cohorts were born

eth_crs <- 32637

global.dist %>%
  filter(ADM0_NAME %in% countries, active.year.01 %in% birthyears) %>%
  rename(place.admin.0 = ADM0_NAME, place.admin.1 = ADM1_NAME, place.admin.2 = ADM2_NAME) %>%
  mutate(year = as.character(active.year.01)) %>%
  select(place.admin.0, place.admin.1, place.admin.2, year, AREA_SQM, GUID) -> shapefile



### One should start with population data, and force for each admin2, the age group has to be 0-4.
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
   #  tidyr::complete(place.admin.2, year) %>%
    mutate(birth_pop = u5pop / 5) %>%
    select(place.admin.0, place.admin.1, place.admin.2, year, birth_pop) -> dist_pop


## interpolate the population using the map in 2026 ##
interpolated_dist_pop <- geo_data_remap(data_to_interpolate = dist_pop,
                                        shapefile = shapefile,
                                        target_year = "2026",
                                        data_col_name = "birth_pop", crs = eth_crs,
                                        geo_id = "GUID", admin_level = "2", na.action = "NA is 0")



# eth_2022 <- dist_pop %>%
#   left_join(shapefile, by = c("place.admin.0", "place.admin.1", "place.admin.2", "year")) %>%
#   filter(year == "2022")
#
# eth_2022_inter <- interpolated_dist_pop %>%
#   filter(year == "2022")
#
# eth_2022 %>%
#   sf::st_as_sf() %>%
#   ggplot() +
#   geom_sf(aes(fill = birth_pop)) +
#   scale_fill_viridis_c(option = "H") +
#   theme_bw()
#
# eth_2022_inter %>%
#   sf::st_as_sf() %>%
#   ggplot() +
#   geom_sf(aes(fill = birth_pop)) +
#   scale_fill_viridis_c(option = "H") +
#   theme_bw()


interpolated_dist_pop %>%
  bind_rows(dist_pop %>% filter(year == "2026")) %>%
  st_drop_geometry() %>%
  select(place.admin.0, place.admin.1, place.admin.2, year, birth_pop)-> new_dist_pop


# dist_pop %>%
#   group_by(place.admin.2) %>%
#   summarise(n_unique = n_distinct(year)) %>%
#   distinct(n_unique)

prov_pop <- raw.data$prov.pop

prov_pop %>%
    rename(place.admin.0 = ctry, place.admin.1 = prov) %>%
    mutate(year = as.character(year)) %>%
    filter(year == "2026") %>%
    select(place.admin.0, place.admin.1, u5pop, -year) -> prov_pop # use population data in 2026 to calculate costs for strategies

##### SIA data from POLIS ###
raw_sia <- raw.data$sia

raw_sia %>%
  mutate(activity.start.date = as.Date(activity.start.date)) %>%
  filter(
    place.admin.0 %in% countries,
    activity.start.date >= date_start & activity.start.date < date_end,
    age.group == "0 to 5 years",
    vaccine.type == "nOPV2", # This is a place holder for different vaccine type
    status == "Done"
  ) %>%
  mutate(
    year = datebreak_to_year_factor(activity.start.date, cohort_birthdays)
  ) %>%
  mutate(place.admin.2 = case_when(place.admin.2 == "WAG HAMRA" ~ "WAGHIMERA", # keep the name consistent between scope, sia, and population data
                                   .default = place.admin.2)) %>%
  select(
    sia.code, place.admin.0, activity.start.date, place.admin.1, place.admin.2, year
  ) %>%
  distinct() %>%
  group_by(place.admin.2, year) %>%
  reframe(
    place.admin.0 = place.admin.0[1],
    place.admin.1 = place.admin.1[1],
    n_sia_per_year = n_distinct(sia.code),
  ) -> sia_count


## remap the district-specific sia count to 2026 map #

interpolated_sia_count <- geo_data_remap(data_to_interpolate = sia_count,
                                shapefile = shapefile,
                                target_year = "2026",
                                data_col_name = "n_sia_per_year", crs = eth_crs,
                                geo_id = "GUID", admin_level = "2", na.action = "NA is 0")

interpolated_sia_count %>%
  bind_rows(sia_count %>% filter(year == "2026")) %>%
  st_drop_geometry() %>%
  select(place.admin.0, place.admin.1, place.admin.2, year, n_sia_per_year)-> new_sia_count


## There are some regions existing in sia data but not in the shapefile nor population data ##
# sia_count %>% filter(year == "2026") %>%
#   anti_join(shapefile %>% filter(year == "2026"), by = "place.admin.2")


# sia_count %>%
#   left_join(shapefile, by = c("place.admin.0", "place.admin.1", "place.admin.2", "year")) %>%
#   sf::st_as_sf() %>%
#   ggplot() +
#   geom_sf(aes(fill = n_sia_per_year)) +
#   scale_fill_viridis_c(option = "H") +
#   facet_wrap(year~., nrow = 2)
#
#
# new_sia_count %>%
#   ggplot() +
#   geom_sf(aes(fill = n_sia_per_year)) +
#   scale_fill_viridis_c(option = "H") +
#   facet_wrap(year~., nrow = 2)



###### scope data from strategies ###
scope <- read.csv("data/scope.csv")

## the admin 1 that are 0 in both strategies will be removed from the analysis ##
# admin1_no_data <- scope %>%
#   filter(is.na(target_population) & is.na(reduced_population))

scope %>%
  # anti_join(admin1_no_data) %>%
  mutate(
    place.admin.1 = toupper(state),
    ideal_population = if_else(is.na(target_population), 0, target_population), # for blank in target population, interpret as 0
    reduced_population = if_else(is.na(reduced_population), 0, reduced_population)
  ) %>%
  rename(place.admin.0 = country) %>%
  select(place.admin.0, place.admin.1, ideal_population, reduced_population) -> scope


#### Join SIA data with dist pop data to calculate unprotected before strategies ###
new_dist_pop %>%
  left_join(new_sia_count, by = c("place.admin.0", "place.admin.1", "place.admin.2", "year")) %>%
# there are NA after joining, carefully impute them #
  mutate(n_sia_per_year = if_else(is.na(n_sia_per_year), 0, n_sia_per_year),
         age_at_strategies = 2026 - as.numeric(as.character(year))) %>% # if NA here, meaning no SIA in this year
  arrange(place.admin.0, place.admin.1, place.admin.2, age_at_strategies) %>% # redo calculation
  group_by(place.admin.0, place.admin.1, place.admin.2) %>%
  mutate(n_sia_experienced = cumsum(n_sia_per_year)) -> sia_pop


#### calculate the unprotected children number before strategies ####
sia_pop %>%
  mutate(n_sia_experienced = round(n_sia_experienced)) %>%
  mutate(n_0_dose_pre = birth_pop * get_p_unprotected(0, n_sia_experienced, p_missing = p_missing),
         n_1_dose_pre = birth_pop * get_p_unprotected(1, n_sia_experienced, p_missing = p_missing),
         n_2_dose_pre = birth_pop * get_p_unprotected(2, n_sia_experienced, p_missing = p_missing)) %>%
  mutate(unprotected_pre = n_0_dose_pre + n_1_dose_pre + n_2_dose_pre) %>%
  group_by(place.admin.0, place.admin.1) %>%
  mutate(all_unprotected_prov_pre = sum(unprotected_pre),
         all_0_dose_prov_pre = sum(n_0_dose_pre),
         all_other_dose_prov_pre = sum(n_1_dose_pre) + sum(n_2_dose_pre)) -> sia_pop_unpro


### check: NA in pop
sia_pop_unpro %>%
  filter(is.na(birth_pop)) -> t

t

#### Join with scope data to calculate unprotected after strategies ####
sia_pop_unpro %>%
  right_join(scope, by = c("place.admin.0", "place.admin.1")) %>%
  left_join(prov_pop, by = c("place.admin.0", "place.admin.1")) -> sia_pop_scope_prov

#### calculate how many remained unprotected after ideal and reduced scope ####
# remained = all_unprotected - (1-p_missing)^2 * ideal(reduced) * ideal(reduced)/u5_pop
sia_pop_scope_prov %>%
  mutate(unprotected_ideal_post = if_else(all_unprotected_prov_pre - calculate_protected(p_missing, ideal_population, u5pop) < 0, 0,
                                         all_unprotected_prov_pre - calculate_protected(p_missing, ideal_population, u5pop)),
         unprotected_reduced_post = if_else(all_unprotected_prov_pre - calculate_protected(p_missing, reduced_population, u5pop) < 0, 0,
                                            all_unprotected_prov_pre - calculate_protected(p_missing, reduced_population, u5pop))) %>%
  mutate(partially_protected_ideal_post = case_when(unprotected_ideal_post > 0 ~ calculate_protected(p_missing, ideal_population, u5pop) * all_0_dose_prov_pre/(all_0_dose_prov_pre + all_other_dose_prov_pre),
                                                    .default = all_0_dose_prov_pre),
         partially_protected_reduced_post = case_when(unprotected_reduced_post > 0 ~ calculate_protected(p_missing, reduced_population, u5pop) * all_0_dose_prov_pre/(all_0_dose_prov_pre + all_other_dose_prov_pre),
                                                      .default = all_0_dose_prov_pre),
         fully_protected_ideal_post = case_when(unprotected_ideal_post > 0 ~ calculate_protected(p_missing, ideal_population, u5pop) * all_other_dose_prov_pre/(all_0_dose_prov_pre + all_other_dose_prov_pre),
                                               .default = all_other_dose_prov_pre),
         fully_protected_reduced_post = case_when(unprotected_reduced_post > 0 ~ calculate_protected(p_missing, reduced_population, u5pop) * all_other_dose_prov_pre/(all_0_dose_prov_pre + all_other_dose_prov_pre),
                                                  .default = all_other_dose_prov_pre)) %>%
  mutate(case_ideal_post = s_to_c(unprotected_ideal_post, attack_rate, ci_r),
         case_reduced_post = s_to_c(unprotected_reduced_post, attack_rate, ci_r),
         case_partially_protected_ideal_post = s_to_c(partially_protected_ideal_post, attack_rate,ci_r),
         case_partially_protected_reduced_post = s_to_c(partially_protected_reduced_post, attack_rate, ci_r)) %>%
  mutate(case_diff = case_reduced_post - case_ideal_post,
         case_diff_partially_protected = case_partially_protected_reduced_post - case_partially_protected_ideal_post) -> sia_pop_scope_prov_post_full

write.xlsx(sia_pop_scope_prov_post_full,file = "strategies_full.xlsx", rowNames = F)


sia_pop_scope_prov_post_full %>%
  distinct(place.admin.1,.keep_all = T) %>%
  select(place.admin.0, place.admin.1, all_unprotected_prov_pre, all_0_dose_prov_pre,
         all_other_dose_prov_pre, ideal_population, reduced_population, unprotected_ideal_post,
         unprotected_reduced_post, partially_protected_ideal_post, partially_protected_reduced_post,
         fully_protected_ideal_post, fully_protected_reduced_post,
         case_ideal_post, case_reduced_post,
         case_partially_protected_ideal_post, case_partially_protected_reduced_post,
         case_diff, case_diff_partially_protected, u5pop) -> sia_pop_scope_prov_post

write.xlsx(sia_pop_scope_prov_post, file = "strategies.xlsx", rowNames = F)

sia_pop_scope_prov_post %>%
  filter(u5pop < ideal_population | u5pop < reduced_population) %>%
  select(place.admin.0, place.admin.1, ideal_population, reduced_population, u5pop)%>%
  mutate(year = 2026) -> underestimated_prov_pop

write.csv(underestimated_prov_pop, file = "underestimated_province_pop.csv", row.names = F)


### If to estimate how many are partially protected vs how many are fully protected:




### fix sia_scope_pop: the NA is the rows that don't have data in sia, meaning no sia at all ###

### verify ideal and reduced pop are the same across admin.1: should return None ###
# sia_scope_pop %>%
# filter(!is.na(ideal_population)) %>%
# group_by(place.admin.1) %>%
# # summarise(n_unique = n_distinct(ideal_population)) %>%
# filter(n_unique > 1)

##### Visualization
sia_pop_scope_prov_post %>%
  rename(`0 dose received before strategies` = "all_0_dose_prov_pre",
         `1 or 2 doses received before strategies` = "all_other_dose_prov_pre") %>%
  tidyr::pivot_longer(cols = c("0 dose received before strategies",
                               "1 or 2 doses received before strategies"),
                      names_to = "Unprotected before strategies", values_to = "number") -> t


sia_pop_scope_prov_post %>%
    rename(`Unprotected Before Strategies` = "all_unprotected_prov_pre",
           `Ideal Scope` = "ideal_population",
           `Reduced Scope` = "reduced_population") %>%
    tidyr::pivot_longer((cols <- c("Unprotected Before Strategies","Ideal Scope", "Reduced Scope")),
                        names_to = "Strategies", values_to = "number") %>%
    ggplot() +
    geom_col(aes(x = place.admin.1, y = number, fill = Strategies), position = "dodge") +
    facet_wrap(place.admin.0 ~ ., nrow = 2, scales = "free") +
    xlab("State") +
    ylab("Number of Population") +
    labs(caption = "CDC Internal Analysis | Pre-decisional Draft") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")
ggsave("Histogram of strategies.png", width = 10, height = 8)


sia_pop_scope_prov_post %>%
  rename(
         `Ideal Scope` = "fully_protected_ideal_post",
         `Reduced Scope` = "fully_protected_reduced_post") %>%
  tidyr::pivot_longer((cols <- c("Ideal Scope", "Reduced Scope")),
                      names_to = "Strategies", values_to = "number") %>%
  ggplot() +
  geom_col(aes(x = place.admin.1, y = number, fill = Strategies), position = "dodge") +
  facet_wrap(place.admin.0 ~ ., nrow = 2, scales = "free") +
  xlab("State") +
  ylab("Number of population that are fully protected after strategies") +
  labs(caption = "CDC Internal Analysis | Pre-decisional Draft") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("Histogram of fully_protected.png", width = 10, height = 8)


sia_pop_scope_prov_post %>%
  rename(
    `Ideal Scope` = "partially_protected_ideal_post",
    `Reduced Scope` = "partially_protected_reduced_post") %>%
  tidyr::pivot_longer((cols <- c("Ideal Scope", "Reduced Scope")),
                      names_to = "Strategies", values_to = "number") %>%
  ggplot() +
  geom_col(aes(x = place.admin.1, y = number, fill = Strategies), position = "dodge") +
  facet_wrap(place.admin.0 ~ ., nrow = 2, scales = "free") +
  xlab("State") +
  ylab("Number of population that are partially protected after strategies") +
  labs(caption = "CDC Internal Analysis | Pre-decisional Draft") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("Histogram of partially_protected.png", width = 10, height = 8)


sia_pop_scope_prov_post %>%
  rename(
    `Ideal Scope` = "case_ideal_post",
    `Reduced Scope` = "case_reduced_post") %>%
  tidyr::pivot_longer((cols <- c("Ideal Scope", "Reduced Scope")),
                      names_to = "Strategies", values_to = "number") %>%
  ggplot() +
  geom_col(aes(x = place.admin.1, y = number, fill = Strategies), position = "dodge") +
  facet_wrap(place.admin.0 ~ ., nrow = 2, scales = "free") +
  xlab("State") +
  ylab("Number of AFP cases that could have after strategies") +
  labs(caption = "CDC Internal Analysis | Pre-decisional Draft") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("Histogram of case.png", width = 10, height = 8)


sia_pop_scope_prov_post %>%
  rename(
    `Ideal Scope` = "case_partially_protected_ideal_post",
    `Reduced Scope` = "case_partially_protected_reduced_post") %>%
  tidyr::pivot_longer((cols <- c("Ideal Scope", "Reduced Scope")),
                      names_to = "Strategies", values_to = "number") %>%
  ggplot() +
  geom_col(aes(x = place.admin.1, y = number, fill = Strategies), position = "dodge") +
  facet_wrap(place.admin.0 ~ ., nrow = 2, scales = "free") +
  xlab("State") +
  ylab("Number of AFP cases that could have from the partially-protected children") +
  labs(caption = "CDC Internal Analysis | Pre-decisional Draft") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("Histogram of partially-protected case.png", width = 10, height = 8)

