library(dplyr)
library(ggplot2)
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

### params ###

### population that have born during this period are eligible for protection
date_start <- as.Date("2021-11-01")
date_end <- as.Date("2026-11-01")

attack_rate <- 0.3963697
ci_r <- 1 / 2000

### coverage per SIA round
p_missing <- 0.3

### age brackets given period ###
cohort_birthdays <- seq(date_start, date_end, "1 year")


### functions ###
# get the probability of eligible for protection: the one that has been exposed to n SIA with n_sia_cumu have been given since born
get_p_unprotected <- function(n, n_sia_cumu, p_missing) {
    dbinom(n, n_sia_cumu, (1 - p_missing))
}

# convert birthdays to year-factor
# split the sia date into intervals with datebreak
datebreak_to_year_factor <- function(date, datebreak) {
    factor(findInterval(date, datebreak),
        levels = c(1:(length(datebreak) - 1)),
        labels = format(datebreak[-1], "%Y")
    )
}

birthyears <- as.character(c(2022:2026)) # this are the years when the to-be-protect cohorts were born

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
    tidyr::complete(place.admin.2, year) -> dist_pop

# dist_pop %>%
# group_by(place.admin.2) %>%
# summarise(n_unique = n_distinct(year)) %>%
# distinct(n_unique)

prov_pop <- raw.data$prov.pop

prov_pop %>%
    rename(place.admin.0 = ctry, place.admin.1 = prov) %>%
    mutate(year = as.character(year)) %>%
    filter(year == "2026") %>%
    select(place.admin.0, place.admin.1, u5pop, -year) -> prov_pop # use population data in 2026 to calculate costs for strategies


### the admin 2 that miss population ###
dist_pop %>%
    filter(
        place.admin.0 %in% sia_scope$place.admin.0,
        place.admin.1 %in% sia_scope$place.admin.1,
        place.admin.2 %in% sia_scope$place.admin.2,
        year %in% sia_scope$year, # NOTE: minor bug here, the year in sia_scope is the interval of birthdays, not real year from dist_pop
    ) %>%
    left_join(sia_scope, by = c("place.admin.0", "place.admin.1", "place.admin.2", "year")) %>%
    select(-c(
        "target_bOPV", "target_fIPV", "reduced_nOPV2", "target_nOPV2",
        "reduced_bOPV", "reduced_fIPV"
    )) %>%
    # mutate(u5pop = case_when(place.admin.1 == "ADDIS ABABA" ~ ideal_population, .default = u5pop)) %>% # the pop data of this province is missing, substitute with target population data (equals to reduced_population)
    mutate(birth_pop = u5pop / 5) -> sia_scope_pop

## vaccine data: SIA
raw_sia <- raw.data$sia


raw_sia %>%
    filter(
        place.admin.0 %in% countries,
        activity.start.date >= date_start & activity.start.date < date_end,
        age.group == "0 to 5 years",
        vaccine.type == "nOPV2", # This is a place holder for different vaccine type
        status == "Done"
    ) %>%
    mutate(
        activity.start.date = as.Date(activity.start.date),
        year = factor(findInterval(activity.start.date, cohort_birthdays),
            levels = c(1:(length(cohort_birthdays) - 1)),
            labels = format(cohort_birthdays[-1], "%Y")
        )
    ) %>%
    select(
        sia.code, place.admin.0, activity.start.date, place.admin.1, place.admin.2, year
    ) %>%
    distinct() %>%
    group_by(place.admin.2, year) %>%
    reframe(
        place.admin.0 = place.admin.0[1],
        place.admin.1 = place.admin.1[1],
        n_sia_year = n_distinct(sia.code),
    ) %>%
    arrange(place.admin.0, place.admin.1, place.admin.2, year) %>%
    group_by(place.admin.0, place.admin.1, place.admin.2) %>%
    mutate(
        n_sia = cumsum(n_sia_year),
        age_at_strategies = 2026 - as.numeric(as.character(year))
    ) -> sia_count # n_sia is the total rounds of SIA done in this admin2 since date_start


### population data in scope
scope <- read.csv("data/scope.csv")
# for blank in target population, interpret as 0

## the admin 1 that are 0 in both strategies will be removed from the analysis ##
admin1_no_data <- scope %>%
    filter(is.na(target_population) & is.na(reduced_population))

scope %>%
    anti_join(admin1_no_data) %>%
    mutate(
        place.admin.1 = toupper(state),
        ideal_population = if_else(is.na(target_population), 0, target_population),
        reduced_population = if_else(is.na(reduced_population), 0, reduced_population)
    ) %>%
    rename(place.admin.0 = country) %>%
    select(-state, -target_population) %>%
    left_join(sia_count, by = c("place.admin.0", "place.admin.1")) %>%
    mutate(place.admin.2 = case_when(place.admin.2 == "WAG HAMRA" ~ "WAGHIMERA", # keep the name consistent between scope, sia, and population data
        .default = place.admin.2
    )) -> sia_scope


### combine with population data
dist_pop <- raw.data$dist.pop

dist_pop %>%
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
    select("place.admin.0", "place.admin.1", "place.admin.2", "year", "u5pop") -> dist_pop

prov_pop <- raw.data$prov.pop

prov_pop %>%
    rename(place.admin.0 = ctry, place.admin.1 = prov) %>%
    mutate(year = as.character(year)) %>%
    filter(year == "2026") %>%
    select(place.admin.0, place.admin.1, u5pop, -year) -> prov_pop # use population data in 2026 to calculate costs for strategies

### the admin 2 that miss population ###
dist_pop %>%
    filter(
        place.admin.0 %in% sia_scope$place.admin.0,
        place.admin.1 %in% sia_scope$place.admin.1,
        place.admin.2 %in% sia_scope$place.admin.2,
        year %in% sia_scope$year, # NOTE: minor bug here, the year in sia_scope is the interval of birthdays, not real year from dist_pop
    ) %>%
    left_join(sia_scope, by = c("place.admin.0", "place.admin.1", "place.admin.2", "year")) %>%
    select(-c(
        "target_bOPV", "target_fIPV", "reduced_nOPV2", "target_nOPV2",
        "reduced_bOPV", "reduced_fIPV"
    )) %>%
    # mutate(u5pop = case_when(place.admin.1 == "ADDIS ABABA" ~ ideal_population, .default = u5pop)) %>% # the pop data of this province is missing, substitute with target population data (equals to reduced_population)
    mutate(birth_pop = u5pop / 5) -> sia_scope_pop # assume pop per age-year is the same under


### fix sia_scope_pop: the NA is the rows that don't have data in sia, meaning no sia at all ###

### verify ideal and reduced pop are the same across admin.1: should return None ###
# sia_scope_pop %>%
# filter(!is.na(ideal_population)) %>%
# group_by(place.admin.1) %>%
# # summarise(n_unique = n_distinct(ideal_population)) %>%
# filter(n_unique > 1)


sia_scope_pop %>%
    select(-ideal_population, -reduced_population) %>%
    left_join(sia_scope_pop %>%
        filter(!is.na(ideal_population), !is.na(reduced_population)) %>% # replace NA to be admin1-specific ideal and reduced pop
        distinct(place.admin.1, ideal_population, reduced_population), by = "place.admin.1") %>%
    mutate(n_sia_year = if_else(is.na(n_sia_year), 0, n_sia_year)) %>% # impute NA with 0
    arrange(place.admin.0, place.admin.1, place.admin.2, desc(year)) %>%
    group_by(place.admin.0, place.admin.1, place.admin.2) %>%
    mutate(
        n_sia = cumsum(n_sia_year),
        age_at_strategies = 2026 - as.numeric(as.character(year))
    ) %>%
    rename(birthyear = year) -> sia_scope_pop # NOTE: year is ambiguous


### get unprotected children ####
sia_scope_pop %>%
    mutate(
        p_missing = p_missing,
    ) %>%
    mutate(
        n_dose_0_pre = birth_pop * get_p_unprotected(n = 0, n_sia_cumu = n_sia, p_missing = p_missing),
        n_dose_1_pre = birth_pop * get_p_unprotected(n = 1, n_sia_cumu = n_sia, p_missing = p_missing),
        n_dose_2_pre = birth_pop * get_p_unprotected(n = 2, n_sia_cumu = n_sia, p_missing = p_missing),
    ) %>%
    group_by(place.admin.1) %>% # aggregate unprotected across age groups and admin 2
    mutate(
        all_dose_0_pre = sum(n_dose_0_pre, na.rm = TRUE),
        all_dose_1_pre = sum(n_dose_1_pre, na.rm = TRUE),
        all_dose_2_pre = sum(n_dose_2_pre, na.rm = TRUE)
    ) %>%
    mutate(all_unprotected_pre = all_dose_0_pre + all_dose_1_pre + all_dose_2_pre) %>%
    select(-u5pop) %>%
    ungroup() %>%
    distinct() -> scope_unprotected

scope_unprotected %>%
    left_join(prov_pop %>% rename(u5pop_prov = u5pop), by = c("place.admin.0", "place.admin.1")) -> scope_unprotected_prov_pop

# NOTE: population is smaller than target population in some admin1 regions!
print(scope_unprotected_prov_pop %>%
    filter(ideal_population > u5pop_prov) %>%
    distinct(place.admin.0, place.admin.1), n = Inf, width = Inf)

bad_pop_prov_names <- scope_unprotected_prov_pop %>%
    filter(ideal_population > u5pop_prov) %>%
    distinct(place.admin.0, place.admin.1)

# Replace the population with the ideal_population for these admin1 regions
scope_unprotected_prov_pop %>%
    mutate(u5pop_prov = case_when(place.admin.1 %in% bad_pop_prov_names$place.admin.1 ~ ideal_population, .default = u5pop_prov)) %>%
    mutate(
        all_unprotected_post_ideal = all_unprotected_pre - (1 - p_missing)^2 * ideal_population * ideal_population / u5pop_prov,
        all_unprotected_post_reduced = all_unprotected_pre - (1 - p_missing)^2 * reduced_population * reduced_population / u5pop_prov,
    ) %>%
    mutate(
        all_unprotected_post_ideal = if_else(all_unprotected_post_ideal < 0, 0, all_unprotected_post_ideal),
        all_unprotected_post_reduced = if_else(all_unprotected_post_reduced < 0, 0, all_unprotected_post_reduced)
    ) %>%
    mutate(
        case_final_ideal = all_unprotected_post_ideal * attack_rate * ci_r,
        case_final_reduced = all_unprotected_post_reduced * attack_rate * ci_r
    ) -> scope_final_case

scope_final_case %>%
    select(
        place.admin.0, place.admin.1, place.admin.2, n_sia, age_at_strategies, ideal_population, reduced_population, birth_pop, n_dose_0_pre, n_dose_1_pre, n_dose_2_pre,
        all_dose_0_pre, all_dose_1_pre, all_dose_2_pre, all_unprotected_pre, all_unprotected_post_ideal, all_unprotected_post_reduced,
        case_final_ideal, case_final_reduced
    ) -> final_unprotected

write.csv(final_unprotected, file = "unprotected.csv", row.names = FALSE)


##### Visualization
scope_final_case %>%
    rename(`Ideal Scope` = "case_final_ideal", `Reduced Scope` = "case_final_reduced") %>%
    tidyr::pivot_longer((cols <- c("Ideal Scope", "Reduced Scope")), names_to = "Scenario", values_to = "case") %>%
    ggplot() +
    geom_col(aes(x = place.admin.1, y = case, fill = Scenario), position = "dodge") +
    facet_wrap(place.admin.0 ~ ., nrow = 2, scales = "free_y") +
    xlab("Number of AFP Cases that would happened") +
    ylab("Province") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")
ggsave("Histogram of cases.png", width = 10, height = 8)


final_unprotected %>%
    ggplot() +
    geom_col(aes(x = place.admin.1, y = case_diff_partially)) +
    facet_wrap(place.admin.0 ~ ., nrow = 2, scales = "free_y") +
    xlab("Difference between the number of AFP cases that would happen for partially-protected children") +
    ylab("Province") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")
ggsave("Histogram of case_diff.png", width = 10, height = 8)


scope_final_case %>%
    filter(place.admin.1 == "ADDIS ABABA") -> t

# check if p_unprotected is unique for each year, age_group, place.admin.2

# ### map: need to use district map as covered area are all covered. So province map lost granularity.
# library(sf)
# library(rnaturalearth)

# province_map <- ne_states(
#     country = c("Nigeria"),
#     returnclass = "sf"
# )

# province_map %>%
#     rename(place.admin.1 = name, place.admin.0 = admin) %>%
#     mutate(place.admin.1 = toupper(place.admin.1), place.admin.0 = toupper(place.admin.0)) %>%
#     select(place.admin.0, place.admin.1, geometry) %>%
#     left_join(sia_scope_pop_unprotected) -> case_map

# # Not obvious
# case_map %>%
#     tidyr::pivot_longer(
#         cols = c("final_case_ideal", "final_case_reduced"),
#         names_transform = list(name = ~ recode(.x,
#             "final_case_ideal" = "AFP infections under ideal scope",
#             "final_case_reduced" = "AFP infections under reduced scope"
#         ))
#     ) %>%
#     mutate(value = cut(value, breaks = 10)) %>%
#     ggplot() +
#     geom_sf(aes(fill = value), color = "grey10", linewidth = 0.15) +
#     scale_fill_viridis_d(option = "A", na.value = "white") +
#     facet_wrap(. ~ name, nrow = 2) +
#     theme_minimal() +
#     theme(
#         strip.text = element_text(size = 13),
#         # legend.position = "none",
#         legend.title = element_text(size = 13),
#     )
# ggsave("Target vs Reduced_d.png", width = 10, height = 5)


# ## Plot difference
# case_map %>%
#     mutate(infec_diff = cut(infec_diff, breaks = 10)) %>%
#     ggplot() +
#     geom_sf(aes(fill = infec_diff), color = "grey10", linewidth = 0.15) +
#     scale_fill_viridis_d(option = "A", na.value = "white") +
#     theme_minimal() +
#     theme(
#         strip.text = element_text(size = 13),
#         # legend.position = "none",
#         legend.title = element_text(size = 13)
#     )
# ggsave("Case Difference_d.png", width = 10, height = 3)


# put a baseline: pop <= 3 rounds
# pop have 0, 1, 2, doses exactly before strategies as all separate columns
# final unprotected for 0 doses and >=1 doses after strategies for A and B.
# then calculate the difference of final unprotected for 0 between two strategies.


# u5 pop at beginning,

## 2 rounds
###
