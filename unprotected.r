# rm(list = ls())
library(dplyr)
library(ggplot2)
## Regions to analyze ##
### countries
countries <- c(
    "NIGERIA", "CHAD",
    "DEMOCRATIC REPUBLIC OF THE CONGO", "ETHIOPIA",
    "NIGER", "CENTRAL AFRICAN REPUBLIC"
)


#### Import the data ###
raw.data <- sirfunctions::get_all_polio_data(
    size = "small", data_folder = "GID/PEB/SIR/Data",
    polis_folder = "GID/PEB/SIR/POLIS"
)

### params ###

### period of SIAs
date_start <- as.Date("2021-11-01")
date_end <- as.Date("2026-11-01")

attack_rate <- 0.5415745
ci_r <- 1 / 2000

p_missing <- 0.3

### functions ###
# get the probability of eligible for protection: the one that has been exposed to 1 SIA since born
# should equal to dbinom(1, n_sia, (1-p_missing))
get_p_unprotected <- function(n_sia, p_missing) {
    n_sia * (1 - p_missing) * p_missing^(n_sia - 1)
}
# get the probability of eligible for protection: the one that has been exposed to 1 SIA since born


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
        year = format(activity.start.date, "%Y")
    ) %>%
    select(
        sia.code, place.admin.0, place.admin.1, place.admin.2, year,
    ) %>%
    distinct() %>%
    group_by(place.admin.2, year) %>%
    reframe(
        place.admin.0 = place.admin.0[1],
        place.admin.1 = place.admin.1[1],
        n_sia_year = n_distinct(sia.code)
    ) %>%
    arrange(place.admin.0, place.admin.1, place.admin.2, desc(year)) %>%
    group_by(place.admin.0, place.admin.1, place.admin.2) %>%
    mutate(n_sia = cumsum(n_sia_year)) -> sia_count # n_sia is the total rounds of SIA done in this admin2 since date_start


### population data in scope
scope <- read.csv("data/scope.csv")
# for blank in target population, interpret as 0

scope %>%
    mutate(
        place.admin.1 = toupper(state),
        target_population = if_else(is.na(target_population), 0, target_population),
        reduced_population = if_else(is.na(reduced_population), 0, reduced_population)
    ) %>%
    rename(place.admin.0 = country) %>%
    select(-state) %>%
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
    select(place.admin.0, place.admin.1, u5pop, -year) -> prov_pop

### the admin 2 that miss population ###
sia_scope %>%
    left_join(dist_pop,
        by = c("place.admin.0", "place.admin.1", "place.admin.2", "year")
    ) %>%
    select(-c(
        "target_bOPV", "target_fIPV", "reduced_nOPV2", "target_nOPV2",
        "reduced_bOPV", "reduced_fIPV"
    )) %>%
    mutate(u5pop = case_when(place.admin.1 == "ADDIS ABABA" ~ target_population, .default = u5pop)) %>% # the pop data of this province is missing, substitute with target population data (equals to reduced_population)
    mutate(age_in_26 = 2026 - as.numeric(year), pop_per_age_year_dist = u5pop / 5) -> sia_scope_pop


### get unprotected children ####
sia_scope_pop %>%
    mutate(
        p_missing = p_missing,
    ) %>%
    mutate(n_unprotected = pop_per_age_year_dist * get_p_unprotected(n_sia = n_sia, p_missing = p_missing)) %>%
    group_by(place.admin.1) %>% # aggregate unprotected across age groups and admin 2
    mutate(all_unprotected = sum(n_unprotected, na.rm = TRUE)) %>%
    select(-year, -u5pop) %>%
    ungroup() %>%
    distinct() -> scope_unprotected

scope_unprotected %>%
    left_join(prov_pop %>% rename(u5pop_prov = u5pop), by = c("place.admin.0", "place.admin.1")) %>%
    mutate(
        final_unprotected_targeted = all_unprotected - (1 - p_missing) * target_population * target_population / u5pop_prov,
        final_unprotected_reduced = all_unprotected - (1 - p_missing) * reduced_population * reduced_population / u5pop_prov,
    ) %>%
    mutate(
        final_case_targeted = if_else(final_unprotected_targeted < 0, 0, final_unprotected_targeted * attack_rate * ci_r),
        final_case_reduced = if_else(final_unprotected_reduced < 0, 0, final_unprotected_reduced * attack_rate * ci_r)
    ) -> sia_scope_pop_unprotected

write.csv(sia_scope_pop_unprotected, file = "unprotected.csv")

# NOTE: population is larger than target population in some provinces!
print(sia_scope_pop_unprotected %>%
    filter(target_population > u5pop_prov) %>%
    distinct(place.admin.0, place.admin.1), n = Inf)


##### Visualization
sia_scope_pop_unprotected %>%
    rename(`Ideal Scope` = "final_case_targeted", `Reduced Scope` = "final_case_reduced") %>%
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

# check if p_unprotected is unique for each year, age_group, place.admin.2

### map: need to use district map as covered area are all covered. So province map lost granularity.
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
#         cols = c("final_case_targeted", "final_case_reduced"),
#         names_transform = list(name = ~ recode(.x,
#             "final_case_targeted" = "AFP infections under ideal scope",
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
