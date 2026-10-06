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
p_cover <- 0.7
p_random_missing <- 0.3


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
        immunized.population = if_else(is.na(immunized.population), targeted.population, immunized.population),
        month = format(activity.start.date, "%Y-%m"),
        year = format(activity.start.date, "%Y")
    ) %>%
    select(
        sia.code, place.admin.0, place.admin.1, place.admin.2, year,
        immunized.population, targeted.population
    ) %>%
    distinct() -> sel_sia

sel_sia %>%
    group_by(place.admin.2, year) %>%
    reframe(
        place.admin.0 = place.admin.0[1],
        place.admin.1 = place.admin.1[1],
        n_sia = n_distinct(sia.code)
    ) %>%
    filter(place.admin.0 == "NIGERIA") -> sia_count


sia_count %>%
    filter(year == "2021") -> sia_2021

sia_count %>%
    filter(place.admin.2 == "ABI") -> sia_abi

View(sia_2021)
hist(sia_count$n_sia)
range(sia_count$n_sia)

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
    select(-state) -> scope

## combine with sia data
scope %>%
    left_join(sel_sia, by = c("place.admin.0", "place.admin.1")) %>%
    mutate(
        immunized.population = if_else(is.na(immunized.population), 0, immunized.population)
    ) -> sia_scope

### population data
dist_pop <- raw.data$dist.pop

dist_pop %>%
    group_by(dist, year) %>%
    reframe(
        ctry, prov, dist,
        u5pop = sum(u5pop, na.rm = T)
    ) %>%
    filter(ctry == "NIGERIA") -> pop_view

sia_scope %>%
    left_join(
        dist_pop %>% rename(place.admin.0 = ctry, place.admin.1 = prov, place.admin.2 = dist) %>%
            mutate(year = as.character(year)) %>%
            select("place.admin.0", "place.admin.1", "place.admin.2", "year", "u5pop"),
        by = c("place.admin.0", "place.admin.1", "place.admin.2", "year")
    ) %>%
    select(-c(
        "target_bOPV", "target_fIPV", "reduced_nOPV2", "target_nOPV2",
        "reduced_bOPV", "reduced_fIPV"
    )) %>%
    mutate(age1 = u5pop / 5, age2 = u5pop / 5, age3 = u5pop / 5, age4 = u5pop / 5, age5 = u5pop / 5) %>%
    tidyr::pivot_longer(
        cols = c("age1", "age2", "age3", "age4", "age5"),
        names_prefix = "age",
        names_to = "age_group",
        values_to = "pop"
    ) -> sia_scope_pop


### get unprotected children ####
sia_scope_pop %>%
    mutate(
        p_missing = p_missing, p_cover = p_cover, p_random_missing = p_random_missing,
        age_group = as.numeric(age_group)
    ) %>%
    mutate(n_unprotected = pop * p_missing + pop * p_cover * p_random_missing^age_group) %>% # assming p_missing is systematic: if it is missed once, it will alwaays be missed.
    mutate(p_unprotected = n_unprotected / pop) %>%
    filter(year == "2026") %>%
    group_by(place.admin.1) %>%
    mutate(all_unprotected = sum(n_unprotected, na.rm = TRUE)) %>%
    select(place.admin.0, place.admin.1, age_group, all_unprotected, target_population, reduced_population, -age_group) %>%
    distinct() %>%
    mutate(
        final_unprotected_targeted = all_unprotected - p_cover * target_population,
        final_unprotected_reduced = all_unprotected - p_cover * reduced_population
    ) %>%
    mutate(
        final_case_targeted = if_else(final_unprotected_targeted < 0, 0, final_unprotected_targeted * attack_rate * ci_r),
        final_case_reduced = if_else(final_unprotected_reduced < 0, 0, final_unprotected_reduced * attack_rate * ci_r)
    ) -> sia_scope_pop_unprotected


sia_scope_pop_unprotected %>%
    tidyr::pivot_longer((cols <- c("final_case_targeted", "final_case_reduced")), names_to = "scenario", values_to = "case") %>%
    ggplot() +
    geom_col(aes(x = place.admin.1, y = case, fill = scenario), position = "dodge")

# check if p_unprotected is unique for each year, age_group, place.admin.2

### visualizaiton
library(sf)
library(rnaturalearth)

province_map <- ne_states(
    country = c("Nigeria"),
    returnclass = "sf"
)

province_map %>%
    rename(place.admin.1 = name, place.admin.0 = admin) %>%
    mutate(place.admin.1 = toupper(place.admin.1), place.admin.0 = toupper(place.admin.0)) %>%
    select(place.admin.0, place.admin.1, geometry) %>%
    left_join(sia_scope_pop_unprotected) -> case_map

# Not obvious
case_map %>%
    tidyr::pivot_longer(
        cols = c("final_case_targeted", "final_case_reduced"),
        names_transform = list(name = ~ recode(.x,
            "final_case_targeted" = "AFP infections under ideal scope",
            "final_case_reduced" = "AFP infections under reduced scope"
        ))
    ) %>%
    mutate(value = cut(value, breaks = 10)) %>%
    ggplot() +
    geom_sf(aes(fill = value), color = "grey10", linewidth = 0.15) +
    scale_fill_viridis_d(option = "A", na.value = "white") +
    facet_wrap(. ~ name, nrow = 2) +
    theme_minimal() +
    theme(
        strip.text = element_text(size = 13),
        # legend.position = "none",
        legend.title = element_text(size = 13),
    )
ggsave("Target vs Reduced_d.png", width = 10, height = 5)


## Plot difference
case_map %>%
    mutate(infec_diff = cut(infec_diff, breaks = 10)) %>%
    ggplot() +
    geom_sf(aes(fill = infec_diff), color = "grey10", linewidth = 0.15) +
    scale_fill_viridis_d(option = "A", na.value = "white") +
    theme_minimal() +
    theme(
        strip.text = element_text(size = 13),
        # legend.position = "none",
        legend.title = element_text(size = 13)
    )
ggsave("Case Difference_d.png", width = 10, height = 3)




sia_scope_pop_unprotected %>%
    filter(final_case_targeted > 0) -> t2

View(t2)
