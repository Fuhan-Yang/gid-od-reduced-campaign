library(dplyr)
library(ggplot2)
## Regions to analyze ##
### countries
countries <- c(
    "NIGERIA", "CHAD",
    "DEMOCRATIC REPUBLIC OF THE CONGO", "ETHIOPIA",
    "NIGER", "CENTRAL AFRICAN REPUBLIC"
)

### Provinces


## functions: estimate counterfactual ##
estimate_counter <- function(infection, vac, VE) {
    counter_i <- infection / (1 - (vac * VE))
    return(counter_i)
}

estimate_obs <- function(counterfactual, vac, VE) {
    infection <- counterfactual * (1 - (vac * VE))
    return(infection)
}

#### Import the data ###
raw.data <- sirfunctions::get_all_polio_data(
    size = "small", data_folder = "GID/PEB/SIR/Data",
    polis_folder = "GID/PEB/SIR/POLIS"
)

### period of SIAs
date_start <- as.Date("2021-11-01")
date_end <- as.Date("2026-11-01")

### fake VE
fake_ve <- 0.12

## case data: AFP from tidypolis

raw_case <- raw.data$afp
raw_case %>%
    group_by(place.admin.0, place.admin.1, dateonset) %>%
    summarize(afp.cases = dplyr::n(), .groups = "drop") %>%
    filter(
        place.admin.0 %in% countries,
        dateonset >= date_start & dateonset < date_end
    ) -> sel_case

sel_case %>%
    group_by(place.admin.0, place.admin.1) %>%
    mutate(
        month = format(dateonset, "%Y-%m"),
        infection = 2000 * sum(afp.cases)
    ) %>%
    select(-afp.cases, -dateonset) %>%
    distinct() -> sel_case

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
        area.population, immunized.population, targeted.population
    ) %>%
    distinct() -> sel_sia


### get the vaccine coverage
ctry_pop <- raw.data$ctry.pop
ctry_pop %>%
    filter(ctry %in% countries, year == 2026) %>%
    select(ctry, u5pop, year) -> sel_pop

sel_sia %>%
    mutate(year = as.numeric(gsub("(\\d{4})-\\d{2}", "\\1", month))) %>%
    left_join(sel_pop %>% rename(place.admin.0 = ctry),
        by = c("place.admin.0", "year")
    ) %>%
    rename(ctry_u5pop = u5pop) %>%
    mutate(vac_cov = immunized.population / ctry_u5pop) %>%
    mutate(vac_cov = if_else(vac_cov > 1, targeted.population / ctry_u5pop, vac_cov)) %>%
    mutate(vac_cov = if_else(vac_cov > 1, 1, vac_cov)) -> sel_sia

sel_case %>%
    full_join(sel_sia, by = c("place.admin.0", "place.admin.1", "month")) %>%
    mutate(counter = estimate_counter(infection, vac_cov, VE = fake_ve)) %>%
    group_by(place.admin.0, place.admin.1) %>%
    summarise(counter = mean(counter, na.rm = TRUE), .groups = "drop") -> counter_df
# fake VE


## Given estimated counterfactual, estimate the cases given varied vaccine coverage
scope <- read.csv("data/scope.csv")
# for blank in target population, interpret as 0
scope %>%
    mutate(
        target_population = if_else(is.na(target_population), 0, target_population),
        reduced_population = if_else(is.na(reduced_population), 0, reduced_population)
    ) -> scope

prov_pop <- raw.data$prov.pop
prov_pop %>%
    filter(ctry %in% countries, year == 2026) %>%
    select(ctry, prov, u5pop) %>%
    rename(prov_u5pop = u5pop) -> prov_pop

counter_df %>%
    right_join( # only match the available regions in the scope
        scope %>%
            rename(place.admin.0 = country, place.admin.1 = state) %>%
            mutate(place.admin.1 = toupper(place.admin.1)),
        by = c("place.admin.0", "place.admin.1")
    ) %>%
    filter(!is.na(counter)) %>% # # only use the data when counterfactual is available, need to double check
    left_join(prov_pop %>% rename(place.admin.0 = ctry, place.admin.1 = prov), by = c("place.admin.0", "place.admin.1")) %>%
    mutate(
        target_cov = target_population / prov_u5pop,
        reduced_cov = reduced_population / prov_u5pop ## prov_u5pop is smaller than target or reduced?
    ) %>%
    mutate(
        target_case = estimate_obs(counter, target_cov, fake_ve),
        reduced_case = estimate_obs(counter, reduced_cov, fake_ve)
    ) %>%
    mutate(infec_diff = reduced_case - target_case) -> counter_df

## visualization
# install.packages(c("sf", "rnaturalearth"))
library(sf)
library(rnaturalearth)

province_map <- ne_states(
    country = c("Nigeria", "Ethiopia"),
    returnclass = "sf"
)

province_map %>%
    rename(place.admin.1 = name, place.admin.0 = admin) %>%
    mutate(place.admin.1 = toupper(place.admin.1), place.admin.0 = toupper(place.admin.0)) %>%
    select(place.admin.0, place.admin.1, geometry) %>%
    left_join(counter_df) -> case_map


# Not obvious
case_map %>%
    tidyr::pivot_longer(
        cols = c("target_case", "reduced_case"),
        names_transform = list(name = ~ recode(.x,
            "target_case" = "AFP infections under ideal scope",
            "reduced_case" = "AFP infections under reduced scope"
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


# raw_sia %>%
#     filter(
#         activity.start.date >= as.Date("2025-01-01"),
#         status == "Done", place.admin.0 %in% countries, age.group == "0 to 5 years"
#     ) %>%
#     select(
#         sia.code, place.admin.0, place.admin.1, place.admin.2, targeted.population,
#         area.population, immunized.population, admin.2.immunized.population
#     ) -> t


# t <- sel_sia %>%
#     filter(place.admin.0 == "NIGERIA")

# t %>%
#     group_by(sia.code) %>%
#     summarize(vac_sum = sum(immunized.population, na.rm = TRUE), .groups = "drop")

# t %>%
#     group_by(sia.code) %>%
#     summarize(admin2_sum = sum(admin.2.immunized.population, na.rm = TRUE), .groups = "drop")


# pos <- raw.data$pos


# sel_sia %>%
#     group_by(place.admin.1) %>%
#     mutate(
#         imm_unique = unique(immunized.population),
#         tar_unique = unique(targeted.population)
#     )


# sel_sia %>%
#     group_by(month, place.admin.0, place.admin.1) %>%
#     summarize(n = n())

# sel_sia %>%
#     distinct(place.admin.0, place.admin.1, month, immunized.population, targeted.population) -> t


# counter_df %>%
#     right_join( # only match the available regions in the scope
#         scope %>%
#             rename(place.admin.0 = country, place.admin.1 = state) %>%
#             mutate(place.admin.1 = toupper(place.admin.1)),
#         by = c("place.admin.0", "place.admin.1")
#     ) %>%
#     filter(!is.na(counter)) -> t


t %>%
    distinct(month) -> t1

View(t1)


sel_sia %>%
    group_by(place.admin.0, place.admin.1, month) %>%
    mutate(n_unique_imm = n_distinct(immunized.population), n_unique_sia = n_distinct(sia.code)) -> t1

View(t1)
sum(t1$n_unique_sia > 1)

t1[which(t1$n_unique_sia > 1 & t1$n_unique_imm > 1), ] -> t2
View(t2)



sel_sia %>%
    group_by(place.admin.0, place.admin.1, place.admin.2, year) %>%
    summarize(n_unique_sia = n_distinct(sia.code)) %>%
    filter(place.admin.0 == "NIGERIA") -> t3


dist_pop <- raw.data$dist.pop
sel_sia %>%
    left_join(dist_pop %>% rename(place.admin.0 = ctry, place.admin.1 = prov, place.admin.2 = dist) %>%
        mutate(year = as.character(year)), by = c("place.admin.0", "place.admin.1", "place.admin.2", "year")) %>%
    select(place.admin.0, place.admin.1, place.admin.2, year, sia.code, u5pop) -> t4
