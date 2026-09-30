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
estimate_counter <- function(case, vac, VE) {
    counter <- case / (1 - (vac * VE))
    return(counter)
}

estimate_obs <- function(counterfactual, vac, VE) {
    case <- counterfactual * (1 - (vac * VE))
    return(case)
}

#### Import the data ###
raw.data <- sirfunctions::get_all_polio_data(
    size = "small", data_folder = "GID/PEB/SIR/Data",
    polis_folder = "GID/PEB/SIR/POLIS"
)

### period of SIAs
date_start <- as.Date("2026-01-01")
date_end <- as.Date("2026-09-01")

### fake VE
fake_ve <- 0.8

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
        case = sum(afp.cases),
        month = format(dateonset, "%Y-%m")
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
    ) %>%
    mutate(area.population = as.numeric(gsub(",", "", area.population))) %>%
    mutate(
        vac_pop = 1e6, month = format(activity.start.date, "%Y-%m"),
        vac_cov = vac_pop / area.population
    ) %>% # fake vac population number
    select(
        place.admin.0, place.admin.1,
        vac_pop, area.population, vac_cov, sia.code, month
    ) %>%
    distinct() -> sel_sia


counter_df <- sel_case %>%
    full_join(sel_sia, by = c("place.admin.0", "place.admin.1", "month")) %>%
    mutate(counter = estimate_counter(case, vac_cov, VE = fake_ve)) # fake VE


## Given estimated counterfactual, estimate the cases given varied vaccine coverage
scope <- read.csv("data/scope.csv")

counter_df %>%
    right_join( # only match the available regions in the scope
        scope %>%
            rename(place.admin.0 = country, place.admin.1 = state) %>%
            mutate(place.admin.1 = toupper(place.admin.1)),
        by = c("place.admin.0", "place.admin.1")
    ) -> counter_df

t <- counter_df %>% filter(!is.na(counter))

## VE needs to be a time-varying process, use a fixed number for now....
counter_df %>%
    filter(!is.na(counter)) %>% # only use the data when counterfactual is available, need to double check
    mutate(
        target_cov = target_population / area.population,
        reduced_cov = reduced_population / area.population
    ) %>%
    mutate(
        target_case = estimate_obs(counter, target_cov, fake_ve),
        reduced_case = estimate_obs(counter, reduced_cov, fake_ve)
    ) %>%
    mutate(case_diff = reduced_case - target_case) -> counter_df
