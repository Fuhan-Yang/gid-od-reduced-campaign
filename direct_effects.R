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


### period of SIAs
date_start <- as.Date("2025-11-01")
date_end <- as.Date("2025-12-01")

#### Import the data ###
raw.data <- sirfunctions::get_all_polio_data(
    size = "small", data_folder = "GID/PEB/SIR/Data",
    polis_folder = "GID/PEB/SIR/POLIS"
)

## case data: AFP from tidypolis

raw_case <- raw.data$afp
raw_case %>%
    group_by(place.admin.0, place.admin.1, dateonset) %>%
    summarize(afp.cases = dplyr::n(), .groups = "drop") %>%
    filter(
        place.admin.0 %in% countries,
        dateonset >= date_start & dateonset < date_end
    ) -> sel_case

## vaccine data: SIA
raw_sia <- raw.data$sia
raw_sia %>%
    filter(
        place.admin.0 %in% countries,
        activity.start.date >= date_start & activity.start.date < date_end,
        age.group == "0 to 5 years"
    ) %>%
    mutate(area.population = as.numeric(gsub(",", "", area.population))) %>%
    mutate(vac_pop = 1e6) %>%
    select(
        activity.start.date, activity.end.date, vac_pop, place.admin.0, place.admin.1,
        area.population
    ) -> sel_sia

## vaccine data: RI

## estimate counterfactual ##
estimate <- function(case, vac, VE) {
    counter <- case / (1 - (vac * VE))
    return(counter)
}

lapply(1:length(countries), function(x) {
    case_this <- sel_case %>%
        filter(place.admin.0 == countries[x]) %>%
        group_by(place.admin.1) %>%
        summarise(case = sum(afp.cases), .groups = "drop") %>%
        mutate(place.admin.0 = countries[x]) %>%
        select(place.admin.0, place.admin.1, case)

    vac_this <- sel_sia %>%
        filter(place.admin.0 == countries[x]) %>%
        group_by(place.admin.1, area.population) %>%
        summarise(vac_sum = sum(vac_pop), pop_sum = sum(area.population), .groups = "drop") %>%
        mutate(place.admin.0 = countries[x], vac_cov = vac_sum / pop_sum) %>%
        select(place.admin.0, place.admin.1, vac_cov)

    ## counterfactual for this month ##
    case_vac <- case_this %>%
        left_join(vac_this, by = c("place.admin.0", "place.admin.1")) %>%
        mutate(counter = estimate(case, vac_cov, VE = 0.8))

    case_vac
}) -> counter_list

counter_df <- do.call(rbind, counter_list)
counter_df
