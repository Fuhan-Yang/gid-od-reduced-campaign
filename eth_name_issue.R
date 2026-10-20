### One should start with population data, and force for each admin2, the age group has to be 0-4.
raw.data <- sirfunctions::get_all_polio_data(
    size = "small", data_folder = "GID/PEB/SIR/Data",
    polis_folder = "GID/PEB/SIR/POLIS"
)


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

dist_pop %>%
  filter(place.admin.0 == "ETHIOPIA", year == "2022") %>%
  arrange(place.admin.1) %>%
  distinct(place.admin.1) -> eth_2022

dist_pop %>%
  filter(place.admin.0 == "ETHIOPIA", year == "2023") %>%
  arrange(place.admin.1) %>%
  distinct(place.admin.1) -> eth_2023

dist_pop %>%
  filter(place.admin.0 == "ETHIOPIA", year == "2024") %>%
  arrange(place.admin.1) %>%
  distinct(place.admin.1) -> eth_2024

dist_pop %>%
  filter(place.admin.0 == "ETHIOPIA", year == "2025") %>%
  arrange(place.admin.1) %>%
  distinct(place.admin.1) -> eth_2025


dist_pop %>%
  filter(place.admin.0 == "ETHIOPIA", year == "2026") %>%
  arrange(place.admin.1) %>%
  distinct(place.admin.1) -> eth_2026


cbind_fill_na <- function(...) {
  dfs <- list(...)
  n <- max(sapply(dfs, nrow))
  dfs <- lapply(dfs, function(df) df[seq_len(n), , drop = FALSE])
  do.call(cbind, dfs)
}

result <- cbind_fill_na(eth_2022, eth_2023, eth_2024, eth_2025, eth_2026)

write.csv(result, "eth_provinces.csv", row.names = F)



