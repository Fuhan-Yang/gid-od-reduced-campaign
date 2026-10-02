install.packages("devtools")
library(devtools)
devtools::install_github("CDCGov/sirfunctions")
devtools::install_github("CDCGov/tidypolis")
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(stringr)
library(sirfunctions)
library(lubridate)


raw_data <- sirfunctions::get_all_polio_data()
sia_dat <- raw_data$sia
dis_pop_dat <- raw_data$dist.pop
prov_pop_dat <- raw_data$prov.pop %>%
  filter(u5pop != "NA",
         ctry == "NIGERIA",
         year == "2026")

table(polio_dat$classification)
strat_dat <- read_excel("C:/Users/riu8/OneDrive - CDC/Data/polio_nigeria.xlsx")

polio_dat <- raw_data$afp %>%
  filter(admin0officialname == "Nigeria") %>%
  mutate(year = year(specimen.date)) %>%
  filter(year == 2026,
         poliovirustypes == "cVDPV2",
         classification == "VDPV")

view(cntry_pop_dat1)

pm_ar <- nrow(polio_dat)/cntry_pop_dat1$u5pop


  

#weunic_IPV1_cov <- c(0.51, 0.55, 0.7, 0.67, 0.69)
#weunic_IPV2_cov <- c(0.17, 0.33, 0.48, 0.54, 0.57)
#https://data.unicef.org/wp-content/uploads/cp/immunisation/wuenic_nga.pdf


#Assuming 2/3 sias confer adequate life-long protection against paralytic polio: 
#To estimate the proportion of children who are left unprotected by switching from ideal to
#reduced strategy scope, we stratified to districts that only have 1/2 sias they are the target
#population in which the difference between scopes is determined. 

#The strategy-specific estimator for this analysis: 
#1. The cumulative weighted sum of district population weights: {district_i/province_i}
#3. The target population of that province

#This estimator was used to aggregate to the province level and estimate the difference in coverage between
#both strategies among those who would benefit (i.e. protection definition)

#Major Assumptions:
#1.The province target population is proportionally spread based on the district weights
#2.The province level coverage can serve as a proxy for district-specific coverage (loses district heterogeneity)
#3.The strategy SIA has its intended coverage of 100%
#4.2+/3+ sias are needed for adequate protection--scenario dependent
#5.Nigeria attack rate at time t is a proxy for province-level attack rate at time t

##############################
#CUM SUM FUNCTION & ESTIMATION
##############################

#######################
### 3 SIA Threshold ###
#######################

prop_pop_protect <- function(data1, data2, sia_data, country,
                             cohort_reset_year = 2026,
                             protection_threshold = 3) {
  
  edge_count <- protection_threshold - 1   
  cum_through_year <- cohort_reset_year - 1 
  
  prov_pop <- data1 %>%
    mutate(prov = clean_name(prov)) %>%
    filter(year == cohort_reset_year) %>%
    rename(province = prov, province_u5pop = u5pop) %>%
    select(province, province_u5pop)
  
  dist_pop <- data2 %>%
    mutate(
      ctry = clean_name(ctry),
      prov = clean_name(prov),
      dist = clean_name(dist),
      year = as.integer(year),
      u5pop = as.numeric(u5pop)
    ) %>%
    filter(ctry == country, year == cohort_reset_year) %>%
    distinct(prov, dist, year, .keep_all = TRUE) %>%
    rename(province = prov, district = dist, district_u5pop = u5pop) %>%
    select(province, district, district_u5pop)
  
  sia_clean <- sia_data %>%
    mutate(
      province = clean_name(province),
      district = clean_name(district),
      year = as.integer(year)
    ) %>%
    distinct(province, district, sia.code, year, .keep_all = TRUE)
  
  
  cum_hist <- sia_clean %>%
    filter(year <= cum_through_year) %>%
    group_by(province, district) %>%
    summarise(cum_sias = n_distinct(sia.code), .groups = "drop")
  
  cohort_hist <- sia_clean %>%
    filter(year == cohort_reset_year) %>%
    group_by(province, district) %>%
    summarise(cohort_sias = n_distinct(sia.code), .groups = "drop")
  
  district_indicator <- dist_pop %>%
    select(province, district) %>%
    left_join(cum_hist, by = c("province", "district")) %>%
    left_join(cohort_hist, by = c("province", "district")) %>%
    mutate(
      cum_sias    = coalesce(cum_sias, 0L),
      cohort_sias = coalesce(cohort_sias, 0L),
      
      cond_stuck_prior = (cum_sias == edge_count) & (cohort_sias == 0L),
      
      cond_cohort_reset = (cum_sias >= protection_threshold) & (cohort_sias == edge_count),
      
      I_dist = as.integer(cond_stuck_prior | cond_cohort_reset)
    )
  
  combine_dat <- dist_pop %>%
    left_join(prov_pop, by = "province") %>%
    left_join(district_indicator, by = c("district", "province")) %>%
    mutate(
      district_weight = district_u5pop / province_u5pop,
      I_dist = coalesce(I_dist, 0L),
      weighted_flag = district_weight * I_dist
    )
  
  combine_dat %>%
    group_by(province) %>%
    summarise(prop_pop_edge = sum(weighted_flag, na.rm = TRUE), .groups = "drop")
}

prov_cov <- prop_pop_protect(
  prov_pop_dat, dis_pop_dat, sia_dat_1,
  country = "NIGERIA",
  cohort_reset_year = 2026,
  protection_threshold = 3
)

print(prov_cov, n = 37)

#Results
strat_dat1 <- strat_dat %>%
  mutate(province = states) %>%
  mutate(province = clean_name(province)) %>%
  select(province, tp_ideal = `tp ideal`, tp_reduced = `tp reduced`)

#Check province-name mismatch before joining
anti_join(strat_dat1, prov_cov, by = "province")
anti_join(prov_cov, strat_dat1, by = "province")

#Join & calculate ideal vs. reduced headcounts
final_df <- strat_dat_1 %>%
  left_join(prov_cov, by = "province") %>%
  mutate(
    prop_pop_edge  = coalesce(prop_pop_edge, 0),
    n_edge_ideal   = prop_pop_edge * tp_ideal,
    n_edge_reduced = prop_pop_edge * tp_reduced,
    gap_edge       = n_edge_ideal - n_edge_reduced
  )

print(final_df, n = Inf)

final_df_3thres <- final_df %>%
  mutate(polio_cases = pm_ar * gap_edge)

sum(final_df_3thres$polio_cases)

library(gt)

library(gt)

final_gt3<-final_df_3thres %>%
  arrange(desc(polio_cases)) %>%
  gt() %>%
  tab_header(
    title = "Province-Level Protection Gap & Polio Case Estimates under Three-SIA Threshold"
  ) %>%
  cols_label(
    province = "Province",
    tp_ideal = "TP Ideal",
    tp_reduced = "TP Reduced",
    prop_pop_edge = "Prop. Pop. Edge",
    n_edge_ideal = "N Edge (Ideal)",
    n_edge_reduced = "N Edge (Reduced)",
    gap_edge = "Difference",
    polio_cases = "Polio Cases"
  ) %>%
  fmt_number(
    columns = c(tp_ideal, tp_reduced, n_edge_ideal, n_edge_reduced, gap_edge, polio_cases),
    decimals = 1
  ) %>%
  fmt_percent(
    columns = prop_pop_edge,
    decimals = 1
  ) %>%
  grand_summary_rows(
    columns = c(n_edge_ideal, n_edge_reduced, gap_edge, polio_cases),
    fns = list(Total = ~sum(., na.rm = TRUE)),
    fmt = ~fmt_number(., decimals = 1)
  )

gtsave(final_gt3, filename = "final_table_3sia.png")




#########################
### 2 SIA Threshold #####
#########################

prop_pop_protect <- function(data1, data2, sia_data, country,
                             cohort_reset_year = 2026,
                             protection_threshold = 2) {
  
  edge_count <- protection_threshold - 1   
  cum_through_year <- cohort_reset_year - 1 
  
  prov_pop <- data1 %>%
    mutate(prov = clean_name(prov)) %>%
    filter(year == cohort_reset_year) %>%
    rename(province = prov, province_u5pop = u5pop) %>%
    select(province, province_u5pop)
  
  dist_pop <- data2 %>%
    mutate(
      ctry = clean_name(ctry),
      prov = clean_name(prov),
      dist = clean_name(dist),
      year = as.integer(year),
      u5pop = as.numeric(u5pop)
    ) %>%
    filter(ctry == country, year == cohort_reset_year) %>%
    distinct(prov, dist, year, .keep_all = TRUE) %>%
    rename(province = prov, district = dist, district_u5pop = u5pop) %>%
    select(province, district, district_u5pop)
  
  sia_clean <- sia_data %>%
    mutate(
      province = clean_name(province),
      district = clean_name(district),
      year = as.integer(year)
    ) %>%
    distinct(province, district, sia.code, year, .keep_all = TRUE)
  
  
  cum_hist <- sia_clean %>%
    filter(year <= cum_through_year) %>%
    group_by(province, district) %>%
    summarise(cum_sias = n_distinct(sia.code), .groups = "drop")
  
  # Cohort-specific exposure--only SIAs conducted in the reset year itself.
  cohort_hist <- sia_clean %>%
    filter(year == cohort_reset_year) %>%
    group_by(province, district) %>%
    summarise(cohort_sias = n_distinct(sia.code), .groups = "drop")
  
  district_indicator <- dist_pop %>%
    select(province, district) %>%
    left_join(cum_hist, by = c("province", "district")) %>%
    left_join(cohort_hist, by = c("province", "district")) %>%
    mutate(
      cum_sias    = coalesce(cum_sias, 0L),
      cohort_sias = coalesce(cohort_sias, 0L),
      
      cond_stuck_prior = (cum_sias == edge_count) & (cohort_sias == 0L),
      
      cond_cohort_reset = (cum_sias >= protection_threshold) & (cohort_sias == edge_count),
      
      I_dist = as.integer(cond_stuck_prior | cond_cohort_reset)
    )
  
  combine_dat <- dist_pop %>%
    left_join(prov_pop, by = "province") %>%
    left_join(district_indicator, by = c("district", "province")) %>%
    mutate(
      district_weight = district_u5pop / province_u5pop,
      I_dist = coalesce(I_dist, 0L),
      weighted_flag = district_weight * I_dist
    )
  
  combine_dat %>%
    group_by(province) %>%
    summarise(prop_pop_edge = sum(weighted_flag, na.rm = TRUE), .groups = "drop")
}

prov_cov <- prop_pop_protect(
  prov_pop_dat, dis_pop_dat, sia_dat_1,
  country = "NIGERIA",
  cohort_reset_year = 2026,
  protection_threshold = 2
)

print(prov_cov, n = 37)

final_df <- strat_dat_1 %>%
  left_join(prov_cov, by = "province") %>%
  mutate(
    prop_pop_edge  = coalesce(prop_pop_edge, 0),
    n_edge_ideal   = prop_pop_edge * tp_ideal,
    n_edge_reduced = prop_pop_edge * tp_reduced,
    gap_edge       = n_edge_ideal - n_edge_reduced
  )

print(final_df, n = Inf)

final_df_2thres <- final_df %>%
  mutate(polio_cases = pm_ar * gap_edge)


final_gt2<-final_df_2thres %>%
  arrange(desc(polio_cases)) %>%
  gt() %>%
  tab_header(
    title = "Province-Level Protection Gap & Polio Case Estimates under Two-SIA Threshold"
  ) %>%
  cols_label(
    province = "Province",
    tp_ideal = "TP Ideal",
    tp_reduced = "TP Reduced",
    prop_pop_edge = "Prop. Pop. Edge",
    n_edge_ideal = "N Edge (Ideal)",
    n_edge_reduced = "N Edge (Reduced)",
    gap_edge = "Difference",
    polio_cases = "Polio Cases"
  ) %>%
  fmt_number(
    columns = c(tp_ideal, tp_reduced, n_edge_ideal, n_edge_reduced, gap_edge, polio_cases),
    decimals = 1
  ) %>%
  fmt_percent(
    columns = prop_pop_edge,
    decimals = 1
  ) %>%
  grand_summary_rows(
    columns = c(n_edge_ideal, n_edge_reduced, gap_edge, polio_cases),
    fns = list(Total = ~sum(., na.rm = TRUE)),
    fmt = ~fmt_number(., decimals = 1)
  )

gtsave(final_gt2, filename = "final_table_2sia.png")

