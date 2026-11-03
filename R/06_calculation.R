#' Join district population with SIA exposure history and compute
#' cumulative SIA rounds experienced per district, ordered by age.
#'
#' @param dist_pop Output of remap_district_population()
#' @param sia_count Output of remap_sia_counts()
#' @param target_year Numeric/character reference year, e.g. 2026
#' @return dataframe with n_sia_experienced per place.admin.0/1/2/year
join_population_sia <- function(dist_pop, sia_count, target_year) {
  dist_pop %>%
    dplyr::left_join(sia_count, by = c("place.admin.0", "place.admin.1", "place.admin.2", "year")) %>%
    dplyr::mutate(
      n_sia_per_year = dplyr::if_else(is.na(n_sia_per_year), 0, n_sia_per_year),
      age_at_strategies = as.numeric(target_year) - as.numeric(as.character(year))
    ) %>%
    dplyr::arrange(place.admin.0, place.admin.1, place.admin.2, age_at_strategies) %>%
    dplyr::group_by(place.admin.0, place.admin.1, place.admin.2) %>%
    dplyr::mutate(n_sia_experienced = cumsum(n_sia_per_year)) %>%
    dplyr::ungroup()
}

#' Calculate unprotected children by dose count, pre-strategy, and
#' roll up to province level.
#'
#' @param sia_pop Output of join_population_sia()
#' @param p_missing Per-round coverage gap
#' @return dataframe with n_0/1/2_dose_pre, unprotected_pre, and province sums
calculate_unprotected_pre <- function(sia_pop, p_missing) {
  sia_pop %>%
    dplyr::mutate(n_sia_experienced = round(n_sia_experienced)) %>%
    dplyr::mutate(
      n_0_dose_pre = birth_pop * get_sia_coverage(0, n_sia_experienced, p_missing = p_missing),
      n_1_dose_pre = birth_pop * get_sia_coverage(1, n_sia_experienced, p_missing = p_missing),
      n_2_dose_pre = birth_pop * get_sia_coverage(2, n_sia_experienced, p_missing = p_missing)
    ) %>%
    dplyr::mutate(unprotected_pre = n_0_dose_pre + n_1_dose_pre + n_2_dose_pre) %>%
    dplyr::group_by(place.admin.0, place.admin.1) %>%
    dplyr::mutate(
      all_unprotected_prov_pre    = sum(unprotected_pre),
      all_0_dose_prov_pre         = sum(n_0_dose_pre),
      all_other_dose_prov_pre     = sum(n_1_dose_pre) + sum(n_2_dose_pre)
    ) %>%
    dplyr::ungroup()
}

#' Join province-level pre-strategy results with scope and province
#' population, then calculate post-strategy protection and estimated
#' AFP case counts for both ideal and reduced strategies.
#'
#' @param sia_pop_unpro Output of calculate_unprotected_pre()
#' @param scope Output of prepare_scope() (same country)
#' @param prov_pop Output of prepare_province_population() (same country)
#' @param attack_rate,ci_r Epidemiological parameters
#' @param p_missing Per-round coverage gap
#' @return list(full = <district-level joined df>, summary = <province-level df>)
calculate_post_strategy <- function(sia_pop_unpro, scope, prov_pop, p_missing) {

  full <- sia_pop_unpro %>%
    dplyr::right_join(scope, by = c("place.admin.0", "place.admin.1")) %>%
    dplyr::left_join(prov_pop, by = c("place.admin.0", "place.admin.1")) %>%
    dplyr::mutate(
      unprotected_ideal_post = dplyr::if_else(
        all_unprotected_prov_pre - calculate_protected(p_missing, ideal_population, u5pop) < 0, 0,
        all_unprotected_prov_pre - calculate_protected(p_missing, ideal_population, u5pop)),
      unprotected_reduced_post = dplyr::if_else(
        all_unprotected_prov_pre - calculate_protected(p_missing, reduced_population, u5pop) < 0, 0,
        all_unprotected_prov_pre - calculate_protected(p_missing, reduced_population, u5pop))
    ) %>%
    dplyr::mutate(
      partially_protected_ideal_post = dplyr::case_when(
        unprotected_ideal_post > 0 ~ calculate_protected(p_missing, ideal_population, u5pop) *
          all_0_dose_prov_pre / (all_0_dose_prov_pre + all_other_dose_prov_pre),
        .default = all_0_dose_prov_pre),
      partially_protected_reduced_post = dplyr::case_when(
        unprotected_reduced_post > 0 ~ calculate_protected(p_missing, reduced_population, u5pop) *
          all_0_dose_prov_pre / (all_0_dose_prov_pre + all_other_dose_prov_pre),
        .default = all_0_dose_prov_pre),
      fully_protected_ideal_post = dplyr::case_when(
        unprotected_ideal_post > 0 ~ calculate_protected(p_missing, ideal_population, u5pop) *
          all_other_dose_prov_pre / (all_0_dose_prov_pre + all_other_dose_prov_pre),
        .default = all_other_dose_prov_pre),
      fully_protected_reduced_post = dplyr::case_when(
        unprotected_reduced_post > 0 ~ calculate_protected(p_missing, reduced_population, u5pop) *
          all_other_dose_prov_pre / (all_0_dose_prov_pre + all_other_dose_prov_pre),
        .default = all_other_dose_prov_pre)
    )

  summary <- full %>%
    dplyr::distinct(place.admin.1, .keep_all = TRUE) %>%
    dplyr::select(
      place.admin.0, place.admin.1, all_unprotected_prov_pre, all_0_dose_prov_pre,
      all_other_dose_prov_pre, ideal_population, reduced_population, unprotected_ideal_post,
      unprotected_reduced_post, partially_protected_ideal_post, partially_protected_reduced_post,
      fully_protected_ideal_post, fully_protected_reduced_post,u5pop
    )

  list(full = full, summary = summary)
}
