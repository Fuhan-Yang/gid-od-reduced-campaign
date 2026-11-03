#' Run the full strategy pipeline for ONE country. Isolated: failure in
#' one country's run does not affect others. Saves its own intermediate
#' .rds and output files so results can be inspected or resumed without
#' re-running the whole batch.
#'
#' @param country Single country name (upper case), e.g. "NIGERIA"
#' @param raw_data,global_dist,scope_all Shared inputs loaded once outside the loop
#' @param config Parsed config list
#' @return list(country=, full=, summary=, underestimated_pop=, success=, error=)
run_country_pipeline <- function(country, raw_data, global_dist, scope_all, config) {

  cli::cli_alert_info(paste("=== Running pipeline for ", country, " ==="))
  out_dir <- file.path(config$paths$output_dir, country)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  result <- tryCatch({

    date_start <- as.Date(config$date_start)
    date_end   <- as.Date(config$date_end)
    cohort_birthdays <- seq(date_start, date_end, "1 year")

    # 2. Shapefile
    cli::cli_alert_info(paste("Prepare shapefile for", country))
    shapefile <- prepare_shapefile(global_dist, country, config$birthyears)
    if (nrow(shapefile) == 0) stop("No shapefile rows found for ", country)
    cli::cli_process_done("Shapefile is ready!")

    # 3. Population
    cli::cli_alert_info(paste("Prepare population data for", country))
    dist_pop <- prepare_district_population(
      raw_data$dist.pop, country, config$birthyears, config$admin2_name_fixes
    )
    new_dist_pop <- remap_district_population(dist_pop, shapefile, config$target_year, config$crs)
    prov_pop <- prepare_province_population(raw_data$prov.pop, country, config$target_year)
    cli::cli_process_done("Population data is ready!")

    # 4. SIA
    sia_count <- prepare_sia_counts(
      raw_data$sia, country, date_start, date_end,
      cohort_birthdays, config$sia_filters, config$sia_admin2_name_fixes
    )
    new_sia_count <- remap_sia_counts(sia_count, shapefile, config$target_year, config$crs)

    # 5. Scope (already loaded for all countries; filter to this one)
    scope <- scope_all %>% dplyr::filter(place.admin.0 == country)
    if (nrow(scope) == 0) stop("No scope rows found for ", country, " in scope.csv")

    # 6. Protection calculations
    sia_pop <- join_population_sia(new_dist_pop, new_sia_count, config$target_year)

    missing_pop_check <- sia_pop %>% dplyr::filter(is.na(birth_pop))
    if (nrow(missing_pop_check) > 0) {
      warning(country, ": ", nrow(missing_pop_check),
              " district-year rows have missing birth_pop.")
    }

    sia_pop_unpro <- calculate_unprotected_pre(sia_pop, config$p_missing)

    post_strategy <- calculate_post_strategy(
      sia_pop_unpro, scope, prov_pop,p_missing = config$p_missing
    )

    # underestimated_pop <- flag_underestimated_population(
    #   post_strategy$summary, as_of_year = as.numeric(config$target_year)
    # )

    # 7. Save per-country intermediate + final outputs
    saveRDS(post_strategy, file.path(out_dir, "post_strategy.rds"))
    openxlsx::write.xlsx(post_strategy$full,    file.path(out_dir, "strategies_full.xlsx"), rowNames = FALSE)
    openxlsx::write.xlsx(post_strategy$summary, file.path(out_dir, "strategies.xlsx"), rowNames = FALSE)
    # write.csv(underestimated_pop, file.path(out_dir, "underestimated_province_pop.csv"), row.names = FALSE)

    save_all_plots(post_strategy$summary, out_dir)

    cli::cli_process_done(paste(country, " complete"))

    list(
      country = country,
      full = post_strategy$full,
      summary = post_strategy$summary,
      underestimated_pop = underestimated_pop,
      success = TRUE,
      error = NULL
    )

  }, error = function(e) {
    cli::cli_alert_warning(paste(country, " FAILED: ", conditionMessage(e)))
    list(country = country, full = NULL, summary = NULL,
         underestimated_pop = NULL, success = FALSE, error = conditionMessage(e))
  })

  result
}
