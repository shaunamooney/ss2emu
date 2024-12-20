#' Calculating EMU incl. and excl. condoms using population data and adjusted users
#' @name calculate_emu_from_users
#' @param pop_data A table of population data
#' @param users_data A table of adjusted users totalled for the LAPM and STM methods - calculated using samples.
#' @param fixed_users_data A table of adjusted users totalled for the LAPM and STM methods - calculated using standard fixed method.
#' @param service_stats_type service statistics type
#' @param reporting_rate_data A dataframe of reporting rates by year
#' @param method_summary Summarise by method or not (TRUE/FALSE)
#' @return A table of calculated EMU data
#' @export
calculate_emu_from_users <- function(pop_data, users_data, fixed_users_data, service_stats_type, reporting_rate_data, method_summary){

  # years where reporting rate is over 60%
  rr_years <- reporting_rate_data %>% filter(reporting_rate >= 0.6) %>% pull(year)

  # emu years
  years_included <- users_data %>% pull(year) %>% unique()

  # filter population data for emu years
  pop_data_subset <- pop_data %>% filter(year %in% years_included) %>% distinct()

  # join population data to samples
  all_data <- left_join(users_data, pop_data_subset, by = "year")

  # calculate emu samples
  all_data <- all_data %>% mutate(emu = estimated_users/population)

  # join reporting rate data to samples and add ss_type column
  all_data <- all_data %>% left_join(reporting_rate_data, by = "year") %>% mutate(ss_type = service_stats_type)

  # filter for adequate reporting rate years
  all_final_emu_data <- all_data %>%
    filter(year %in% rr_years) %>%
    mutate(ss_type = ifelse(ss_type == "Contraceptive commodities distributed to clients", "clients",
                            ifelse(ss_type == "Contraceptive commodities distributed to facilities", "facilities",
                                   ifelse(ss_type == "FP visits", "visits", ifelse(ss_type == "FP users", "users", ss_type)))))

  # join fixed estimate data to population data
  all_fixed_data <- left_join(fixed_users_data, pop_data_subset, by = "year")

  # calculate fixed emu data
  all_fixed_data <- all_fixed_data %>% mutate(emu = estimated_users/population)

  # join with reporting rate data
  all_fixed_data <- all_fixed_data %>% left_join(reporting_rate_data, by = "year") %>% mutate(ss_type = service_stats_type)

  # filter for reporting rate years
  all_fixed_emu_final_data <- all_fixed_data %>%
    filter(year %in% rr_years)  %>%
    mutate(ss_type = ifelse(ss_type == "Contraceptive commodities distributed to clients", "clients",
                            ifelse(ss_type == "Contraceptive commodities distributed to facilities", "facilities",
                                   ifelse(ss_type == "FP visits", "visits", ifelse(ss_type == "FP users", "users", ss_type)))))




  return(list(emu_samps = all_final_emu_data, fixed_emu = all_fixed_emu_final_data))

}
