#' Calculating EMU from excel spreadsheet using modelled supply-share estimates, and saving output in specified files
#' @name country_ss_to_emu
#' @param country_tools_info Country tools info (get_tools_info() output)
#' @param input_type The chosen service type (in the Shiny UI)
#' @param method_summary Summarise by method or not (TRUE/FALSE)
#' @param save_samples Save samples for analysis (TRUE/FALSE)
#' @import tidyverse
#' @import stringr
#' @export

country_ss_to_emu <- function(country_tools_info, input_type = NULL, method_summary = FALSE, save_samples = FALSE){

  ss_tools_info <- country_tools_info
  country_emu_df <- list()
  fixed_country_emu_df <- list()
  sector_share_samples <- list()
  facility_share_samples <- list()
  private_sector_adj_samples <- list()
  commodities_table_samples <- list()

  if(is.null(input_type)){
    ss_data_types <- c("Contraceptive commodities distributed to clients",
                       "Contraceptive commodities distributed to facilities",
                       "FP visits",
                       "FP users")
  }

  else {

    ss_data_types <- ifelse(input_type == "clients", "Contraceptive commodities distributed to clients",
                       ifelse(input_type == "facilities", "Contraceptive commodities distributed to facilities",
                              ifelse(input_type == "users", "FP users",
                                     ifelse(input_type == "visits", "FP visits", NA))))

  }

  for(s in ss_data_types) {

    ss_quantity_data <- ss_tools_info$ss_quantity_data %>% filter(ss_type == s)
    pop_dataset <- ss_tools_info$pop_dataset
    setup_data <- ss_tools_info$setup_data
    recode_sectors_reporting <- ss_tools_info$recode_sectors_reporting
    recode_scaleup_table <- ss_tools_info$recode_scaleup_table
    ss_info <- ss_tools_info$ss_info %>% filter(ss_type == s)
    cyp_table <- ss_tools_info$cyp_table %>% filter(ss_type == s)
    reporting_rates <- ss_tools_info$reporting_rates_table %>% filter(ss_type == s)
    ss_val_type <- ss_tools_info$ss_val_type

    if(s != "FP users"){
    long_term_rates <- ss_tools_info$method_continuation_data %>%
      mutate_at(vars(-(1:2)), as.numeric)
    }

    user_input_adjustment_table <- ss_tools_info$user_input_adjustment_table
    include_condoms_df <- ss_tools_info$include_condoms_df %>% filter(ss_type == s)

    Country <- setup_data$Country

    cyp_table_clean <- cyp_table %>%
      mutate(method_type = ifelse(method_type == "long", "Long", "Short")) %>%
      select(-ss_type, -relative_sd)

    if(nrow(ss_quantity_data) == 0){
      next
    }

    ss_data <- left_join(ss_quantity_data, cyp_table_clean, by = c("method_detail"))

    ss_data <- ss_data %>%
      select(method_type, method_overview, method_detail, cyp_factor, cyp_factor_adjusted, units, everything(), -ss_type) %>%
      select_if(~any(!is.na(.)))

    if(s == "FP users"){
      baseline_users <- NULL
    }
    else {
      # Baseline users
      baseline_users <- baseline_users(ss_data, s, recode_scaleup_table, long_term_rates)
    }

    uncertainty_adjust <- adjust_users_uncertainty(recode_sectors_reporting,
                                                   reporting_rates,
                                                   baseline_users,
                                                   ss_data,
                                                   long_term_rates,
                                                   user_input_adjustment_table)


    adjust_users_priv_sector <- uncertainty_adjust$users_incl_private

    fixed_adjust_users_priv_sector <- uncertainty_adjust$user_incl_private_fixed

    total_adj_users <- total_adjusted_users(adjust_users_priv_sector, fixed_adjust_users_priv_sector, include_condoms_df, method_summary)

    total_users <- total_adj_users$total_emu_df
    total_fixed_users <- total_adj_users$fixed_total_emu_df
    rr_data <- reporting_rates %>% rename(year = Year, ss_type = ss_type, name = Country)

    emu_data <- calculate_emu_from_users(pop_dataset, total_users, total_fixed_users, s, rr_data, method_summary)

    ss_type_number <- match(s, ss_data_types)


    commodities_table_samples[[ss_type_number]] <- uncertainty_adjust$users_incl_private %>% mutate(ss_type = s)
    sector_share_samples[[ss_type_number]] <- uncertainty_adjust$supply_share_mod_res %>% mutate(ss_type = s)
    facility_share_samples[[ss_type_number]] <- uncertainty_adjust$FP_source_data_samples %>% mutate(ss_type = s)
    private_sector_adj_samples[[ss_type_number]] <- uncertainty_adjust$inverse_adjustment_table %>% mutate(ss_type = s)


    if(Country == "DR Congo"){
      country_name <- "Democratic Republic of the Congo"
    }

    else if(Country == "Tanzania"){
      country_name <- "United Republic of Tanzania"
    }
    else if(Country == "Cote d'Ivoire"){
      country_name <- "Côte d'Ivoire"
    }
    else if(Country == "Lao PDR"){
      country_name <- "Lao People's Democratic Republic"
    }
    else if(Country == "Micronesia"){
      country_name <- "Micronesia (Federated States of)"
    }
    else {
      country_name <- Country
    }

    country_code <- country_code_data %>% filter(Country == country_name) %>% pull(division_numeric_code)

    if(length(country_code) == 0){
      country_code <- NA
    }


    country_emu_df[[ss_type_number]] <- emu_data$emu_samps %>% mutate(division_numeric_code = country_code)
    fixed_country_emu_df[[ss_type_number]] <- emu_data$fixed_emu %>% mutate(division_numeric_code = country_code)


  }

  emu_samps <- data.table::rbindlist(country_emu_df) %>% as_tibble()
  fixed_emu <- data.table::rbindlist(fixed_country_emu_df) %>% as_tibble()
  all_emu_out <- emu_samps %>% group_by(sample_id, name, ss_type) %>% mutate(delta_emu = emu - lag(emu))

  all_commodities_table_samples <- data.table::rbindlist(commodities_table_samples) %>% as_tibble() %>% mutate(name = Country) %>%
    mutate(ss_type = ifelse(ss_type == "Contraceptive commodities distributed to clients", "clients",
                            ifelse(ss_type == "Contraceptive commodities distributed to facilities", "facilities",
                                   ifelse(ss_type == "FP visits", "visits", ifelse(ss_type == "FP users", "users", ss_type)))))

  all_sector_share_samples <- data.table::rbindlist(sector_share_samples) %>% as_tibble() %>% mutate(name = Country) %>%
    mutate(ss_type = ifelse(ss_type == "Contraceptive commodities distributed to clients", "clients",
                            ifelse(ss_type == "Contraceptive commodities distributed to facilities", "facilities",
                                   ifelse(ss_type == "FP visits", "visits", ifelse(ss_type == "FP users", "users", ss_type)))))

  all_facility_share_samples <- data.table::rbindlist(facility_share_samples) %>% as_tibble() %>% mutate(name = Country) %>%
    mutate(ss_type = ifelse(ss_type == "Contraceptive commodities distributed to clients", "clients",
                            ifelse(ss_type == "Contraceptive commodities distributed to facilities", "facilities",
                                   ifelse(ss_type == "FP visits", "visits", ifelse(ss_type == "FP users", "users", ss_type)))))

  all_private_sector_adj_samples <- data.table::rbindlist(private_sector_adj_samples) %>% as_tibble() %>% mutate(name = Country) %>%
    mutate(ss_type = ifelse(ss_type == "Contraceptive commodities distributed to clients", "clients",
                            ifelse(ss_type == "Contraceptive commodities distributed to facilities", "facilities",
                                   ifelse(ss_type == "FP visits", "visits", ifelse(ss_type == "FP users", "users", ss_type)))))


  overall_emu <- all_emu_out %>%
    group_by(division_numeric_code, name, pop_type, ss_type, year) %>%
    summarise(median_emu = median(emu),
              emu_roc = median(delta_emu),
              sd_emu = sd(emu),
              sd_emu_roc = sd(delta_emu, na.rm = TRUE)) %>%
    arrange(ss_type) %>%
    rename(emu = median_emu)

  if ("Region" %in% colnames(setup_data)){
    region_name <- setup_data$Region
  }
  else {
    region_name <- NA
  }

  overall_emu <- overall_emu %>%
    mutate(emu = signif(emu,4),
           emu_roc = signif(emu_roc, 4),
           sd_emu = signif(sd_emu, 4),
           sd_emu_roc = signif(sd_emu_roc, 4),
           Region = region_name
          ) %>% mutate(sd_emu_roc = ifelse(is.na(sd_emu_roc), 0, sd_emu_roc),
                       ss_val_type = ss_val_type)

  if(save_samples == TRUE){
    return(list(sector_share_samples = all_sector_share_samples,
                facility_share_samples = all_facility_share_samples,
                private_sector_adj_samples = all_private_sector_adj_samples,
                emu_samples = all_emu_out,
                commodities_table_samples = all_commodities_table_samples,
                emu_dataset = overall_emu))
  }
  else{
    return(overall_emu)
  }
}
