#' Adjusting quantity data to reflect users with uncertainty.
#' @name adjust_users_uncertainty
#' @param sectors_reporting_input A table of values indicating if a sector is fully/partial/non-reporting.
#' @param reporting_rates_df A table of reporting rates for your chosen ss_type
#' @param baseline_users A table of baseline users calculated for the years prior to your chosen ss_type data.
#' @param commodities_table A table of commodities for your chosen service statistic type
#' @param method_continuation_df A table of method continuation rates for long term methods.
#' @param user_input_adjustment_table A table indicating whether a method should be adjusted or not.
#' @return  A table returning the samples of users for each method each year adjusted for the missing private sector.
#' @import R2jags
#' @export
#'

adjust_users_uncertainty <- function(sectors_reporting_input, reporting_rates_df, baseline_users, commodities_table, method_continuation_df, user_input_adjustment_table) {

  s <- reporting_rates_df %>% pull(ss_type) %>% unique()

  sectors_reporting_input <- sectors_reporting_input %>%
    filter(ss_type == s) %>%
    mutate(Sector = ifelse(Sector == "Government Health Facilities and Home/Community Delivery", "Public Medical Sector", Sector))

  emu_years <- reporting_rates_df %>% pull(Year)

  country_name <- sectors_reporting_input %>% pull(Country) %>% unique()

  if(country_name == "Côte d'Ivoire"){
    country_name <- "Cote d'Ivoire"
  }

  if(country_name == "Example"){
    country_name <- "Cameroon"
  }

  country_fp_source_data <- fp_source_data_wide %>% filter(name == country_name) #%>% filter(year %in% emu_years)

  FP_source_data_temporal <- country_fp_source_data %>%
    mutate(method_overview = ifelse(method_overview == "OC Pills", "Pill",
                                    ifelse(method_overview == "EC", "Emergency contraception", method_overview)))

  FP_source_data_temp <- FP_source_data_temporal %>%
    mutate(public = `Public Medical Sector`, # just one sector for public
           private = ifelse(is.na(`NGO`), 0, `NGO`) +
             ifelse(is.na(`Private Hospital/ Clinic Delivery`), 0, `Private Hospital/ Clinic Delivery`) +
             ifelse(is.na(`Pharmacy`), 0, `Pharmacy`), # add all private sectors together
           other = 1 - (public + private), # calculate other based on public and private
           ratio = private/(1-public))

  FP_source_data_temp1 <- FP_source_data_temp %>% mutate(ratio = ifelse(ratio > 0.99999, 0.99999,
                          ifelse(ratio < 0.000001, 0.000001, ratio)))

  FP_source_data_full_rep <- FP_source_data_temp1 %>% filter(public == 1) %>% select(method_overview, year, `Public Medical Sector`,
                                                                                     `NGO`,
                                                                                     `Private Hospital/ Clinic Delivery`,
                                                                                     `Pharmacy`,
                                                                                     `Shop/ Church/ Friend`,
                                                                                     Other) %>%
    pivot_longer(cols = -c(method_overview, year), names_to = "sector", values_to = "supply_share_sample") %>% distinct()

  FP_source_data_summary <- FP_source_data_temp1 %>% filter(public < 1) %>% mutate(ngo_prop = `NGO`/private,
                                                            private_hosp_prop = `Private Hospital/ Clinic Delivery`/private,
                                                            pharmacy_prop = `Pharmacy`/private,
                                                            shop_prop = `Shop/ Church/ Friend`/other,
                                                            other_prop = Other/other)

  FP_source_data_temp <- FP_source_data_summary %>%
    mutate(logit_ratio = log(ratio/(1-ratio)), logit_public = log(public/(1-public))) %>%
    drop_na(logit_ratio) %>%
    select(method_overview, year, logit_ratio, logit_public, public, private, other, ngo_prop, private_hosp_prop, pharmacy_prop, shop_prop, other_prop) %>%
    distinct() %>%
    filter(!(method_overview %in% c("Male Condom", "Female Condom"))) # this is what I use


  FP_source_data_long <- FP_source_data_temporal %>%
    pivot_longer(cols = c(-method_overview, -name, -year, -survey), names_to = "sector", values_to = "supply_share")

  fixed_inverse_adjustment_table <- left_join(FP_source_data_long, sectors_reporting_input %>%
                                                rename(sector = Sector) %>%
                                                select(-Country), by = "sector") %>%
                                                mutate(method_adj = supply_share*Presence_recode) %>%
                                                group_by(year, method_overview) %>%
                                                summarise(fixed_adj_factor = sum(method_adj, na.rm = TRUE)) %>%
                                                mutate(fixed_adj_factor = ifelse(fixed_adj_factor > 1, 1,
                                                                                 ifelse(fixed_adj_factor == 0, 1,
                                                                                        ifelse(is.na(fixed_adj_factor), 1, fixed_adj_factor)))) %>%
                                                ungroup() %>%
                                                mutate(fixed_inv_adj_factor = 1/fixed_adj_factor)

  FP_source_data_long <- FP_source_data_long %>% mutate(sector_category = ifelse(sector == "Public Medical Sector", "public",
                                                                                 ifelse(sector %in% c("NGO", "Private Hospital/ Clinic Delivery", "Pharmacy"), "private", "other")))

  if(country_name == "DR Congo"){
    country_name <- "Congo Democratic Republic"
  }



  # & year %in% emu_years
  supply_share_sd <- supply_share_sd %>%
    mutate(year = floor(average_year)) %>%
    filter(name == country_name) %>%
    mutate(method_overview = case_when(
      method_overview == "Implants" ~ "Implant",
      method_overview == "Injectables" ~ "Injectable",
      method_overview == "OC Pills" ~ "Pill",
      method_overview == "EC" ~ "Emergency contraception",
      method_overview == "Female Sterilization" ~ "Sterilization (F)",
      TRUE ~ method_overview
    ))


  if(nrow(supply_share_sd) > 0) {

  model_input <- left_join(FP_source_data_temp, supply_share_sd %>% mutate(method_overview = as.character(method_overview)))

  model_methods <- supply_share_sd %>% pull(method_overview) %>% unique()

  FP_source_no_mod_est <- FP_source_data_temp1 %>%
    filter(!(method_overview %in% model_methods)) %>%
    select(method_overview, year, `Public Medical Sector`,
                                                          `NGO`,
                                                          `Private Hospital/ Clinic Delivery`,
                                                          `Pharmacy`,
                                                          `Shop/ Church/ Friend`,
                                                          Other) %>%
    pivot_longer(cols = -c(method_overview, year), names_to = "sector", values_to = "supply_share_sample")

  model_input <- model_input %>% filter(!is.na(sd_logit_pub)) %>%
    mutate(info_id = row_number())

  model_code <- "
    model {
      for(i in 1:N)
      {
        l_pub_mod[i] ~ dnorm(logit_public[i], (sd_logit_pub[i])^-2)
        l_rat_mod[i] ~ dnorm(logit_ratio[i], (sd_logit_priv_ratio[i])^-2)

        pub_mod[i] <- 1/(1+exp(-l_pub_mod[i]))
        priv_mod[i] <- (1-pub_mod[i])*(1/(1+exp(-l_rat_mod[i])))
        other_mod[i] <- 1 - (pub_mod[i] + priv_mod[i])
      }

      scale_up_new ~ dnorm(0.5, tau)T(0, 1)
      tau <- pow(0.1, -2)

    }
    "

  # Data list for JAGS
  model_data <- list(
    N = nrow(model_input),
    logit_public = model_input %>% pull(logit_public),
    sd_logit_pub = model_input %>% pull(sd_logit_pub),
    logit_ratio = model_input %>% pull(logit_ratio),
    sd_logit_priv_ratio = model_input %>% pull(sd_logit_priv_ratio)
  )

  # Choose the parameters to watch
  model_parameters <-c("pub_mod", "priv_mod", "other_mod", "scale_up_new")

  # Run the model
  model_run <- jags(
    data = model_data,
    parameters.to.save = model_parameters,
    model.file = textConnection(model_code),
    n.iter = 1000,
    n.burnin = 500,
    n.chains = 4,
    DIC = FALSE)

  # Pull out posterior samples
  post_samps <- model_run$BUGSoutput$sims.list

  pub_samples <- post_samps$pub_mod %>% as_tibble() %>% mutate(sample_id = as.character(row_number()))

  priv_samples <- post_samps$priv_mod %>% as_tibble() %>% mutate(sample_id = as.character(row_number()))

  other_samples <- post_samps$other_mod %>% as_tibble() %>% mutate(sample_id = as.character(row_number()))

  pub_table <- pub_samples %>% mutate(sample_id = as.character(row_number()))

  priv_table <- priv_samples %>% mutate(sample_id = as.character(row_number()))

  other_table <- other_samples %>% mutate(sample_id = as.character(row_number()))

  pub_table_long <- pub_table %>%
    pivot_longer(-sample_id, names_to = "info_id", values_to = "pub_share_sample") %>%
    mutate(info_id = substr(info_id, 2, nchar(info_id)) %>% as.numeric())

  priv_table_long <- priv_table %>%
    pivot_longer(-sample_id, names_to = "info_id", values_to = "priv_share_sample") %>%
    mutate(info_id = substr(info_id, 2, nchar(info_id)) %>% as.numeric())

  other_table_long <- other_table %>%
    pivot_longer(-sample_id, names_to = "info_id", values_to = "other_share_sample") %>%
    mutate(info_id = substr(info_id, 2, nchar(info_id)) %>% as.numeric())

  supply_share_mod_res_temp <- left_join(pub_table_long, priv_table_long) %>% left_join(other_table_long)

  supply_share_mod_res <- left_join(supply_share_mod_res_temp, model_input)

  FP_source_data_samples <- supply_share_mod_res %>%
    mutate(`Public Medical Sector` = pub_share_sample,
           `NGO` = priv_share_sample*ngo_prop,
           `Private Hospital/ Clinic Delivery` = priv_share_sample*private_hosp_prop,
           `Pharmacy` = priv_share_sample*pharmacy_prop,
           `Shop/ Church/ Friend` = other_share_sample*shop_prop,
           Other = other_share_sample*other_prop) %>% select(sample_id,
                                                method_overview,
                                                year,
                                                `Public Medical Sector`,
                                                `NGO`,
                                                `Private Hospital/ Clinic Delivery`,
                                                `Pharmacy`,
                                                `Shop/ Church/ Friend`,
                                                Other) %>%
    pivot_longer(cols = -c(sample_id, method_overview, year), names_to = "sector", values_to = "supply_share_sample") %>%
    mutate(supply_share_sample = ifelse(is.nan(supply_share_sample), 0, supply_share_sample))


  n_samples <- FP_source_data_samples %>% pull(sample_id) %>% unique() %>% length()

  FP_source_data_full_rep <- FP_source_data_full_rep %>% rbind(FP_source_no_mod_est)

  n_full_rep_row <- FP_source_data_full_rep %>% nrow()

  if(n_full_rep_row > 0){
    FP_source_data_full_rep <- FP_source_data_full_rep %>%
      slice(rep(1:n(), each = n_samples)) %>%
      mutate(sample_id = rep(1:n_samples, times = n_full_rep_row))
  }
  scale_up_samples <- post_samps$scale_up_new %>% as_tibble()

  scale_up_samples_df <- scale_up_samples %>%
    rename(partial_reporting_samples = 1) %>%
    mutate(non_reporting_samples = 0,
           full_reporting_samples = 1,
           sample_id = row_number()) %>%
    pivot_longer(-sample_id, names_to = "sector_presence", values_to = "Presence_recode_sample") %>%
    mutate(Presence_recode = ifelse(sector_presence == "partial_reporting_samples", 0.5,
                                    ifelse(sector_presence == "non_reporting_samples", 0, 1))) %>% select(sample_id, Presence_recode, Presence_recode_sample)

  sectors_reporting_samples <- full_join(sectors_reporting_input, scale_up_samples_df, by = "Presence_recode") %>% arrange(sample_id)

  commodities_table_samples <- commodities_table

  # ----------------------------------------------------------------------------

  # CALCULATING ADJUSTMENT FACTOR ----------------------------------------------

 FP_source_data_samples_long <- FP_source_data_samples %>% rbind(FP_source_data_full_rep) %>%
                                   mutate(sample_id = as.integer(sample_id))

 sector_data <- sectors_reporting_samples %>%
   ungroup() %>%
   select(sample_id, Sector, Presence_recode_sample) %>%
   rename(sector = Sector)

 FP_source_data_samples_join <- left_join(FP_source_data_samples_long, sector_data %>% drop_na(sector))

 adj_factor_table <- FP_source_data_samples_join %>%
                          mutate(method_adj = supply_share_sample*Presence_recode_sample) %>%
                          group_by(sample_id, year, method_overview) %>%
                          summarise(adj_factor = sum(method_adj, na.rm = TRUE)) %>%
                          ungroup()


}

  if(s == "FP users"){
    users_table_overview_fixed <- commodities_table %>%
      select(-cyp_factor, -units) %>%
      pivot_longer(-c(method_type, method_overview, method_detail, cyp_factor_adjusted),
                   names_to = "year",
                   values_to = "count") %>%
      mutate(year = as.numeric(year)) %>%
      mutate(year_index = NA, current_users = count) %>% mutate(method_overview = ifelse(method_detail == "Tubal Ligation (F)", "Sterilization (F)",
                                                                        ifelse(method_detail == "Male Condom", "Condom (M)", method_overview)))

  }

 else {

   method_continuation_df_long <- method_continuation_df %>%
                                    pivot_longer(-c(method_overview, method_detail),
                                                 names_to = "year",
                                                 values_to = "continuation") %>%
                                    separate(year, into = c("year","year_index")) %>%
                                    select(-year) %>%
                                    mutate(year_index = as.integer(year_index))

   baseline_users_long <- baseline_users %>%
                            pivot_longer(-c(method_overview, method_detail),
                                         names_to = "year",
                                         values_to = "baseline_users") %>%
                            mutate(year = as.numeric(year)) %>%
                            select(-method_overview) %>% drop_na(baseline_users)


   # FIXED ---------------------------------------------------------------------
   commodities_table_overview_fixed <- commodities_table %>%
     select(-c(cyp_factor,  units)) %>%
     pivot_longer(-c(method_type, method_overview, method_detail, cyp_factor_adjusted),
                  names_to = "year",
                  values_to = "count") %>%
     mutate(year = as.numeric(year)) %>%
     group_by(method_detail) %>%
     mutate(year_index = 1:n()) %>%
     ungroup() %>% mutate(method_overview = ifelse(method_detail == "Tubal Ligation (F)", "Sterilization (F)",
                                                   ifelse(method_detail == "Male Condom", "Condom (M)", method_overview)))


   commodities_table_ltm_fixed <- commodities_table_overview_fixed %>% filter(method_type == "Long") #%>% drop_na(count)

   commodities_table_ltm_join_temp_fixed <- left_join(commodities_table_ltm_fixed,method_continuation_df_long %>% mutate(method_overview = ifelse(method_detail == "Tubal Ligation (F)", "Sterilization (F)",method_overview)))
   commodities_table_ltm_join_fixed <- left_join(commodities_table_ltm_join_temp_fixed, baseline_users_long)
   commodities_table_ltm_join_fixed <- commodities_table_ltm_join_fixed %>% mutate(baseline_users = ifelse(is.na(baseline_users), 0, baseline_users))

   n_years <- commodities_table_ltm_join_fixed %>% pull(year) %>% unique() %>% length()

   commodities_table_ltm_totalled_fixed <- commodities_table_ltm_join_fixed %>%
     group_by(method_overview, method_detail) %>%
     mutate(year_index_rev = year_index[n():1]) %>%
     group_by(method_detail, year) %>%
     slice(rep(1:n(), each = year_index_rev)) %>%
     mutate(grouping_index = unique(year_index):n_years) %>%
     arrange(grouping_index) %>%
     group_by(method_detail,grouping_index) %>%
     summarise(year = max(year), new_count = sum(count[1:max(grouping_index)]*continuation[max(grouping_index):1], na.rm = TRUE)) %>%
     rename(year_index = grouping_index)

   commodities_table_ltm_overview_fixed <- left_join(commodities_table_ltm_join_fixed, commodities_table_ltm_totalled_fixed) %>%
     mutate(current_users = new_count + baseline_users)


   commodities_table_stm_fixed <- commodities_table_overview_fixed %>% filter(method_type == "Short")

   commodities_table_stm_overview_fixed <- commodities_table_stm_fixed %>% mutate(current_users = count*cyp_factor_adjusted)

   users_table_overview_fixed <- rbind(commodities_table_stm_overview_fixed, commodities_table_ltm_overview_fixed %>% select(-baseline_users,
                                                                                                           -new_count,
                                                                                                           -continuation))



  }

 # USING INVERSE ADJUSTMENT FACTOR --------------------------------------------
 users_table_overview_fixed <- users_table_overview_fixed %>%
   mutate(method_overview = ifelse(method_detail == "Vasectomy (M)", "Male Sterilization", method_overview))

  if(nrow(supply_share_sd) == 0){
    users_inc_private_sector_df <- users_table_overview_fixed %>%
      mutate(total_users = current_users*1) %>% filter(total_users > 0) %>% mutate(sample_id = 1)

    users_inc_private_sector_df_fixed <- users_inc_private_sector_df %>% select(-sample_id)

    inverse_adjustment_table <- NULL

    supply_share_mod_res <- NULL

    FP_source_data_samples <- NULL
  }

  else {
    inverse_adjustment_table <- adj_factor_table %>%
      mutate(inv_adj_factor = 1/adj_factor)

    users_adj_df <- full_join(inverse_adjustment_table, users_table_overview_fixed)

    users_adj_df <- users_adj_df %>%
      drop_na(count)

    users_adj_df_no_inv_adj <- users_adj_df %>% filter(is.na(inv_adj_factor)) %>% select(-sample_id)
    users_adj_df <- users_adj_df %>% filter(!is.na(sample_id))
    n_now_na_inv_adj <- nrow(users_adj_df_no_inv_adj)
    if(n_now_na_inv_adj > 0){
      users_adj_df_no_inv_adj <- users_adj_df_no_inv_adj %>%
        slice(rep(1:n(), each = n_samples)) %>%
        mutate(sample_id = rep(1:n_samples, times = n_now_na_inv_adj)) %>% mutate(inv_adj_factor = 1)

      users_adj_df <- rbind(users_adj_df, users_adj_df_no_inv_adj)
    }

    users_adj_df_fixed <- users_table_overview_fixed %>%
      left_join(fixed_inverse_adjustment_table)

    users_adj_df_fixed <- users_adj_df_fixed %>%
      drop_na(count)

    if(!is.null(user_input_adjustment_table)){
      user_input_no_adjustment_methods <- user_input_adjustment_table %>% filter(include_adjustment == "No") %>% pull(method_overview)

      if(length(user_input_no_adjustment_methods) > 0 ){
        users_adj_df <- users_adj_df %>% mutate(inv_adj_factor = ifelse(method_overview %in% user_input_no_adjustment_methods, 1, inv_adj_factor))
        users_adj_df_fixed <- users_adj_df_fixed %>% mutate(fixed_inv_adj_factor = ifelse(method_overview %in% user_input_no_adjustment_methods, 1, fixed_inv_adj_factor))
      }
  }
    users_inc_private_sector_df <- users_adj_df %>%
      mutate(inv_adj_factor = ifelse(is.na(inv_adj_factor), 1, inv_adj_factor)) %>%
      mutate(total_users = current_users*inv_adj_factor) %>% filter(total_users > 0)

    users_inc_private_sector_df_fixed <- users_adj_df_fixed %>%
      mutate(fixed_inv_adj_factor = ifelse(is.na(fixed_inv_adj_factor), 1, fixed_inv_adj_factor)) %>%
      mutate(total_users = current_users*fixed_inv_adj_factor) %>% filter(total_users > 0)

  }

  # ----------------------------------------------------------------------------
  return(list(users_incl_private = users_inc_private_sector_df,
              user_incl_private_fixed = users_inc_private_sector_df_fixed,
              inverse_adjustment_table = inverse_adjustment_table,
              commodities_table = commodities_table,
              supply_share_mod_res = supply_share_mod_res,
              FP_source_data_samples = FP_source_data_samples))
}
