#' Total Users
#' @name total_adjusted_users
#' @param adjusted_users_df A CYP table of adjusted contraceptive users for each year for a given service statistic type - calculated with samples.
#' @param fixed_adjusted_users_df A CYP table of adjusted contraceptive users for each year for a given service statistic type - calculated using fixed method.
#' @param incl_condoms Include condoms in totals or not (TRUE/FALSE)
#' @param method_summary Summarise by method or not (TRUE/FALSE)
#' @return Returns a table of adjusted users that is totalled for LAPM and STM methods.
total_adjusted_users <- function(adjusted_users_df, fixed_adjusted_users_df, include_condoms_df, method_summary) {

  # remove condoms users from dataset
  if(include_condoms_df$include_exclude_condoms == "Exclude Condoms"){
    adjusted_users_df <- adjusted_users_df %>% filter(!(method_detail %in% c("Male Condom", "Female Condom")))
    fixed_adjusted_users_df <- fixed_adjusted_users_df %>% filter(!(method_detail %in% c("Male Condom", "Female Condom")))
  }

  # total users for each method for samples and fixed estimates
  if(method_summary == TRUE){
    total_emu_df <- adjusted_users_df %>%
      group_by(sample_id, method_detail, year) %>%
      summarize(estimated_users = sum(total_users))

    fixed_total_emu_df <- fixed_adjusted_users_df %>%
      group_by(method_detail, year) %>%
      summarize(estimated_users = sum(total_users))
  }

  # total overall emu for samples and fixed dataset
  else{
    total_emu_df <- adjusted_users_df %>%
    group_by(sample_id, year) %>%
    summarize(estimated_users = sum(total_users))

    fixed_total_emu_df <- fixed_adjusted_users_df %>%
      group_by(year) %>%
      summarize(estimated_users = sum(total_users))}

  return(list(total_emu_df = total_emu_df, fixed_total_emu_df = fixed_total_emu_df))
}
