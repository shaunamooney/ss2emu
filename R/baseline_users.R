#' Calculating the Baseline Users
#' @name baseline_users
#' @param commodities_table A table of commodities for your chosen service statistic type
#' @param service_stats_type The type of service statistics chosen.
#' @param scale_up_table Recoded table using scale up actions of service statistics.
#' @param method_contination_rates Method continutaion rates for long term methods.
#' @return  A table calculating the baseline users for the years prior to those included in the data.
baseline_users <- function(commodities_table, service_stats_type, scale_up_table, method_contination_rates) {

  years_included <- colnames(commodities_table %>% dplyr::select(!c("method_type", "method_overview", "method_detail", "cyp_factor", "cyp_factor_adjusted", "units")))
  commodities_table <- commodities_table %>% dplyr::select(!c("method_type", "method_overview", "cyp_factor", "cyp_factor_adjusted", "units"))

  baseline_table <- tibble(
    method_overview = c("Female Sterilization", "Male Sterilization", rep("IUD", 2), rep("Implant", 3)),
    method_detail = c("Tubal Ligation (F)", "Vasectomy (M)", "Copper- T 380-A IUD", "LNG-IUS", "Implanon", "Jadelle", "Sino-Implant")
  )
  for (i in years_included) {
    baseline_table[, ncol(baseline_table) + 1] <- rep(as.numeric(NA), nrow(baseline_table))
    names(baseline_table)[ncol(baseline_table)] <- i
  } # Setting up blank table for years and methods

  scale_up_table_ss <- scale_up_table %>%
    dplyr::filter(ss_type == service_stats_type) # multiplier from service stats

  for (i in 1:nrow(baseline_table)) {
    counter <- 3
    for (j in years_included) {

      method <- baseline_table$method_detail[i]
      if(nrow(scale_up_table_ss) > 0){
        scale_up_rate <- scale_up_table_ss %>% filter(method_detail == method) %>% pull(Action_recode) %>% as.numeric()
      }
      else{
        scale_up_rate <- NA
      }
      counter <- counter + 1 # columns of rates to use
      comm_adj_data <- commodities_table[which(commodities_table$method_detail == baseline_table$method_detail[i]), ]
      method_cont_rates <- method_contination_rates[which(method_contination_rates$method_detail == baseline_table$method_detail[i]), ]
      if(counter + 1 <= ncol(method_cont_rates)) {

        baseline_table[i, j] <- as.numeric(comm_adj_data[1, 2]) * sum(as.numeric(method_cont_rates[, c(counter:ncol(method_cont_rates))]), na.rm = TRUE) * scale_up_rate
      } else {
        baseline_table[i, j] <- as.numeric(comm_adj_data[1, 2]) * 0 * scale_up_rate
      }
    }
  }

  return(baseline_table)
}
