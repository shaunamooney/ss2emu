#' Running subnational shinyss2emu for subnational levels. Saves a CSV file and a pdf in the specified directory.
#' @name run_subnational
#' @param subnational_tools_filepath The filepath in which all individual subnational SS-to-EMU tools are located.
#' @param input_type The service statistics type to use.
#' @return a list containing an emu database and a list of plots.
#' @import R2jags
#' @export
#'

run_subnational <- function(subnational_tools_filepath, input_type){

  tools_info <- list()
  emu_output <- list()
  emu_dataset <- list()
  mcpr <- list()
  plot <- list()

  region_files <- list.files(subnational_tools_filepath, pattern = "\\.xlsx$|\\.xlsm$")
  for (i in 1:length(region_files)) {
    print(region_files[i])
    tools_info[[i]] <- get_tools_info(country_file_path = paste0(subnational_tools_filepath, "/",region_files[i]), input_type)
    emu_output[[i]] <- country_ss_to_emu(tools_info[[i]], input_type, method_summary = FALSE)

    emu_dataset[[i]] <- emu_output[[i]]

    mcpr[[i]] <- tools_info[[i]]$fpet_mcpr_data

    plot[[i]] <- plot_emu_data(emu_dataset[[i]], mcpr[[i]])
  }

  all_emu_output <- data.table::rbindlist(emu_dataset) %>% as_tibble() %>%
    mutate(pop_type=case_when(pop_type=="MW" ~ "Married women",
                              pop_type=="AW" ~ "All women")) %>%
    mutate(Include=1) %>%
    select(division_numeric_code,	name,	ss_type,	year,	pop_type,	emu,	sd_emu,	emu_roc,	sd_emu_roc, Include, Region) %>%
    rename("ISO code"=division_numeric_code,	"Country" = name,	"SS type"= ss_type,	"Year" = year,	"PopType" = pop_type,
           EMU = emu,	SD_EMU = sd_emu,	EMU_ROC = emu_roc, SD_EMU_ROC =	sd_emu_roc)

  country_name <- all_emu_output %>% pull(Country) %>% unique()

  write.csv(all_emu_output, paste0(subnational_tools_filepath, "/", country_name, "_subnational_data.csv"), row.names = F, na="")

  output_pdf_path <- paste0(subnational_tools_filepath, "/", country_name, "_subnational_plots.pdf")

  # Open a PDF device
  pdf(output_pdf_path, width = 12, height = 8)
  # Loop through each combined plot and print to the PDF
  for (combined_mod_plot in plot) {
    print(combined_mod_plot)
    # Add a page break after each plot
    cat("\f")  # This is a page break for PDF devices
  }

  # Close the PDF device
  dev.off()

  return(list(all_emu_output, plot))

}




