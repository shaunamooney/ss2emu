path <- get_example_data_path()
input_type <- "clients"

tools_data <- get_tools_info(path, input_type)

emu_dataset <- country_ss_to_emu(tools_data, input_type)

emu_plot <- plot_emu_data(emu_dataset, mcpr_data = tools_data$fpet_mcpr_data)

print(tools_data$ss_quantity_data)
