#' Plotting EMU data at the level and rate of change.
#' @name plot_emu_data
#' @param emu_data A table of EMU data for one chosen type
#' @param mcpr_data A table of mCPR run data
#' @return A plot of EMU data
#' @import ggplot2
#' @import patchwork
#' @export
plot_emu_data <- function(emu_data, mcpr_data){

  language <- mcpr_data %>% pull(country_language) %>% unique()
  mcpr_data <- mcpr_data %>% filter(mcpr > 0)

  country_name <- emu_data %>% pull(name) %>% unique()
  data_type <- emu_data %>% pull(ss_type) %>% unique()
  region_name <- emu_data %>% pull(Region) %>% unique()

  emu_years <- emu_data %>% pull(year)

  if(length(region_name) == 0 | is.na(region_name)){
    plot_title <- paste0(country_name, " (", data_type, ")")
  }
  else{
    plot_title <- paste0(region_name, " (", data_type, ")")
  }


  if(nrow(mcpr_data) == 0){

    # no mcpr data to pull from, will have to come back to a fix for this
    language <- "English"

    if(language == "English"){
      emu_plot_caption <- "Left visual shows EMU with uncertainty (shown using standard deviation error bars). \n The uncertainty associated with EMU is due to the uncertainty \nassociated with the private sector adjustment factor in the SS-to-EMU calculation process."
      delta_plot_caption <-"Right visual shows rates of change in EMU with uncertainty (shown using standard deviation error bars).\n Rate of change refers to the annual difference between observations. \nFor example, the EMU rate of change in 2023 is the difference between the 2023 EMU and 2022 EMU."
      emu_plot_title <- "EMU with uncertainty over time"
      delta_plot_title <- "Rate of change over time"
      x_axis <- "Year"
      y_axis <- "EMU"
      y_axis_delta <- "Rate of change"
      mcpr_desc <- "mCPR (FPET2)"
      emu_w_uncertainty <- "EMU with uncertainty\n(standard deviation bars)"
      colour_type <- "Data type"
      emu_colour_data <- "Rate of change in EMU\n(with standard deviation bars)"

    }
    else {
      emu_plot_caption <- "Le visuel de gauche montre l'EMU avec incertitude au fil du temps.  L’incertitude liée à l’UEM est due à l’incertitude associée à l’ajustement du secteur privé dans le processus de calcul d'outil SS to EMU."
      delta_plot_caption <- "Le visuel de droite montre les taux de changement de l’EMU avec l’incertitude au fil du temps.  Le taux de changement fait référence à la différence annuelle entre les observations.  Par exemple, le taux de variation de l’EMU en 2023 est la différence entre l’EMU de 2023 et l’EMU de 2022."
      emu_plot_title <- "L'EMU avec incertitude au fil du temps"
      delta_plot_title <- "Les taux de changement au fil du temps"
      x_axis <- "An"
      y_axis <- "EMU"
      y_axis_delta <- "Taux de changement"
      mcpr_desc <- "TPCm (FPET2)"
      emu_w_uncertainty <- "EMU avec incertitude"
      colour_type <- "Type de données"
      emu_colour_delta <- "Taux de changement de l'EMU\navec incertitude"
      }

    # plotting EMU data
    emu_plot <- ggplot() +
      geom_point(emu_data, mapping = aes(year, emu, colour = emu_w_uncertainty)) +
      geom_errorbar(emu_data, mapping = aes(x = year, ymin = emu - sd_emu, ymax = emu + sd_emu, colour = emu_w_uncertainty), width = 0.2) +
      theme_bw() +
      labs(x = x_axis, y = y_axis, colour = colour_type, linetype = " ", caption = emu_plot_caption) +
      ggtitle(emu_plot_title) +
      theme(legend.position = "top")

    # plotting delta EMU data
    delta_emu_plot <- ggplot() +
      geom_point(emu_data, mapping = aes(year, emu_roc, colour = emu_colour_data)) +
      geom_errorbar(emu_data, mapping = aes(x = year, ymin = emu_roc - sd_emu_roc, ymax = emu_roc + sd_emu_roc, colour = emu_colour_data), width = 0.2) +
      theme_bw() +
      labs(x = x_axis, colour = colour_type, y = y_axis_delta, linetype = " ", shape = " ", caption = delta_plot_caption) +
      ggtitle(delta_plot_title) +
      theme(legend.position = "top")

  }

  else {

    if(language == "English"){
      emu_plot_caption <- "Left visual shows EMU with uncertainty and FPET2 mCPR over time. EMU uncertainty is due to the\nuncertainty associated with the private sector adjustment in the SS-to-EMU calculation process."
      delta_plot_caption <-"Right visual shows rates of change in EMU with uncertainty and rates of change in FPET2 mCPR\nover time. Rate of change refers to the annual difference between observations. For example, the\nEMU rate of change in 2023 is the difference between the 2023 EMU and 2022 EMU."
      emu_plot_title <- "EMU with uncertainty over time"
      delta_plot_title <- "Rate of change over time"
      x_axis <- "Year"
      y_axis <- "EMU"
      y_axis_delta <- "Rate of change"
      colour_type <- "Data type"
      mcpr_desc <- "mCPR (FPET2)"
      emu_w_uncertainty <- "EMU with uncertainty\n(standard deviation bars)"
      emu_colour_delta <- "Rate of change in EMU\nwith uncertainty"
      mcpr_colour_delta <- "Rate of change in mCPR (FPET2)"

    }
    else {
      emu_plot_caption <- "Le visuel de gauche montre l'EMU avec incertitude et le TPCm dans FPET2 au fil du temps.\nL'incertitude liée à l'EMU est due à l'incertitude associée à l'ajustement du\nsecteur privé dans le processus de calcul d'outil SS to EMU."
      delta_plot_caption <- "Le visuel de droite montre les taux de changement de l'EMU avec l'incertitude et les taux de changement\nde TPCm dans FPET2 au fil du temps. Le taux de changement fait référence à la différence annuelle\nentre les observations. Par exemple, le taux de variation de l'EMU en 2023 est la différence entre\nl'EMU de 2023 et l'EMU de 2022."
      emu_plot_title <- "L'EMU avec incertitude et le TPCm au fil du temps"
      delta_plot_title <- "Les taux de changement au fil du temps"
      x_axis <- "An"
      y_axis <- "EMU"
      colour_type <- "Type de données"
      y_axis_delta <- "Taux de changement"
      mcpr_desc <- "TPCm (FPET2)"
      emu_w_uncertainty <- "EMU avec incertitude"
      emu_colour_delta <- "Taux de changement de l'EMU\navec incertitude"
      mcpr_colour_delta <- "Taux de changement de TPCm (FPET2)"

    }
    mcpr_plot_data <- mcpr_data %>% filter(year %in% emu_years) %>% mutate(mcpr_roc = mcpr - lag(mcpr))


    # plotting EMU data
    emu_plot <- ggplot() +
      geom_point(emu_data, mapping = aes(year, emu, colour = emu_w_uncertainty)) +
      geom_line(mcpr_plot_data, mapping = aes(year, mcpr, linetype = mcpr_desc, colour = mcpr_desc)) +
      geom_point(mcpr_plot_data, mapping = aes(year, mcpr, colour = mcpr_desc)) +
      geom_errorbar(emu_data, mapping = aes(x = year, ymin = emu - sd_emu, ymax = emu + sd_emu, colour = emu_w_uncertainty), width = 0.2) +
      theme_bw() +
      labs(x = x_axis, y = y_axis, colour = " ", linetype = " ", caption = emu_plot_caption) +
      ggtitle(emu_plot_title) +
      theme(legend.position = "top") +
      guides(linetype = "none")

    # plotting delta EMU data
    delta_emu_plot <- ggplot() +
      geom_point(emu_data, mapping = aes(year, emu_roc, colour = emu_colour_delta)) +
      geom_errorbar(emu_data, mapping = aes(x = year, ymin = emu_roc - sd_emu_roc, ymax = emu_roc + sd_emu_roc, colour = emu_colour_delta), width = 0.2) +
      geom_line(mcpr_plot_data, mapping = aes(year, mcpr_roc, linetype = mcpr_colour_delta, colour = mcpr_colour_delta)) +
      geom_point(mcpr_plot_data, mapping = aes(year, mcpr_roc, colour = mcpr_colour_delta)) +
      theme_bw() + labs(x = x_axis, colour = " ", y = y_axis_delta, linetype = " ", shape = " ", caption = delta_plot_caption)  +
      ggtitle(delta_plot_title) +
      theme(legend.position = "top") +
      guides(linetype = "none")

  }
  # combining plots horizontally
  combined_plots <- emu_plot + delta_emu_plot + plot_annotation(title = plot_title)


  return(combined_plots)
}
