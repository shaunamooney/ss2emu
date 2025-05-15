#' @name get_tools_info
#' @title Get tools information from SS-to-EMU Excel spreadsheet for the ss2emu shiny app
#' @description This function reads an Excel spreadsheet containing tools information
#'   and returns the relevant information.
#' @param country_file_path Location of the country's SS-to-EMU Excel spreadsheet
#' @param input_type Chosen service statistics type
#' @import tidyverse
#' @import dplyr
#' @import readxl
#' @import readr
#' @import tidyr
#' @export


get_tools_info <- function(country_file_path, input_type){


  # Pop_Prev tab
  # Read the sheet names from the Excel file
  sheet_names <- readxl::excel_sheets(country_file_path)

  # Define initial sheet names of interest
  country_sheet <- "1. Country_Language Set Up"
  pop_sheet <- "2. Pop_Prev Set Up"

  if ("EMU Output" %in% sheet_names){
    emu_output_sheet <- "EMU Output"
  }
  else{
    emu_output_sheet <- "\"EMU\" Output"
  }

  ss_setup_sheet <- "3. ServiceStats Set Up"

  # Read data from "1. Country_Language Set Up" sheet
  country_sheet_data <- readxl::read_excel(country_file_path, sheet = country_sheet)
  important_info_column <- country_sheet_data %>% tidyr::drop_na(...3) %>% dplyr::pull(...3)
  df <- data.frame(important_info_column)


  # Filter out numbers greater than zero using dplyr
  years_info <- df %>%
    dplyr::filter(as.numeric(important_info_column) > 0)

  first_year_of_data <- years_info %>% dplyr::pull(important_info_column) %>% min()
  last_year_of_data <- years_info %>% dplyr::pull(important_info_column) %>% max()


  # Create a tibble using the information from the previous step
  set_up_table <- setup_data <- tibble(
    language = important_info_column[1] %>% as.character(),
    Country = important_info_column[2] %>% as.character(),
    National_or_Subnational = important_info_column[3] %>% as.character(),
    AW_or_MW = important_info_column[4] %>% as.character(),
    Most_recent_year_of_data = last_year_of_data,
    First_year_of_data = first_year_of_data
  ) %>% dplyr::mutate(AW_or_MW = ifelse(AW_or_MW == "All Women", "AW",
                                 ifelse(AW_or_MW == "Toutes les Femmes", "AW", "MW")))

  if (set_up_table$language == "English") {
    pop_sheet_data <- readxl::read_excel(country_file_path, sheet = pop_sheet)
    pop_col_index <- which(colnames(pop_sheet_data) == "Set up: Enter Background Data - Population and Prevalence") + 1
    pop_data <- pop_sheet_data[,pop_col_index]
    pop_dataset <- pop_sheet_data %>% dplyr::mutate(year = as.numeric(`Set up: Enter Background Data - Population and Prevalence`))
    pop_dataset$population <- pop_data
    pop_dataset <- pop_dataset %>%
      tidyr::drop_na(year) %>% dplyr::select(year, population) %>% dplyr::mutate(population = as.numeric(unlist(population))) %>% dplyr::mutate(pop_type = set_up_table$AW_or_MW)

    mcpr_mw <- pop_sheet_data %>% tidyr::drop_na(...11) %>% dplyr::pull(...11) %>% as.numeric() %>% .[complete.cases(.)]
    mcpr_aw <- pop_sheet_data %>% tidyr::drop_na(...12) %>% dplyr::pull(...12) %>% as.numeric() %>% .[complete.cases(.)]

    if(set_up_table$AW_or_MW == "AW")
    {
      fpet_mcpr_data <- tibble(mcpr = mcpr_aw) %>%
        mutate(year = pop_dataset$year )
    }
    else {
      fpet_mcpr_data <- tibble(mcpr = mcpr_mw) %>%
        mutate(year = pop_dataset$year)
    }
  } else if (set_up_table$language == "Francais") {
    pop_sheet_data <- readxl::read_excel(country_file_path, sheet = pop_sheet)
    pop_col_index <- which(colnames(pop_sheet_data) == "Configuration : Entrez les Données Générales") + 1
    pop_data <- pop_sheet_data[,pop_col_index]
    pop_dataset <- pop_sheet_data %>% mutate(year = as.numeric(`Configuration : Entrez les Données Générales`))
    pop_dataset$population <- pop_data
    pop_dataset <- pop_dataset %>%
      tidyr::drop_na(year) %>%
      dplyr::select(year, population) %>%
      dplyr::mutate(population = as.numeric(unlist(population))) %>% dplyr::mutate(pop_type = set_up_table$AW_or_MW)

    mcpr_mw <- pop_sheet_data %>% tidyr::drop_na(...11) %>% dplyr::pull(...11) %>% as.numeric() %>% .[complete.cases(.)]
    mcpr_aw <- pop_sheet_data %>% tidyr::drop_na(...12) %>% dplyr::pull(...12) %>% as.numeric() %>% .[complete.cases(.)]

    if(set_up_table$AW_or_MW == "AW")
    {
      fpet_mcpr_data <- tibble(mcpr = mcpr_aw) %>%
        mutate(year = pop_dataset$year)
    }
    else {
      fpet_mcpr_data <- tibble(mcpr = mcpr_mw) %>%
        mutate(year = pop_dataset$year)
    }
  }
  country_name <- set_up_table$Country
  language <- set_up_table$language

  if(setup_data$National_or_Subnational != "National"){
    setup_data <- setup_data %>% mutate(Region = country_sheet_data[7,7] %>% as.character())
  }

  # Service stats tab scaling step
  ss_setup_sheet_data <- readxl::read_excel(country_file_path, sheet = ss_setup_sheet) %>% as.matrix()
  test_sheet3 <- ss_setup_sheet_data
  # Scaling Up

  # Pattern to be matched
  pat_en <- "Long-Acting Methods : Introduced or Scaled-Up?"
  pat_fr <- "Méthodes à long durée: introduites ou mis à l'echelle?"
  find_test_sheet3_en <-as.data.frame(as.matrix(str_detect(test_sheet3, pat_en) )) %>%
    tibble::rowid_to_column()
  find_test_sheet3_fr <-as.data.frame(as.matrix(str_detect(test_sheet3, pat_fr) )) %>%
    tibble::rowid_to_column()
  find_test_sheet3 <- bind_rows(find_test_sheet3_en, find_test_sheet3_fr) %>% dplyr::filter(V1=="TRUE")

  first_cell <- min(find_test_sheet3$rowid)
  rows <- nrow(test_sheet3)
  cols <- ncol(test_sheet3)

  col_1 <-  (first_cell %/%  rows ) + 1
  col_last <- col_1 + 9

  row_1 <- first_cell %%  rows
  row_last <-    row_1 + 22

  sheet3_clean.scale <- data.frame(test_sheet3[row_1:row_last, col_1:col_last])
  sheet3_clean.scale <- sheet3_clean.scale[-1:-7, -1] %>%
    dplyr::select(1,3,6,9) %>%
    dplyr::rename(Method=1, Clients=2, Facilities=3, Visits=4) %>%
    dplyr::mutate( Country=country_name) %>%
    dplyr::filter(!is.na(Method)) %>% dplyr::rename(method_detail = Method) %>% dplyr::slice_tail(n = 7)

  # fix method names
  sheet3_clean.scale <- sheet3_clean.scale %>% dplyr::mutate(method_detail = ifelse(grepl("ligat", method_detail, ignore.case = TRUE), "Tubal Ligation (F)",
                                                                             ifelse(grepl("vasect", method_detail, ignore.case = TRUE), "Vasectomy (M)",
                                                                                    ifelse(grepl("copper", method_detail, ignore.case = TRUE), "Copper- T 380-A IUD",
                                                                                           ifelse(grepl("lng", method_detail, ignore.case = TRUE), "LNG-IUS",
                                                                                                  ifelse(grepl("implanon", method_detail, ignore.case = TRUE), "Implanon",
                                                                                                         ifelse(grepl("sino", method_detail, ignore.case = TRUE), "Sino-Implant",
                                                                                                                ifelse(grepl("jadelle", method_detail, ignore.case = TRUE), "Jadelle",
                                                                                                                       ifelse(grepl("4 Year implant", method_detail, ignore.case = TRUE), "Sino-Implant", method_detail)))))))))

  ster_methods <- c("Tubal Ligation (F)", "Vasectomy (M)")
  iud_methods <- c("Copper- T 380-A IUD", "LNG-IUS")
  implant_methods <- c("Implanon", "Jadelle", "Sino-Implant")

  # add method overview col
  sheet3_clean.scale <- sheet3_clean.scale %>% dplyr::mutate(method_overview = ifelse(method_detail %in% ster_methods, "Sterilization",
                                                                               ifelse(method_detail %in% iud_methods, "IUD",
                                                                                      ifelse(method_detail %in% implant_methods, "Implant", NA))))


  recode_scaleup_table <- sheet3_clean.scale  %>%
    tidyr::pivot_longer(cols = c(Clients, Facilities, Visits),
                 names_to = "SS_type",
                 values_to = "Action_recode") %>% dplyr::mutate(SS_type = ifelse(SS_type == "Clients", "Contraceptive commodities distributed to clients",
                                                                          ifelse(SS_type == "Facilities", "Contraceptive commodities distributed to facilities",
                                                                                 ifelse(SS_type == "Visits", "FP visits", SS_type))))




  # FPsource - Sectors Reporting ---------------------------------------------
  test_sheet4 <- as.matrix(read_excel(country_file_path, sheet = "4. FPSource Set Up"))

  user_input_adjustment_table <- tibble(method_overview = test_sheet4[168:189,3], include_adjustment = test_sheet4[168:189,8]) %>% drop_na(method_overview) %>% distinct()


  # Pattern to be matched
  pat_en <- "Method"
  pat_fr <- "Method"
  find_test_sheet4_en <-as.data.frame(as.matrix(str_detect(test_sheet4, pat_en) )) %>%
    tibble::rowid_to_column()
  find_test_sheet4_fr <-as.data.frame(as.matrix(str_detect(test_sheet4, pat_fr) )) %>%
    tibble::rowid_to_column()
  find_test_sheet4 <- bind_rows(find_test_sheet4_en, find_test_sheet4_fr) %>% dplyr::filter(V1=="TRUE")

  first_cell <- min(find_test_sheet4$rowid)
  rows <- nrow(test_sheet4)
  cols <- ncol(test_sheet4)

  col_1 <-  (first_cell %/%  rows ) + 1
  col_last <- col_1 + 8

  row_1 <- first_cell %%  rows
  row_last <- row_1 + 23

  sheet4_surveysource_clean <- data.frame(test_sheet4[row_1:row_last, col_1:col_last]) %>%
    rename(Method=2, Type=3, PublicSector=4, NGO=5, PrivateHospital=6, Pharmacy=7, StoreChurchFriend=8, Other=9) %>%
    select(Method, Type, PublicSector, NGO, PrivateHospital, Pharmacy, StoreChurchFriend, Other) %>%
    filter(!is.na(Method)) %>%
    mutate(PublicSector=as.numeric(PublicSector), NGO=as.numeric(NGO), PrivateHospital=as.numeric(PrivateHospital),
           Pharmacy=as.numeric(Pharmacy), StoreChurchFriend=as.numeric(StoreChurchFriend), Other=as.numeric(Other))  %>%
    mutate(Country=country_name)

  # # # # # # # # # # # # # # # # # # # # # #
  #Pattern to be matched
  pat_en <- "Step 2 of 3: What sectors are reporting in your data?"
  pat_fr <- "Étape 2 de 3: Quels secteurs rapportent dans votre système?"
  find_test_sheet4_en <-as.data.frame(as.matrix(str_detect(test_sheet4, pat_en) )) %>%
    tibble::rowid_to_column()
  find_test_sheet4_fr <-as.data.frame(as.matrix(str_detect(test_sheet4, pat_fr) )) %>%
    tibble::rowid_to_column()
  find_test_sheet4 <- bind_rows(find_test_sheet4_en, find_test_sheet4_fr) %>% filter(V1=="TRUE")

  first_cell <- min(find_test_sheet4$rowid)
  rows <- nrow(test_sheet4)
  cols <- ncol(test_sheet4)

  col_1 <-  (first_cell %/%  rows ) + 1
  col_last <- col_1 + 8

  row_1 <- first_cell %%  rows
  row_last <-    row_1 + 13

  sheet4_reporting_clean <- data.frame(test_sheet4[row_1:row_last, col_1:col_last]) %>%
    rename( Type=1, PublicSector=4, NGO=5, PrivateHospital=6, Pharmacy=7, StoreChurchFriend=8, Other=9) %>%
    select(Type, PublicSector, NGO, PrivateHospital, Pharmacy, StoreChurchFriend, Other) %>%
    filter(PublicSector=="1" | PublicSector=="0") %>%
    mutate(PublicSector=as.numeric(PublicSector), NGO=as.numeric(NGO), PrivateHospital=as.numeric(PrivateHospital),
           Pharmacy=as.numeric(Pharmacy), StoreChurchFriend=as.numeric(StoreChurchFriend), Other=as.numeric(Other))  %>%
    mutate(Country=country_name) %>% rename(ss_type = Type)


  sheet4_reporting_clean <- sheet4_reporting_clean %>% tidyr::pivot_longer(cols = c(PublicSector, NGO, PrivateHospital, Pharmacy, StoreChurchFriend, Other),
                                                                    names_to = "Sector",
                                                                    values_to = "Presence_recode")

  # fixing sector names
  sheet4_reporting_clean <- sheet4_reporting_clean %>% mutate(Sector = ifelse(Sector == "PublicSector", "Government Health Facilities and Home/Community Delivery",
                                                                              ifelse(Sector == "PrivateHospital", "Private Hospital/ Clinic Delivery",
                                                                                     ifelse(Sector == "StoreChurchFriend", "Shop/ Church/ Friend", Sector))))
  public_sector <- "Government Health Facilities and Home/Community Delivery"
  private_sector <- c("NGO", "Private Hospital/ Clinic Delivery", "Pharmacy")

  # add sector overview
  sheet4_reporting_clean <- sheet4_reporting_clean %>% mutate(Sector_Overview = ifelse(Sector %in% public_sector, "Public",
                                                                                       ifelse(Sector %in% private_sector, "Private", "Other")))

  sheet4_reporting_clean <- sheet4_reporting_clean %>% mutate(ss_type = ifelse(grepl("Utilisatrices", ss_type, ignore.case = TRUE), "FP users",
                                                                                   ifelse(grepl("Utilisateurs", ss_type, ignore.case = TRUE), "FP users",
                                                                                          ifelse(grepl("clients", ss_type, ignore.case = TRUE), "Contraceptive commodities distributed to clients",
                                                                                                 ifelse(grepl("visites", ss_type, ignore.case = TRUE), "FP visits",
                                                                                                        ifelse(grepl("visits", ss_type, ignore.case = TRUE), "FP visits",
                                                                                                               ifelse(grepl("établissements", ss_type, ignore.case = TRUE), "Contraceptive commodities distributed to facilities",
                                                                                                                      ifelse(grepl("facilities", ss_type, ignore.case = TRUE), "Contraceptive commodities distributed to facilities",
                                                                                                                             ifelse(grepl("users", ss_type, ignore.case = TRUE), "FP users", ss_type)))))))))

  sectors_reporting <- test_sheet4[45:48, 1:3] %>% as.data.frame() %>% select(1,3)
  colnames(sectors_reporting) <- c("ss_type", "Sectors Reporting")
  sectors_reporting <- sectors_reporting %>% mutate(ss_type = ifelse(grepl("Utilisatrices", ss_type, ignore.case = TRUE), "FP users",
                                                                         ifelse(grepl("Utilisateurs", ss_type, ignore.case = TRUE), "FP users",
                                                                                ifelse(grepl("clients", ss_type, ignore.case = TRUE), "Contraceptive commodities distributed to clients",
                                                                                       ifelse(grepl("visites", ss_type, ignore.case = TRUE), "FP visits",
                                                                                              ifelse(grepl("visits", ss_type, ignore.case = TRUE), "FP visits",
                                                                                                     ifelse(grepl("établissements", ss_type, ignore.case = TRUE), "Contraceptive commodities distributed to facilities",
                                                                                                            ifelse(grepl("facilities", ss_type, ignore.case = TRUE), "Contraceptive commodities distributed to facilities",
                                                                                                                   ifelse(grepl("users", ss_type, ignore.case = TRUE), "FP users", ss_type)))))))))

  recode_sectors_reporting <- left_join(sheet4_reporting_clean, sectors_reporting, by = "ss_type") %>% mutate(Presence = ifelse(Presence_recode == 1, "Yes",
                                                                                                                                  ifelse(Presence_recode == 0.5, "Partially",
                                                                                                                                         ifelse(Presence_recode == "0", "No", NA))))






  # FPsource step 3

  # Reporting rates ----------------------------------------------------------

  emu_output_sheet_data <- readxl::read_excel(country_file_path, sheet = emu_output_sheet)

  # Find the location of the "Reporting Rates" in the 'emu_sheet_output_data'
  reporting_rate_name <- ifelse(language == "English", "Reporting Rates", "Taux de complétude")
  reporting_rate_location <- which(emu_output_sheet_data == reporting_rate_name, arr.ind = TRUE) %>%
    as_tibble() %>%
    filter(col %in% c(1, 2))
  rr_row_index <- reporting_rate_location$row + 1
  rr_col_index <- reporting_rate_location$col

  # Put "Year" below the cell with reporting rates in it
  emu_output_sheet_data[rr_row_index, rr_col_index] <- "Year"

  # Find the first NA value to the right
  rr_first_na_col <- (which(is.na(emu_output_sheet_data[rr_row_index, (rr_col_index + 1):ncol(emu_output_sheet_data)])) + rr_col_index) %>% min()

  # Find the first NA value downwards
  rr_first_na_row <- (which(is.na(emu_output_sheet_data[(rr_row_index + 1):nrow(emu_output_sheet_data), rr_col_index])) + rr_row_index) %>% min()

  rr_final_col_index <- rr_first_na_col - 1
  rr_final_row_index <- rr_first_na_row - 1

  # Extract the desired section of the dataframe - reporting rate table
  reporting_rate_dataset <- emu_output_sheet_data[rr_row_index:rr_final_row_index, rr_col_index:rr_final_col_index]

  # Set the first row as column names
  colnames(reporting_rate_dataset) <- reporting_rate_dataset[1, ]

  # Remove the first row (it's now the column names)
  reporting_rate_dataset <- reporting_rate_dataset[-1, ]

  # pivot longer for joining with other datasets with ease - also translate some ss_type cols
  reporting_rate_dataset_long <- reporting_rate_dataset %>%
    #mutate(name = country_name) %>%
    tidyr::pivot_longer(
      cols = starts_with(paste0(reporting_rate_name, " : ")),
      names_to = "ss_type",
      values_to = "reporting_rate") %>%
    #drop_na(reporting_rate) %>%
    #filter(reporting_rate > 0) %>%
    mutate(year = as.numeric(Year)) %>% select(year, ss_type, reporting_rate)  %>%
    mutate(ss_type = ifelse(grepl("Utilisatrices", ss_type, ignore.case = TRUE), "FP users",
                            ifelse(grepl("Utilisateurs", ss_type, ignore.case = TRUE), "FP users",
                                   ifelse(grepl("clients", ss_type, ignore.case = TRUE), "Contraceptive commodities distributed to clients",
                                          ifelse(grepl("visites", ss_type, ignore.case = TRUE), "FP visits",
                                                 ifelse(grepl("établissements", ss_type, ignore.case = TRUE), "Contraceptive commodities distributed to facilities",
                                                        ifelse(grepl("facilities", ss_type, ignore.case = TRUE), "Contraceptive commodities distributed to facilities",
                                                               ifelse(grepl("users", ss_type, ignore.case = TRUE), "FP users",
                                                                      ifelse(grepl("visits", ss_type, ignore.case = TRUE), "FP visits",ss_type)))))))))


  reporting_rates_data <- reporting_rate_dataset_long %>%
    tidyr::pivot_wider(id_cols = ss_type, names_from = year, values_from = reporting_rate) %>% rename(ss_type = ss_type)


  # SS type input data

  # Service statistics info data
  # Tab 3, 3 separate sections to pull
  test_sheet3 <- as.matrix(read_excel(country_file_path, sheet = "3. ServiceStats Set Up"))

  # Clients
  #Pattern to be matched
  pat_en <- "Do you have data on contraceptive commodities?"
  pat_fr <- "Produits de planification familiale distribués aux clients"
  find_test_sheet3_en <-as.data.frame(as.matrix(str_detect(test_sheet3, pat_en) )) %>%
    tibble::rowid_to_column()
  find_test_sheet3_fr <-as.data.frame(as.matrix(str_detect(test_sheet3, pat_fr) )) %>%
    tibble::rowid_to_column()
  find_test_sheet3 <- bind_rows(find_test_sheet3_en, find_test_sheet3_fr) %>% filter(V1=="TRUE")

  first_cell <- min(find_test_sheet3$rowid)
  rows <- nrow(test_sheet3)
  cols <- ncol(test_sheet3)

  col_1 <-  (first_cell %/%  rows ) + 1
  col_last <- col_1 + 1

  row_1 <- first_cell %%  rows
  row_last <-    row_1 + 5

  sheet3_clean.clients <- data.frame(test_sheet3[row_1:row_last, col_1:col_last]) %>%
    rename(Question=1, Answer=2) %>%
    mutate( Type="Clients", Country=country_name)
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # Facilities
  #Pattern to be matched
  pat_en <- "Contraceptive commodities distributed to facilities"
  pat_fr <- "Produits de planification familiale distribués aux établissements"
  find_test_sheet3_en <-as.data.frame(as.matrix(str_detect(test_sheet3, pat_en) )) %>%
    tibble::rowid_to_column()
  find_test_sheet3_fr <-as.data.frame(as.matrix(str_detect(test_sheet3, pat_fr) )) %>%
    tibble::rowid_to_column()
  find_test_sheet3 <- bind_rows(find_test_sheet3_en, find_test_sheet3_fr) %>% filter(V1=="TRUE") %>%
    arrange(rowid)

  first_cell <- find_test_sheet3[2,1]
  rows <- nrow(test_sheet3)
  cols <- ncol(test_sheet3)

  col_1 <-  (first_cell %/%  rows ) +1
  col_last <- col_1 + 2

  row_1 <- first_cell %%  rows
  row_last <-    row_1 + 5

  sheet3_clean.facilities <- data.frame(test_sheet3[row_1:row_last, col_1:col_last]) %>% select(1, 3) %>%
    rename(Question=1, Answer=2) %>%
    mutate( Type="Facilities", Country=country_name)
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # Visits
  #Pattern to be matched
  pat_en <- "Do you have data on FP visits?"
  pat_fr <- "Disposez-vous de données sur les visites ?"
  find_test_sheet3_en <-as.data.frame(as.matrix(str_detect(test_sheet3, pat_en) )) %>%
    tibble::rowid_to_column()
  find_test_sheet3_fr <-as.data.frame(as.matrix(str_detect(test_sheet3, pat_fr) )) %>%
    tibble::rowid_to_column()
  find_test_sheet3 <- bind_rows(find_test_sheet3_en, find_test_sheet3_fr) %>% filter(V1=="TRUE")

  first_cell <- min(find_test_sheet3$rowid)
  rows <- nrow(test_sheet3)
  cols <- ncol(test_sheet3)

  col_1 <-  (first_cell %/%  rows ) + 1
  col_last <- col_1 + 1

  row_1 <- (first_cell %%  rows) -1
  row_last <-    row_1 + 5

  sheet3_clean.visits <- data.frame(test_sheet3[row_1:row_last, col_1:col_last])  %>%
    rename(Question=1, Answer=2) %>%
    mutate( Type="Visits", Country=country_name)
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # Users
  #Pattern to be matched
  pat_en <- "Do you have data on FP users?"
  pat_fr <- "Disposez-vous de données sur les utilisatrices ?"
  find_test_sheet3_en <-as.data.frame(as.matrix(str_detect(test_sheet3, pat_en) )) %>%
    tibble::rowid_to_column()
  find_test_sheet3_fr <-as.data.frame(as.matrix(str_detect(test_sheet3, pat_fr) )) %>%
    tibble::rowid_to_column()
  find_test_sheet3 <- bind_rows(find_test_sheet3_en, find_test_sheet3_fr) %>% filter(V1=="TRUE")

  first_cell <- min(find_test_sheet3$rowid)
  rows <- nrow(test_sheet3)
  cols <- ncol(test_sheet3)

  col_1 <-  (first_cell %/%  rows ) + 1
  col_last <- col_1 + 2

  row_1 <- first_cell %%  rows -1
  row_last <-    row_1 + 5

  sheet3_clean.users <- data.frame(test_sheet3[row_1:row_last, col_1:col_last]) %>%
    select(1,3) %>%
    rename(Question=1, Answer=2) %>%
    mutate( Type="Users", Country=country_name)

  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  sheet3_servicestats <- bind_rows(sheet3_clean.clients, sheet3_clean.facilities, sheet3_clean.visits, sheet3_clean.users) %>%
    filter(!is.na(Question)) %>% select(-Country)

  english_cols <- c("Type",
                    "Do you have data on contraceptive commodities?",
                    "Source:",
                    "First year of data available:",
                    "Most recent full year of data available:",
                    "What sectors are reporting?")

  french_cols <- c("Type",
                   "Disposez-vous de données sur les produits?",
                   "Source:",
                   "Première année de données disponible:",
                   "Plus récent complet année de données disponibles:",
                   "Quels secteurs rapportent?")

  if(language == "English"){
    subset_cols <- english_cols
  } else{
    subset_cols <- french_cols
  }


  ss_info <- sheet3_servicestats %>%
    tidyr::pivot_wider(id_cols = Type, names_from = Question, values_from = Answer) %>%
    select(all_of(subset_cols)) %>%
    rename(ss_type = 1,
           `Do you have data on contraceptive commodities?` = 2,
           `Source` = 3,
           `First year of data available` = 4,
           `Most recent full year of data available` = 5,
           `What sectors are reporting?` = 6) %>%
    mutate(`Do you have data on contraceptive commodities?` = ifelse(!is.na(`Source`), "Yes", `Do you have data on contraceptive commodities?`)) %>%
    mutate(ss_type = ifelse(grepl("clients", ss_type, ignore.case = TRUE), "Contraceptive commodities distributed to clients",
                                ifelse(grepl("facilities", ss_type, ignore.case = TRUE), "Contraceptive commodities distributed to facilities",
                                       ifelse(grepl("visits", ss_type, ignore.case = TRUE), "FP visits",
                                              ifelse(grepl("users", ss_type, ignore.case = TRUE), "FP users", ss_type)))))


  ss_info <- ss_info %>%
    mutate(data_exists = ifelse(is.na(ss_info$`Do you have data on contraceptive commodities?`) |
                                  ss_info$`Do you have data on contraceptive commodities?` %in% c("No", "Non"), "No", "Yes"
    ))

  # SS quantity data
  sheet3_servicestats_data <- setNames(data.frame(matrix(ncol = 4,  nrow = 0)), c("Question", "Answer", "Type", "Country")) %>%
    mutate(Question=as.character(Question), Answer=as.character(Answer), Type=as.character(Type), Country=as.character(country_name))

  sheet3_servicestats_data <- bind_rows(sheet3_servicestats_data, sheet3_servicestats) # not sure if this is necessary

  # Does type of service stats exist:
  service_stats_type_exist <-  sheet3_servicestats_data %>% filter(Question=="First year of data available:" | Question=="Première année de données disponible:" | Question=="Most recent full year of data available:" | Question=="Plus récent complet année de données disponibles:") %>%
    mutate(Question_Simp= case_when(Question=="First year of data available:" | Question=="Première année de données disponible:" ~ "FirstYear",
                                    Question=="Most recent full year of data available:" | Question=="Plus récent complet année de données disponibles:" ~ "LastYear")) %>%
    mutate(Type_Year=paste(Type, Question_Simp, sep="_")) %>%
    select(Type_Year, Answer) %>%
    tidyr::spread(Type_Year, Answer) %>%
    mutate(CountryName=country_name)


  clients_rr_data <- setNames(data.frame(matrix(ncol = 4,  nrow = 0)), c("Year", "reporting_rate", "Type", "Country")) %>%
    mutate( Year=as.numeric(Year), reporting_rate=as.numeric(reporting_rate), Type=as.character(Type), Country=as.character(Country))

  clients_inputs_data <- setNames(data.frame(matrix(ncol = 8,  nrow = 0)), c("Method", "CYP FACTOR", "CYP", "UNITS","Year", "Commodities", "Type", "Country")) %>%
    mutate( Method=as.character(Method),"CYP FACTOR"=as.character("CYP FACTOR"), CYP=as.character(CYP),  UNITS=as.character(UNITS), Year=as.numeric(Year), Commodities=as.numeric(Commodities), Type=as.character(Type), Country=as.character(Country))

  if(input_type == "clients"){

    sheet_data <- readxl::read_excel(country_file_path, sheet = "Commodities (Clients) Input")
    test_type <- as.matrix(read_excel(country_file_path, sheet = "Commodities (Clients) Input"))

    output_data <- readxl::read_excel(country_file_path, sheet = "Commodities (Clients) Output")
    condoms <- output_data[99,2] %>% mutate(ss_type = "Contraceptive commodities distributed to clients")

    first_year_df <- sheet3_servicestats_data %>% filter(Type=="Clients") %>% filter(Question=="First year of data available:" | Question=="Première année de données disponible:") %>%
      mutate(Answer=as.numeric(Answer))
    last_year_df <- sheet3_servicestats_data %>% filter(Type=="Clients") %>% filter(Question=="Most recent full year of data available:" | Question=="Plus récent complet année de données disponibles:") %>%
      mutate(Answer=as.numeric(Answer))

    first_year <- first_year_df[1,2]

    last_year <- last_year_df[1,2]

    rr_clean <- reporting_rate_dataset %>% select(Year, matches("(?i)clients")) %>%
      rename(reporting_rate=2) %>%
      mutate(Year=as.numeric(Year), reporting_rate=as.numeric(reporting_rate), Type="Clients")  %>%
      mutate(Country=country_name) %>% tidyr::drop_na(reporting_rate)

    # # # # # # # # # # #  # # # # # #
    #Pattern to be matched
    pat_en <- "STEP 2 of 3. ENTER COMMODITIES DATA"
    pat_fr <- "ÉTAPE 2 de 3. SAISIR LES DONNÉES DES PRODUITS"
    find_test_en <-as.data.frame(as.matrix(str_detect(test_type, pat_en) )) %>%
      tibble::rowid_to_column()
    find_test_fr <-as.data.frame(as.matrix(str_detect(test_type, pat_fr) )) %>%
      tibble::rowid_to_column()
    find_test <- bind_rows(find_test_en, find_test_fr) %>% filter(V1=="TRUE")

    first_cell <- min(find_test$rowid)
    rows <- nrow(test_type)
    cols <- ncol(test_type)

    col_1 <-  (first_cell %/%  rows ) + 2
    col_last <- col_1 + 4 + (last_year - first_year)

    row_1 <- (first_cell %%  rows) + 5
    row_last <-    row_1 + 25

    matrix_type <- test_type[row_1:row_last, col_1:col_last]
    matrix_type[1,1] <- "Method"
    matrix_type[1,3] <- "CYP"

    type_inputs_clean <- data.frame(matrix_type)
    colnames(type_inputs_clean) <- type_inputs_clean[1,]
    type_inputs_clean <- type_inputs_clean[-1, ]

    n_col <- ncol(type_inputs_clean %>% select(-matches("^NA$")))

    type_inputs_clean <- type_inputs_clean %>% select(-matches("^NA$")) %>% gather(Year, Commodities, 5:n_col) %>%
      select(-matches("^NA$")) %>%
      filter(!is.na(Method)) %>%
      mutate(Year=as.numeric(Year), Commodities=as.numeric(Commodities), Type="Clients")  %>%
      mutate(Country=country_name)


    rr_data <- bind_rows(clients_rr_data, rr_clean)
    rr_years <- rr_data %>% filter(reporting_rate >= 0.8) %>% pull(Year)
    type_inputs_data <- bind_rows(clients_inputs_data, type_inputs_clean) #%>% filter(Year %in% rr_years)
    year_acceptance_location <- which(test_type == "Year of method acceptance", arr.ind = TRUE) %>%
      as_tibble()
    type_method_continuation <- test_type[(year_acceptance_location$row + 1):(year_acceptance_location$row + 7), (year_acceptance_location$col-1):(year_acceptance_location$col+16)] %>% as_tibble() %>%
      rename(method_overview = 1, method_detail = 2) %>%
      rename_with(~paste0("Year_", seq_along(.)), .cols = -(1:2)) %>% fill(method_overview) %>% mutate(method_detail = ifelse(grepl("ligat", method_detail, ignore.case = TRUE), "Tubal Ligation (F)",
                                                                                                                              ifelse(grepl("vasect", method_detail, ignore.case = TRUE), "Vasectomy (M)",
                                                                                                                                     ifelse(grepl("copper", method_detail, ignore.case = TRUE), "Copper- T 380-A IUD",
                                                                                                                                            ifelse(grepl("lng", method_detail, ignore.case = TRUE), "LNG-IUS",
                                                                                                                                                   ifelse(grepl("implanon", method_detail, ignore.case = TRUE), "Implanon",
                                                                                                                                                          ifelse(grepl("sino", method_detail, ignore.case = TRUE), "Sino-Implant",
                                                                                                                                                                 ifelse(grepl("jadelle", method_detail, ignore.case = TRUE), "Jadelle",
                                                                                                                                                                        ifelse(grepl("4 Year implant", method_detail, ignore.case = TRUE), "Sino-Implant", method_detail)))))))))








    }

   facilities_rr_data <- setNames(data.frame(matrix(ncol = 4,  nrow = 0)), c("Year", "reporting_rate", "Type", "Country")) %>%
    mutate( Year=as.numeric(Year), reporting_rate=as.numeric(reporting_rate), Type=as.character(Type), Country=as.character(Country))

  facilities_inputs_data <- setNames(data.frame(matrix(ncol = 8,  nrow = 0)), c("Method", "CYP FACTOR", "CYP", "UNITS","Year", "Commodities", "Type", "Country")) %>%
    mutate( Method=as.character(Method),"CYP FACTOR"=as.character("CYP FACTOR"), CYP=as.character(CYP),  UNITS=as.character(UNITS), Year=as.numeric(Year), Commodities=as.numeric(Commodities), Type=as.character(Type), Country=as.character(Country))


  if(input_type == "facilities"){
    sheet_data <- readxl::read_excel(country_file_path, sheet = "Commodities (Facility) Input")
    test_type <- as.matrix(read_excel(country_file_path, sheet = "Commodities (Facility) Input"))

    output_data <- readxl::read_excel(country_file_path, sheet = "Commodities (Facility) Output")
    condoms <- output_data[99,2] %>% mutate(ss_type = "Contraceptive commodities distributed to facilities")

    first_year_df <- sheet3_servicestats_data %>% filter(Type=="Facilities") %>% filter(Question=="First year of data available:" | Question=="Première année de données disponible:") %>%
      mutate(Answer=as.numeric(Answer))
    last_year_df <- sheet3_servicestats_data %>% filter(Type=="Facilities") %>% filter(Question=="Most recent full year of data available:" | Question=="Plus récent complet année de données disponibles:") %>%
      mutate(Answer=as.numeric(Answer))
    first_year <- first_year_df[1,2]
    last_year <- last_year_df[1,2]

    #Pattern to be matched

    rr_clean <- reporting_rate_dataset %>% select(Year, matches("(?i)facilities|établissements")) %>%
      rename(reporting_rate=2) %>%
      mutate(Year=as.numeric(Year), reporting_rate=as.numeric(reporting_rate), Type="Facilities")  %>%
      mutate(Country=country_name) %>% tidyr::drop_na(reporting_rate)

    # # # # # # # # # # #  # # # # # #
    #Pattern to be matched
    pat_en <- "STEP 2 of 3. ENTER COMMODITIES DATA"
    pat_fr <- "ÉTAPE 2 of 3. SAISIR LES DONNÉES DES PRODUITS"
    find_test_en <-as.data.frame(as.matrix(str_detect(test_type, pat_en) )) %>%
      tibble::rowid_to_column()
    find_test_fr <-as.data.frame(as.matrix(str_detect(test_type, pat_fr) )) %>%
      tibble::rowid_to_column()
    find_test <- bind_rows(find_test_en, find_test_fr) %>% filter(V1=="TRUE")

    first_cell <- min(find_test$rowid)
    rows <- nrow(test_type)
    cols <- ncol(test_type)

    col_1 <-  (first_cell %/%  rows ) + 2
    col_last <- col_1 + 4 + (last_year - first_year)

    row_1 <- (first_cell %%  rows) + 5
    row_last <-    row_1 + 25

    matrix_type <- test_type[row_1:row_last, col_1:col_last]
    matrix_type[1,1] <- "Method"
    matrix_type[1,3] <- "CYP"

    type_inputs_clean <- data.frame(matrix_type)
    colnames(type_inputs_clean) <- type_inputs_clean[1,]
    type_inputs_clean <- type_inputs_clean[-1, ]

    n_col <- ncol(type_inputs_clean)

    type_inputs_clean <- type_inputs_clean %>% gather(Year, Commodities, 5:n_col) %>%
      filter(!is.na(Method)) %>%
      mutate(Year=as.numeric(Year), Commodities=as.numeric(Commodities), Type="Facilities")  %>%
      mutate(Country=country_name)


    rr_data <- bind_rows(facilities_rr_data, rr_clean)
    rr_years <- rr_data %>% filter(reporting_rate >= 0.8) %>% pull(Year)
    type_inputs_data <- bind_rows(facilities_inputs_data, type_inputs_clean) #%>% filter(Year %in% rr_years)
    year_acceptance_location <- which(test_type == "Year of method acceptance", arr.ind = TRUE) %>%
      as_tibble()
    type_method_continuation <- test_type[(year_acceptance_location$row + 1):(year_acceptance_location$row + 7), (year_acceptance_location$col-1):(year_acceptance_location$col+16)] %>% as_tibble() %>%
      rename(method_overview = 1, method_detail = 2) %>%
      rename_with(~paste0("Year_", seq_along(.)), .cols = -(1:2)) %>% fill(method_overview) %>% mutate(method_detail = ifelse(grepl("ligat", method_detail, ignore.case = TRUE), "Tubal Ligation (F)",
                                                                                                                              ifelse(grepl("vasect", method_detail, ignore.case = TRUE), "Vasectomy (M)",
                                                                                                                                     ifelse(grepl("copper", method_detail, ignore.case = TRUE), "Copper- T 380-A IUD",
                                                                                                                                            ifelse(grepl("lng", method_detail, ignore.case = TRUE), "LNG-IUS",
                                                                                                                                                   ifelse(grepl("implanon", method_detail, ignore.case = TRUE), "Implanon",
                                                                                                                                                          ifelse(grepl("sino", method_detail, ignore.case = TRUE), "Sino-Implant",
                                                                                                                                                                 ifelse(grepl("jadelle", method_detail, ignore.case = TRUE), "Jadelle",
                                                                                                                                                                        ifelse(grepl("4 Year implant", method_detail, ignore.case = TRUE), "Sino-Implant", method_detail)))))))))









  }

  # Visits
  visits_rr_data <- setNames(data.frame(matrix(ncol = 4,  nrow = 0)), c("Year", "reporting_rate", "Type", "Country")) %>%
    mutate( Year=as.numeric(Year), reporting_rate=as.numeric(reporting_rate), Type=as.character(Type), Country=as.character(Country))

  visits_inputs_data <- setNames(data.frame(matrix(ncol = 8,  nrow = 0)), c("Method", "CYP FACTOR", "CYP", "UNITS","Year", "Commodities", "Type", "Country")) %>%
    mutate( Method=as.character(Method),"CYP FACTOR"=as.character("CYP FACTOR"), CYP=as.character(CYP),  UNITS=as.character(UNITS), Year=as.numeric(Year), Commodities=as.numeric(Commodities), Type=as.character(Type), Country=as.character(Country))

  if(input_type == "visits"){

    sheet_data <- readxl::read_excel(country_file_path, sheet = "Visits Input")
    test_type <- as.matrix(read_excel(country_file_path, sheet = "Visits Input"))

    output_data <- readxl::read_excel(country_file_path, sheet = "Visits Output")
    condoms <- output_data[99,2] %>% mutate(ss_type = "FP visits")

    first_year_df <- sheet3_servicestats_data %>% filter(Type=="Visits") %>% filter(Question=="First year of data available:" | Question=="Première année de données disponible:") %>%
      mutate(Answer=as.numeric(Answer))
    last_year_df <- sheet3_servicestats_data %>% filter(Type=="Visits") %>% filter(Question=="Most recent full year of data available:" | Question=="Plus récent complet année de données disponibles:") %>%
      mutate(Answer=as.numeric(Answer))
    first_year <- first_year_df[1,2]
    last_year <- last_year_df[1,2]
    rr_clean <- reporting_rate_dataset %>% select(Year, matches("(?i)visits|visites")) %>%
      rename(reporting_rate=2) %>%
      mutate(Year=as.numeric(Year), reporting_rate=as.numeric(reporting_rate), Type="Visits")  %>%
      mutate(Country=country_name) %>% tidyr::drop_na(reporting_rate)
    # # # # # # # # # # #  # # # # # #
    #Pattern to be matched
    pat_en <- "STEP 2 of 3. ENTER VISITS DATA"
    pat_fr <- "ÉTAPE 2 de 3. SAISIR LES DONNÉES DES VISITES"
    find_test_en <-as.data.frame(as.matrix(str_detect(test_type, pat_en) )) %>%
      tibble::rowid_to_column()
    find_test_fr <-as.data.frame(as.matrix(str_detect(test_type, pat_fr) )) %>%
      tibble::rowid_to_column()
    find_test <- bind_rows(find_test_en, find_test_fr) %>% filter(V1=="TRUE")

    first_cell <- min(find_test$rowid)
    rows <- nrow(test_type)
    cols <- ncol(test_type)

    col_1 <-  (first_cell %/%  rows ) + 2
    col_last <- col_1 + 4 + (last_year - first_year)

    row_1 <- (first_cell %%  rows) + 5
    row_last <-    row_1 + 25

    matrix_type<- test_type[row_1:row_last, col_1:col_last]
    matrix_type[1,1] <- "Method"
    matrix_type[1,3] <- "CYP"

    type_inputs_clean <- data.frame(matrix_type)
    colnames(type_inputs_clean) <- type_inputs_clean[1,]
    type_inputs_clean <- type_inputs_clean[-1, ]

    n_col <- ncol(type_inputs_clean)

    type_inputs_clean <- type_inputs_clean %>% gather(Year, Commodities, 5:n_col) %>%
      filter(!is.na(Method)) %>%
      mutate(Year=as.numeric(Year), Commodities=as.numeric(Commodities), Type="Visits")  %>%
      mutate( Country=country_name)


    rr_data <- bind_rows(visits_rr_data, rr_clean)
    rr_years <- rr_data %>% filter(reporting_rate >= 0.8) %>% pull(Year)
    type_inputs_data <- bind_rows(visits_inputs_data, type_inputs_clean) #%>% filter(Year %in% rr_years)

    year_acceptance_location <- which(test_type == "Year of method acceptance", arr.ind = TRUE) %>%
      as_tibble()
    type_method_continuation <- test_type[(year_acceptance_location$row + 1):(year_acceptance_location$row + 7), (year_acceptance_location$col-1):(year_acceptance_location$col+16)] %>% as_tibble() %>%
      rename(method_overview = 1, method_detail = 2) %>%
      rename_with(~paste0("Year_", seq_along(.)), .cols = -(1:2)) %>% fill(method_overview) %>% mutate(method_detail = ifelse(grepl("ligat", method_detail, ignore.case = TRUE), "Tubal Ligation (F)",
                                                                                                                              ifelse(grepl("vasect", method_detail, ignore.case = TRUE), "Vasectomy (M)",
                                                                                                                                     ifelse(grepl("copper", method_detail, ignore.case = TRUE), "Copper- T 380-A IUD",
                                                                                                                                            ifelse(grepl("lng", method_detail, ignore.case = TRUE), "LNG-IUS",
                                                                                                                                                   ifelse(grepl("implanon", method_detail, ignore.case = TRUE), "Implanon",
                                                                                                                                                          ifelse(grepl("sino", method_detail, ignore.case = TRUE), "Sino-Implant",
                                                                                                                                                                 ifelse(grepl("jadelle", method_detail, ignore.case = TRUE), "Jadelle",
                                                                                                                                                                        ifelse(grepl("4 Year implant", method_detail, ignore.case = TRUE), "Sino-Implant", method_detail)))))))))










  }

  # Users
  users_rr_data <- setNames(data.frame(matrix(ncol = 4,  nrow = 0)), c("Year", "reporting_rate", "Type", "Country")) %>%
    mutate( Year=as.numeric(Year), reporting_rate=as.numeric(reporting_rate), Type=as.character(Type), Country=as.character(Country))

  users_inputs_data <- setNames(data.frame(matrix(ncol = 8,  nrow = 0)), c("Method", "CYP FACTOR", "CYP", "UNITS","Year", "Commodities", "Type", "Country")) %>%
    mutate( Method=as.character(Method),"CYP FACTOR"=as.character("CYP FACTOR"), CYP=as.character(CYP),  UNITS=as.character(UNITS), Year=as.numeric(Year), Commodities=as.numeric(Commodities), Type=as.character(Type), Country=as.character(Country))

  if(input_type == "users"){

    sheet_data <- readxl::read_excel(country_file_path, sheet = "Users Input")
    test_type <- as.matrix(read_excel(country_file_path, sheet = "Users Input"))

    output_data <- readxl::read_excel(country_file_path, sheet = "Users Output")
    condoms <- output_data[99,2] %>% mutate(ss_type = "FP users")


    first_year_df <- sheet3_servicestats_data %>% filter(Type=="Users") %>% filter(Question=="First year of data available:" | Question=="Première année de données disponible:") %>%
      mutate(Answer=as.numeric(Answer))
    last_year_df <- sheet3_servicestats_data %>% filter(Type=="Users") %>% filter(Question=="Most recent full year of data available:" | Question=="Plus récent complet année de données disponibles:") %>%
      mutate(Answer=as.numeric(Answer))
    first_year <- first_year_df[1,2]
    last_year <- last_year_df[1,2]
    rr_clean <- reporting_rate_dataset %>% select(Year, matches("(?i)users|Utilisatrices|Utilisateurs")) %>%
      rename(reporting_rate=2) %>%
      mutate(Year=as.numeric(Year), reporting_rate=as.numeric(reporting_rate), Type="Users")  %>%
      mutate(Country=country_name) %>% tidyr::drop_na(reporting_rate)

    # # # # # # # # # # #  # # # # # #
    #Pattern to be matched
    pat_en <- "STEP 2 of 3. ENTER USERS DATA"
    pat_fr <- "ÉTAPE 2 of 3. SAISIR LES DONNÉES DES UTILISATEURS"
    find_test_en <-as.data.frame(as.matrix(str_detect(test_type, pat_en) )) %>%
      tibble::rowid_to_column()
    find_test_fr <-as.data.frame(as.matrix(str_detect(test_type, pat_fr) )) %>%
      tibble::rowid_to_column()
    find_test <- bind_rows(find_test_en, find_test_fr) %>% filter(V1=="TRUE")

    first_cell <- min(find_test$rowid)
    rows <- nrow(test_type)
    cols <- ncol(test_type)

    col_1 <-  (first_cell %/%  rows ) + 2
    col_last <- col_1 + 4 + (last_year - first_year)

    row_1 <- (first_cell %%  rows) + 5
    row_last <-    row_1 + 25

    matrix_type <- test_type[row_1:row_last, col_1:col_last]
    matrix_type[1,1] <- "Method"
    matrix_type[1,3] <- "CYP"

    type_inputs_clean <- data.frame(matrix_type)
    colnames(type_inputs_clean) <- type_inputs_clean[1,]
    type_inputs_clean <- type_inputs_clean[-1, ]

    n_col <- ncol(type_inputs_clean)

    type_inputs_clean <- type_inputs_clean %>% gather(Year, Commodities, 5:n_col) %>%
      filter(!is.na(Method)) %>%
      mutate(Year=as.numeric(Year), Commodities=as.numeric(Commodities), Type="Users")  %>%
      mutate( Country=country_name)

    rr_data <- bind_rows(users_rr_data, rr_clean)
    rr_years <- users_rr_data %>% filter(reporting_rate >= 0.8) %>% pull(Year)
    type_inputs_data <- bind_rows(users_inputs_data, type_inputs_clean) #%>% filter(Year %in% rr_years)

    type_method_continuation <- NULL

  }


  # Combining Data
  #rr_clean <- bind_rows(clients_rr_data, facilities_rr_data, visits_rr_data, users_rr_data)
  inputs_clean <- type_inputs_data

  ss_quantity_data <- inputs_clean %>%
    select(Type, Method, Year, Commodities) %>%
    arrange(Year) %>%
    tidyr::pivot_wider(id_cols = c(Type, Method), names_from = Year, values_from = Commodities) %>%
    rename(ss_type = Type,
           method_detail = Method) %>%
    mutate(ss_type = ifelse(grepl("Utilisatrices", ss_type, ignore.case = TRUE), "FP users",
                            ifelse(grepl("Utilisateurs", ss_type, ignore.case = TRUE), "FP users",
                                   ifelse(grepl("clients", ss_type, ignore.case = TRUE), "Contraceptive commodities distributed to clients",
                                          ifelse(grepl("visites", ss_type, ignore.case = TRUE), "FP visits",
                                                 ifelse(grepl("établissements", ss_type, ignore.case = TRUE), "Contraceptive commodities distributed to facilities",
                                                        ifelse(grepl("facilities", ss_type, ignore.case = TRUE), "Contraceptive commodities distributed to facilities",
                                                               ifelse(grepl("users", ss_type, ignore.case = TRUE), "FP users",
                                                                      ifelse(grepl("visits", ss_type, ignore.case = TRUE), "FP visits",ss_type))))))))) %>%
    rename(ss_type = ss_type) %>%
    mutate(method_detail = ifelse(grepl("implanon", method_detail, ignore.case = TRUE), "Implanon",
                                  ifelse(grepl("jadelle", method_detail, ignore.case = TRUE), "Jadelle",
                                         ifelse(grepl("LNG-IUS", method_detail, ignore.case = TRUE), "LNG-IUS",
                                                ifelse(grepl("4 Year implant", method_detail, ignore.case = TRUE), "Sino-Implant",
                                                       ifelse(grepl("ligat", method_detail, ignore.case = TRUE), "Tubal Ligation (F)",
                                                              ifelse(grepl("vasect", method_detail, ignore.case = TRUE), "Vasectomy (M)",
                                                                     ifelse(grepl("copper", method_detail, ignore.case = TRUE), "Copper- T 380-A IUD",
                                                                            ifelse(grepl("lng", method_detail, ignore.case = TRUE), "LNG-IUS",
                                                                                   ifelse(grepl("implanon", method_detail, ignore.case = TRUE), "Implanon",
                                                                                          ifelse(grepl("sino", method_detail, ignore.case = TRUE), "Sino-Implant",
                                                                                                 ifelse(grepl("jadelle", method_detail, ignore.case = TRUE), "Jadelle",
                                                                                                        ifelse(grepl("autre injectable", method_detail, ignore.case = TRUE), "Other Injectable",
                                                                                                               ifelse(grepl("Oraux combinés", method_detail, ignore.case = TRUE), "Combined Oral (COC)",
                                                                                                                      ifelse(grepl("(POP)", method_detail, ignore.case = TRUE), "Progestin only (POP)",
                                                                                                                             ifelse(grepl("autre pilule", method_detail, ignore.case = TRUE), "Other OC Pill",
                                                                                                                                    ifelse(grepl("masculin", method_detail, ignore.case = TRUE), "Male Condom",
                                                                                                                                           ifelse(grepl("feminin", method_detail, ignore.case = TRUE), "Female Condom",
                                                                                                                                                  ifelse(grepl("MAMA", method_detail, ignore.case = TRUE), "LAM",
                                                                                                                                                         ifelse(grepl("jours", method_detail, ignore.case = TRUE), "SDM (Standard Days)",
                                                                                                                                                                ifelse(grepl("vaginale", method_detail, ignore.case = TRUE), "Vaginal barrier",
                                                                                                                                                                       ifelse(grepl("CU", method_detail, ignore.case = TRUE), "EC", method_detail))))))))))))))))))))))
  #ss_quantity_data <- ss_quantity_data






  cyp_factors_long <- sheet_data[21:28, 2:6] %>%
    as_tibble() %>%
    rename(method_overview = 1, method_detail = 2, cyp_factor = 3, cyp_factor_adjusted = 4, units = 5) %>%
    fill(method_overview, .direction = "down") %>%
    mutate(method_type = "long",
           cyp_factor = as.numeric(cyp_factor),
           ss_type = input_type)


  cyp_factors_short <- sheet_data[30:44, 2:6] %>%
    as_tibble() %>%
    rename(method_overview = 1, method_detail = 2, cyp_factor = 3, cyp_factor_adjusted = 4, units = 5) %>%
    fill(method_overview, .direction = "down") %>%
    mutate(method_type = "short",
           cyp_factor = as.numeric(cyp_factor),
           ss_type = input_type)

   #browser()
  cyp_table_all <- rbind(cyp_factors_long,
                         cyp_factors_short)  %>%
    mutate(method_detail = ifelse(grepl("implanon", method_detail, ignore.case = TRUE), "Implanon",
                                  ifelse(grepl("jadelle", method_detail, ignore.case = TRUE), "Jadelle",
                                         ifelse(grepl("LNG-IUS", method_detail, ignore.case = TRUE), "LNG-IUS",
                                                ifelse(grepl("4 Year implant", method_detail, ignore.case = TRUE), "Sino-Implant",
                                                       ifelse(grepl("ligat", method_detail, ignore.case = TRUE), "Tubal Ligation (F)",
                                                              ifelse(grepl("vasect", method_detail, ignore.case = TRUE), "Vasectomy (M)",
                                                                     ifelse(grepl("copper", method_detail, ignore.case = TRUE), "Copper- T 380-A IUD",
                                                                            ifelse(grepl("lng", method_detail, ignore.case = TRUE), "LNG-IUS",
                                                                                   ifelse(grepl("implanon", method_detail, ignore.case = TRUE), "Implanon",
                                                                                          ifelse(grepl("sino", method_detail, ignore.case = TRUE), "Sino-Implant",
                                                                                                 ifelse(grepl("jadelle", method_detail, ignore.case = TRUE), "Jadelle",
                                                                                                        ifelse(grepl("autre injectable", method_detail, ignore.case = TRUE), "Other Injectable",
                                                                                                               ifelse(grepl("Oraux combinés", method_detail, ignore.case = TRUE), "Combined Oral (COC)",
                                                                                                                      ifelse(grepl("(POP)", method_detail, ignore.case = TRUE), "Progestin only (POP)",
                                                                                                                             ifelse(grepl("autre pilule", method_detail, ignore.case = TRUE), "Other OC Pill",
                                                                                                                                    ifelse(grepl("masculin", method_detail, ignore.case = TRUE), "Male Condom",
                                                                                                                                           ifelse(grepl("feminin", method_detail, ignore.case = TRUE), "Female Condom",
                                                                                                                                                  ifelse(grepl("MAMA", method_detail, ignore.case = TRUE), "LAM",
                                                                                                                                                         ifelse(grepl("jours", method_detail, ignore.case = TRUE), "SDM (Standard Days)",
                                                                                                                                                                ifelse(grepl("vaginale", method_detail, ignore.case = TRUE), "Vaginal barrier",
                                                                                                                                                                       ifelse(grepl("CU", method_detail, ignore.case = TRUE), "EC", method_detail)))))))))))))))))))))) %>%
    mutate(method_overview = ifelse(grepl("Stérilisation", method_overview, ignore.case = TRUE), "Sterilization",
                                    ifelse(grepl("diu", method_overview, ignore.case = TRUE), "IUD",
                                           ifelse(grepl("Produits injectables", method_overview, ignore.case = TRUE), "Injectable",
                                                  ifelse(grepl("Pilule", method_overview, ignore.case = TRUE), "Pill",
                                                         ifelse(grepl("Autre", method_overview, ignore.case = TRUE), "Other Modern Methods",
                                                                ifelse(grepl("d'urgence", method_overview, ignore.case = TRUE), "Emergency contraception",
                                                                       ifelse(grepl("Préservatifs", method_overview, ignore.case = TRUE), "Condom", method_overview)))))))) %>%
    mutate(units = ifelse(grepl("années de protection", units, ignore.case = TRUE), "years of protection",
                          ifelse(grepl("par utilisateur par année", units, ignore.case = TRUE), "per user per year",
                                 ifelse(grepl("utilisatrices", units, ignore.case = TRUE), "users",
                                        ifelse(grepl("visites par an", units, ignore.case = TRUE), "visits per year",
                                               ifelse(grepl("consultation par utilisateur", units, ignore.case = TRUE), "consultations per user", units)))))) %>% tidyr::drop_na(cyp_factor)

  cyp_table <- cyp_table_all








  condoms_include_df <- condoms %>% rename(include_exclude_condoms = 1)

  user_input_adjustment_table <- user_input_adjustment_table %>% mutate(method_overview = ifelse(method_overview == "Stérilisation (F)", "Sterilization (F)",
                                                                                                 ifelse(method_overview == "Stérilisation (M)", "Sterilization (M)",
                                                                                                        ifelse(method_overview == "DIU", "IUD",
                                                                                                               ifelse(method_overview == "Produits injectables", "Injectable",
                                                                                                                      ifelse(method_overview == "Pilule", "Pill",
                                                                                                                             ifelse(method_overview == "Préservatifs (M)", "Condom (M)",
                                                                                                                                    ifelse(method_overview == "Préservatifs (F)", "Condom (F)",
                                                                                                                                           ifelse(method_overview == "Autres Méthodes Modernes", "Other Modern Methods",
                                                                                                                                                  ifelse(method_overview == "Contraception d'urgence", "Emergency contraception", method_overview)))))))))) %>%
    mutate(include_adjustment = ifelse(include_adjustment == "Oui", "Yes",
                                       ifelse(include_adjustment == "Non", "No", include_adjustment)))

  condoms_include_df <- condoms_include_df %>% mutate(include_exclude_condoms = ifelse(grepl("exc", ignore.case = TRUE, include_exclude_condoms), "Exclude Condoms", "Include Condoms"))


  fpet_mcpr_data <- fpet_mcpr_data %>% mutate(country_language = setup_data$language)
  return(list(
    ss_quantity_data = ss_quantity_data,
    pop_dataset = pop_dataset,
    setup_data = setup_data,
    recode_sectors_reporting = recode_sectors_reporting,
    recode_scaleup_table = recode_scaleup_table %>% rename(ss_type = SS_type),
    reporting_rates_table = rr_clean %>% rename(ss_type = Type) %>% mutate(ss_type = ifelse(ss_type == "Clients", "Contraceptive commodities distributed to clients",
                                                                                                    ifelse(ss_type == "Facilities", "Contraceptive commodities distributed to facilities",
                                                                                                           ifelse(ss_type == "Users", "FP users",
                                                                                                                  ifelse(ss_type == "Visits", "FP visits", ss_type))))),
    ss_info = ss_info,
    cyp_table = cyp_table %>% mutate(ss_type = ifelse(ss_type == "clients", "Contraceptive commodities distributed to clients",
                                                      ifelse(ss_type == "facilities", "Contraceptive commodities distributed to facilities",
                                                             ifelse(ss_type == "users", "FP users",
                                                                    ifelse(ss_type == "visits", "FP visits", ss_type))))),
    method_continuation_data = type_method_continuation,
    user_input_adjustment_table = user_input_adjustment_table,
    include_condoms_df = condoms_include_df,
    fpet_mcpr_data = fpet_mcpr_data
  ))
}
