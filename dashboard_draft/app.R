# A Shiny app for investigating U.S. Seafood trade, landings, and processing
  # data through time
# Contact: Cameron Van Horn
#          cameron.vanhorn@noaa.gov

# A note on the general data formatting:
  # all 'value' data are calculated in Real 2024 U.S. Dollars (USD) to account
  # for inflation (see 2_data_munge.R)

# Packages Sources, and Data ---------------------------------------------------
if(!require("googledrive")) install.packages("googledrive")
if(!require("shiny"))       install.packages("shiny")
if(!require("bslib"))       install.packages("bslib")
if(!require("tidyverse"))   install.packages("tidyverse")
if(!require("scales"))      install.packages("scales")
if(!require("ggh4x"))       install.packages("ggh4x")
if(!require("shinycssloaders")) install.packages("shinycssloaders")
# if(!require("nmfspalette")) install.packages("nmfspalette")
# Due to some limitations in downloading nmfspalette on devices, use source
  # file located in app directory for nmfspalette colors
source("nmfs_cols.R")

# Pull Data (most recent version)
load('seafood_trade_data_munge_04_30_25.RData')

com_landings <- com_landings %>%
  filter(CONFIDENTIALITY == 'Public')

# create matrix of all categorization terms available in the data
categorization_matrix <- bind_rows(trade_data, com_landings, pp_data) %>%
  select(SPECIES_NAME, SPECIES_GROUP, 
         SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  group_by(SPECIES_NAME, SPECIES_GROUP, 
           SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  distinct() %>%
  ungroup()

# create matrix of all trade categorization terms available in the data
trade_categorization_matrix <- trade_data %>%
  select(SPECIES_NAME, SPECIES_GROUP, 
         SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  group_by(SPECIES_NAME, SPECIES_GROUP, 
           SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  distinct() %>%
  ungroup()

# create matrix of all landings categorization terms available in the data
landings_categorization_matrix <- com_landings %>%
  select(SPECIES_NAME, SPECIES_GROUP,
         SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  group_by(SPECIES_NAME, SPECIES_GROUP,
           SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  distinct() %>%
  ungroup()

# create matrix of all products categorization terms available in the data
products_categorization_matrix <- pp_data %>%
  select(SPECIES_NAME, SPECIES_GROUP,
         SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  group_by(SPECIES_NAME, SPECIES_GROUP,
           SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  distinct() %>%
  ungroup()

# Create list of terms for each level of organization hierarchy
  # these lists will be used to determine where a provided species input is 
  # found in the hierarchy
ecat_list <- unique(categorization_matrix %>%
                      select(ECOLOGICAL_CATEGORY) %>%
                      distinct() %>%
                      filter(!is.na(ECOLOGICAL_CATEGORY)) %>%
                      mutate(ECOLOGICAL_CATEGORY = 
                               str_to_title(ECOLOGICAL_CATEGORY)) %>%
                      pull()) 

scat_list <- unique(categorization_matrix %>%
                      select(SPECIES_CATEGORY) %>%
                      distinct() %>%
                      filter(!is.na(SPECIES_CATEGORY)) %>%
                      mutate(SPECIES_CATEGORY = 
                               str_to_title(SPECIES_CATEGORY)) %>%
                      pull())

sgrp_list <- unique(categorization_matrix %>%
                      select(SPECIES_GROUP) %>%
                      distinct() %>%
                      filter(!is.na(SPECIES_GROUP)) %>%
                      mutate(SPECIES_GROUP = str_to_title(SPECIES_GROUP)) %>%
                      pull())

sname_list <- unique(categorization_matrix %>%
                       select(SPECIES_NAME) %>%
                       distinct() %>%
                       filter(!is.na(SPECIES_NAME)) %>%
                       mutate(SPECIES_NAME = str_to_title(SPECIES_NAME)) %>%
                       pull())

# list of all categorizations available in trade data
trade_terms <- c('All Species',
                 str_to_title(unique(trade_data$ECOLOGICAL_CATEGORY)),
                 str_to_title(unique(trade_data$SPECIES_CATEGORY)),
                 str_to_title(unique(trade_data$SPECIES_GROUP)),
                 str_to_title(unique(trade_data$SPECIES_NAME)))

# list of all categorizations available in landings data
landings_terms <- c('All Species',
                    str_to_title(unique(com_landings$ECOLOGICAL_CATEGORY)),
                    str_to_title(unique(com_landings$SPECIES_CATEGORY)),
                    str_to_title(unique(com_landings$SPECIES_GROUP)),
                    str_to_title(unique(com_landings$SPECIES_NAME)))

# list of all categorizations available in production data
pp_terms <- c('All Species',
              str_to_title(unique(pp_data$ECOLOGICAL_CATEGORY)),
              str_to_title(unique(pp_data$SPECIES_CATEGORY)),
              str_to_title(unique(pp_data$SPECIES_GROUP)),
              str_to_title(unique(pp_data$SPECIES_NAME)))

# Custom Functions -------------------------------------------------------------
# stop functions without outputting error message
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
### filter species
filter_species <- function(data, species) {
  # data is a formatted data frame created from 2_data_munge.R (see GitHub)
  # species is a character vector of a species of interest 
    # (e.g., 'Albacore Tuna')
  
  # species are organized in a hierarhcy of four levels:
    # ecological category (e.g., 'Large Pelagics')
    # species category (e.g., 'Tunas')
    # species group (e.g., 'Hawaii Tuna')
    # species name (e.g., 'Yellowfin Tuna')
  
  # store unique values in each species hierarchy level
  ecology_categories <- unique(data$ECOLOGICAL_CATEGORY)
  species_categories <- unique(data$SPECIES_CATEGORY)
  species_groups <- unique(data$SPECIES_GROUP)
  species_names <- unique(data$SPECIES_NAME)
  
  # coerce species input to upper case to align with data frame formatting
  species <- toupper(species)
  
  # ifelse loop to find which hierarchy level the input species is stored
  locate_level <- 
    # first search highest level 'Ecological Category'
    ifelse(species %in% ecology_categories, 
           'ECOLOGICAL_CATEGORY',
           # return the category if species is found, otherwise continue loop
           ifelse(species %in% species_categories, 
                  'SPECIES_CATEGORY',
                  ifelse(species %in% species_groups, 
                         'SPECIES_GROUP',
                         ifelse(species %in% species_names, 
                                'SPECIES_NAME',
                                # if the species is not found in the data, 
                                # return 'UNAVAILABLE'
                                'UNAVAILABLE'))))
  
  # if species was not found, stop function with message to try a different
    # species input or search for available entries
  if (locate_level == 'UNAVAILABLE') {
    stop()
  } 
  
  # only runs if species is found
  # store the hierarchy level as symbol, then as object of type quosure
    # (see RLang package for more information on quosures)
    # this enables the object to be called in a dplyr pipe via bang-bang (!!)
  level <- as.symbol(locate_level)
  level <- rlang::enquo(level)
  
  # filter the input data frame for the species of interest using the hierarchy
    # level column in which the entry was found
  new_data <- data %>%
    filter(!!level == species)
  
  return(new_data)
  
  # a note on the hierarchy level conventions:
    # including multiple levels of species classification to each product 
    # enables more data to be used in the event that a product does not contain
    # a specific species on the label (e.g., 'tuna'). Also, it enables us to 
    # investigate the data at different resolutions (e.g., all tunas compared
    # to just Yellowfin Tuna)
}

### summary + calculation functions
summarize_trade_yr_spp <- function(trade_table, species) {
  # this function summarizes trade data by year and species of interest
  # trade_table is a formatted data frame of FOSS trade data (see 2_data_munge.R)
  # species is a character vector of a species of interest
  
  # coerce species to upper case
    # IF NOT COERCED TO UPPER CASE: app would not display data as species input
    # is sourced from a selected user input of pre-determined values, which are 
    # provided in lower case (as title; e.g., 'Tuna' instead of 'tuna')
  species <- toupper(species)
  
  # if a species is selected, find the level of the categorization hierarchy in
    # which the species input resides
  # see filter_species function for info on why we store as symbol and quosure
  if (species != 'ALL SPECIES') {
    which_level <- as.symbol(
      ifelse(species %in% unique(trade_table$ECOLOGICAL_CATEGORY), 
             'ECOLOGICAL_CATEGORY',
             ifelse(species %in% unique(trade_table$SPECIES_CATEGORY), 
                    'SPECIES_CATEGORY',
                    ifelse(species %in% unique(trade_table$SPECIES_GROUP), 
                           'SPECIES_GROUP',
                           'SPECIES_NAME')))
    )
  # if species is not selected (default is ALL SPECIES), summarize all trade
      # i.e., no filter_species needed
  } else if (species == 'ALL SPECIES') {
    summarized_data <- trade_table %>%
      # select only necessary columns (exports, imports, year)
      select(YEAR, EXP_VALUE_2024USD, EXP_VOLUME_KG, IMP_VALUE_2024USD,
             IMP_VOLUME_KG) %>%
      # replace NA values with 0 so that sums and averages are not NA
      mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD), 0,
                                        EXP_VALUE_2024USD),
             IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD), 0,
                                        IMP_VALUE_2024USD),
             EXP_VOLUME_KG = ifelse(is.na(EXP_VOLUME_KG), 0,
                                    EXP_VOLUME_KG),
             IMP_VOLUME_KG = ifelse(is.na(IMP_VOLUME_KG), 0,
                                    IMP_VOLUME_KG)) %>%
      # group by YEAR to aggregate data within each year
      group_by(YEAR) %>%
      # sum all numeric columns within the group, drop groups at end
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      # create columns of price per KG, value in millions/billions, 
        # volume in metric tons
      mutate(EXP_PRICE_USD_PER_KG = EXP_VALUE_2024USD / EXP_VOLUME_KG,
             IMP_PRICE_USD_PER_KG = IMP_VALUE_2024USD / IMP_VOLUME_KG,
             EXP_VALUE_2024USD_MILLIONS = EXP_VALUE_2024USD / 1000000,
             IMP_VALUE_2024USD_MILLIONS = IMP_VALUE_2024USD / 1000000,
             EXP_VALUE_2024USD_BILLIONS = EXP_VALUE_2024USD / 1000000000,
             IMP_VALUE_2024USD_BILLIONS = IMP_VALUE_2024USD / 1000000000,
             EXP_VOLUME_MT = EXP_VOLUME_KG / 1000,
             IMP_VOLUME_MT = IMP_VOLUME_KG / 1000)
    return(summarized_data)
  }
  
  # store level as object of quosure to work in dplyr pipe (via !!)
  level <- rlang::enquo(which_level)
  
  summarized_data <- trade_table %>%
    # below is identical to dplyr pipe above save for three distinctions:
      # 1) filter_species used to include only data of specified species
      # 2) retain column of the hierarchy level in which the species was found
      # 3) group the data by Year AND species (this lets us keep the species
        # in the data as a column)
    filter_species(species) %>%
    select(YEAR, !!level, EXP_VALUE_2024USD, EXP_VOLUME_KG, 
           IMP_VALUE_2024USD, IMP_VOLUME_KG) %>%
    mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD), 0,
                                      EXP_VALUE_2024USD),
           IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD), 0,
                                      IMP_VALUE_2024USD),
           EXP_VOLUME_KG = ifelse(is.na(EXP_VOLUME_KG), 0,
                                  EXP_VOLUME_KG),
           IMP_VOLUME_KG = ifelse(is.na(IMP_VOLUME_KG), 0,
                                  IMP_VOLUME_KG)) %>%
    group_by(YEAR, !!level) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    mutate(EXP_PRICE_USD_PER_KG = EXP_VALUE_2024USD / EXP_VOLUME_KG,
           IMP_PRICE_USD_PER_KG = IMP_VALUE_2024USD / IMP_VOLUME_KG,
           EXP_VALUE_2024USD_MILLIONS = EXP_VALUE_2024USD / 1000000,
           IMP_VALUE_2024USD_MILLIONS = IMP_VALUE_2024USD / 1000000,
           EXP_VALUE_2024USD_BILLIONS = EXP_VALUE_2024USD / 1000000000,
           IMP_VALUE_2024USD_BILLIONS = IMP_VALUE_2024USD / 1000000000,
           EXP_VOLUME_MT = EXP_VOLUME_KG / 1000,
           IMP_VOLUME_MT = IMP_VOLUME_KG / 1000)
  
  return(summarized_data)
}
summarize_trade_ctry_yr_spp <- function(trade_table, species, 
                                        time.frame, value = F, volume = F) {
  # this function summarizes trade data by year and species of interest
    # within the top 5 trading partners of the U.S. for that species during
    # the specified period of time
  # trade_table is a formatted data frame of FOSS trade data (see 2_data_munge.R)
  # species is a character vector of a species of interest
  # time.frame is a vector of two years that bookend a desired time period
  # value is logical that specifies if the function should output summaries by 
    # trade value, set to FALSE by default
  # volume is logical that specifies if the function should output summaries by
    # trade volume, set to FALSE by default
  
  # Both value and volume cannot be FALSE so user must specify one
  if (value == F & volume == F) {
    stop('Please designate either value or volume as TRUE')
  }
  # Both value and volume cannot be TRUE so user must choose one
  if (value == T & volume == T) {
    stop('Please designate either value or volume as FALSE')
  }
  
  # Function only proceeds if either value OR volume are T
  # store which column ('field') to summarize by as object of type symbol, then 
    # as type quosure to function within dplyr pipe 
      # (see RLang package for more details)
  if (value == T) {
    field <- as.symbol('TOTAL_REAL_TRADE_VALUE')
    field <- rlang::enquo(field)
  } else {
    field <- as.symbol('TOTAL_TRADE_VOLUME')
    field <- rlang::enquo(field)
  }
  
  # coerce species to upper case to match data formatting
  species <- toupper(species)
  
  # if no species is selected ('ALL SPECIES' is the default), do not filter for species
  if (species == 'ALL SPECIES') {
    filtered_data <- trade_table
  } else {
    # otherwise, filter trade table by species
    filtered_data <- trade_table %>%
      filter_species(species)
  }
  
  # dplyr pipe to summarize exports and imports by year and country
  summarized_data <- filtered_data %>%
    # select only columns of interest: year, country, exports and imports
    select(YEAR, COUNTRY_NAME, EXP_VALUE_2024USD, EXP_VOLUME_KG, 
           IMP_VALUE_2024USD, IMP_VOLUME_KG) %>%
    # filter data to be within the specified time frame
    filter(YEAR >= time.frame[1],
           YEAR <= time.frame[2]) %>%
    # set NAs to 0 so sums and averages can be calculated without outputting NA
    mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD), 0,
                                      EXP_VALUE_2024USD),
           IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD), 0,
                                      IMP_VALUE_2024USD),
           EXP_VOLUME_KG = ifelse(is.na(EXP_VOLUME_KG), 0,
                                  EXP_VOLUME_KG),
           IMP_VOLUME_KG = ifelse(is.na(IMP_VOLUME_KG), 0,
                                  IMP_VOLUME_KG)) %>%
    # group_by year and country
    group_by(YEAR, COUNTRY_NAME) %>%
    # sum all numeric columns, drop groups
    summarise(across(where(is.numeric), sum),
              .groups = 'drop')
  
  # dplyr pipe to find top 5 trading nations during time frame for input species
  top5 <- summarized_data %>%
    # remove YEAR so it does not get summed
    select(!YEAR) %>%
    # group by country
    group_by(COUNTRY_NAME) %>%
    # sum all numeric columns (i.e., export and import value and volume)
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    # calculate total real trade value by summing export and import values
    # calculate total real trade volume by summing export and import volumes 
    mutate(TOTAL_REAL_TRADE_VALUE = EXP_VALUE_2024USD + IMP_VALUE_2024USD,
           TOTAL_TRADE_VOLUME = EXP_VOLUME_KG + IMP_VOLUME_KG) %>%
    # filter for the top 5 countries based on the field specified from 
      # the logical value and volume function inputs
    top_n(5, !!field) %>%
    # pull() outputs the values in the specified field as a vector
    pull(COUNTRY_NAME)
  
  # summarize trade data by top five countries during time period
  # summarized_data is already filtered for the time period
  final_data <- summarized_data %>%
    # filter for the top 5 countries
    filter(COUNTRY_NAME %in% top5) %>%
    # calculate export and import values in millions/billions,
    # calculate export and import volumes in metric tons,
    # calculate net value and net volume by subtracting imports from exports
    mutate(EXP_VALUE_2024USD_BILLIONS = EXP_VALUE_2024USD / 1000000000,
           IMP_VALUE_2024USD_BILLIONS = IMP_VALUE_2024USD / 1000000000,
           NET_VALUE_2024USD_BILLIONS = 
             EXP_VALUE_2024USD_BILLIONS - IMP_VALUE_2024USD_BILLIONS,
           EXP_VALUE_2024USD_MILLIONS = EXP_VALUE_2024USD / 1000000,
           IMP_VALUE_2024USD_MILLIONS = IMP_VALUE_2024USD / 1000000,
           NET_VALUE_2024USD_MILLIONS =
             EXP_VALUE_2024USD_MILLIONS - IMP_VALUE_2024USD_MILLIONS,
           EXP_VOLUME_MT = EXP_VOLUME_KG / 1000,
           IMP_VOLUME_MT = IMP_VOLUME_KG / 1000,
           NET_VOLUME_MT = EXP_VOLUME_MT - IMP_VOLUME_MT,
           NET_PRICE_2024USD_PER_KG = 
             (EXP_VALUE_2024USD - IMP_VALUE_2024USD) / 
             (EXP_VOLUME_KG - IMP_VOLUME_KG))
  
  return(final_data)
}
summarize_pp_yr_spp <- function(product_data, species) {
  # this function summarizes processed product data by year and species of 
    # interest
  # product_data is a formatted data frame of FOSS processed product data
    # (see 2_data_munge.R)
  # species is a character vector of a species of interest
  
  # coerce species to upper case to match data formatting
  species <- toupper(species)
  
  # if no species is provided (default is 'ALL SPECIES'), summarize data without 
    # filtering for a species
  if (species == 'ALL SPECIES') {
    summarized_data <- product_data %>%
      # select only necessary columns: year, PRODUCT_NAME (e.g., canned), 
        # volume (KG), and value (DOLLARS_2024)
      select(YEAR, PRODUCT_NAME, KG, DOLLARS_2024) %>%
      # group by year and the product condition (PRODUCT_NAME)
      group_by(YEAR, PRODUCT_NAME) %>%
      # sum all numeric columns (i.e., value and volume), drop groups
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      # convert kilograms to metric tons
      # convert value to billions and millions
      mutate(MT = KG / 1000,
             MILLIONS_2024USD = DOLLARS_2024 / 1000000,
             BILLIONS_2024USD = DOLLARS_2024 / 1000000000,
             PP_PRICE_2024USD_PER_KG = DOLLARS_2024 / KG) %>%
      # rename columns to label value, volume, and PP (processed product) prefix
      rename(PP_VALUE_2024USD = DOLLARS_2024,
             PP_VOLUME_MT = MT, 
             PP_VALUE_MILLIONS_2024USD = MILLIONS_2024USD,
             PP_VALUE_BILLIONS_2024USD = BILLIONS_2024USD,
             PP_VOLUME_KG = KG)
    
    return(summarized_data)
  }
  
  # identical to dplyr pipe above, save for filtering for a specified species
  # only runs if species != 'ALL SPECIES'
  product_data %>%
    filter_species(species) %>%
    select(YEAR, PRODUCT_NAME, KG, DOLLARS_2024) %>%
    group_by(YEAR, PRODUCT_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    mutate(MT = KG / 1000,
           MILLIONS_2024USD = DOLLARS_2024 / 1000000,
           BILLIONS_2024USD = DOLLARS_2024 / 1000000000,
           PP_PRICE_2024USD_PER_KG = DOLLARS_2024 / KG) %>%
    rename(PP_VALUE_2024USD = DOLLARS_2024,
           PP_VOLUME_MT = MT,
           PP_VALUE_MILLIONS_2024USD = MILLIONS_2024USD,
           PP_VALUE_BILLIONS_2024USD = BILLIONS_2024USD,
           PP_VOLUME_KG = KG)
}
summarize_landings_yr_spp <- function(landings_data, species) {
  # this function summarizes landings data (not exclusively commercial) by 
    # year and species of interest
  # landings_data is a formatted data frame of FOSS landings data 
    # (see 2_data_munge.R)
  # species is a character vector of a species of interest
  
  # coerce species to upper case to match data formatting
  species <- toupper(species)
  
  # if species is provided, find the level of the categorization hierarchy in 
    # which it exists
  if (species != 'ALL SPECIES') {
    which_level <- as.symbol(
      ifelse(species %in% unique(landings_data$ECOLOGICAL_CATEGORY), 
             'ECOLOGICAL_CATEGORY',
             ifelse(species %in% unique(landings_data$SPECIES_CATEGORY), 
                    'SPECIES_CATEGORY',
                    ifelse(species %in% unique(landings_data$SPECIES_GROUP), 
                           'SPECIES_GROUP',
                           'SPECIES_NAME')))
    )
  } else if (species == 'ALL SPECIES') {
    # for the default case (no species provided), summarize all landings data
    summarized_data <- landings_data %>%
      # remove confidential data as to only represent public data
        # this removes species whose data is exclusively confidential
      # filter out data that do not provide a value or volume
      filter(CONFIDENTIALITY != 'Confidential',
             !is.na(DOLLARS),
             !is.na(KG)) %>%
      # select only necessary columns (year, value, volume)
      select(YEAR, KG, DOLLARS_2024) %>%
      # group by year
      group_by(YEAR) %>%
      # sum values across all numeric columns (i.e., value and volume)
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      # convert KG to metric tons and dollars to millions/billions
      mutate(MT = KG / 1000,
             MILLIONS_DOLLARS_2024 = DOLLARS_2024 / 1000000,
             BILLIONS_DOLLARS_2024 = DOLLARS_2024 / 1000000000,
             COM_PRICE_2024USD_PER_KG = DOLLARS_2024 / KG) %>%
      # add COM (commercial) as well as volume or value as column prefix
        # this will benefit table joinings later
      rename(COM_VOLUME_KG = KG,
             COM_VOLUME_MT = MT,
             COM_VALUE_MILLIONS_2024USD = MILLIONS_DOLLARS_2024,
             COM_VALUE_BILLIONS_2024USD = BILLIONS_DOLLARS_2024)
    
    return(summarized_data)
  }
  # for all other cases (i.e., when a species is provided) 
  # set the hierarchy level found above as object of type quosure (see RLang)
  level <- rlang::enquo(which_level)
  
  # an identical dplyr pipe from that above save for one difference:
    # group by Year AND the hierarchy level to retain species name
  summarized_data <- landings_data %>%
    filter_species(species) %>%
    filter(CONFIDENTIALITY != 'Confidential',
           !is.na(DOLLARS),
           !is.na(KG)) %>%
    select(YEAR, !!level, KG, DOLLARS_2024) %>%
    group_by(YEAR, !!level) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    mutate(MT = KG / 1000,
           MILLIONS_DOLLARS_2024 = DOLLARS_2024 / 1000000,
           BILLIONS_DOLLARS_2024 = DOLLARS_2024 / 1000000000,
           COM_PRICE_2024USD_PER_KG = DOLLARS_2024 / KG) %>%
    rename(COM_VOLUME_KG = KG,
           COM_VOLUME_MT = MT,
           COM_VALUE_MILLIONS_2024USD = MILLIONS_DOLLARS_2024,
           COM_VALUE_BILLIONS_2024USD = BILLIONS_DOLLARS_2024) 
  
  return(summarized_data)
  
}
summarize_yr_spp <- function(species) {
  # this function utilizes the summary functions for trade, processed products,
    # and landings by year and species of interest and joins the data sets
    # produced by these functions
  # this enables more complex visualizations and calculations of these data
    # for species of interest, specifically for the function calculate_supply_metrics
  # species is a character vector of a species of interest
  
  # coerce species to uppercase to match data formatting
  species <- toupper(species)
  combined_data <- 
    # the order of joining is fairly irrelevant
    left_join(left_join(summarize_trade_yr_spp(trade_data, species),
                        # for processed produccts, we must perform an additional
                          # step by removing the product name (condition) from
                          # the data to prevent duplicated data from subsequent
                          # joins
                        summarize_pp_yr_spp(pp_data, species) %>%
                          select(!PRODUCT_NAME) %>%
                          # regroup by Year and sum value and volume columns
                          group_by(YEAR) %>%
                          summarise(across(where(is.numeric), sum),
                                    .groups = 'drop')),
              summarize_landings_yr_spp(com_landings, species)) 
  
  return(combined_data)
}
calculate_mlti <- function(species, exports = F, imports = F) {
  # this function calculates the multi-lateral Lowe trade index (MLTI) among
    # the top 9 trading countries for a given species, either for imports
    # or exports
  # species is a character vector of a species of interest
  # exports is logical that specifies if the MLTI is an export index
  # imports is logical that specifies if the MLTI is an import index
  
  # stop function if exports or imports are not specified
  if (exports == F & imports == F) {
    stop('Please set either "exports" or "imports" to "T"')
  }
  
  # coerce species to uppercase to match data formatting
  species <- toupper(species)
  
  # set value and volume to class of type symbol, specify if the value and 
    # volume are export or import
  which_value <- as.symbol(ifelse(exports == T, 'EXP_VALUE_2024USD',
                                  'IMP_VALUE_2024USD'))
  which_volume <- as.symbol(ifelse(exports == T, 'EXP_VOLUME_KG',
                                   'IMP_VOLUME_KG'))
  # set value and volume to type quosure (see RLang package for details)
  which_value <- rlang::enquo(which_value)
  which_volume <- rlang::enquo(which_volume)
  
  # if a species is specified, find the level of the classification hierarchy
    # in which it resides
  if (species != 'ALL SPECIES') {
    which_level <- as.symbol(
      ifelse(species %in% unique(trade_data$ECOLOGICAL_CATEGORY), 
             'ECOLOGICAL_CATEGORY',
             ifelse(species %in% unique(trade_data$SPECIES_CATEGORY), 
                    'SPECIES_CATEGORY',
                    ifelse(species %in% unique(trade_data$SPECIES_GROUP), 
                           'SPECIES_GROUP',
                           'SPECIES_NAME')))
    )
    # coerce the level to be quosure
    which_level <- rlang::enquo(which_level)
    
    # step 1: filter trade data for species of interest
    spp_data <- trade_data %>%
      filter_species(species) %>%
      # do not include absent values
      filter(is.na(!!which_value) == F)
    
    # step 2: calculate the average price per year per country
    summary_spp_data <- spp_data %>%
      select(YEAR, COUNTRY_NAME, !!which_group, !!which_value,
             !!which_volume) %>%
      group_by(YEAR, COUNTRY_NAME, !!which_group) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      filter(!!which_volume > 0) %>%
      mutate(PRICE = !!which_value / !!which_volume)
    
  } else if (species == 'ALL SPECIES') {
    # alternative: if no species is selected
    # same steps as before except no species is selected
    spp_data <- trade_data %>%
      filter(is.na(!!which_value) == F)
    
    summary_spp_data <- spp_data %>%
      select(YEAR, COUNTRY_NAME, !!which_value, !!which_volume) %>%
      group_by(YEAR, COUNTRY_NAME) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      filter(!!which_volume > 0) %>%
      mutate(PRICE = !!which_value / !!which_volume)
  }
  
  # step 3: count the number of years and the number of countries represented
  total_years <- length(unique(summary_spp_data$YEAR))
  total_countries <- length(unique(summary_spp_data$COUNTRY_NAME))
  
  # step 4: sum the average prices across all countries
  average_price <- summary_spp_data %>%
    select(!c(YEAR, !!which_value, !!which_volume)) %>%
    group_by(COUNTRY_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    summarise(across(where(is.numeric), sum)) 
  
  # step 5: calculate the overall average price by dividing step 4's output
    # by the product of the number of years and the number of countries
  average_price <- average_price$PRICE / (total_years * total_countries)
  
  # step 6: find top 9 trading partners by value during most recent year (2024)
  top9 <- summary_spp_data %>%
    filter(YEAR == 2024) %>%
    group_by(COUNTRY_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    arrange(-!!which_value) %>%
    top_n(9, !!which_value)
  
  # step 7: set base country as the middle (fifth) country in the list
    # the list is arranged by value
  base_country <- top9$COUNTRY_NAME[5]
  # output trading partners from first year of period (2004)
  trade_nations <- summary_spp_data %>%
    filter(YEAR == 2004) %>%
    select(COUNTRY_NAME) %>%
    distinct() 
  
  # make sure that the base country was a trade partner in 2004
    # if it is not, set base country as the fourth listed country
      # if that is not, set base country as the third listed country
    # this is a band-aid solution
  if (base_country %in% trade_nations$COUNTRY_NAME) {} else {
    base_country <- top9$COUNTRY_NAME[4]
    if (base_country %in% trade_nations$COUNTRY_NAME) {} else {
      base_country <- top9$COUNTRY_NAME[3]
    }
  } 
  
  # step 8: calculate the Q-index of the base country in 2004
    # the Q-index is the base country's trade volume in the base year multiplied
    # by the average price calculated in step 5; in other words, it is the
    # normalized value of the traded volume determined by the average price
    # of the traded product during the time period by all trading partners
  base_country_q <- summary_spp_data %>%
    filter(YEAR == 2004,
           COUNTRY_NAME == base_country) %>%
    mutate(Q_INDEX = !!which_volume * average_price)
  
  # set this value as the index base
  index_base <- base_country_q$Q_INDEX
  
  # step 9: calculate the MLTI for the top 9 countries throughout the time period
    # the MLTI is each country's Q-index divided by the index base, or the base
    # country's Q-index during the base year
  mlti_data <- summary_spp_data %>%
    filter(COUNTRY_NAME %in% top9$COUNTRY_NAME) %>%
    mutate(Q_INDEX = !!which_volume * average_price) %>%
    select(YEAR, COUNTRY_NAME, Q_INDEX) %>%
    mutate(MLTI = Q_INDEX / index_base)
  
  return(mlti_data)
}
calculate_mlti_table <- function(species, exports = F, imports = F) {
  # this function is identical to calculate_mlti save for one major difference:
    # it calculates the top 5 countries rather than the top 9; this enables
    # a more concise table to be outputted for the app
  # see calculate_mlti for notes on this function
  if (exports == F & imports == F) {
    stop('Please set either "exports" or "imports" to "T"')
  }
  
  species <- toupper(species)
  
  which_value <- as.symbol(ifelse(exports == T, 'EXP_VALUE_2024USD',
                                  'IMP_VALUE_2024USD'))
  which_volume <- as.symbol(ifelse(exports == T, 'EXP_VOLUME_KG',
                                   'IMP_VOLUME_KG'))
  which_value <- rlang::enquo(which_value)
  which_volume <- rlang::enquo(which_volume)
  
  if (species != 'ALL SPECIES') {
    which_group <- as.symbol(
      ifelse(species %in% unique(trade_data$ECOLOGICAL_CATEGORY), 
             'ECOLOGICAL_CATEGORY',
             ifelse(species %in% unique(trade_data$SPECIES_CATEGORY), 
                    'SPECIES_CATEGORY',
                    ifelse(species %in% unique(trade_data$SPECIES_GROUP), 
                           'SPECIES_GROUP',
                           'SPECIES_NAME')))
    )
    which_group <- rlang::enquo(which_group)
    
    spp_data <- trade_data %>%
      filter_species(species) %>%
      filter(is.na(!!which_value) == F) 
    
    summary_spp_data <- spp_data %>%
      select(YEAR, COUNTRY_NAME, !!which_group, !!which_value,
             !!which_volume) %>%
      group_by(YEAR, COUNTRY_NAME, !!which_group) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      filter(!!which_volume > 0) %>%
      mutate(PRICE = !!which_value / !!which_volume)
    
  } else if (species == 'ALL SPECIES') {
    spp_data <- trade_data %>%
      filter(is.na(!!which_value) == F)
    
    summary_spp_data <- spp_data %>%
      select(YEAR, COUNTRY_NAME, !!which_value, !!which_volume) %>%
      group_by(YEAR, COUNTRY_NAME) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      filter(!!which_volume > 0) %>%
      mutate(PRICE = !!which_value / !!which_volume)
  }
  
  total_years <- length(unique(summary_spp_data$YEAR))
  total_countries <- length(unique(summary_spp_data$COUNTRY_NAME))
  
  
  average_price <- summary_spp_data %>%
    select(!c(YEAR, !!which_value, !!which_volume)) %>%
    group_by(COUNTRY_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    summarise(across(where(is.numeric), sum)) 
  
  average_price <- average_price$PRICE / (total_years * total_countries)
  
  top9 <- summary_spp_data %>%
    filter(YEAR == 2024) %>%
    group_by(COUNTRY_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    arrange(-!!which_value) %>%
    top_n(5, !!which_value)
  
  base_country <- top9$COUNTRY_NAME[3]
  trade_nations <- summary_spp_data %>%
    filter(YEAR == 2004) %>%
    select(COUNTRY_NAME) %>%
    distinct() 
  
  base_country_q <- summary_spp_data %>%
    filter(YEAR == 2004,
           COUNTRY_NAME == base_country) %>%
    mutate(Q_INDEX = !!which_volume * average_price)
  
  index_base <- base_country_q$Q_INDEX
  
  mlti_data <- summary_spp_data %>%
    filter(COUNTRY_NAME %in% top9$COUNTRY_NAME) %>%
    mutate(Q_INDEX = !!which_volume * average_price) %>%
    select(YEAR, COUNTRY_NAME, Q_INDEX) %>%
    mutate(MLTI = Q_INDEX / index_base) %>%
    select(!Q_INDEX) %>%
    mutate(COUNTRY_NAME = str_to_title(COUNTRY_NAME)) %>%
    rename(Year = YEAR) %>%
    pivot_wider(names_from = COUNTRY_NAME,
                values_from = MLTI) %>%
    mutate(Year = as.character(Year))
  
  return(mlti_data)
}
calculate_hi <- function(species) {
  # this function calculates the herfindahl trade index for a species of interest
  # species is a character vector of a species of interest
  
  # if no species provided
  if(species == 'All Species') {
    # calculate index from trade data
    hi_data <- trade_data %>%
      # select only columns of interest
      select(YEAR, COUNTRY_NAME, EXP_VALUE_2024USD, IMP_VALUE_2024USD) %>%
      # set export and import NAs to 0 to prevent NA as sum values
      mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD) == T,
                                        0, EXP_VALUE_2024USD),
             IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD) == T,
                                        0, IMP_VALUE_2024USD)) %>%
      # sum the total value by each country in each year
      group_by(YEAR, COUNTRY_NAME) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      group_by(YEAR) %>%
      # for each year,
        # step 1: sum the export and import value
        # step 2: calculate the proportion of export and import value for each
          # country
        # step 3: square the proportion of export and import value
        # step 4: sum the squares to calculate the HI for exports and imports
      mutate(TOTAL_EXP_VALUE_YR = sum(EXP_VALUE_2024USD),
             TOTAL_IMP_VALUE_YR = sum(IMP_VALUE_2024USD),
             PROPORT_EXP_VALUE = EXP_VALUE_2024USD / TOTAL_EXP_VALUE_YR,
             PROPORT_IMP_VALUE = IMP_VALUE_2024USD / TOTAL_IMP_VALUE_YR,
             PROPORT_EXP_SQUARED = PROPORT_EXP_VALUE^2,
             PROPORT_IMP_SQUARED = PROPORT_IMP_VALUE^2,
             EXP_HI = sum(PROPORT_EXP_SQUARED),
             IMP_HI = sum(PROPORT_IMP_SQUARED)) %>%
      # retain year and HI's of exports and imports
      select(YEAR, EXP_HI, IMP_HI) %>%
      # remove duplicate columns so there is one of each per year
      distinct()
    
    return(hi_data)
  }
  
  # duplicate the above steps, except now filter for species of interest
  hi_data <- trade_data %>%
    filter_species(species) %>%
    select(YEAR, COUNTRY_NAME, EXP_VALUE_2024USD, IMP_VALUE_2024USD) %>%
    mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD) == T,
                                      0, EXP_VALUE_2024USD),
           IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD) == T,
                                      0, IMP_VALUE_2024USD)) %>%
    group_by(YEAR, COUNTRY_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    group_by(YEAR) %>%
    mutate(TOTAL_EXP_VALUE_YR = sum(EXP_VALUE_2024USD),
           TOTAL_IMP_VALUE_YR = sum(IMP_VALUE_2024USD),
           PROPORT_EXP_VALUE = EXP_VALUE_2024USD / TOTAL_EXP_VALUE_YR,
           PROPORT_IMP_VALUE = IMP_VALUE_2024USD / TOTAL_IMP_VALUE_YR,
           PROPORT_EXP_SQUARED = PROPORT_EXP_VALUE^2,
           PROPORT_IMP_SQUARED = PROPORT_IMP_VALUE^2,
           EXP_HI = sum(PROPORT_EXP_SQUARED),
           IMP_HI = sum(PROPORT_IMP_SQUARED)) %>%
    select(YEAR, EXP_HI, IMP_HI) %>%
    distinct()
  
  return(hi_data)
}
calculate_supply_metrics <- function(species) {
  # this function calculates three metrics that we visualize:
    # apparent supply, apparent supply relative to domestic production, and
    # unexported domestic production relative to apparent supply
  # the function relies on summarize_yr_spp for data formatting
  # species is a character vector of a species of interest
  data <- summarize_yr_spp(species) %>%
    # calculate apparent supply by summing domestic production and imports 
      # and subtracting export volume
    # calculate apparent supply relative to domestic production by dividing
      # apparent supply by domestic production
    # calculate unexported domestic production relative to apparent supply by
      # dividing the absolute value of the difference of domestic production and
      # export volume by apparent supply
    mutate(APPARENT_SUPPLY = (PP_VOLUME_MT - EXP_VOLUME_MT) + IMP_VOLUME_MT,
           APPARENT_SUPPLY_REL_US_PROD = APPARENT_SUPPLY / PP_VOLUME_MT,
           UNEXPORTED_US_PROD_REL_APPARENT_SUPPLY = 
             abs(PP_VOLUME_MT - EXP_VOLUME_MT) / APPARENT_SUPPLY) 
  
  # if no species is provided, add column for species to be 'ALL SPECIES'
  if(species == 'All Species') {
    data <- data %>%
      mutate(SPECIES = 'All Species')
    
    return(data)
  } else {
    # otherwise rename the column specifying the species to 'SPECIES'
    data <- data %>%
      rename(SPECIES = 2)
    
    return(data)
  }
}

# plot functions
plot_trade <- function(data, plot_format, export = F, import = F, species) {
  # this function has the power to generate multiple plot types of trade data
  # data is formatted trade data from summarize_trade_yr_spp
  # plot_format is a character vector that currently accepts these inputs:
    # 'VALUE', 'VOLUME', 'PRICE', 'BALANCE', 'RATIO'
  # export is logical that specifies if the output should be for export data
  # import is logical that specifies if the output should be for import data
 
  
  # if both export and import are true, output is Net Export data
  if (export == T & import == T) {
    # calculate net export value in billions/millions, and net export volume
    data <- data %>%
      mutate(NET_VALUE_2024USD_BILLIONS = 
               EXP_VALUE_2024USD_BILLIONS - IMP_VALUE_2024USD_BILLIONS,
             NET_VALUE_2024USD_MILLIONS = 
               EXP_VALUE_2024USD_MILLIONS - IMP_VALUE_2024USD_MILLIONS,
             NET_VOLUME_MT = EXP_VOLUME_MT - IMP_VOLUME_MT,
             NET_PRICE = EXP_PRICE_USD_PER_KG - IMP_PRICE_USD_PER_KG)
    
    # set shortform and longform values for plot labeling
    shortform <- 'NET'
    longform <- 'Net Export'
  }
  
  # set shortform and longform values for plot labeling if export
  if (export == T & import == F) {
    shortform <- 'EXP'
    longform <- 'Exports'
  }
  # set shortform and longform values for plot labeling if import
  if (import == T & export == F) {
    shortform <- 'IMP'
    longform <- 'Imports'
  }
  # coerce plot_format to uppercase to work within function
  plot_format <- toupper(plot_format)
  
  # set labels and y values for plots of VALUE
  if (plot_format == 'VALUE') {
    # y <- as.symbol(paste0(shortform, '_VALUE_2024USD_BILLIONS'))
    y <- as.symbol(paste0(shortform, '_VALUE_2024USD_MILLIONS'))
    y <- rlang::enquo(y)
    # label <- label_currency(suffix = 'B')
    label <- label_currency(suffix = 'M')
    # ylab <- paste0('Total ', longform, ' Value (Real 2024 USD)')
    ylab <- 'Millions (Real 2024 USD)'
    tlab <- 'Value'
  }
  
  # set labels and y values for plots of VOLUME
  if (plot_format == 'VOLUME') {
    y <- as.symbol(paste0(shortform, '_VOLUME_MT'))
    y <- rlang::enquo(y)
    label <- comma
    # ylab <- paste0('Total ', longform, ' Volume (Metric Tons)')
    ylab <- 'Metric Tons'
    tlab <- 'Volume'
  }
  
  # set labels and y values for plots of PRICE
  if (plot_format == 'PRICE') {
    y <- as.symbol(paste0(shortform, '_PRICE_USD_PER_KG'))
    y <- rlang::enquo(y)
    label <- label_currency(suffix = '/kg')
    ylab <- 'Average Price (Real 2024 USD)'
  }
  
  # plots of VALUE and VOLUME
  if (plot_format %in% c('VALUE', 'VOLUME')) {
    plot <- 
      ggplot(data = data,
             aes(x = factor(YEAR),
                 # call for unique y value set earlier (see RLang)
                 y = !!y)) + 
      geom_col(fill = 'black') +
      scale_x_discrete(breaks = seq(2006, 2022, by = 4),
                       limits = factor(2004:2024)) +
      scale_y_continuous(labels = label) +
      labs(x = '',
           y = ylab,
           title = paste0(species, ' ', longform)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            plot.title = element_text(size = 18),
            axis.title = element_text(size = 15))
  } else if (plot_format == 'PRICE') {
    # plot of PRICE
    # PRICE is a line chart, so we need a column to group by
    data$GROUP <- 'group'
    
    plot <- 
      ggplot(data = data,
             aes(x = factor(YEAR),
                 y = !!y)) +
      geom_line(aes(group = GROUP),
                color = 'black',
                linewidth = 1.5) +
      geom_point(color = 'black',
                 size = 2) +
      scale_x_discrete(breaks = seq(2006, 2022, by = 4),
                       limits = factor(2004:2024)) +
      scale_y_continuous(labels = label) +
      labs(x = '',
           y = ylab,
           title = paste0(species, ' ', longform)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            plot.title = element_text(size = 18),
            axis.title = element_text(size = 15))
  } else if (plot_format == 'RATIO') {
    # plot of RATIO
    # RATIO is a line chart, so we need a column to group by
    data$GROUP <- 'group'
    
    plot <- 
      ggplot(data = data, 
             aes(x = factor(YEAR),
                 # calculate export / import volume ratio here
                 y = (EXP_VOLUME_MT / IMP_VOLUME_MT))) +
      geom_line(aes(group = GROUP),
                color = 'black',
                linewidth = 1.5) +
      geom_point(color = 'black',
                 size = 2) +
      scale_x_discrete(breaks = seq(2006, 2022, by = 4),
                       limits = factor(2004:2024)) +
      labs(x = '', 
           y = 'Export / Import',
           title = paste0('Volume Ratio of ', species)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            plot.title = element_text(size = 18),
            axis.title = element_text(size = 15))
  } else {
    # plot of BALANCE
    # create trade balance data by including both export and import data
    # rename value to exports and imports for display of groups on plot
    balance_data <- data %>%
      # rename(EXPORTS = EXP_VALUE_2024USD_BILLIONS,
      #        IMPORTS = IMP_VALUE_2024USD_BILLIONS) %>%
      rename(EXPORTS = EXP_VALUE_2024USD_MILLIONS,
             IMPORTS = IMP_VALUE_2024USD_MILLIONS) %>%
      select(YEAR, EXPORTS, IMPORTS) %>%
      # calculate trade balance value
      mutate(TRADE_BALANCE = EXPORTS - IMPORTS) %>%
      # pivot longer so there are three groups: exports, imports, and balance
      pivot_longer(cols = c(EXPORTS, IMPORTS, TRADE_BALANCE)) %>%
      # factor the column storing the groups
      mutate(name = as.factor(name))
    
    plot <- 
      ggplot(data = balance_data,
             aes(x = factor(YEAR),
                 y = value)) +
      geom_bar(aes(fill = name),
               stat = 'identity',
               position = 'dodge') +
      labs(x = '',
           # y = 'Billions (Real 2024 USD)',
           y = 'Millions (Real 2024 USD)',
           fill = '',
           title = paste0('Value Balance of ', species)) +
      scale_fill_discrete(labels = c('Exports',
                                     'Imports',
                                     'Trade Balance')) +
      coord_axes_inside(labels_inside = T) +
      scale_x_discrete(limits = factor(2004:2024)) +
      scale_y_continuous(labels = label_currency()) +
      geom_hline(yintercept = 0, color = 'black') +
      theme_minimal() +
      theme(legend.position = 'top',
            axis.line.y = element_line(color = 'black'),
            axis.text.x = element_text(hjust = 0.8,
                                       size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(vjust = 23,
                                        size = 15),
            legend.text = element_text(size = 15),
            plot.title = element_text(size = 18),
            plot.background = element_rect(fill = 'white',
                                           color = 'white'),
            panel.grid = element_blank(),
            plot.margin = margin(5.5, 5.5, 5.5, 75.5, 'points'))
  }
  
  return(plot)
}
plot_trade_ctry_yr_spp <- function(data, value = F, volume = F, species) {
  # this function plots trade among the top five trading partners for a species
    # using data generated by summarize_trade_ctry_yr_spp
  # value is logical that specifies if the data is formatted for value
  # volume is logical that specifies if the data is formatted for volume
  
  # stop function if neither value nor volume are true
  if (value == F & volume == F) {
    stop('Please specify which plot to create by setting either value or volume to T')
  }
  # stop function if both value and volume are true
  if (value == T & volume == T) {
    stop('Please specify only one plot to create')
  }
  
  # set plot labels for value plot
  if (value == T) {
    # y <- as.symbol('NET_VALUE_2024USD_BILLIONS')
    y <- as.symbol('NET_VALUE_2024USD_MILLIONS')
    y <- rlang::enquo(y)
    # label <- label_currency(suffix = 'B')
    label <- label_currency(suffix = 'M')
    # ylab <- 'Net Export Value (Real 2024 USD, Billions)'
    ylab <- 'Millions (Real 2024 USD)'
  } else {
    # set plot labels for volume plot
    y <- as.symbol('NET_VOLUME_MT')
    y <- rlang::enquo(y)
    label <- comma
    ylab <- 'Net Export Volume (Metric Tons)'
  }
  
  ggplot(data = data,
         aes(x = factor(gsub(' ', '\n', COUNTRY_NAME)),
             y = !!y, 
             fill = factor(YEAR))) +
    geom_col(position = 'dodge') +
    scale_fill_nmfs(palette = 'oceans') +
    labs(x = '',
         y = ylab,
         fill = 'Year',
         title = paste0('Net Export Value for Top 5 Trading Partners \nof ', 
                        species)) +
    scale_y_continuous(labels = label) +
    theme_bw() +
    geom_hline(yintercept = 0, 'black') +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 18))
}
plot_spp_pp <- function(processed_product_data, plot.format, species) {
  # function that plots processed product data 
  # processed_product_data is data formatted by summarize_pp_yr_spp
  # plot.format is a character vector of three inputs:
    # VALUE, VOLUME, and PRICE
  
  # coerce plot.format to uppercase to work within function
  plot.format <- toupper(plot.format)
  
  # we group product conditions of low proportions in the data (less than 2%)
    # to a subgroup called 'OTHER*'
  # find the low proportion types by value and volume and combine
  low_prop_types_value <- processed_product_data %>% 
    select(PP_VALUE_BILLIONS_2024USD, PRODUCT_NAME) %>%
    group_by(PRODUCT_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    mutate(TOTAL_VALUE = sum(PP_VALUE_BILLIONS_2024USD),
           VALUE_SHARE = PP_VALUE_BILLIONS_2024USD / TOTAL_VALUE) %>%
    filter(VALUE_SHARE < 0.02) %>%
    select(PRODUCT_NAME)
  
  low_prop_types_volume <- processed_product_data %>%
    select(PP_VOLUME_MT, PRODUCT_NAME) %>%
    group_by(PRODUCT_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    mutate(TOTAL_VOLUME = sum(PP_VOLUME_MT),
           VOLUME_SHARE = PP_VOLUME_MT / TOTAL_VOLUME) %>%
    filter(VOLUME_SHARE < 0.02) %>%
    select(PRODUCT_NAME)
  
  low_prop_types <- bind_rows(low_prop_types_value, low_prop_types_volume) %>%
    distinct() %>%
    pull(PRODUCT_NAME)
  
  # rename these low proportion types as 'OTHER*' and re-summarise
  new_data <- processed_product_data %>%
    mutate(PRODUCT_NAME = ifelse(PRODUCT_NAME %in% c('OTHER', low_prop_types),
                                 'OTHER*', PRODUCT_NAME)) %>%
    group_by(YEAR, PRODUCT_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    mutate(PP_PRICE_2024USD_PER_KG = PP_VALUE_2024USD / PP_VOLUME_KG,
           PP_VOLUME_THOUSAND_MT = PP_VOLUME_MT / 1000,
           PRODUCT_NAME = factor(PRODUCT_NAME))
  
  # set labels for VALUE plots
  if (plot.format == 'VALUE') {
    # y <- as.symbol('PP_VALUE_BILLIONS_2024USD')
    y <- as.symbol('PP_VALUE_MILLIONS_2024USD')
    y <- rlang::enquo(y)
    # ylab <- 'Value (Billions, 2024 Real USD)'
    ylab <- 'Millions (2024 Real USD)'
    # label <- label_currency(suffix = 'B')
    label <- label_currency(suffix = 'M')
    tlab <- 'Production Value of '
    
    # calculate the total value per year to find upper limit
    yr_value <- new_data %>%
      # select(YEAR, PP_VALUE_BILLIONS_2024USD) %>%
      select(YEAR, PP_VALUE_MILLIONS_2024USD) %>%
      group_by(YEAR) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop')
    
    # ylim <- max(yr_value$PP_VALUE_BILLIONS_2024USD)
    ylim <- max(yr_value$PP_VALUE_MILLIONS_2024USD + 5)
  }
  
  if (plot.format == 'VOLUME') {
    # set labels for VOLUME plots
    y <- as.symbol('PP_VOLUME_THOUSAND_MT')
    y <- rlang::enquo(y)
    ylab <- 'Metric Tons (Thousands)'
    label <- comma
    tlab <- 'Production Volume of '
    
    # calculate the total value per year to find upper limit
    yr_volume <- new_data %>%
      select(YEAR, PP_VOLUME_THOUSAND_MT) %>%
      group_by(YEAR) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') 
    
    ylim <- max(yr_volume$PP_VOLUME_THOUSAND_MT + 1)
  }
  
  if (plot.format == 'PRICE') {
    # because price is a line chart rather than a bar (as VALUE and VOLUME are),
      # just create plot for PRICE instead of setting label definitions
    plot <- ggplot(data = new_data,
                   aes(x = factor(YEAR),
                       y = PP_PRICE_2024USD_PER_KG,
                       color = PRODUCT_NAME)) +
      geom_line(aes(group = PRODUCT_NAME),
                linewidth = 1.5) +
      geom_point(color = 'black',
                 size = 1) +
      scale_color_manual(values = colors,
                        name = 'Product Condition') +
      labs(x = '',
           y = 'Average Price (Real 2024 USD)',
           fill = 'Product Condition',
           title = paste0('Production Price of ', species)) +
      scale_x_discrete(breaks = seq(2006, 2022, by = 4)) +
      scale_y_continuous(limits = c(0, max(new_data$PP_PRICE_2024USD_PER_KG) + 0.5),
                         expand = c(0, 0),
                         labels = label_currency(suffix = '/kg')) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 15),
            plot.title = element_text(size = 18))
      
    return(plot)
  }
  
  # plot for VALUE or VOLUME depending on plot.format
  plot <- ggplot(data = new_data,
                 aes(x = factor(YEAR),
                     y = !!y,
                     fill = PRODUCT_NAME)) +
    geom_col(position = 'stack') +
    scale_fill_manual(values = colors,
                      name = 'Product Condition') +
    labs(x = '',
         y = ylab,
         fill = 'Product Condition',
         title = paste0(tlab, species)) +
    scale_x_discrete(breaks = seq(2006, 2022, by = 4)) +
    scale_y_continuous(limits = c(0, ylim), 
                       expand = c(0, 0),
                       labels = label) +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 15),
          plot.title = element_text(size = 18))
  
  return(plot)
}
plot_landings <- function(data, plot.format, species) {
  # this function plots landings data formatted by summarize_landings_yr_spp
  # plot.format is a character vector that accepts inputs of VALUE, VOLUME
    # and PRICE
  
  # coerce plot.format to uppercase to work within function
  plot.format <- toupper(plot.format)
  
  # set labels for VALUE plot
  if (plot.format == 'VALUE') {
    # y <- as.symbol('COM_VALUE_BILLIONS_2024USD')
    y <- as.symbol('COM_VALUE_MILLIONS_2024USD')
    y <- rlang::enquo(y)
    
    # label <- label_currency(suffix = 'B')
    label <- label_currency(suffix = 'M')
    # ylab <- 'Total Landed Value (Billions, Real 2024 USD)'
    ylab <- 'Millions (Real 2024 USD)'
    tlab <- 'Ex-Vessel Value of '
  }
  
  # set labels for VOLUME plot
  if (plot.format == 'VOLUME') {
    y <- as.symbol('COM_VOLUME_THOUSAND_MT')
    y <- rlang::enquo(y)
    
    # format metric tons by thousands
    data$COM_VOLUME_THOUSAND_MT <- data$COM_VOLUME_MT / 1000
    
    label <- comma
    ylab <- 'Metric Tons (Thousands)'
    tlab <- 'Landed Volume of '
  }
  
  # create plot for PRICE (this is a line chart which contrasts with VALUE and
    # VOLUME bar charts)
  if (plot.format == 'PRICE') {
    # create GROUP column for the line chart to GROUP by
    data$GROUP <- 'group'
    
    plot <- 
      ggplot(data = data,
             aes(x = factor(YEAR),
                 y = COM_PRICE_2024USD_PER_KG)) +
      geom_line(aes(group = GROUP),
                color = 'black',
                linewidth = 1.5) +
      geom_point(color = 'black',
                 size = 2) +
      scale_x_discrete(breaks = seq(2006, 2022, by = 4),
                       limits = factor(2004:2023)) +
      scale_y_continuous(labels = label_currency(suffix = '/kg')) +
      labs(x = '',
           y = 'Average Price (Real 2024 USD)',
           title = paste0('Ex-Vessel Price of ', species)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 18))
    
    return(plot)
  }
  
  # output plot of VALUE or VOLUME
  plot <- 
    ggplot(data = data,
           aes(x = factor(YEAR),
               y = !!y)) +
    geom_col(fill = 'black') +
    scale_x_discrete(breaks = seq(2006, 2022, by = 4),
                     limits = factor(2004:2023)) +
    scale_y_continuous(labels = label) +
    labs(x = '',
         y = ylab,
         title = paste0(tlab, species)) +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          plot.title = element_text(size = 18))
  
  return(plot)
}
plot_mlti <- function(mlti_data, exports = F, imports = F, species) {
  # this function generates a grid of plots that display MLTI data
  # mlti_data is a data set formatted by calculate_mlti
  # exports is logical that reflects if the data input is for exports
  # imports is logical that reflects if the data input is for imports
  
  # stop function if neither exports nor imports were specified
  if (exports == F & imports == F) {
    stop('Please set "exports" or "imports" to "T"')
  }
  
  # set label for plot based on exports logical
  label <- ifelse(exports == T, 'Export', 'Import')
  
  ggplot(data = mlti_data,
         aes(x = factor(YEAR),
             y = MLTI)) +
    geom_point() +
    facet_wrap( ~ factor(COUNTRY_NAME), nrow = 3) +
    scale_x_discrete(breaks = seq(2006, 2022, by = 4)) +
    # hline sets baseline to compare points from base index for all plots
    geom_hline(yintercept = 1, color = 'black') +
    labs(x = '',
         y = paste0('Multilateral ', label, ' Quantity Index of ', species)) +
    theme_bw() +
    theme(axis.text = element_text(size = 15),
          axis.title.y = element_text(size = 20),
          strip.text = element_text(size = 15,
                                    color = 'white'),
          strip.background = element_rect(fill = 'black'))
}
plot_hi <- function(hi_data, species) {
  # this function generates a line plot that compares HI for exports and imports
  # hi_data is a data set formatted by calculate_hi
  
  # format the data by renaming columns for plot labels
  format_hi_data <- hi_data %>%
    rename(Exports = EXP_HI,
           Imports = IMP_HI) %>%
    # pivot the plot longer to create a grouping column by export or import
    pivot_longer(cols = c(Exports, Imports))
  
  ggplot(data = format_hi_data,
         aes(x = as.factor(YEAR),
             y = value)) +
    # group lines by the pivoted longer column 'name'
    geom_line(aes(group = name, 
                  colour = name),
              linewidth = 1.5) +
    geom_point(size = 2,
               color = 'black') +
    scale_color_discrete(name = '') +
    labs(x = '',
         y = 'Index',
         title = paste0('Herfindahl Index of \n', species)) +
    scale_x_discrete(breaks = seq(2006, 2022, by = 4)) +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.position = 'top',
          plot.title = element_text(size = 18))
  
}
plot_supply_metrics <- function(supply_data, metric, species) {
  # this function generates three types of plots 
  # supply_data is data formatted by calculate_supply_metrics in tandem with
    # summarize_yr_spp
  # metric is a character vector of three accepted inputs: 
    # SUPPLY, RATIO, AND SHARE
    # SUPPLY outputs plots of apparent supply 
    # RATIO outputs plots of apparent supply relative to domestic production
    # SHARE outputs plots of Unexported domestic production relative to 
      # apparent supply
  
  if (metric == 'SUPPLY') {
    plot <- 
      ggplot(data = supply_data %>%
               # we do not have landings or processing data for 2024 despite
                # having so for trade data
               filter(YEAR < 2024),
             aes(x = factor(YEAR),
                 # divided by 1000 for thousand metric tons (volume metric)
                 y = APPARENT_SUPPLY / 1000)) +
      geom_col(fill = 'black') +
      labs(x = '',
           y = 'Metric Tons (Thousands)',
           title = paste0('Apparent Supply of \n', species)) +
      scale_x_discrete(limits = factor(c(2004:2023)),
                       breaks = seq(2006, 2022, by = 4)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 18))
  }
  
  if (metric == 'RATIO') {
    plot <- 
      ggplot(data = supply_data %>%
               filter(YEAR < 2024),
             aes(x = factor(YEAR),
                 y = APPARENT_SUPPLY_REL_US_PROD,
                 group = SPECIES)) +
      geom_point(color = 'black', 
                 size = 3) +
      geom_line(color = 'black',
                linewidth = 1) +
      labs(x = '',
           y = 'Ratio',
           title = paste0('Apparent Supply of \n', species, 
                          '\nRelative to Domestic \nProduction')) +
      scale_x_discrete(limits = factor(c(2004:2023)),
                       breaks = seq(2006, 2022, by = 4)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 18))
  }
  
  if (metric == 'SHARE') {
    plot <- 
      ggplot(data = supply_data %>%
               filter(YEAR < 2024),
             aes(x = factor(YEAR),
                 y = UNEXPORTED_US_PROD_REL_APPARENT_SUPPLY)) +
      geom_col(fill = 'black') +
      labs(x = '',
           y = 'Share of Apparent Supply',
           title = paste0('Unexported Domestic \nProduction Relative \nto Apparent Supply of \n', species)) +
      scale_x_discrete(limits = factor(c(2004:2023)),
                       breaks = seq(2006, 2022, by = 4)) +
      scale_y_continuous(labels = label_percent()) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 18))
  }
  
  return(plot)
}

# Colors -----------------------------------------------------------------------
# colors designed primarily for processed products at the moment
colors <- c(nmfs_palette('coral')(6)[6:3], 
            nmfs_palette('waves')(6)[6:2], 
            nmfs_palette('crustacean')(6)[c(6, 4, 2)],
            nmfs_cols()[42:39])

names(colors) <- levels(factor(levels = c(
  'FILLETS', 'STEAKS', 'SURIMI', 'SHUCKED MEATS',
  'CANNED', 'OIL', 'DRESSED', 'SMOKED (EXCL. CANNED)', 'CHOWDERS',
  'FISH STICKS', 'BREADED SHRIMP', 'CAKES/PATTIES',
  'OTHER*', 'OTHER INDUSTRIAL', 'MEAL', 'FISH PORTIONS')))
# App --------------------------------------------------------------------------
# Define UI --------------------------------------------------------------------
ui <- page_sidebar(
  
  sidebar = sidebar(
    title = 'Species Selection', 
    uiOutput('filter_1'),
    # these outputs only appear once a selection is made for the prior input
      # this means filter_4 only appears once filter_3 has input, which only
      # appears once filter_2 has input, etc.
    uiOutput('filter_2'),
    uiOutput('filter_3'),
    uiOutput('filter_4'),
    # these outputs only appear once a selection is not available for a given
      # section (landings, trade, production)
    uiOutput('trade_unfilter_button'),
    uiOutput('product_unfilter_button'),
    uiOutput('landings_unfilter_button'),
    # search bar that outputs directions for how to filter for the searched 
      # species (if available)
    selectizeInput(inputId = 'search_term',
                   label = 'or Search for a Species',
                   choices = NULL),
    # htmlOutput allows us to incorporate page breaks ('<br>')
    htmlOutput('search_term_ecat'),
    # add breaks between text for readability
    br(), br(),
    htmlOutput('search_term_scat'),
    br(), br(),
    htmlOutput('search_term_sgrp'),
    br(), br(),
    htmlOutput('search_term_sname')
  ),
  fluidRow(
    navset_card_pill(title = 'Trade',
                     nav_panel(title = 'Aggregate',
                               fluidRow(
                                 withSpinner(
                                   plotOutput('balance'), 
                                   type = 7)
                               ),
                               fluidRow(
                                 column(
                                   withSpinner(
                                     plotOutput('trade_ratio'), 
                                     type = 7),
                                   width = 6
                                 ),
                                 column(
                                   withSpinner(
                                     plotOutput('top5_trade'), 
                                     type = 7),
                                   width = 6
                                 ))),
                     nav_panel(title = 'Value',
                               fluidRow(
                                 column(
                                   withSpinner(
                                     plotOutput('exp_value'), 
                                     type = 7),
                                   width = 6
                                 ),
                                 column(
                                   withSpinner(
                                     plotOutput('imp_value'), 
                                     type = 7),
                                   width = 6
                                 ))),
                     nav_panel(title = 'Volume',
                               fluidRow(
                                 column(
                                   withSpinner(
                                     plotOutput('exp_volume'), 
                                     type = 7),
                                   width = 6
                                 ),
                                 column(
                                   withSpinner(
                                     plotOutput('imp_volume'), 
                                     type = 7),
                                   width = 6
                                 ))),
                     nav_panel(title = 'Price',
                               fluidRow(
                                 column(
                                   withSpinner(
                                     plotOutput('exp_price'), 
                                     type = 7),
                                   width = 6
                                 ),
                                 column(
                                   withSpinner(
                                     plotOutput('imp_price'), 
                                     type = 7),
                                   width = 6
                                 ))),
                     nav_panel(title = 'Advanced Metrics',
                               fluidRow(
                                 column(
                                   fluidRow(
                                     'Export Multilateral Trade Index'
                                   ),
                                   withSpinner(
                                     tableOutput('exp_mlti_table'), 
                                     type = 7),
                                   # plotOutput('exp_mlti'),
                                   width = 6
                                 ),
                                 column(
                                   fluidRow(
                                     'Import Multilateral Trade Index'
                                   ),
                                   withSpinner(
                                     tableOutput('imp_mlti_table'), 
                                     type = 7),
                                   # plotOutput('imp_mlti'),
                                   width = 6
                                 )),
                               fluidRow(
                                 column(
                                   withSpinner(
                                     plotOutput('hi'), 
                                     type = 7),
                                   width = 3
                                 ),
                                 column(
                                   withSpinner(
                                     plotOutput('supply'), 
                                     type = 7),
                                   width = 3
                                 ),
                                 column(
                                   withSpinner(
                                     plotOutput('supply_ratio'), 
                                     type = 7),
                                   width = 3
                                 ),
                                 column(
                                   withSpinner(
                                     plotOutput('supply_share'), 
                                     type = 7),
                                   width = 3
                                 ))))),
  fluidRow(
    column(
      navset_card_pill(title = 'Commercial Landings',
                       nav_panel(title = 'Value',
                                 withSpinner(
                                   plotOutput('landings_value'),
                                   type = 7)),
                       nav_panel(title = 'Volume',
                                 withSpinner(
                                   plotOutput('landings_volume'),
                                   type = 7)),
                       nav_panel(title = 'Price',
                                 withSpinner(
                                   plotOutput('landings_price'),
                                   type = 7))),
      width = 6
    ),
    column(
      navset_card_pill(title = 'Processed Products',
                       nav_panel(title = 'Value',
                                 withSpinner(
                                   plotOutput('pp_value'),
                                   type = 7)),
                       nav_panel(title = 'Volume',
                                 withSpinner(
                                   plotOutput('pp_volume'),
                                   type = 7)),
                       nav_panel(title = 'Price',
                                 withSpinner(
                                   plotOutput('pp_price'),
                                   type = 7))),
      width = 6)) # ,
  # tags$head(tags$style(HTML('* {font-family: "Gill Sans MT"};')))
  )

# Define server logic ----------------------------------------------------------
server <- function(input, output, session) {
  
  # creates input: ecol_cat
  # filter_1 is always present in the sidebar
  output$filter_1 <- renderUI({
    # grab all ecological categories
    ecol_cats <- c('All Species', com_landings %>% 
                     filter(CONFIDENTIALITY != 'Confidential') %>%
                     select(ECOLOGICAL_CATEGORY) %>%
                     distinct() %>%
                     # remove NA category
                     filter(!is.na(ECOLOGICAL_CATEGORY)) %>%
                     # display strings as titles (first letter capitalized)
                     mutate(ECOLOGICAL_CATEGORY = 
                              str_to_title(ECOLOGICAL_CATEGORY)) %>%
                     pull())
    selectInput('ecol_cat', 'Choose a Category', ecol_cats)
    
  })
  
  # creates input: species_cat
  # filter_2 appears once an ecological category (ecol_cat) is selected
  output$filter_2 <- renderUI({
    # req prevents anything from being run if ecol_cat is not specified
    req(input$ecol_cat != 'All Species')
    # grab all species categories for the selected ecological category
    species_cats <- c('All Species', com_landings %>%
                        filter_species(input$ecol_cat) %>%
                        select(SPECIES_CATEGORY) %>%
                        distinct() %>%
                        # remove NA category
                        filter(!is.na(SPECIES_CATEGORY)) %>%
                        # display strings as titles (first letter capitalized)
                        mutate(SPECIES_CATEGORY = 
                                 str_to_title(SPECIES_CATEGORY)) %>%
                        pull())
    selectInput('species_cat', 'Choose a Secondary Category', species_cats)
  })
  
  # creates input: species_grp
  # filter_3 appears once a species category (species_cat) is selected
  output$filter_3 <- renderUI({
    # req prevents anything from being run if both species_cat AND ecol_cat
      # are not specified
    req(input$species_cat != 'All Species' & input$ecol_cat != 'All Species')
    # grab all species groups for the selected species category
    species_groups <- c('All Species', com_landings %>%
                          filter_species(input$species_cat) %>%
                          select(SPECIES_GROUP) %>%
                          distinct() %>%
                          # remove NA category
                          filter(!is.na(SPECIES_GROUP)) %>%
                          # display strings as titles (first letter capitalized)
                          mutate(SPECIES_GROUP = 
                                   str_to_title(SPECIES_GROUP)) %>%
                          pull())
    selectInput('species_grp', 'Choose a Group', species_groups)
  })
  
  # creates input: species_name
  # filter_4 appears once a species group (species_grp) is selected
  output$filter_4 <- renderUI({
    # req prevents anything from being run if species_cat, ecol_cat, and 
      # species_grp are not selected
    req(input$species_grp != 'All Species' & 
          input$species_cat != 'All Species' & 
          input$ecol_cat != 'All Species')
    # grab all species names for the selected species group
    species_names <- c('All Species', com_landings %>%
                         filter_species(input$species_grp) %>%
                         select(SPECIES_NAME) %>%
                         distinct() %>%
                         # remove NA category
                         filter(!is.na(SPECIES_NAME)) %>%
                         # display strings as titles (first letter capitalized)
                         mutate(SPECIES_NAME = str_to_title(SPECIES_NAME)) %>%
                         pull())
    selectInput('species_name', 'Choose a Species', species_names)
  })
  
  # creates checkbox to unfilter trade up one level
    # requires the selected species to NOT be available in trade categories
  output$trade_unfilter_button <- renderUI({
    req(!(species_selected() %in% trade_terms))
    
    checkboxInput('trade_button', 'Unfilter Trade Plots Up One Level')
  })
  
  # creates checkbox to unfilter production up one level
    # requires the selected species to NOT be available in production categories
  output$product_unfilter_button <- renderUI({
    req(!(species_selected() %in% pp_terms))
    
    checkboxInput('products_button', 'Unfilter Products Plots Up One Level')
  })
  
  # creates checkbox to unfilter landings up one level
    # requires the selected species to NOT be available in landings categories
  output$landings_unfilter_button <- renderUI({
    req(!(species_selected() %in% landings_terms))
    
    checkboxInput('landings_button', 'Unfilter Landings Plots Up One Level')
  })
  
  # define search bar terms
  updateSelectizeInput(session = session,
                       'search_term',
                       choices = 
                         c('', sort(c(com_landings %>%
                                  filter(CONFIDENTIALITY != 'Confidential') %>%
                                  select(ECOLOGICAL_CATEGORY) %>%
                                  distinct() %>%
                                  filter(!is.na(ECOLOGICAL_CATEGORY)) %>%
                                  mutate(ECOLOGICAL_CATEGORY = 
                                           str_to_title(ECOLOGICAL_CATEGORY)) %>%
                                  pull(),
                                com_landings %>%
                                  filter(CONFIDENTIALITY != 'Confidential') %>%
                                  select(SPECIES_CATEGORY) %>%
                                  distinct() %>%
                                  filter(!is.na(SPECIES_CATEGORY)) %>%
                                  mutate(SPECIES_CATEGORY = 
                                           str_to_title(SPECIES_CATEGORY)) %>%
                                  pull(),
                                com_landings %>%
                                  filter(CONFIDENTIALITY != 'Confidential') %>%
                                  select(SPECIES_GROUP) %>%
                                  distinct() %>%
                                  filter(!is.na(SPECIES_GROUP)) %>%
                                  mutate(SPECIES_GROUP = 
                                           str_to_title(SPECIES_GROUP)) %>%
                                  pull(),
                                com_landings %>%
                                  filter(CONFIDENTIALITY != 'Confidential') %>%
                                  select(SPECIES_NAME) %>%
                                  distinct() %>%
                                  filter(!is.na(SPECIES_NAME)) %>%
                                  mutate(SPECIES_NAME = 
                                           str_to_title(SPECIES_NAME)) %>%
                                  pull()))),
                       server = T)
  
  # Display search term categories for user to filter by
  output$search_term_ecat <- renderText({
    # require a search term to be inputted
    req(input$search_term != '')
    # set string to title to match data formatting
    # pull all ecological categories matching the term (there can be multiple)
    term <- str_to_title(as.character(com_landings %>%
                           filter_species(input$search_term) %>%
                           select(ECOLOGICAL_CATEGORY) %>%
                           distinct() %>%
                           pull()))
    
    # use collapse to convert multiple strings into one with ', <br>' separating
    paste('Select the following: <br><br>Ecological Category: <br>', 
          paste(term, collapse = ', <br>'))
  })
  
  # see above notes
  output$search_term_scat <- renderText({
    req(input$search_term != '')
    # require that the search_term appears in this and preceding levels of the
      # organization hierarchy for text to appear
      # This ensures that we only display instructions for filtering up to
      # the level of the desired species input
    req(input$search_term %in% scat_list |
          input$search_term %in% sgrp_list |
          input$search_term %in% sname_list)
    term <- str_to_title(as.character(com_landings %>%
                                        filter_species(input$search_term) %>%
                                        select(SPECIES_CATEGORY) %>%
                                        distinct() %>%
                                        pull()))
    paste('Species Category: <br>',
          paste(term, collapse = ', <br>'))
  })
  
  # see above notes
  output$search_term_sgrp <- renderText({
    req(input$search_term != '')
    req(input$search_term %in% sgrp_list |
          input$search_term %in% sname_list)
    term <- str_to_title(as.character(com_landings %>%
                                        filter_species(input$search_term) %>%
                                        select(SPECIES_GROUP) %>%
                                        distinct() %>%
                                        pull()))
    paste('Species Group: <br>',
          paste(term, collapse = ', <br>'))
  })
  
  # see above notes
  output$search_term_sname <- renderText({
    req(input$search_term != '')
    req(input$search_term %in% sname_list)
    term <- str_to_title(as.character(com_landings %>%
                                        filter_species(input$search_term) %>%
                                        select(SPECIES_NAME) %>%
                                        distinct() %>%
                                        pull()))
    paste('Species Name: <br>',
          paste(term, collapse = ', <br>'))
  })
  
  # sets aside species selected by the user
  species_selected <- reactive({
    ifelse(input$ecol_cat == 'All Species', 'All Species',
           ifelse(input$species_cat == 'All Species', input$ecol_cat,
                  ifelse(input$species_grp == 'All Species', input$species_cat,
                         ifelse(input$species_name == 'All Species', input$species_grp,
                                input$species_name))))
  })
  
  # identifies which species is the next highest level based on if the 
    # selected category is not available from available trade data
  unfilter_species_trade <- reactive({
    req(input$trade_button == T)
    ifelse(input$species_name != '', input$species_grp,
           ifelse(input$species_group != '', input$species_cat,
                  ifelse(input$species_cat != '', input$ecol_cat,
                         NA)))
  })
  
  # determines if the selected species OR the next highest level of categorization
    # (unfilter_species_trade) should be used for trade data visualization based
    # on whether the selected species is available in trade data
  # because unfilter_species_trade requires the trade_button to be checked by
    # the user, this will only switch to unfilter_species_trade once the user
    # checks the box
  species_selection_trade <- reactive({
    ifelse(species_selected() %in% trade_terms, species_selected(),
           unfilter_species_trade())
  })

  # creates trade data
  trade_df <- reactive({
    summarize_trade_yr_spp(
      trade_data,
      species_selection_trade()
      )
    })
  
  # validation reactive; outputs message if species is not available in trade data
  trade_data_validation <- reactive({
    validate(need(try(species_selection_trade() %in% trade_terms),
                  'There is no available trade data for the selected species'))
  })
  
  # creates trade balance plot (value)
  balance_plot <- reactive({
    plot_trade(trade_df(), 'BALANCE', species = species_selection_trade())
  })
  
  # outputs trade balance plot (value)
  output$balance <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(balance_plot())),
                  'Data for this species is insufficient to produce this plot'))
    balance_plot()
  })
  
  # creates export/import ratio plot
  ratio_plot <- reactive({
    plot_trade(trade_df(), 'RATIO', export = T, import = T, 
               species = species_selection_trade())
  })
  
  # outputs export/import ratio plot
  output$trade_ratio <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(ratio_plot())),
                  'Data for this species is insufficient to produce this plot'))
    ratio_plot()
  })
  
  # creates top 5 net export data
  top5_trade_df <- reactive({
    summarize_trade_ctry_yr_spp(
      trade_data,
      species_selection_trade(),
      time.frame = c(2020, 2024),
      value = T)
  })
  
  # creates top 5 net export plot
  top5_trade_plot <- reactive({
    plot_trade_ctry_yr_spp(top5_trade_df(), value = T, 
                           species = species_selection_trade())
  })
  
  # outputs top 5 net export plot
  output$top5_trade <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(top5_trade_plot())),
                  'Data for this species is insufficient to produce this plot'))
    top5_trade_plot()
  })
  
  # creates export value plot
  exp_value_plot <- reactive({
    plot_trade(trade_df(), 'VALUE', export = T, 
               species = species_selection_trade())
  })
  
  # outputs export value plot
  output$exp_value <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(exp_value_plot())),
                  'Data for this species is insufficient to produce this plot'))
    exp_value_plot()
  })
  
  # creates import value plot
  imp_value_plot <- reactive({
    plot_trade(trade_df(), 'VALUE', import = T, 
               species = species_selection_trade())
  })
  
  # outputs import value plot
  output$imp_value <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(imp_value_plot())),
                  'Data for this species is insufficient to produce this plot'))
    imp_value_plot()
  })

  # creates export volume plot
  exp_volume_plot <- reactive({
    plot_trade(trade_df(), 'VOLUME', export = T, 
               species = species_selection_trade())
  })
  
  # outputs export volume plot
  output$exp_volume <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(exp_volume_plot())),
                  'Data for this species is insufficient to produce this plot'))
    exp_volume_plot()
  })

  # creates import volume plot
  imp_volume_plot <- reactive({
    plot_trade(trade_df(), 'VOLUME', import = T, 
               species = species_selection_trade())
  })
  
  # outputs import volume plot
  output$imp_volume <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(imp_volume_plot())),
                  'Data for this species is insufficient to produce this plot'))
    imp_volume_plot()
  })
  
  # creates export price plot
  exp_price_plot <- reactive({
    plot_trade(trade_df(), 'PRICE', export = T, 
               species = species_selection_trade())
  })
  
  # outputs export price plot
  output$exp_price <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(exp_price_plot())),
                  'Data for this species is insufficient to produce this plot'))
    exp_price_plot()
  })
  
  # creates import price plot
  imp_price_plot <- reactive({
    plot_trade(trade_df(), 'PRICE', import = T, 
               species = species_selection_trade())
  })
  
  # outputs import price plot
  output$imp_price <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(imp_price_plot())),
                  'Data for this species is insufficient to produce this plot'))
    imp_price_plot()
  })
  
  # identifies which species is the next highest level based on if the 
    # selected category is not available from available landings data 
  unfilter_species_landings <- reactive({
    req(input$landings_button == T)
    ifelse(input$species_name != '', input$species_grp,
           ifelse(input$species_group != '', input$species_cat,
                  ifelse(input$species_cat != '', input$ecol_cat,
                         NA)))
  })
  
  # determines if the selected species OR the next highest level of categorization
    # (unfilter_species_landings) should be used for landings data visualization 
    # based on whether the selected species is available in landings data
  # because unfilter_species_landings requires the landings_button to be checked by
    # the user, this will only switch to unfilter_species_landings once the user
    # checks the box
  species_selection_landings <- reactive({
    ifelse(species_selected() %in% landings_terms, species_selected(),
           unfilter_species_landings())
  })
  
  # validation reactive; displays message if species is not found in landings data
  landings_data_validation <- reactive({
    validate(need(try(species_selection_landings() %in% landings_terms),
                  'There is no available landings data for this species'))
  })
  
  # creates landings data
  landings_df <- reactive({
    summarize_landings_yr_spp(
      com_landings,
      species_selection_landings())
    })
  
  # creates landings value plot
  landings_value_plot <- reactive({
    plot_landings(landings_df(), 'VALUE', 
                  species = species_selection_landings())
  })
  
  # outputs landings value plot
  output$landings_value <- renderPlot({
    landings_data_validation()
    validate(need(try(!is.na(landings_value_plot())),
                  'Data for this species is insufficient to produce this plot'))
    landings_value_plot()
  })
  
  # creates landings volume plot
  landings_volume_plot <- reactive({
    plot_landings(landings_df(), 'VOLUME', 
                  species = species_selection_landings())
  })
  
  # outputs landings volume plot
  output$landings_volume <- renderPlot({
    landings_data_validation()
    validate(need(try(!is.na(landings_volume_plot())),
                  'Data for this species is insufficient to produce this plot'))
    landings_volume_plot()
  })
  
  # creates landings price plot
  landings_price_plot <- reactive({
    plot_landings(landings_df(), 'PRICE', 
                  species = species_selection_landings())
  })
  
  # outputs landings price plot
  output$landings_price <- renderPlot({
    landings_data_validation()
    validate(need(try(!is.na(landings_price_plot())),
                  'Data for this species is insufficient to produce this plot'))
    landings_price_plot()
  })
  
  # identifies which species is the next highest level based on if the 
    # selected category is not available from available production data 
  unfilter_species_products <- reactive({
    req(input$products_button == T)
    ifelse(input$species_name != '', input$species_grp,
           ifelse(input$species_group != '', input$species_cat,
                  ifelse(input$species_cat != '', input$ecol_cat,
                         NA)))
  })
  
  # determines if the selected species OR the next highest level of categorization
    # (unfilter_species_products) should be used for production data visualization 
    # based on whether the selected species is available in production data
  # because unfilter_species_products requires the products_button to be checked by
    # the user, this will only switch to unfilter_species_products once the user
    # checks the box
  species_selection_products <- reactive({
    ifelse(species_selected() %in% pp_terms, species_selected(),
           unfilter_species_products())
  })
  
  # validation reactive; outputs message if species is not found in production data
  pp_data_validation <- reactive({
    validate(need(try(species_selection_products() %in% pp_terms),
                  'There is no available production data for this species'))
  })
  
  # creates processed products data
  pp_df <- reactive({
    summarize_pp_yr_spp(
      pp_data,
      species_selection_products())
    })
  
  # creates processed products value plot
  pp_value_plot <- reactive({
    plot_spp_pp(pp_df(), 'VALUE', 
                species = species_selection_products())
  })
  
  # outputs processed products value plot
  output$pp_value <- renderPlot({
    pp_data_validation()
    validate(need(try(!is.na(pp_value_plot())),
                  'Data for this species is insufficient to produce this plot'))
    pp_value_plot()
  })
  
  # creates processed products volume plot
  pp_volume_plot <- reactive({
    plot_spp_pp(pp_df(), 'VOLUME', 
                species = species_selection_products())
  })
  
  # outputs processed products volume plot
  output$pp_volume <- renderPlot({
    pp_data_validation()
    validate(need(try(!is.na(pp_volume_plot())),
                  'Data for this species is insufficient to produce this plot'))
    pp_volume_plot()
  })
  
  # creates processed products price plot
  pp_price_plot <- reactive({
    plot_spp_pp(pp_df(), 'PRICE', 
                species = species_selection_products())
  })
  
  # outputs processed products price plot
  output$pp_price <- renderPlot({
    pp_data_validation()
    validate(need(try(!is.na(pp_price_plot())),
                  'Data for this species is insufficient to produce this plot'))
    pp_price_plot()
  })
  
  # creates MLTI export table
  exp_mlti_table_df <- reactive({
    calculate_mlti_table(species_selection_trade(), exports = T)
  })
  
  # outputs MLTI export table
  output$exp_mlti_table <- renderTable({
    trade_data_validation()
    validate(need(try(!is.na(exp_mlti_table_df())),
                  'Data for this species is insufficient to produce this table'))
    exp_mlti_table_df()
  })
  
  # creates MLTI export plot
  exp_mlti_plot <- reactive({
    plot_mlti(calculate_mlti(species_selection_trade(), exports = T), 
              exports = T, species = species_selection_trade())
  })
  
  # outputs MLTI export plot
  output$exp_mlti <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(exp_mlti_plot())),
                  'Data for this species is insufficient to produce this plot'))
    exp_mlti_plot()
  })
  
  # creates MLTI import table
  imp_mlti_table_df <- reactive({
    calculate_mlti_table(species_selection_trade(), imports = T)
  })
  
  # outputs MLTI import table
  output$imp_mlti_table <- renderTable({
    trade_data_validation()
    validate(need(try(!is.na(imp_mlti_table_df())),
                  'Data for this species is insufficient to produce this table'))
    imp_mlti_table_df()
  })
  
  # creates MLTI import plot
  imp_mlti_plot <- reactive({
    plot_mlti(calculate_mlti(species_selection_trade(), imports = T), 
              imports = T, species = species_selection_trade())
  })
  
  # outputs MLTI import plot
  output$imp_mlti <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(imp_mlti_plot())),
                  'Data for this species is insufficient to produce this plot'))
    imp_mlti_plot()
  })
  
  # creates HI plot
  hi_plot <- reactive({
    plot_hi(calculate_hi(species_selection_trade()), 
            species = species_selection_trade())
  })
  
  # outputs HI plot
  output$hi <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(hi_plot())),
                  'Data for this species is insufficient to produce this table'))
    hi_plot()
  })
  
  # creates supply metric data
  supply_df <- reactive({
    calculate_supply_metrics(
      species_selection_trade())
    })
  
  # creates apparent supply plot
  supply_plot <- reactive({
    plot_supply_metrics(supply_df(), 'SUPPLY', 
                        species = species_selection_trade())
  })
  
  # outputs apparent supply plot
  output$supply <- renderPlot({
    trade_data_validation()
    validate(need(try(supply_plot()),
                  'Data for this species is insufficient to produce this table'))
    supply_plot()
  })
  
  # creates apparent supply (ratio) plot
  supply_ratio_plot <- reactive({
    plot_supply_metrics(supply_df(), 'RATIO', 
                        species = species_selection_trade())
  })
  
  # outputs apparent supply (ratio) plot
  output$supply_ratio <- renderPlot({
    trade_data_validation()
    validate(need(try(supply_ratio_plot()),
                  'Data for this species is insufficient to produce this table'))
    supply_ratio_plot()
  })
  
  # creates apparent supply (share) plot
  supply_share_plot <- reactive({
    plot_supply_metrics(supply_df(), 'SHARE', 
                        species = species_selection_trade())
  })
  
  # outputs apparent supply (share) plot
  output$supply_share <- renderPlot({
    trade_data_validation()
    validate(need(try(supply_share_plot()),
                  'Data for this species is insufficient to produce this table'))
    supply_share_plot()
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)