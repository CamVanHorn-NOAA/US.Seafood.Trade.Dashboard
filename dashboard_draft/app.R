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
# if(!require("nmfspalette")) install.packages("nmfspalette")
# Due to some limitations in downloading nmfspalette on devices, use source
  # file located in app directory for nmfspalette colors
source("nmfs_cols.R")

# Pull Data (most recent version)
load('seafood_trade_data_munge_03_13_25.RData')

# Custom Functions -------------------------------------------------------------
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
    stop("The species you provided is either too specific or not available.
         Try 'unique(your_data$your_column) to find acceptable calls.")
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
  if (species != 'ALL') {
    which_level <- as.symbol(
      ifelse(species %in% unique(trade_table$ECOLOGICAL_CATEGORY), 
             'ECOLOGICAL_CATEGORY',
             ifelse(species %in% unique(trade_table$SPECIES_CATEGORY), 
                    'SPECIES_CATEGORY',
                    ifelse(species %in% unique(trade_table$SPECIES_GROUP), 
                           'SPECIES_GROUP',
                           'SPECIES_NAME')))
    )
  # if species is not selected (default is ALL), summarize all trade
      # i.e., no filter_species needed
  } else if (species == 'ALL') {
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
  
  # if no species is selected ('ALL' is the default), do not filter for species
  if (species == 'ALL') {
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
  
  # if no species is provided (default is 'ALL'), summarize data without 
    # filtering for a species
  if (species == 'ALL') {
    summarized_data <- product_data %>%
      # select only necessary columns: year, product_name (e.g., canned), 
        # volume (KG), and value (DOLLARS_2024)
      select(YEAR, PRODUCT_NAME, KG, DOLLARS_2024) %>%
      # group by year and the product condition (product_name)
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
  # only runs if species != 'ALL'
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
  if (species != 'ALL') {
    which_level <- as.symbol(
      ifelse(species %in% unique(landings_data$ECOLOGICAL_CATEGORY), 
             'ECOLOGICAL_CATEGORY',
             ifelse(species %in% unique(landings_data$SPECIES_CATEGORY), 
                    'SPECIES_CATEGORY',
                    ifelse(species %in% unique(landings_data$SPECIES_GROUP), 
                           'SPECIES_GROUP',
                           'SPECIES_NAME')))
    )
  } else if (species == 'ALL') {
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
  if (species != 'ALL') {
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
    
  } else if (species == 'ALL') {
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
  
  if (species != 'ALL') {
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
    
  } else if (species == 'ALL') {
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
  if(species == 'ALL') {
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
  
  # if no species is provided, add column for species to be 'ALL'
  if(species == 'ALL') {
    data <- data %>%
      mutate(SPECIES = 'ALL')
    
    return(data)
  } else {
    # otherwise rename the column specifying the species to 'SPECIES'
    data <- data %>%
      rename(SPECIES = 2)
    
    return(data)
  }
}

# plot functions
plot_trade <- function(data, plot_format, export = F, import = F) {
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
    longform <- 'Export'
  }
  # set shortform and longform values for plot labeling if import
  if (import == T & export == F) {
    shortform <- 'IMP'
    longform <- 'Import'
  }
  # coerce plot_format to uppercase to work within function
  plot_format <- toupper(plot_format)
  # stop function if plot format is not included
  if (!(plot_format %in% c('VALUE', 'VOLUME', 'PRICE', 'BALANCE', 'RATIO'))) {
    stop('acceptable plot_format inputs include \"Value\", \"Volume\", \"Price\",  \"Balance\", and \"Ratio\"')
  }
  
  # set labels and y values for plots of VALUE
  if (plot_format == 'VALUE') {
    # y <- as.symbol(paste0(shortform, '_VALUE_2024USD_BILLIONS'))
    y <- as.symbol(paste0(shortform, '_VALUE_2024USD_MILLIONS'))
    y <- rlang::enquo(y)
    # label <- label_currency(suffix = 'B')
    label <- label_currency(suffix = 'M')
    ylab <- paste0('Total ', longform, ' Value (Real 2024 USD)')
  }
  
  # set labels and y values for plots of VOLUME
  if (plot_format == 'VOLUME') {
    y <- as.symbol(paste0(shortform, '_VOLUME_MT'))
    y <- rlang::enquo(y)
    label <- comma
    ylab <- paste0('Total ', longform, ' Volume (Metric Tons)')
  }
  
  # set labels and y values for plots of PRICE
  if (plot_format == 'PRICE') {
    y <- as.symbol(paste0(shortform, '_PRICE_USD_PER_KG'))
    y <- rlang::enquo(y)
    label <- label_currency(suffix = '/kg')
    ylab <- paste0('Average ', longform, ' Price (Real 2024 USD)')
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
      labs(x = 'Year',
           y = ylab) +
      theme_bw() +
      theme(axis.text = element_text(size = 10))
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
      labs(x = 'Year',
           y = ylab) +
      theme_bw() +
      theme(axis.text = element_text(size = 10))
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
           y = 'Export / Import Volume Ratio') +
      theme_bw() +
      theme(axis.text = element_text(size = 10))
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
           fill = '') +
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
                                       size = 8),
            axis.title.y = element_text(vjust = 23),
            plot.background = element_rect(fill = 'white',
                                           color = 'white'),
            panel.grid = element_blank(),
            plot.margin = margin(5.5, 5.5, 5.5, 55.5, 'points'))
  }
  
  return(plot)
}
plot_trade_ctry_yr_spp <- function(data, value = F, volume = F) {
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
    ylab <- 'Net Export Value (Real 2024 USD, Millions)'
  } else {
    # set plot labels for volume plot
    y <- as.symbol('NET_VOLUME_MT')
    y <- rlang::enquo(y)
    label <- comma
    ylab <- 'Net Export Volume (Metric Tons)'
  }
  
  ggplot(data = data,
         aes(x = factor(COUNTRY_NAME),
             y = !!y, 
             fill = factor(YEAR))) +
    geom_col(position = 'dodge') +
    scale_fill_nmfs(palette = 'oceans') +
    labs(x = '',
         y = ylab,
         fill = 'Year') +
    scale_y_continuous(labels = label) +
    theme_bw() +
    geom_hline(yintercept = 0, 'black') +
    theme(axis.text = element_text(color = 'black',
                                   size = 10),
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 10))
}
plot_spp_pp <- function(processed_product_data, plot.format) {
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
    pull(PRODUCT_NAME)
  
  low_prop_types_volume <- processed_product_data %>%
    select(PP_VOLUME_MT, PRODUCT_NAME) %>%
    group_by(PRODUCT_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    mutate(TOTAL_VOLUME = sum(PP_VOLUME_MT),
           VOLUME_SHARE = PP_VOLUME_MT / TOTAL_VOLUME) %>%
    filter(VOLUME_SHARE < 0.02) %>%
    pull(PRODUCT_NAME)
  
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
           PRODUCT_NAME = factor(PRODUCT_NAME))
  
  # set labels for VALUE plots
  if (plot.format == 'VALUE') {
    y <- as.symbol('PP_VALUE_BILLIONS_2024USD')
    y <- rlang::enquo(y)
    ylab <- 'Value (Billions, 2024 Real USD)'
    
    # calculate the total value per year to find upper limit
    yr_value <- new_data %>%
      select(YEAR, PP_VALUE_BILLIONS_2024USD) %>%
      group_by(YEAR) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop')
    
    ylim <- max(yr_value$PP_VALUE_BILLIONS_2024USD)
  }
  
  if (plot.format == 'VOLUME') {
    # set labels for VOLUME plots
    y <- as.symbol('THOUSAND_MT')
    y <- rlang::enquo(y)
    ylab <- 'Volume (Thousand Metric Tons)'
    
    # calculate the total value per year to find upper limit
    yr_volume <- new_data %>%
      select(YEAR, THOUSAND_MT) %>%
      group_by(YEAR) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') 
    
    ylim <- max(yr_volume$THOUSAND_MT)
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
           fill = 'Product Condition') +
      scale_x_discrete(breaks = seq(2006, 2022, by = 4)) +
      scale_y_continuous(limits = c(0, max(new_data$PP_PRICE_2024USD_PER_KG) + 0.5),
                         expand = c(0, 0)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 15))
      
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
         fill = 'Product Condition') +
    scale_x_discrete(breaks = seq(2006, 2022, by = 4)) +
    scale_y_continuous(limits = c(0, ylim), 
                       expand = c(0, 0)) +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 15))
  
  return(plot)
}
plot_landings <- function(data, plot.format) {
  # this function plots landings data formatted by summarize_landings_yr_spp
  # plot.format is a character vector that accepts inputs of VALUE, VOLUME
    # and PRICE
  
  # coerce plot.format to uppercase to work within function
  plot.format <- toupper(plot.format)
  
  # set labels for VALUE plot
  if (plot.format == 'VALUE') {
    y <- as.symbol('COM_VALUE_BILLIONS_2024USD')
    y <- rlang::enquo(y)
    
    label <- label_currency(suffix = 'B')
    ylab <- 'Total Landed Value (Billions, Real 2024 USD)'
  }
  
  # set labels for VOLUME plot
  if (plot.format == 'VOLUME') {
    y <- as.symbol('COM_VOLUME_THOUSAND_MT')
    y <- rlang::enquo(y)
    
    # format metric tons by thousands
    data$COM_VOLUME_THOUSAND_MT <- data$COM_VOLUME_MT / 1000
    
    label <- comma
    ylab <- 'Total Landed Volume (Thousand Metric Tons)'
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
           y = 'Average Ex-Vessel Price (Real 2024 USD)') +
      theme_bw() +
      theme(axis.text = element_text(size = 10))
    
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
         y = ylab) +
    theme_bw() +
    theme(axis.text = element_text(size = 10))
  
  return(plot)
}
plot_mlti <- function(mlti_data, exports = F, imports = F) {
  
  if (exports == F & imports == F) {
    stop('Please set "exports" or "imports" to "T"')
  }
  
  label <- ifelse(exports == T, 'Export', 'Import')
  
  ggplot(data = mlti_data,
         aes(x = factor(YEAR),
             y = MLTI)) +
    geom_point() +
    facet_wrap( ~ factor(COUNTRY_NAME), nrow = 3) +
    scale_x_discrete(breaks = seq(2006, 2022, by = 4)) +
    geom_hline(yintercept = 1, color = 'black') +
    labs(x = '',
         y = paste0('Multilateral ', label, ' Quantity Index')) +
    theme_bw() +
    theme(axis.text = element_text(size = 15),
          axis.title.y = element_text(size = 20),
          strip.text = element_text(size = 15,
                                    color = 'white'),
          strip.background = element_rect(fill = 'black'))
}
plot_hi <- function(hi_data) {
  
  format_hi_data <- hi_data %>%
    rename(EXPORTS = EXP_HI,
           IMPORTS = IMP_HI) %>%
    pivot_longer(cols = c(EXPORTS, IMPORTS))
  
  ggplot(data = format_hi_data,
         aes(x = as.factor(YEAR),
             y = value)) +
    geom_line(aes(group = name, 
                  colour = name),
              linewidth = 1.5) +
    geom_point(size = 2,
               color = 'black') +
    scale_color_discrete(name = '',
                         labels = c('Exports', 'Imports')) +
    labs(x = '',
         y = 'Herfindahl Index (HI)') +
    scale_x_discrete(breaks = seq(2006, 2022, by = 4)) +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.position = 'top',
          plot.title = element_text(size = 18))
  
}
plot_supply_metrics <- function(supply_data, metric) {
  
  if (metric == 'SUPPLY') {
    plot <- 
      ggplot(data = supply_data %>%
               filter(YEAR < 2024),
             aes(x = factor(YEAR),
                 y = APPARENT_SUPPLY / 1000)) +
      geom_col(fill = 'black') +
      labs(x = '',
           y = 'Volume (Thousand Metric Tons)',
           title = 'Apparent Supply') +
      scale_x_discrete(limits = factor(c(2004:2023)),
                       breaks = seq(2006, 2022, by = 4)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 17))
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
           title = 'Apparent Supply Relative to \nDomestic Production') +
      scale_x_discrete(limits = factor(c(2004:2023)),
                       breaks = seq(2006, 2022, by = 4)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 17))
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
           title = 'Unexported Domestic Production \nRelative to Apparent Supply') +
      scale_x_discrete(limits = factor(c(2004:2023)),
                       breaks = seq(2006, 2022, by = 4)) +
      scale_y_continuous(labels = label_percent()) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 15))
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
    selectInput(inputId = 'ecol_cat', 
                label = 'Choose a Category',
                choices = c('ALL', com_landings %>% 
                              filter(CONFIDENTIALITY != 'Confidential') %>%
                              select(ECOLOGICAL_CATEGORY) %>%
                              distinct() %>%
                              mutate(ECOLOGICAL_CATEGORY = 
                                       str_to_title(ECOLOGICAL_CATEGORY)) %>%
                              pull())),
    uiOutput('filter_2'),
    uiOutput('filter_3'),
    uiOutput('filter_4')
  ),
  fluidRow(
    navset_card_pill(title = 'Trade',
                     nav_panel(title = 'Aggregate',
                               fluidRow(
                                 plotOutput('balance')
                               ),
                               fluidRow(
                                 column(
                                   plotOutput('trade_ratio'),
                                   width = 6
                                 ),
                                 column(
                                   plotOutput('top5_trade'),
                                   width = 6
                                 ))),
                     nav_panel(title = 'Value',
                               fluidRow(
                                 column(
                                   plotOutput('exp_value'),
                                   width = 6
                                 ),
                                 column(
                                   plotOutput('imp_value'),
                                   width = 6
                                 ))),
                     nav_panel(title = 'Volume',
                               fluidRow(
                                 column(
                                   plotOutput('exp_volume'),
                                   width = 6
                                 ),
                                 column(
                                   plotOutput('imp_volume'),
                                   width = 6
                                 ))),
                     nav_panel(title = 'Price',
                               fluidRow(
                                 column(
                                   plotOutput('exp_price'),
                                   width = 6
                                 ),
                                 column(
                                   plotOutput('imp_price'),
                                   width = 6
                                 ))),
                     nav_panel(title = 'Advanced Metrics',
                               fluidRow(
                                 column(
                                   fluidRow(
                                     'Exports'
                                   ),
                                   tableOutput('exp_mlti_table'),
                                   # plotOutput('exp_mlti'),
                                   width = 6
                                 ),
                                 column(
                                   fluidRow(
                                     'Imports'
                                   ),
                                   tableOutput('imp_mlti_table'),
                                   # plotOutput('imp_mlti'),
                                   width = 6
                                 )),
                               fluidRow(
                                 column(
                                   plotOutput('hi_plot'),
                                   width = 3
                                 ),
                                 column(
                                   plotOutput('supply_plot'),
                                   width = 3
                                 ),
                                 column(
                                   plotOutput('supply_ratio'),
                                   width = 3
                                 ),
                                 column(
                                   plotOutput('supply_share'),
                                   width = 3
                                 ))))),
  fluidRow(
    column(
      navset_card_pill(title = 'Commercial Landings',
                       nav_panel(title = 'Value',
                                 plotOutput('landings_value')),
                       nav_panel(title = 'Volume',
                                 plotOutput('landings_volume')),
                       nav_panel(title = 'Price',
                                 plotOutput('landings_price'))),
      width = 6
    ),
    column(
      navset_card_pill(title = 'Processed Products',
                       nav_panel(title = 'Value',
                                 plotOutput('pp_value')),
                       nav_panel(title = 'Volume',
                                 plotOutput('pp_volume')),
                       nav_panel(title = 'Price',
                                 plotOutput('pp_price'))),
      width = 6
    )
  )
             # tabPanel(title = 'Trade',
             #          navset_pill(
             #            nav_panel(title = 'Aggregate',
             #                      plotOutput('balance')),
             #            nav_panel(title = 'Exports',
             #                      fluidRow(
             #                        plotOutput('exp_volume')
             #                      ),
             #                      fluidRow(
             #                        plotOutput('imp_volume')
             #                      ))
             #          )),
             # tabPanel(title = 'Commercial Landings'),
             # tabPanel(title = 'Processed Products')
             # )
  )

# Define server logic ----------------------------------------------------------
server <- function(input, output, session) {
  
  # creates input: species_cat
  output$filter_2 <- renderUI({
    req(input$ecol_cat != 'ALL')
    species_cats <- c('ALL', com_landings %>%
                        filter_species(input$ecol_cat) %>%
                        select(SPECIES_CATEGORY) %>%
                        distinct() %>%
                        mutate(SPECIES_CATEGORY = 
                                 str_to_title(SPECIES_CATEGORY)) %>%
                        pull())
    selectInput('species_cat', 'Choose a Secondary Category', species_cats)
  })
  
  # creates input: species_grp
  output$filter_3 <- renderUI({
    req(input$species_cat != 'ALL' & input$ecol_cat != 'ALL')
    species_groups <- c('ALL', com_landings %>%
                          filter_species(input$species_cat) %>%
                          select(SPECIES_GROUP) %>%
                          distinct() %>%
                          mutate(SPECIES_GROUP = 
                                   str_to_title(SPECIES_GROUP)) %>%
                          pull())
    selectInput('species_grp', 'Choose a Group', species_groups)
  })
  
  # creates input: species_name
  output$filter_4 <- renderUI ({
    req(input$species_grp != 'ALL' & input$species_cat != 'ALL' & input$ecol_cat != 'ALL')
    species_names <- c('ALL', com_landings %>%
                         filter_species(input$species_grp) %>%
                         select(SPECIES_NAME) %>%
                         distinct() %>%
                         mutate(SPECIES_NAME = str_to_title(SPECIES_NAME)) %>%
                         pull())
    selectInput('species_name', 'Choose a Species', species_names)
  })

  # creates trade data
  trade_df <- reactive(summarize_trade_yr_spp(
    trade_data,
    ifelse(input$ecol_cat == 'ALL', 'ALL',
           ifelse(input$species_cat == 'ALL', input$ecol_cat,
                  ifelse(input$species_grp == 'ALL', input$species_cat,
                         ifelse(input$species_name == 'ALL', input$species_grp,
                                input$species_name))))))
  
  # creates trade balance plot (value)
  output$balance <- renderPlot({
    plot_trade(trade_df(), 
               'BALANCE')
  })
  
  # creates export/import ratio plot
  output$trade_ratio <- renderPlot({
    plot_trade(trade_df(), 
               'RATIO', export = T, import = T)
  })
  
  # creates top 5 net export plot
  output$top5_trade <- renderPlot({
    plot_trade_ctry_yr_spp(
      summarize_trade_ctry_yr_spp(
        trade_data,
        ifelse(input$ecol_cat == 'ALL', 'ALL',
               ifelse(input$species_cat == 'ALL', input$ecol_cat,
                      ifelse(input$species_grp == 'ALL', input$species_cat,
                             ifelse(input$species_name == 'ALL', input$species_grp,
                                    input$species_name)))),
        time.frame = c(2020, 2024),
        value = T),
      value = T
    )
  })
  
  # creates export value plot
  output$exp_value <- renderPlot({
    plot_trade(trade_df(), 
               'VALUE', export = T)
  })
  
  # creates import value plot
  output$imp_value <- renderPlot({
    plot_trade(trade_df(),
               'VALUE', import = T)
  })

  # creates export volume plot
  output$exp_volume <- renderPlot({
    plot_trade(trade_df(), 
               'VOLUME', export = T)
  })

  # creates import volume plot
  output$imp_volume <- renderPlot({
    plot_trade(trade_df(), 
               'VOLUME', import = T)
  })
  
  # creates export price plot
  output$exp_price <- renderPlot({
    plot_trade(trade_df(),
               'PRICE', export = T)
  })
  
  # creates import price plot
  output$imp_price <- renderPlot({
    plot_trade(trade_df(),
               'PRICE', import = T)
  })
  
  # creates landings data
  landings_df <- reactive(summarize_landings_yr_spp(
    com_landings,
    ifelse(input$ecol_cat == 'ALL', 'ALL', 
           ifelse(input$species_cat == 'ALL', input$ecol_cat,
                  ifelse(input$species_grp == 'ALL', input$species_cat,
                         ifelse(input$species_name == 'ALL', input$species_grp,
                                input$species_name))))))
  
  # creates landings value plot
  output$landings_value <- renderPlot({
    plot_landings(landings_df(), 
                  'VALUE')
  })
  
  # creates landings volume plot
  output$landings_volume <- renderPlot({
    plot_landings(landings_df(),
                  'VOLUME')
  })
  
  # creates landings price plot
  output$landings_price <- renderPlot({
    plot_landings(landings_df(),
                  'PRICE')
  })
  
  # creates processed products data
  pp_df <- reactive(summarize_pp_yr_spp(
    pp_data,
    ifelse(input$ecol_cat == 'ALL', 'ALL',
           ifelse(input$species_cat == 'ALL', input$ecol_cat,
                  ifelse(input$species_grp == 'ALL', input$species_cat,
                         ifelse(input$species_name == 'ALL', input$species_grp,
                                input$species_name))))))
  
  # creates processed products value plot
  output$pp_value <- renderPlot({
    plot_spp_pp(pp_df(),
                'VALUE')
  })
  
  # creates processed products volume plot
  output$pp_volume <- renderPlot({
    plot_spp_pp(pp_df(),
                'VOLUME')
  })
  
  # creates processed products price plot
  output$pp_price <- renderPlot({
    plot_spp_pp(pp_df(),
                'PRICE')
  })
  
  # creates MLTI export table
  output$exp_mlti_table <- renderTable({
    calculate_mlti_table(
      ifelse(input$ecol_cat == 'ALL', 'ALL',
             ifelse(input$species_cat == 'ALL', input$ecol_cat,
                    ifelse(input$species_grp == 'ALL', input$species_cat,
                           ifelse(input$species_name == 'ALL', input$species_grp,
                                  input$species_name)))),
      exports = T)
  })
  
  # creates MLTI export plot
  output$exp_mlti <- renderPlot({
    plot_mlti(
      calculate_mlti(
        ifelse(input$ecol_cat == 'ALL', 'ALL',
               ifelse(input$species_cat == 'ALL', input$ecol_cat,
                      ifelse(input$species_grp == 'ALL', input$species_cat,
                             ifelse(input$species_name == 'ALL', input$species_grp,
                                    input$species_name)))),
        exports = T),
    exports = T)
  })
  
  # creates MLTI import table
  output$imp_mlti_table <- renderTable({
    calculate_mlti_table(
      ifelse(input$ecol_cat == 'ALL', 'ALL',
             ifelse(input$species_cat == 'ALL', input$ecol_cat,
                    ifelse(input$species_grp == 'ALL', input$species_cat,
                           ifelse(input$species_name == 'ALL', input$species_grp,
                                  input$species_name)))),
      imports = T)
  })
  
  # creates MLTI import plot
  output$imp_mlti <- renderPlot({
    plot_mlti(
      calculate_mlti(
        ifelse(input$ecol_cat == 'ALL', 'ALL',
               ifelse(input$species_cat == 'ALL', input$ecol_cat,
                      ifelse(input$species_grp == 'ALL', input$species_cat,
                             ifelse(input$species_name == 'ALL', input$species_grp,
                                    input$species_name)))),
        imports = T),
      imports = T)
  })
  
  # creates HI plot
  output$hi_plot <- renderPlot({
    plot_hi(
      calculate_hi(
        ifelse(input$ecol_cat == 'ALL', 'ALL',
               ifelse(input$species_cat == 'ALL', input$ecol_cat,
                      ifelse(input$species_grp == 'ALL', input$species_cat,
                             ifelse(input$species_name == 'ALL', input$species_grp,
                                    input$species_name))))))
  })
  
  # creates supply metric data
  supply_df <- reactive(calculate_supply_metrics(
    ifelse(input$ecol_cat == 'ALL', 'ALL',
           ifelse(input$species_cat == 'ALL', input$ecol_cat,
                  ifelse(input$species_grp == 'ALL', input$species_cat,
                         ifelse(input$species_name == 'ALL', input$species_grp,
                                input$species_name))))))
  
  # creates apparent supply plot
  output$supply_plot <- renderPlot({
    plot_supply_metrics(supply_df(),
                        'SUPPLY')
  })
  
  # creates apparent supply (ratio) plot
  output$supply_ratio <- renderPlot({
    plot_supply_metrics(supply_df(),
                        'RATIO')
  })
  
  # creates apparent supply (share) plot
  output$supply_share <- renderPlot({
    plot_supply_metrics(supply_df(),
                        'SHARE')
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)