# Developing a proof-of-concept dashboard for U.S. seafood trade metrics
# Contact: Cameron Van Horn
#          cameron.vanhorn@noaa.gov

if(!require("googledrive")) install.packages("googledrive")
if(!require("shiny"))       install.packages("shiny")
if(!require("bslib"))       install.packages("bslib")
if(!require("tidyverse"))   install.packages("tidyverse")
if(!require("scales"))      install.packages("scales")
if(!require("ggh4x"))       install.packages("ggh4x")
# if(!require("nmfspalette")) install.packages("nmfspalette")
source("nmfs_cols.R")

# Pull Data 
load('seafood_trade_data_munge_03_13_25.RData')

# Store functions --------------------------------------------------------------
# filter species
filter_species <- function(data, species) {
  species_names <- unique(data$SPECIES_NAME)
  species_groups <- unique(data$SPECIES_GROUP)
  species_categories <- unique(data$SPECIES_CATEGORY)
  ecology_categories <- unique(data$ECOLOGICAL_CATEGORY)
  
  species <- toupper(species)
  
  locate_group <- 
    ifelse(species %in% ecology_categories, 
           'ECOLOGICAL_CATEGORY',
           ifelse(species %in% species_categories, 
                  'SPECIES_CATEGORY',
                  ifelse(species %in% species_groups, 
                         'SPECIES_GROUP',
                         ifelse(species %in% species_names, 
                                'SPECIES_NAME',
                                'UNAVAILABLE'))))
  
  if (locate_group == 'UNAVAILABLE') {
    stop("The species you provided is either too specific or not available.
         Try 'unique(your_data$your_column) to find acceptable calls.")
  } 
  
  group <- as.symbol(locate_group)
  group <- rlang::enquo(group)
  
  new_data <- data %>%
    filter(!!group == species)
  
  return(new_data)
  
}

# summary + calculation functions
summarize_trade_yr_spp <- function(trade_table, species) {
  
  species <- toupper(species)
  
  if (species != 'ALL') {
    which_group <- as.symbol(
      ifelse(species %in% unique(trade_table$ECOLOGICAL_CATEGORY), 
             'ECOLOGICAL_CATEGORY',
             ifelse(species %in% unique(trade_table$SPECIES_CATEGORY), 
                    'SPECIES_CATEGORY',
                    ifelse(species %in% unique(trade_table$SPECIES_GROUP), 
                           'SPECIES_GROUP',
                           'SPECIES_NAME')))
    )
  } else if (species == 'ALL') {
    summarized_data <- trade_table %>%
      select(YEAR, EXP_VALUE_2024USD, EXP_VOLUME_KG, IMP_VALUE_2024USD,
             IMP_VOLUME_KG) %>%
      mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD), 0,
                                        EXP_VALUE_2024USD),
             IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD), 0,
                                        IMP_VALUE_2024USD),
             EXP_VOLUME_KG = ifelse(is.na(EXP_VOLUME_KG), 0,
                                    EXP_VOLUME_KG),
             IMP_VOLUME_KG = ifelse(is.na(IMP_VOLUME_KG), 0,
                                    IMP_VOLUME_KG)) %>%
      group_by(YEAR) %>%
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
  
  group <- rlang::enquo(which_group)
  
  summarized_data <- trade_table %>%
    filter_species(species) %>%
    select(YEAR, !!group, EXP_VALUE_2024USD, EXP_VOLUME_KG, 
           IMP_VALUE_2024USD, IMP_VOLUME_KG) %>%
    mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD), 0,
                                      EXP_VALUE_2024USD),
           IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD), 0,
                                      IMP_VALUE_2024USD),
           EXP_VOLUME_KG = ifelse(is.na(EXP_VOLUME_KG), 0,
                                  EXP_VOLUME_KG),
           IMP_VOLUME_KG = ifelse(is.na(IMP_VOLUME_KG), 0,
                                  IMP_VOLUME_KG)) %>%
    group_by(YEAR, !!group) %>%
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
  
  if (value == F & volume == F) {
    stop('Please designate either value or volume as TRUE')
  }
  if (value == T & volume == T) {
    stop('Please designate either value or volume as FALSE')
  }
  
  if (value == T) {
    field <- as.symbol('TOTAL_REAL_TRADE_VALUE')
    field <- rlang::enquo(field)
  } else {
    field <- as.symbol('TOTAL_TRADE_VOLUME')
    field <- rlang::enquo(field)
  }
  species <- toupper(species)
  
  if (species == 'ALL') {
    filtered_data <- trade_table
  } else {
    filtered_data <- trade_table %>%
      filter_species(species)
  }
  
  summarized_data <- filtered_data %>%
    select(YEAR, COUNTRY_NAME, EXP_VALUE_2024USD, EXP_VOLUME_KG, 
           IMP_VALUE_2024USD, IMP_VOLUME_KG) %>%
    filter(YEAR >= time.frame[1],
           YEAR <= time.frame[2]) %>%
    mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD), 0,
                                      EXP_VALUE_2024USD),
           IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD), 0,
                                      IMP_VALUE_2024USD),
           EXP_VOLUME_KG = ifelse(is.na(EXP_VOLUME_KG), 0,
                                  EXP_VOLUME_KG),
           IMP_VOLUME_KG = ifelse(is.na(IMP_VOLUME_KG), 0,
                                  IMP_VOLUME_KG)) %>%
    group_by(YEAR, COUNTRY_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop')
  
  top5 <- summarized_data %>%
    select(!YEAR) %>%
    group_by(COUNTRY_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    mutate(TOTAL_REAL_TRADE_VALUE = EXP_VALUE_2024USD + IMP_VALUE_2024USD,
           TOTAL_TRADE_VOLUME = EXP_VOLUME_KG + IMP_VOLUME_KG) %>%
    top_n(5, !!field) %>%
    pull(COUNTRY_NAME)
  
  final_data <- summarized_data %>%
    filter(COUNTRY_NAME %in% top5) %>%
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
  
  species <- toupper(species)
  
  if (species != 'ALL') {
    which_group <- as.symbol(
      ifelse(species %in% unique(product_data$ECOLOGICAL_CATEGORY), 
             'ECOLOGICAL_CATEGORY',
             ifelse(species %in% unique(product_data$SPECIES_CATEGORY), 
                    'SPECIES_CATEGORY',
                    ifelse(species %in% unique(product_data$SPECIES_GROUP), 
                           'SPECIES_GROUP',
                           'SPECIES_NAME')))
    )
  } else if (species == 'ALL') {
    summarized_data <- product_data %>%
      select(YEAR, PRODUCT_NAME, KG, DOLLARS_2024) %>%
      group_by(YEAR, PRODUCT_NAME) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      mutate(MT = KG / 1000,
             BILLIONS_2024USD = DOLLARS_2024 / 1000000000,
             PP_PRICE_2024USD_PER_KG = DOLLARS_2024 / KG) %>%
      rename(PP_VALUE_2024USD = DOLLARS_2024,
             PP_VOLUME_MT = MT, 
             PP_VALUE_BILLIONS_2024USD = BILLIONS_2024USD,
             PP_VOLUME_KG = KG)
    
    return(summarized_data)
  }
  
  group <- rlang::enquo(which_group)
  
  product_data %>%
    filter_species(species) %>%
    select(YEAR, !!group, PRODUCT_NAME, KG, DOLLARS_2024) %>%
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
  
  species <- toupper(species)
  
  if (species != 'ALL') {
    which_group <- as.symbol(
      ifelse(species %in% unique(landings_data$ECOLOGICAL_CATEGORY), 
             'ECOLOGICAL_CATEGORY',
             ifelse(species %in% unique(landings_data$SPECIES_CATEGORY), 
                    'SPECIES_CATEGORY',
                    ifelse(species %in% unique(landings_data$SPECIES_GROUP), 
                           'SPECIES_GROUP',
                           'SPECIES_NAME')))
    )
  } else if (species == 'ALL') {
    summarized_data <- landings_data %>%
      filter(CONFIDENTIALITY != 'Confidential',
             !is.na(DOLLARS),
             !is.na(KG)) %>%
      select(YEAR, KG, DOLLARS_2024) %>%
      group_by(YEAR) %>%
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
  
  group <- rlang::enquo(which_group)
  
  landings_data %>%
    filter_species(species) %>%
    filter(CONFIDENTIALITY != 'Confidential',
           !is.na(DOLLARS),
           !is.na(KG)) %>%
    select(YEAR, !!group, KG, DOLLARS_2024) %>%
    group_by(YEAR, !!group) %>%
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
}
summarize_yr_spp <- function(species) {
  combined_data <- 
    left_join(left_join(summarize_trade_yr_spp(trade_data, species),
                        summarize_pp_yr_spp(pp_data, species) %>%
                          select(!PRODUCT_NAME) %>%
                          group_by(YEAR) %>%
                          summarise(across(where(is.numeric), sum),
                                    .groups = 'drop')),
              summarize_landings_yr_spp(com_landings, species)) 
  
  return(combined_data)
}
calculate_mlti <- function(species, exports = F, imports = F) {
  
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
    top_n(9, !!which_value)
  
  base_country <- top9$COUNTRY_NAME[5]
  trade_nations <- summary_spp_data %>%
    filter(YEAR == 2004) %>%
    select(COUNTRY_NAME) %>%
    distinct() 
  
  if (base_country %in% trade_nations$COUNTRY_NAME) {} else {
    base_country <- top9$COUNTRY_NAME[4]
    if (base_country %in% trade_nations$COUNTRY_NAME) {} else {
      base_country <- top9$COUNTRY_NAME[3]
    }
  } 
  
  base_country_q <- summary_spp_data %>%
    filter(YEAR == 2004,
           COUNTRY_NAME == base_country) %>%
    mutate(Q_INDEX = !!which_volume * average_price)
  
  index_base <- base_country_q$Q_INDEX
  
  mlti_data <- summary_spp_data %>%
    filter(COUNTRY_NAME %in% top9$COUNTRY_NAME) %>%
    mutate(Q_INDEX = !!which_volume * average_price) %>%
    select(YEAR, COUNTRY_NAME, Q_INDEX) %>%
    mutate(MLTI = Q_INDEX / index_base)
  
  return(mlti_data)
}
calculate_mlti_table <- function(species, exports = F, imports = F) {
  # major difference: calculate top 5 countries, not 9
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
  
  if(species == 'ALL') {
    hi_data <- trade_data %>%
      select(YEAR, COUNTRY_NAME, EXP_VALUE_2024USD, IMP_VALUE_2024USD) %>%
      mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD) == T,
                                        0, EXP_VALUE_2024USD),
             IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD) == T,
                                        0, IMP_VALUE_2024USD)) %>%
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
  
  data <- summarize_yr_spp(species) %>%
    mutate(APPARENT_SUPPLY = (PP_VOLUME_MT - EXP_VOLUME_MT) + IMP_VOLUME_MT,
           APPARENT_SUPPLY_REL_US_PROD = APPARENT_SUPPLY / PP_VOLUME_MT,
           UNEXPORTED_US_PROD_REL_APPARENT_SUPPLY = 
             abs(PP_VOLUME_MT - EXP_VOLUME_MT) / APPARENT_SUPPLY) 
  
  if(species == 'ALL') {
    data <- data %>%
      mutate(SPECIES = 'ALL')
    
    return(data)
  } else {
    data <- data %>%
      rename(SPECIES = 2)
    
    return(data)
  }
}

# plot functions
plot_trade <- function(data, plot_format, export = F, import = F) {
  
  if (export == T & import == T) {
    data <- data %>%
      mutate(NET_VALUE_2024USD_BILLIONS = 
               EXP_VALUE_2024USD_BILLIONS - IMP_VALUE_2024USD_BILLIONS,
             NET_VALUE_2024USD_MILLIONS = 
               EXP_VALUE_2024USD_MILLIONS - IMP_VALUE_2024USD_MILLIONS,
             NET_VOLUME_MT = EXP_VOLUME_MT - IMP_VOLUME_MT,
             NET_PRICE = EXP_PRICE_USD_PER_KG - IMP_PRICE_USD_PER_KG)
    
    shortform <- 'NET'
    longform <- 'Net Export'
  }
  
  if (export == T & import == F) {
    shortform <- 'EXP'
    longform <- 'Export'
  }
  
  if (import == T & export == F) {
    shortform <- 'IMP'
    longform <- 'Import'
  }
  
  plot_format <- toupper(plot_format)
  
  if (!(plot_format %in% c('VALUE', 'VOLUME', 'PRICE', 'BALANCE', 'RATIO'))) {
    stop('acceptable plot_format inputs include \"Value\", \"Volume\", \"Price\",  \"Balance\", and \"Ratio\"')
  }
  
  if (plot_format == 'VALUE') {
    # y <- as.symbol(paste0(shortform, '_VALUE_2024USD_BILLIONS'))
    y <- as.symbol(paste0(shortform, '_VALUE_2024USD_MILLIONS'))
    y <- rlang::enquo(y)
    # label <- label_currency(suffix = 'B')
    label <- label_currency(suffix = 'M')
    ylab <- paste0('Total ', longform, ' Value (Real 2024 USD)')
  }
  
  if (plot_format == 'VOLUME') {
    y <- as.symbol(paste0(shortform, '_VOLUME_MT'))
    y <- rlang::enquo(y)
    label <- comma
    ylab <- paste0('Total ', longform, ' Volume (Metric Tons)')
  }
  
  if (plot_format == 'PRICE') {
    y <- as.symbol(paste0(shortform, '_PRICE_USD_PER_KG'))
    y <- rlang::enquo(y)
    label <- label_currency(suffix = '/kg')
    ylab <- paste0('Average ', longform, ' Price (Real 2024 USD)')
  }
  
  if (plot_format %in% c('VALUE', 'VOLUME')) {
    plot <- 
      ggplot(data = data,
             aes(x = factor(YEAR),
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
    data$GROUP <- 'group'
    
    plot <- 
      ggplot(data = data, 
             aes(x = factor(YEAR),
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
    balance_data <- data %>%
      # rename(EXPORTS = EXP_VALUE_2024USD_BILLIONS,
      #        IMPORTS = IMP_VALUE_2024USD_BILLIONS) %>%
      rename(EXPORTS = EXP_VALUE_2024USD_MILLIONS,
             IMPORTS = IMP_VALUE_2024USD_MILLIONS) %>%
      select(YEAR, EXPORTS, IMPORTS) %>%
      mutate(TRADE_BALANCE = EXPORTS - IMPORTS) %>%
      pivot_longer(cols = c(EXPORTS, IMPORTS, TRADE_BALANCE)) %>%
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
  
  if (value == F & volume == F) {
    stop('Please specify which plot to create by setting either value or volume to T')
  }
  if (value == T & volume == T) {
    stop('Please specify only one plot to create')
  }
  
  if (value == T) {
    # y <- as.symbol('NET_VALUE_2024USD_BILLIONS')
    y <- as.symbol('NET_VALUE_2024USD_MILLIONS')
    y <- rlang::enquo(y)
    # label <- label_currency(suffix = 'B')
    label <- label_currency(suffix = 'M')
    # ylab <- 'Net Export Value (Real 2024 USD, Billions)'
    ylab <- 'Net Export Value (Real 2024 USD, Millions)'
  } else {
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
  
  plot.format <- toupper(plot.format)
  
  if (plot.format == 'VALUE') {
    y <- as.symbol('PP_VALUE_BILLIONS_2024USD')
    y <- rlang::enquo(y)
    ylab <- 'Value (Billions, 2024 Real USD)'
    
    low_prop_types <- processed_product_data %>%
      select(PP_VALUE_BILLIONS_2024USD, PRODUCT_NAME) %>%
      group_by(PRODUCT_NAME) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      mutate(TOTAL_VALUE = sum(PP_VALUE_BILLIONS_2024USD),
             VALUE_SHARE = PP_VALUE_BILLIONS_2024USD / TOTAL_VALUE) %>%
      filter(VALUE_SHARE < 0.02) %>%
      pull(PRODUCT_NAME)
    
    new_data <- processed_product_data %>%
      mutate(PRODUCT_NAME = ifelse(PRODUCT_NAME %in% c('OTHER', low_prop_types),
                                   'OTHER*', PRODUCT_NAME)) %>%
      mutate(PRODUCT_NAME = factor(PRODUCT_NAME))
    
    yr_value <- new_data %>%
      select(YEAR, PP_VALUE_BILLIONS_2024USD) %>%
      group_by(YEAR) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop')
    
    ylim <- max(yr_value$PP_VALUE_BILLIONS_2024USD)
  }
  
  if (plot.format == 'VOLUME') {
    y <- as.symbol('THOUSAND_MT')
    y <- rlang::enquo(y)
    ylab <- 'Volume (Thousand Metric Tons)'
    
    low_prop_types <- processed_product_data %>%
      select(PP_VOLUME_MT, PRODUCT_NAME) %>%
      group_by(PRODUCT_NAME) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      mutate(TOTAL_VOLUME = sum(PP_VOLUME_MT),
             VOLUME_SHARE = PP_VOLUME_MT / TOTAL_VOLUME) %>%
      filter(VOLUME_SHARE < 0.02) %>%
      pull(PRODUCT_NAME)
    
    new_data <- processed_product_data %>%
      mutate(PRODUCT_NAME = ifelse(PRODUCT_NAME %in% c('OTHER', low_prop_types),
                                   'OTHER*', PRODUCT_NAME)) %>%
      mutate(PRODUCT_NAME = factor(PRODUCT_NAME),
             THOUSAND_MT = PP_VOLUME_MT / 1000)
    
    yr_volume <- new_data %>%
      select(YEAR, THOUSAND_MT) %>%
      group_by(YEAR) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') 
    
    ylim <- max(yr_volume$THOUSAND_MT)
  }
  
  if (plot.format == 'PRICE') {
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
    
    new_data <- processed_product_data %>%
      mutate(PRODUCT_NAME = ifelse(PRODUCT_NAME %in% c('OTHER', low_prop_types),
                                   'OTHER*', PRODUCT_NAME)) %>%
      group_by(YEAR, PRODUCT_NAME) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      mutate(PP_PRICE_2024USD_PER_KG = PP_VALUE_2024USD / PP_VOLUME_KG,
             PRODUCT_NAME = factor(PRODUCT_NAME))
    
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
  
  plot.format <- toupper(plot.format)
  
  if (plot.format == 'VALUE') {
    y <- as.symbol('COM_VALUE_BILLIONS_2024USD')
    y <- rlang::enquo(y)
    
    label <- label_currency(suffix = 'B')
    ylab <- 'Total Landed Value (Billions, Real 2024 USD)'
  }
  
  if (plot.format == 'VOLUME') {
    y <- as.symbol('COM_VOLUME_THOUSAND_MT')
    y <- rlang::enquo(y)
    
    data$COM_VOLUME_THOUSAND_MT <- data$COM_VOLUME_MT / 1000
    
    label <- comma
    ylab <- 'Total Landed Volume (Thousand Metric Tons)'
  }
  
  if (plot.format == 'PRICE') {
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