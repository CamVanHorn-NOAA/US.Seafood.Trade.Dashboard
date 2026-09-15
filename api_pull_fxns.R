##############################
#' *CONNECT TO FOSS VIA API* #
##############################
# Functions to connect to the NOAA Fisheries One-Stop Shop (FOSS) and download
  # trade and landings data within specified year ranges via API 
# Outputs are structured to be compatible within 1_data_pulls.R in the 
  # US.Seafood.Trade.Dashboard GitHub 
  # (https://github.com/CamVanHorn-NOAA/US.Seafood.Trade.Dashboard)
#' *FIELDS OMITTED FROM API PULL FUNCTIONS*
  # Landings
    # region_name, source, collection, links
  # Trade
    # fus_group_code2, fus_group1, fus_group2, cntry_code, fao, 
    # custom_district_code, edible_code, association, rfmo, nmfs_region_code,
    # links
  # If any of the above fields are desired for pulls, go into _api functions
    # and add desired fields to select() arguments in output dplyr pipes.
    # NOTE: add 'items.' before each field if added to functions.

# Libraries
if(!require("tidyverse"))   install.packages("tidyverse")
if(!require("jsonlite"))   install.packages("jsonlite")
if(!require("httr"))   install.packages("httr")

# interior functions for get_landings and get_trade
landings_api <- function(year) {
  # split up the url call into different objects
  url <- 'https://apps-st.fisheries.noaa.gov/ods/foss/landings/'
  # in the filter call, must start with ?q={} to establish query language
  # any columns we wish to index by must be in quotations "" NOT ''
  filter <- paste0('?q={"year":', year,',"collection":"Commercial"}')
  
  # get data from FOSS
  res <- GET(paste0(url, filter),
             # high limit so we don't lose data to a cutoff
             query = list(limit = 100000))
  
  df <- content(res, as = "text", encoding = "UTF-8")
  df <- fromJSON(df, flatten = TRUE)
  df$links <- NULL
  df <- df %>% data.frame()
  
  if (any(df$hasMore)) {
    partial_yr_pull <- df[which(df$hasMore == T), c(items.year)] %>%
      distinct() %>%
      pull()
    
    cat('Warning: Data from', partial_yr_pull, 'incomplete due download limit reached. Visit https://www.fisheries.noaa.gov/foss/f?p=215:200:::::: to download complete data.\n')
  }
  
  # our data is embedded in content of object, translate from JSON (default)
  # data <- fromJSON(rawToChar(res$content))
  
  # data stored in items field as a df
  output <- df %>%
  # get necessary columns
  select(items.tsn, items.ts_afs_name, items.ts_scientific_name, items.state_name, 
         items.year, items.pounds, items.dollars) %>%
  # replace NAs with 0s for aggregation
  replace(is.na(.), 0) %>%
  # round pounds and dollars
  mutate(items.pounds = round(items.pounds),
         items.dollars = round(items.dollars)) %>%
  group_by(items.tsn, items.ts_afs_name, items.ts_scientific_name, 
           items.state_name, items.year) %>%
  summarise(across(where(is.numeric), sum), .groups = 'drop') %>%
  # coerce names
  rename(TSN = items.tsn,
         NMFS_NAME = items.ts_afs_name,
         SCIENTIFIC_NAME = items.ts_scientific_name,
         STATE = items.state_name,
         POUNDS = items.pounds,
         DOLLARS = items.dollars,
         YEAR = items.year) %>%
  # fix error where POLLOCK, WALLEYE and SEA HARES have extra space in string
  mutate(NMFS_NAME = ifelse(NMFS_NAME == 'POLLOCK, WALLEYE ',
                            'POLLOCK, WALLEYE',
                            NMFS_NAME),
         NMFS_NAME = ifelse(NMFS_NAME == 'SEA HARES ',
                            'SEA HARES',
                            NMFS_NAME))
  return(output)
}
trade_api <- function(year, type) {
  # the data is too large to download in a single call, so need to pull on a 
  # month to month basis
  months <- c("01", "02", "03", "04", "05", "06", 
              "07", "08", "09", "10", "11", "12")
  
  # create empty data frame for the loop
  df <- data.frame()
  # loop for months to download per month
  for (i in months) {
    # split up the url call into different objects
    url <- 'https://apps-st.fisheries.noaa.gov/ods/foss/trade_data/'
    # in the filter call, must start with ?q={} to establish query language
    # any columns we wish to index by must be in quotations "" NOT ''
    filter <- paste0('?q={"year":', year,',"source":"', 
                     type, '","month":"', i, '"}')
    
    # get data from FOSS
    res <- GET(paste0(url, filter),
               # high limit so we don't lose data to a cutoff
               query = list(limit = 100000))
    
    pull <- content(res, as = "text", encoding = "UTF-8")
    pull <- fromJSON(pull, flatten = TRUE)
    pull$links <- NULL
    pull <- pull %>% data.frame()
    
    # our data is embedded in content of object, translate from JSON (default)
    # data <- fromJSON(rawToChar(res$content))
    
    df <- bind_rows(df, pull)
  }
  
  if (any(df$hasMore)) {
    partial_yr_pull <- df[which(df$hasMore == T), c(items.year)] %>%
      distinct() %>%
      pull()
    
    cat('Warning: Data from', partial_yr_pull, 'incomplete due download limit reached. Visit https://www.fisheries.noaa.gov/foss/f?p=215:200:::::: to download complete data.\n')
  }
  
  # data stored in items field as a df
  output <- df %>%
    # get necessary columns
    select(items.year, items.month, items.hts_number, items.name, items.fus_group1, 
           items.cntry_name, items.continent, items.custom_district_name, 
           items.kilos, items.val, items.source) %>%
    # replace NAs with 0s for aggregation
    replace(is.na(.), 0) %>%
    # round kilos and dollars
    mutate(items.kilos = round(items.kilos),
           items.val = round(items.val)) %>%
    group_by(items.year, items.month, items.hts_number, items.name, items.fus_group1, 
             items.cntry_name, items.continent, items.custom_district_name, 
             items.source) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>%
    # coerce names
    rename(YEAR = items.year,
           MONTH = items.month,
           HTS_NUMBER = items.hts_number,
           PRODUCT_NAME = items.name,
           GROUP_TS = items.fus_group1,
           COUNTRY_NAME = items.cntry_name,
           CONTINENT = items.continent,
           US_CUSTOMS_DISTRICT = items.custom_district_name,
           SOURCE = items.source,
           VOLUME_KG = items.kilos,
           VALUE_USD = items.val) %>%
    # clean data
    # remove leading zeroes in HTS to match other data sources
    mutate(HTS_NUMBER = ifelse(str_sub(HTS_NUMBER, 1, 1) == '0',
                               str_sub(HTS_NUMBER, 2, -1),
                               HTS_NUMBER),
           # create a state column from customs district info
           STATE = substr(US_CUSTOMS_DISTRICT, nchar(US_CUSTOMS_DISTRICT) - 1,
                          nchar(US_CUSTOMS_DISTRICT)),
           # for Low value shipments and other non-cities, set state as NA
           STATE = ifelse(STATE %in% c('NT', 'DS'), NA, STATE),
           # remove state from customs district string
           US_CUSTOMS_DISTRICT = ifelse(is.na(STATE), US_CUSTOMS_DISTRICT,
                                        substr(US_CUSTOMS_DISTRICT, 0, nchar(US_CUSTOMS_DISTRICT) - 4)))
  
  return(output)
}

# functions to pull data
get_landings <- function(years) {
  if (!(length(years) %in% c(1, 2))) stop('Years argument must be either a single year or the first and last year of a range within a concatenate.')
  
  # set empty data frame
  df <- data.frame()
  if (length(years) > 1) {
    for (i in years[1]:years[2]) {
      data <- landings_api(i)
      df <- bind_rows(df, data)
    }
  } else {
    df <- landings_api(years)
  }
  
  return(df)
}
get_trade <- function(years, exports = F, imports = F) {
  # apply export or import argument
  if (exports == T & imports == F) {
    type <- 'EXP'
  } else if (exports == F & imports == T) {
    type <- 'IMP'
  } else if (exports == F & imports == F) {
    stop('Please set either the "exports" or "imports" arguments to be TRUE.')
  } else {
    stop('Specify only ONE of either the "exports" or "imports" arguments to be TRUE.')
  }
  
  if (!(length(years) %in% c(1, 2))) stop('Years argument must be either a single year or the first and last year of a range within a concatenate.')
  
  # set empty data frame
  df <- data.frame()
  if (length(years) > 1) {
    for (i in years[1]:years[2]) {
      data <- trade_api(i, type)
      df <- bind_rows(df, data)
    }
  } else {
    df <- trade_api(years, type)
  }
  
  
  return(df)
}
