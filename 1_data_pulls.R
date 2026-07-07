# Script 1: Pull Data for Seafood Dashboard
# Data sources include the fisheries one-stop shop (FOSS, 
  # https://www.fisheries.noaa.gov/foss/f?p=215:2:742090237412:::::) and SQL
  # databases. 
  # TODO: link to SQL database for processed product download
  # TODO: get new GDPDEF reference that can calculate real 2024 USD
# Contact: Cameron Van Horn
#          cameron.vanhorn@noaa.gov




# Load Packages ----------------------------------------------------------------
if(!require("googledrive")) install.packages("googledrive")
if(!require("tidyverse"))   install.packages("tidyverse")
if(!require("jsonlite"))   install.packages("jsonlite")
if(!require("httr"))   install.packages("httr")
######################
### API CONNECTION ###
######################
# functions --------------------------------------------------------------------
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
  
  # our data is embedded in content of object, translate from JSON (default)
  data <- fromJSON(rawToChar(res$content))
  
  # data stored in items field as a df
  output <- data$items %>%
    # get necessary columns
    select(tsn, ts_afs_name, ts_scientific_name, state_name, year, pounds, dollars) %>%
    # replace NAs with 0s for aggregation
    replace(is.na(.), 0) %>%
    # round pounds and dollars
    mutate(pounds = round(pounds),
           dollars = round(dollars)) %>%
    group_by(tsn, ts_afs_name, ts_scientific_name, state_name, year) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>%
    # coerce names 
    rename(TSN = tsn,
           NMFS_NAME = ts_afs_name,
           SCIENTIFIC_NAME = ts_scientific_name,
           STATE = state_name,
           POUNDS = pounds,
           DOLLARS = dollars,
           YEAR = year) %>%
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
    
    # our data is embedded in content of object, translate from JSON (default)
    data <- fromJSON(rawToChar(res$content))
    
    df <- bind_rows(df, data$items)
  }
  
  # data stored in items field as a df
  output <- df %>%
    # get necessary columns
    select(year, month, hts_number, name, cntry_name, continent,
           custom_district_name, kilos, val, source) %>%
    # replace NAs with 0s for aggregation
    replace(is.na(.), 0) %>%
    # round kilos and dollars
    mutate(kilos = round(kilos),
           val = round(val)) %>%
    group_by(year, month, hts_number, name, cntry_name, continent,
             custom_district_name, source) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>%
    # coerce names 
    rename(YEAR = year,
           MONTH = month,
           HTS_NUMBER = hts_number, 
           PRODUCT_NAME = name,
           COUNTRY_NAME = cntry_name, 
           CONTINENT = continent,
           US_CUSTOMS_DISTRICT = custom_district_name,
           SOURCE = source,
           VOLUME_KG = kilos,
           VALUE_USD = val) %>%
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
  
  # set empty data frame
  df <- data.frame()
  for (i in years[1]:years[2]) {
    data <- landings_api(i)
    df <- bind_rows(df, data)
  }
  
  return(df)
}
get_trade <- function(years, type) {
  
  # set empty data frame
  df <- data.frame()
  for (i in years[1]:years[2]) {
    data <- trade_api(i, type)
    df <- bind_rows(df, data)
  }
  
  return(df)
}


####################################
### CONNECT TO YOUR GOOGLE DRIVE ###
####################################
# The googledrive package comes from tidyverse and enables R users to connect
  # directly to google drive for access to files.
# We can run drive_auth() to obtain access credentials and link the R session to 
  # a google drive account
drive_auth()

#############################################
### DOWNLOAD DATA FILES FROM GOOGLE DRIVE ###
#############################################
# IMPORTANT: ADD DOWNLOADS BELOW FOR FUTURE DATA PULLS
# to download data from google drive, specify the file name
  # it might be easier to use drive_find to see the name of the file
# FOSS Exports, split by ~10 years
  # UNCOMMENT IF DATA IS UPDATED
# drive_download('foss_exports_15-25.csv',
#                overwrite = T)
# drive_download('foss_exports_04-14.csv',
#                overwrite = T)
# drive_download('foss_imports_15-25.csv',
#                overwrite = T)
# drive_download('foss_imports_04-14.csv',
#                overwrite = T)
# drive_download('foss_pp_15-23.csv',
#                overwrite = T)
# drive_download('foss_pp_04-14.csv',
#                overwrite = T)
# drive_download('GDPDEF_2024_index.csv',
#                overwrite = T)
# drive_download('FTS_PRODUCTS.csv',
#                overwrite = T)
# drive_download('foss_com_landings_04-14.csv',
#                overwrite = T)
# drive_download('foss_com_landings_15-23.csv',
#                overwrite = T)
# drive_download('pp_com_landings_mapping.csv',
#                overwrite = T)
# drive_download('com_landings_mapping_sheet.csv',
#                overwrite = T)
# drive_download('trade_data_mapping_sheet.csv',
#                overwrite = T)
# drive_download('pp_mapping_sheet.csv',
#                overwrite = T)

##################################
### LOAD DOWNLOADED DATA FILES ###
##################################
# Some csv's are downloaded staright from FOSS without any prior modification
# I.e., their headers are setup improperly and need adjustment

# Exports ----------------------------------------------------------------------
# read csv's
foss_exports_1525 <- read.csv('foss_exports_15-25.csv') %>%
  # use setNames from 'stats' to assign first row values as column names
  setNames(.[1, ]) %>%
  rename_with( ~ toupper(gsub(' ', '_', .x, fixed = T))) %>%
  rename_with( ~ toupper(gsub('(', '', .x, fixed = T))) %>%
  rename_with( ~ toupper(gsub(')', '', .x, fixed = T))) %>%
  # remove first row
  .[-1, ] %>%
  # HTS_NUMBER, which is the key to attach species information, is not properly
    # formatted as some keys have an incorrect leading '0'
  # Remove the leading 0 from any keys containing one
  # set ifelse such that if the first character in HTS_NUMBER == 0, it is
    # removed from the string
  mutate(HTS_NUMBER = ifelse(str_sub(HTS_NUMBER, 1, 1) == '0',
                             str_sub(HTS_NUMBER, 2, -1),
                             HTS_NUMBER),
         STATE = substr(US_CUSTOMS_DISTRICT, nchar(US_CUSTOMS_DISTRICT) - 1,
                        nchar(US_CUSTOMS_DISTRICT)),
         STATE = ifelse(STATE %in% c('NT', 'DS'), NA, STATE),
         US_CUSTOMS_DISTRICT = ifelse(is.na(STATE), US_CUSTOMS_DISTRICT,
                                      substr(US_CUSTOMS_DISTRICT, 0, nchar(US_CUSTOMS_DISTRICT) - 4)))

foss_exports_0414 <- read.csv('foss_exports_04-14 (2).csv') %>%
  setNames(.[1, ]) %>%
  rename_with( ~ toupper(gsub(' ', '_', .x, fixed = T))) %>%
  rename_with( ~ toupper(gsub('(', '', .x, fixed = T))) %>%
  rename_with( ~ toupper(gsub(')', '', .x, fixed = T))) %>%
  .[-1, ] %>%
  mutate(HTS_NUMBER = ifelse(str_sub(HTS_NUMBER, 1, 1) == '0',
                             str_sub(HTS_NUMBER, 2, -1),
                             HTS_NUMBER),
         STATE = substr(US_CUSTOMS_DISTRICT, nchar(US_CUSTOMS_DISTRICT) - 1,
                        nchar(US_CUSTOMS_DISTRICT)),
         STATE = ifelse(STATE %in% c('NT', 'DS'), NA, STATE),
         US_CUSTOMS_DISTRICT = ifelse(is.na(STATE), US_CUSTOMS_DISTRICT,
                                      substr(US_CUSTOMS_DISTRICT, 0, nchar(US_CUSTOMS_DISTRICT) - 4)))

# combine data (stack)
foss_exports <- bind_rows(foss_exports_0414, foss_exports_1525)

# Imports ----------------------------------------------------------------------
# read csv's
foss_imports_1525 <- read.csv('foss_imports_15-25.csv') %>%
  setNames(.[1, ]) %>%
  rename_with( ~ toupper(gsub(' ', '_', .x, fixed = T))) %>%
  rename_with( ~ toupper(gsub('(', '', .x, fixed = T))) %>%
  rename_with( ~ toupper(gsub(')', '', .x, fixed = T))) %>%
  .[-1, ] %>%
  mutate(HTS_NUMBER = ifelse(str_sub(HTS_NUMBER, 1, 1) == '0',
                             str_sub(HTS_NUMBER, 2, -1),
                             HTS_NUMBER),
         STATE = substr(US_CUSTOMS_DISTRICT, nchar(US_CUSTOMS_DISTRICT) - 1,
                        nchar(US_CUSTOMS_DISTRICT)),
         STATE = ifelse(STATE %in% c('NT', 'DS'), NA, STATE),
         US_CUSTOMS_DISTRICT = ifelse(is.na(STATE), US_CUSTOMS_DISTRICT,
                                      substr(US_CUSTOMS_DISTRICT, 0, nchar(US_CUSTOMS_DISTRICT) - 4)))

foss_imports_0414 <- read.csv('foss_imports_04-14 (2).csv') %>%
  setNames(.[1, ]) %>%
  rename_with( ~ toupper(gsub(' ', '_', .x, fixed = T))) %>%
  rename_with( ~ toupper(gsub('(', '', .x, fixed = T))) %>%
  rename_with( ~ toupper(gsub(')', '', .x, fixed = T))) %>%
  .[-1, ] %>%
  mutate(HTS_NUMBER = ifelse(str_sub(HTS_NUMBER, 1, 1) == '0',
                             str_sub(HTS_NUMBER, 2, -1),
                             HTS_NUMBER),
         STATE = substr(US_CUSTOMS_DISTRICT, nchar(US_CUSTOMS_DISTRICT) - 1,
                        nchar(US_CUSTOMS_DISTRICT)),
         STATE = ifelse(STATE %in% c('NT', 'DS'), NA, STATE),
         US_CUSTOMS_DISTRICT = ifelse(is.na(STATE), US_CUSTOMS_DISTRICT,
                                      substr(US_CUSTOMS_DISTRICT, 0, nchar(US_CUSTOMS_DISTRICT) - 4)))

# combine data (stack)
foss_imports <- bind_rows(foss_imports_0414, foss_imports_1525)

# Processed Products & Species Metadata ----------------------------------------
# read csv's
# foss_pp_1523 <- read.csv('foss_pp_15-23.csv') %>%
#   setNames(.[1, ]) %>%
#   rename_with( ~ toupper(gsub(' ', '_', .x, fixed = T))) %>%
#   .[-1, ]
# 
# foss_pp_0414 <- read.csv('foss_pp_04-14.csv') %>%
#   setNames(.[1, ]) %>%
#   rename_with( ~ toupper(gsub(' ', '_', .x, fixed = T))) %>%
#   .[-1, ]
# 
# # combine data (stack)
# foss_pp <- bind_rows(foss_pp_0414, foss_pp_1523)
# 
# # read csv of Species metadata
# pp_landings_map <- read.csv('pp_com_landings_mapping.csv') %>%
#   select(!X)

# Above are FOSS processed products; below is direct from database
  # 'Direct' meaning downloaded initially from our database
pp_address <- read.csv('pp_address.csv') %>%
  mutate(STATE = STATE_ABRV,
         STATE = ifelse(STATE %in% c('CM', 'MP'), 'NORTHERN MARIANA IS.',
                        ifelse(STATE == 'GU', 'GUAM',
                               ifelse(STATE == 'PR', 'PUERTO RICO',
                                      ifelse(STATE == 'AS', 'AMERICAN SAMOA',
                                             STATE)))),
         PLANT_CITY = ifelse(PLANT_CITY == 'BOZEMEN', 'BOZEMAN', PLANT_CITY),
         STATE = ifelse(PLANT_CITY == 'SAN FRANCISCO', 'CA', STATE),
         PLANT_CITY = ifelse(PLANT_CITY == 'EAST QUOQUE', 'EAST QUOGUE', PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'EAST SEATAUKUT', 'EAST SETAUKET', PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'GLEN FALLS', 'GLENS FALLS', PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'Hodgkins', 'HODGKINS', PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'APALCHICOLA', 'APALACHICOLA',
                             PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'FT LAUDERDALE', 'FORT LAUDERDALE',
                             PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'FT PIERCE', 'FORT PIERCE',
                             PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'GODDLAND', 'GOODLAND',
                             PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'Jacksonville', 'JACKSONVILLE',
                             PLANT_CITY),
         PLANT_STATE_ABRV = ifelse(PLANT_CITY == 'IRVINGTON', 'AL', PLANT_STATE_ABRV))

# Merge address data with pp_processed csv
pp_processed <- read.csv('pp_processed.csv') %>%
  filter(YEAR >= 2004) %>%
  left_join(pp_address %>%
              select(PP_IDNUM, PLANT_CITY, PLANT_STATE_ABRV, PLANT_STREET) %>%
              rename(STATE = PLANT_STATE_ABRV,
                     CITY = PLANT_CITY))


# Commercial Landings ----------------------------------------------------------
# read csv's
foss_com_1524 <- read.csv('foss_com_landings_15-24.csv') %>%
  setNames(.[1, ]) %>%
  rename_with( ~ toupper(gsub(' ', '_', .x, fixed = T))) %>%
  .[-1, ] %>%
  # fix error where POLLOCK, WALLEYE and SEA HARES have extra space in string
  mutate(NMFS_NAME = ifelse(NMFS_NAME == 'POLLOCK, WALLEYE ', 
                            'POLLOCK, WALLEYE',
                            NMFS_NAME),
         NMFS_NAME = ifelse(NMFS_NAME == 'SEA HARES ',
                            'SEA HARES', 
                            NMFS_NAME))

foss_com_0414 <- read.csv('foss_com_landings_04-14.csv') %>%
  setNames(.[1, ]) %>%
  rename_with( ~ toupper(gsub(' ', '_', .x, fixed = T))) %>%
  .[-1, ] %>%
  mutate(NMFS_NAME = ifelse(NMFS_NAME == 'POLLOCK, WALLEYE ', 
                            'POLLOCK, WALLEYE',
                            NMFS_NAME),
         NMFS_NAME = ifelse(NMFS_NAME == 'SEA HARES ',
                            'SEA HARES', 
                            NMFS_NAME))

# combine data (stack)
foss_com_landings <- bind_rows(foss_com_0414, foss_com_1524)

# GDPDEF Index -----------------------------------------------------------------
# read csv's
def_index <- read.csv('GDPDEF_2024_index.csv') %>%
  rename_with( ~ toupper(.x)) %>%
  rename(DEFLATOR_VALUE = GDPDEF_NBD20240101,
         YEAR = OBSERVATION_DATE) %>%
  # remove -01-01 from year as it is negligible (the index is averaged per year)
  mutate(YEAR = as.numeric(gsub('-01-01', '', YEAR)),
         INDEX = (100/DEFLATOR_VALUE))

# Species Reference and Mapping Data -------------------------------------------
# Imports and Exports in FOSS provide detailed descriptions of the products
  # However, these descriptions are cumbersome strings that are difficult
    # to parse for species names without a brute force method
# Thankfully, FOSS includes HTS (Harmonized Tariff Schedule) numbers which,
  # through the use of FTS reference tables
# We will import the reference table here to munge into the tables later
species_ref <- read.csv('FTS_PRODUCTS.csv')

# this sheet was developed by hand to categorize species into three groups:
  # species group, which groups like species by identifiers such as genera
  # species category, which groups like generas by collections, such as all crabs
  # ecological category, which groups like categories by shared traits, such as
    # crustaceans, or small pelagic fish, etc.
trade_map <- read.csv('trade_data_mapping_sheet.csv') %>%
  mutate(HTS_NUMBER = as.character(HTS_NUMBER))

# this sheet was developed by the same effort described above
landings_map <- read.csv('com_landings_mapping_sheet.csv') %>%
  mutate(TSN = as.character(TSN))

# this sheet was developed by the same effort described above
  # however, with a lack of available scientific names and specified common
  # names, this map contains more NAs and generic maps
# pp_map <- read.csv('pp_mapping_sheet.csv')

# Above contains the mapping sheet for FOSS processed products
# Below contains the mapping sheet for database processed products
pp_form_map <- read.csv('pp_form_map.csv')
pp_map <- read.csv('pp_db_map.csv') %>%
  left_join(pp_form_map)

# map to assign coasts to florida cities
florida_coast_map <- read.csv('florida_city_map.csv')

# map to assign great lakes cities
great_lakes_cities <- read.csv('gl_border_state_cities.csv')

# conversion factor map
conversion_factors <- read.csv('conversion_factors/conversion_factors.csv') %>%
  select(HTS_NUMBER, CF)

# Metadata (time of data access) -----------------------------------------------
trade_date <- 
  format(file.info('foss_exports_15-25.csv')$ctime,
         '%m/%d/%Y')
landings_date <- 
  format(file.info('foss_com_landings_15-24.csv')$ctime,
         '%m/%d/%Y')
products_date <- 
  format(file.info('pp_processed.csv')$ctime,
         '%m/%d/%Y')

#####################
### SAVE THE DATA ###
#####################
# create the save file name
# preserve the date of data creation for reference between data files
file_name <- paste0('seafood_trade_data_pull_', 
                    format(Sys.Date(), '%m_%d_%y'), 
                    '.RData')

# create the file
  # NOTE: add new data to this list upon creation in this script
save(list = c('foss_exports', 'foss_imports', 'pp_processed', 'def_index',
              'species_ref', 'foss_com_landings', 'florida_coast_map',
              'great_lakes_cities', 'trade_map', 'landings_map', 'pp_map',
              'conversion_factors', 'trade_date', 'landings_date',
              'products_date'),
     file = file_name)

# upload to google drive
drive_upload(file_name,
             # IMPORTANT: change path below to match your personal Drive
              # NOTE: no path results in save to Drive location specified
              #       above
             path = 'Seafood Trade Dashboard Project/Seafood Trade Data/',
             overwrite = T)
