# Script 2: Munge data for visualization
# See 1_data_pulls for data sourcing

# Contact: Cameron Van Horn
#          cameron.vanhorn@noaa.gov



#########################
### PACKAGES AND DATA ###
#########################
# Packages
if(!require("googledrive")) install.packages("googledrive")
if(!require("tidyverse")) install.packages("tidyverse")

# Authorize link to google drive
drive_auth()

# Grab most recent data file
  # only take first two columns (the third column is a list of metadata)
data_file <- drive_find(pattern = 'seafood_trade_data_pull')[, 1:2] %>%
  # set column names to caps for best data practice
  rename_with(toupper) %>%
  # isolate date of data creation into separate string
    # recall that script 1 embeds date of data creation into file name
  mutate(DATE = str_remove(NAME, 'seafood_trade_data_pull_'),
         DATE = str_remove(DATE, '.RData'),
         # format date to find the most recent date
         date = as.Date(DATE, format = '%m_%d_%y')) %>%
  # filter for most recent date (max)
  filter(DATE == max(DATE)) %>%
  select(NAME)
  
# Download the data
drive_download(data_file$NAME,
               overwrite = T)

# Load the data
load(data_file$NAME)
# clean environment
rm(data_file)

##########################
### DATA SUMMARIZATION ###
##########################
# TODO: summarize export & import volumes 
# TODO: summarize export & import values
# TODO: summarize export & import prices (value/volume)
# TODO: extract species from product information
# Exports ----------------------------------------------------------------------
# Data formatting
foss_exports <- foss_exports %>%
  # set necessary columns to numeric
  # value and volume need commas removed for coercion
  mutate(VALUE_USD = as.numeric(gsub(',', '', VALUE_USD)),
         VOLUME_KG = as.numeric(gsub(',', '', VOLUME_KG)),
         CENSUS_COUNTRY_CODE = as.numeric(CENSUS_COUNTRY_CODE),
         CENSUS_DISTRICT_CODE = as.numeric(CENSUS_DISTRICT_CODE),
         FAO_COUNTRY_CODE = as.numeric(FAO_COUNTRY_CODE),
         YEAR = as.numeric(YEAR)) %>%
  # arrange by year then country name 
  arrange(YEAR, COUNTRY_NAME)

# Data summarization
exports_summary <- foss_exports %>%
  select(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, FAO_COUNTRY_CODE,
         VALUE_USD, VOLUME_KG) %>%
  group_by(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, 
           FAO_COUNTRY_CODE) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  mutate(AVERAGE_PRICE_PER_KG = VALUE_USD / VOLUME_KG)


# Imports ----------------------------------------------------------------------
# Data formatting
foss_imports <- foss_imports %>%
  mutate(VALUE_USD = as.numeric(gsub(',', '', VALUE_USD)),
         VOLUME_KG = as.numeric(gsub(',', '', VOLUME_KG)),
         CENSUS_COUNTRY_CODE = as.numeric(CENSUS_COUNTRY_CODE),
         CENSUS_DISTRICT_CODE = as.numeric(CENSUS_DISTRICT_CODE),
         FAO_COUNTRY_CODE = as.numeric(FAO_COUNTRY_CODE),
         CALCULATED_DUTY_USD = as.numeric(gsub(',', '', CALCULATED_DUTY_USD)),
         YEAR = as.numeric(YEAR)) %>%
  arrange(YEAR, COUNTRY_NAME)

# Data summarization
imports_summary <- foss_imports %>%
  select(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, FAO_COUNTRY_CODE,
         VALUE_USD, VOLUME_KG, CALCULATED_DUTY_USD) %>%
  group_by(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, 
           FAO_COUNTRY_CODE) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  mutate(AVERAGE_PRICE_PER_KG = VALUE_USD / VOLUME_KG)