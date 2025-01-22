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
# TODO: extract species from product information
# TODO: standardize prices with a given year's real dollar value
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

# Data summarizing
# Must do piece-wise due to two summarise() calls
# First piece: summarise # of product types exported by year, 
  # country name (exported to), customs district (exported from)
exports_products_smry <- foss_exports %>%
  select(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, FAO_COUNTRY_CODE,
         PRODUCT_NAME) %>%
  group_by(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, 
           FAO_COUNTRY_CODE) %>%
  summarise(PRODUCT_DIVERSITY = n_distinct(PRODUCT_NAME),
            .groups = 'drop')

# Second piece: summarise value and volume of exports by year, 
  # country name (exported to), customs district (exported from)
exports_price_smry <- foss_exports %>%
  select(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, FAO_COUNTRY_CODE,
         VALUE_USD, VOLUME_KG) %>%
  group_by(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, 
           FAO_COUNTRY_CODE) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  mutate(AVERAGE_PRICE_PER_KG = VALUE_USD / VOLUME_KG)

# Now combine to form one summary sheet
exports_smry <- full_join(exports_products_smry, exports_price_smry)


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

# Data summarizing
imports_products_smry <- foss_imports %>%
  select(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, FAO_COUNTRY_CODE,
         PRODUCT_NAME) %>%
  group_by(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT,
           FAO_COUNTRY_CODE) %>%
  summarise(PRODUCT_DIVERSITY = n_distinct(PRODUCT_NAME),
            .groups = 'drop')

imports_price_smry <- foss_imports %>%
  select(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, FAO_COUNTRY_CODE,
         VALUE_USD, VOLUME_KG, CALCULATED_DUTY_USD) %>%
  group_by(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, 
           FAO_COUNTRY_CODE) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  mutate(AVERAGE_PRICE_PER_KG = VALUE_USD / VOLUME_KG)

# Now combine to form one summary sheet
imports_smry <- full_join(imports_products_smry, imports_price_smry)

# Combine exports and imports --------------------------------------------------
# By combining exports and imports into one sheet, we can compare trade data
  # by continent, country, and us customs district and calculate surplus/deficit
# Eventually, once we parse out species IDs from product data, we can determine
# which species drive trends

# We need to change column names to coerce their join properly
exports_smry <- exports_smry %>%
  rename(EXP_PRODUCT_DIVERSITY = PRODUCT_DIVERSITY,
         EXP_VALUE_USD = VALUE_USD,
         EXP_VOLUME_KG = VOLUME_KG,
         EXP_AVERAGE_PRICE_PER_KG = AVERAGE_PRICE_PER_KG)

imports_smry <- imports_smry %>%
  rename(IMP_PRODUCT_DIVERSITY = PRODUCT_DIVERSITY,
         IMP_VALUE_USD = VALUE_USD,
         IMP_VOLUME_KG = VOLUME_KG,
         IMP_AVERAGE_PRICE_PER_KG = AVERAGE_PRICE_PER_KG) 
# calculated_duty_usd is unique to imports, so no need to change name

# full_join the tables to account for countries or customs districts that 
  # exclusively import or export
trade_data <- full_join(exports_smry, imports_smry)
# The resulting data frame includes import and export data attached to each
  # US Custom's District and Country of Origin or Export for every year from 
  # 2004 - 2024

