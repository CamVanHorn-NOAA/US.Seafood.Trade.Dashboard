# Script 2: Munge data for visualization
# See 1_data_pulls for data sourcing

# Contact: Cameron Van Horn
#          cameron.vanhorn@noaa.gov



#########################
### PACKAGES AND DATA ###
#########################
# Packages ---------------------------------------------------------------------
if(!require("googledrive")) install.packages("googledrive")
if(!require("tidyverse")) install.packages("tidyverse")

# Pull Data --------------------------------------------------------------------
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

# Regional designations --------------------------------------------------------
# FEUS 2022 source for regions: https://s3.amazonaws.com/media.fisheries.noaa.gov/2024-11/FEUS-2022-SPO248B.pdf
norpac <- c('AK', 'ALASKA')
pac <- c('CA', 'CALIFORNIA', 'OR', 'OREGON', 'WA', 'WASHINGTON')
pacisl <- c('HI', 'HAWAII', 'AS', 'CM', 'MP', 'GU')
neweng <- c('CT', 'CONNECTICUT', 'ME', 'MAINE', 'MA', 'MASSACHUSETTS', 'NH', 
            'NEW HAMPSHIRE', 'RI', 'RHODE ISLAND')
midatl <- c('DE', 'DELAWARE', 'MD', 'MARYLAND', 'NJ', 'NEW JERSEY', 'NY',
            'NEW YORK', 'VA', 'VIRGINIA', 'PA', 'PENNSYLVANIA', 'DC')
souatl <- c('GA', 'GEORGIA', 'NC', 'NORTH CAROLINA', 'SC', 'SOUTH CAROLINA',
            'FL-E', 'FLORIDA', 'PR', 'PUERTO RICO', 'VI', 'U.S. VIRGIN ISLANDS')
gulf <- c('AL', 'ALABAMA', 'LA', 'LOUISIANA', 'MS', 'MISSISSIPPI', 'TX', 'TEXAS',
          'FL-W')
# We are adding a Great Lakes region that is city-based, not state-based like
  # the FEUS. State exceptions include OH and MI, which are considered great
  # lake states
grlake <- c('OH', 'OHIO', 'MI', 'MICHIGAN')
# great lakes cities are defined as cities within 75 miles of the nearest great
  # lake
grlake_cities <- great_lakes_cities %>%
  mutate(MILES_TO_LAKE = as.numeric(MILES_TO_LAKE)) %>%
  filter(MILES_TO_LAKE <= 75)

##########################
### DATA SUMMARIZATION ###
##########################
# TODO: extract species from product information
# Exports ----------------------------------------------------------------------
# Data formatting
exports <- foss_exports %>%
  # set necessary columns to numeric
  # value and volume need commas removed for coercion
  mutate(VALUE_USD = as.numeric(gsub(',', '', VALUE_USD)),
         VOLUME_KG = as.numeric(gsub(',', '', VOLUME_KG)),
         CENSUS_COUNTRY_CODE = as.numeric(CENSUS_COUNTRY_CODE),
         CENSUS_DISTRICT_CODE = as.numeric(CENSUS_DISTRICT_CODE),
         FAO_COUNTRY_CODE = as.numeric(FAO_COUNTRY_CODE),
         YEAR = as.numeric(YEAR)) %>%
  filter(YEAR < 2025) %>%
  # use species_ref to attach species info to products
  left_join(species_ref %>% 
              select(HTS_NUMBER, GROUP_NAME, GROUP_TS, GROUP_CBP) %>%
              # remove duplicates to not create many-to-many relationships for
                # the join
              distinct()) %>%
  # use trade_map to attach categories to products
  left_join(trade_map %>%
              select(SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, 
                     ECOLOGICAL_CATEGORY, HTS_NUMBER) %>%
              distinct()) %>%
  # arrange by year then country name 
  arrange(YEAR, COUNTRY_NAME) %>%
  # Calculate standard prices with 2024 index
  # We standardize prices by setting a year as a reference for inflation indexing
    # Effectively, by setting 2024 as our reference year, we can calculate what
    # prices from prior years would be in 2024 dollars
    # This is accounts for price fluctuations exclusively due to inflation
  # To standardize, we set the reference year's index (2024) as the numerator 
    # and a given year's index (e.g., 2020) as the denominator
    # Then multiply this value by the price of the good in the given year to 
    # determine it's value in real 2024 dollars
    # We calculated the Index value in script 1
  left_join(def_index %>% select(YEAR, INDEX)) %>%
  mutate(EXP_VALUE_2024USD = VALUE_USD * INDEX) %>%
  select(-INDEX) %>%
  left_join(conversion_factors %>%
              mutate(HTS_NUMBER = as.character(HTS_NUMBER))) %>%
  mutate(CF = ifelse(is.na(CF), 1, CF),
         CONVERTED_VOLUME = VOLUME_KG * CF)


# Data summarizing
# Must do piece-wise due to two summarise() calls
# First piece: summarise # of product types exported by year, 
  # country name (exported to), customs district (exported from)
exports_products_smry <- exports %>%
  select(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE, FAO_COUNTRY_CODE,
         PRODUCT_NAME, GROUP_NAME, GROUP_TS, GROUP_CBP, SPECIES_NAME,
         SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  group_by(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE,
           FAO_COUNTRY_CODE, GROUP_NAME, GROUP_TS, GROUP_CBP, SPECIES_NAME,
           SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  summarise(EXP_PRODUCT_DIVERSITY = n_distinct(PRODUCT_NAME),
            .groups = 'drop')

# Second piece: summarise value and volume of exports by year, 
  # country name (exported to), customs district (exported from)
exports_price_smry <- exports %>%
  select(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE, FAO_COUNTRY_CODE,
         VALUE_USD, EXP_VALUE_2024USD, VOLUME_KG, CONVERTED_VOLUME, GROUP_NAME, 
         GROUP_TS, GROUP_CBP, SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY,
         ECOLOGICAL_CATEGORY) %>%
  group_by(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE, 
           FAO_COUNTRY_CODE, GROUP_NAME, GROUP_TS, GROUP_CBP, SPECIES_NAME,
           SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  mutate(EXP_AVERAGE_PRICE_PER_KG = VALUE_USD / VOLUME_KG,
         EXP_AVERAGE_2024PRICE_PER_KG = EXP_VALUE_2024USD / VOLUME_KG)

# Now combine to form one summary sheet
exports_smry <- full_join(exports_products_smry, exports_price_smry)


# Imports ----------------------------------------------------------------------
# Data formatting
imports <- foss_imports %>%
  mutate(VALUE_USD = as.numeric(gsub(',', '', VALUE_USD)),
         VOLUME_KG = as.numeric(gsub(',', '', VOLUME_KG)),
         CENSUS_COUNTRY_CODE = as.numeric(CENSUS_COUNTRY_CODE),
         CENSUS_DISTRICT_CODE = as.numeric(CENSUS_DISTRICT_CODE),
         FAO_COUNTRY_CODE = as.numeric(FAO_COUNTRY_CODE),
         CALCULATED_DUTY_USD = as.numeric(gsub(',', '', CALCULATED_DUTY_USD)),
         YEAR = as.numeric(YEAR)) %>%
  filter(YEAR < 2025) %>%
  left_join(species_ref %>% 
              select(HTS_NUMBER, GROUP_NAME, GROUP_TS, GROUP_CBP) %>%
              distinct()) %>%
  left_join(trade_map %>%
              select(SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, 
                     ECOLOGICAL_CATEGORY, HTS_NUMBER) %>%
              distinct()) %>%
  arrange(YEAR, COUNTRY_NAME) %>%
  left_join(def_index %>% select(YEAR, INDEX)) %>%
  mutate(IMP_VALUE_2024USD = VALUE_USD * INDEX,
         IMP_CALCULATED_DUTY_2024USD = CALCULATED_DUTY_USD * INDEX) %>%
  select(-INDEX) %>%
  left_join(conversion_factors %>%
              mutate(HTS_NUMBER = as.character(HTS_NUMBER))) %>%
  mutate(CF = ifelse(is.na(CF), 1, CF),
         CONVERTED_VOLUME = VOLUME_KG * CF)
  

# Data summarizing
imports_products_smry <- imports %>%
  select(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE, FAO_COUNTRY_CODE,
         PRODUCT_NAME, GROUP_NAME, GROUP_TS, GROUP_CBP, SPECIES_NAME, 
         SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  group_by(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE,
           FAO_COUNTRY_CODE, GROUP_NAME, GROUP_TS, GROUP_CBP, SPECIES_NAME,
           SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  summarise(IMP_PRODUCT_DIVERSITY = n_distinct(PRODUCT_NAME),
            .groups = 'drop')

imports_price_smry <- imports %>%
  select(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE, FAO_COUNTRY_CODE,
         VALUE_USD, VOLUME_KG, CONVERTED_VOLUME, IMP_VALUE_2024USD, CALCULATED_DUTY_USD,
         IMP_CALCULATED_DUTY_2024USD, GROUP_NAME, GROUP_TS, GROUP_CBP,
         SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  group_by(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE,
           FAO_COUNTRY_CODE, GROUP_NAME, GROUP_TS, GROUP_CBP, SPECIES_NAME,
           SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  mutate(IMP_AVERAGE_PRICE_PER_KG = VALUE_USD / VOLUME_KG,
         IMP_AVERAGE_2024PRICE_PER_KG = IMP_VALUE_2024USD / VOLUME_KG)

# Now combine to form one summary sheet
imports_smry <- full_join(imports_products_smry, imports_price_smry)

# Combine exports and imports --------------------------------------------------
# By combining exports and imports into one sheet, we can compare trade data
  # by continent, country, and us customs district and calculate surplus/deficit
# Eventually, once we parse out species IDs from product data, we can determine
# which species drive trends

# We need to change column names to coerce their join properly
exports_smry <- exports_smry %>%
  rename(EXP_VALUE_USD = VALUE_USD,
         EXP_VOLUME_KG = VOLUME_KG,
         EXP_CONVERTED_VOLUME = CONVERTED_VOLUME)

imports_smry <- imports_smry %>%
  rename(IMP_VALUE_USD = VALUE_USD,
         IMP_VOLUME_KG = VOLUME_KG,
         IMP_CONVERTED_VOLUME = CONVERTED_VOLUME) 
# calculated_duty_usd is unique to imports, so no need to change name

# full_join the tables to account for countries or customs districts that 
  # exclusively import or export
trade_data <- full_join(exports_smry, imports_smry) %>%
  # split florida by east and west
  left_join(florida_coast_map %>%
              rename(US_CUSTOMS_DISTRICT = PLANT_CITY,
                     FLORIDA_STATE = PLANT_STATE_ABRV) %>%
              select(!c(PLANT_COAST_GEMINI, PLANT_COAST))) %>%
  mutate(STATE = ifelse(!is.na(FLORIDA_STATE), FLORIDA_STATE, STATE)) %>%
  select(!FLORIDA_STATE) %>%
  # add regions
  mutate(REGION = ifelse(STATE %in% norpac, 'North Pacific', NA),
         REGION = ifelse(STATE %in% pac, 'Pacific', REGION),
         REGION = ifelse(STATE %in% pacisl, 'West Pacific', REGION),
         REGION = ifelse(STATE %in% neweng, 'New England', REGION),
         REGION = ifelse(STATE %in% midatl, 'Mid-Atlantic', REGION),
         REGION = ifelse(STATE %in% souatl, 'South Atlantic', REGION),
         REGION = ifelse(STATE %in% gulf, 'Gulf', REGION),
         REGION = ifelse(STATE %in% grlake, 'Great Lakes', REGION),
         REGION = ifelse(STATE %in% grlake_cities$PLANT_STATE_ABRV &
                           US_CUSTOMS_DISTRICT %in% grlake_cities$PLANT_CITY,
                         'Great Lakes', REGION)) 
# The resulting data frame includes import and export data attached to each
  # US Custom's District and Country of Origin or Export, with species data, 
  # for every year from 2004 - 2024


# Grabbing confidential data from processed products ---------------------------
# confidential data in this case only exists in processed products data
# Data is considered confidential if there are less than 3 (i.e., 1 or 2) records
  # in a group. Here, the group would be all species classification levels (i.e.,
  # ecological category, species category, species group, and species name), 
  # product form, and plant WITHIN A REGION. In other words, if only one or two 
  # plants process a species to a unique condition (e.g., fillet, canned, etc.)
  # within a region, those records are confidential. 
processed_confids <- left_join(pp_processed, pp_map) %>%
  # split florida by east and west
  left_join(florida_coast_map %>%
              rename(CITY = PLANT_CITY,
                     FLORIDA_STATE = PLANT_STATE_ABRV) %>%
              select(!c(PLANT_COAST_GEMINI, PLANT_COAST))) %>%
  mutate(STATE = ifelse(!is.na(FLORIDA_STATE), FLORIDA_STATE, STATE)) %>%
  select(!FLORIDA_STATE) %>%
  # add regions
  mutate(REGION = ifelse(STATE %in% norpac, 'North Pacific', NA),
         REGION = ifelse(STATE %in% pac, 'Pacific', REGION),
         REGION = ifelse(STATE %in% pacisl, 'West Pacific', REGION),
         REGION = ifelse(STATE %in% neweng, 'New England', REGION),
         REGION = ifelse(STATE %in% midatl, 'Mid-Atlantic', REGION),
         REGION = ifelse(STATE %in% souatl, 'South Atlantic', REGION),
         REGION = ifelse(STATE %in% gulf, 'Gulf', REGION),
         REGION = ifelse(STATE %in% grlake, 'Great Lakes', REGION),
         REGION = ifelse(STATE %in% grlake_cities$PLANT_STATE_ABRV &
                           CITY %in% grlake_cities$PLANT_CITY,
                         'Great Lakes', REGION))
# Pacific Confidentials
confid_pacific <- processed_confids %>%
  filter(REGION == 'Pacific') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'Pacific')
# West Pacific Confidentials
confid_westpacific <- processed_confids %>%
  filter(REGION == 'West Pacific') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'West Pacific')
# North Pacific Confidentials
confid_norpac <- processed_confids %>%
  filter(REGION == 'North Pacific') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'North Pacific')
# New England Confidentials
confid_newengland <- processed_confids %>%
  filter(REGION == 'New England') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'New England')
# Mid-Atlantic Confidentials
confid_midatlantic <- processed_confids %>%
  filter(REGION == 'Mid-Atlantic') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'Mid-Atlantic')
# South Atlantic Confidentials
confid_southatlantic <- processed_confids %>%
  filter(REGION == 'South Atlantic') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'South Atlantic')
# Gulf Confidentials
confid_gulf <- processed_confids %>%
  filter(REGION == 'Gulf') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'Gulf')
# Great Lakes Confidentials
confid_greatlakes <- processed_confids %>%
  filter(REGION == 'Great Lakes') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'Great Lakes')
# combine
confid_products <- rbind(confid_pacific, confid_westpacific, confid_norpac,
                         confid_newengland, confid_midatlantic, confid_southatlantic,
                         confid_gulf, confid_greatlakes) %>%
  ungroup() %>%
  filter(n < 3) %>%
  select(!n) %>%
  mutate(CONFIDENTIAL = 1)
# Processed Products -----------------------------------------------------------
# Data formatting
pp_data <- pp_processed %>%
  # connect groups from map
  left_join(pp_map) %>%
  mutate(YEAR = as.numeric(YEAR),
         POUNDS = as.numeric(gsub(',', '', POUNDS)),
         DOLLARS = as.numeric(gsub(',', '', DOLLARS)),
         # convert pounds to kilograms in separate column
         KG = POUNDS * 0.45359237) %>%
  arrange(YEAR, SPECIES_NAME, PRODUCT_FORM) %>%
  # reorder columns so species is left of PRODUCT_FORM for ease of viewing
  select(YEAR, SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, STATE, CITY,
         ECOLOGICAL_CATEGORY, PRODUCT_FORM, POUNDS, DOLLARS, KG) %>%
  left_join(def_index %>% select(YEAR, INDEX)) %>%
  mutate(DOLLARS_2024 = DOLLARS * INDEX,
         DOLLARS_PER_LB = DOLLARS / POUNDS,
         DOLLARS_PER_KG = DOLLARS / KG,
         DOLLARS_2024_PER_LB = DOLLARS_2024 / POUNDS,
         DOLLARS_2024_PER_KG = DOLLARS_2024 / KG) %>%
  select(-INDEX) %>%
  # split florida by east and west
  left_join(florida_coast_map %>%
              rename(CITY = PLANT_CITY,
                     FLORIDA_STATE = PLANT_STATE_ABRV) %>%
              select(!c(PLANT_COAST_GEMINI, PLANT_COAST))) %>%
  mutate(STATE = ifelse(!is.na(FLORIDA_STATE), FLORIDA_STATE, STATE)) %>%
  select(!FLORIDA_STATE) %>%
  # add regions
  mutate(REGION = ifelse(STATE %in% norpac, 'North Pacific', NA),
         REGION = ifelse(STATE %in% pac, 'Pacific', REGION),
         REGION = ifelse(STATE %in% pacisl, 'West Pacific', REGION),
         REGION = ifelse(STATE %in% neweng, 'New England', REGION),
         REGION = ifelse(STATE %in% midatl, 'Mid-Atlantic', REGION),
         REGION = ifelse(STATE %in% souatl, 'South Atlantic', REGION),
         REGION = ifelse(STATE %in% gulf, 'Gulf', REGION),
         REGION = ifelse(STATE %in% grlake, 'Great Lakes', REGION),
         REGION = ifelse(STATE %in% grlake_cities$PLANT_STATE_ABRV &
                           CITY %in% grlake_cities$PLANT_CITY,
                         'Great Lakes', REGION)) %>%
  left_join(confid_products) %>%
  # mark confidential records' values as 0
  mutate(CONFIDENTIAL = ifelse(is.na(CONFIDENTIAL), 0, CONFIDENTIAL),
         POUNDS = ifelse(CONFIDENTIAL == 1, 0, POUNDS),
         DOLLARS = ifelse(CONFIDENTIAL == 1, 0, DOLLARS),
         KG = ifelse(CONFIDENTIAL == 1, 0, KG),
         DOLLARS_2024 = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_2024),
         DOLLARS_PER_LB = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_PER_LB),
         DOLLARS_PER_KG = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_PER_KG),
         DOLLARS_2024_PER_LB = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_2024_PER_LB),
         DOLLARS_2024_PER_KG = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_2024_PER_KG))

# Commercial Landings ----------------------------------------------------------
# Data formatting
com_landings <- foss_com_landings %>%
  mutate(YEAR = as.numeric(YEAR),
         POUNDS = as.numeric(gsub(',', '', POUNDS)),
         METRIC_TONS = as.numeric(gsub(',', '', METRIC_TONS)),
         DOLLARS = as.numeric(gsub(',', '', DOLLARS))) %>%
  # connect groups from map
  left_join(landings_map %>%
              select(NMFS_NAME, TSN, SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
              distinct()) %>%
  left_join(def_index %>% select(YEAR, INDEX)) %>%
  mutate(DOLLARS_2024 = DOLLARS * INDEX,
         KG = POUNDS * 0.45359237,
         DOLLARS_2024_PER_LB = DOLLARS_2024 / POUNDS,
         DOLLARS_2024_PER_KG = DOLLARS_2024 / KG) %>%
  select(-INDEX) %>%
  # add regions
  mutate(REGION = ifelse(STATE %in% norpac, 'North Pacific', NA),
         REGION = ifelse(STATE %in% pac, 'Pacific', REGION),
         REGION = ifelse(STATE %in% pacisl, 'West Pacific', REGION),
         REGION = ifelse(STATE %in% neweng, 'New England', REGION),
         REGION = ifelse(STATE %in% midatl, 'Mid-Atlantic', REGION),
         REGION = ifelse(STATE %in% souatl, 'South Atlantic', REGION),
         REGION = ifelse(STATE %in% gulf, 'Gulf', REGION),
         REGION = ifelse(STATE %in% grlake, 'Great Lakes', REGION))

# Remove cities and states for confidentiality ---------------------------------
pp_data <- pp_data %>%
  select(!c(CITY, STATE, DOLLARS_PER_LB, DOLLARS_PER_KG, DOLLARS_2024_PER_LB,
            DOLLARS_2024_PER_KG)) %>%
  group_by(YEAR, SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, 
           ECOLOGICAL_CATEGORY, PRODUCT_FORM, REGION, CONFIDENTIAL) %>%
  summarise(across(where(is.numeric), sum),
            .groups ='drop') %>%
  mutate(DOLLARS_PER_LB = DOLLARS / POUNDS,
         DOLLARS_PER_KG = DOLLARS / KG,
         DOLLARS_2024_PER_LB = DOLLARS_2024 / POUNDS,
         DOLLARS_2024_PER_KG = DOLLARS_2024 / KG,
         DOLLARS_PER_LB = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_PER_LB),
         DOLLARS_PER_KG = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_PER_KG),
         DOLLARS_2024_PER_LB = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_2024_PER_LB),
         DOLLARS_2024_PER_KG = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_2024_PER_KG))

#####################
### SAVE THE DATA ###
#####################
# create the save file name
# preserve the date of data creation for reference between data files
file_name <- paste0('seafood_trade_data_munge_', 
                    format(Sys.Date(), '%m_%d_%y'), 
                    '.RData')

# create the file
# NOTE: add new data to this list upon creation in this script
save(list = c('trade_data', 'pp_data', 'com_landings'),
     file = file_name)

# upload to google drive
drive_upload(file_name,
             # IMPORTANT: change path below to match your personal Drive
             # NOTE: no path results in save to Drive location specified
             #       above
             path = 'Seafood Trade Dashboard Project/Seafood Trade Data/',
             overwrite = T)
