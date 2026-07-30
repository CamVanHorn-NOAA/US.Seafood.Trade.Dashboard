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
# Data Validation --------------------------------------------------------------
source('data_pull_check.R')
# Pull Data --------------------------------------------------------------------
# Authorize link to google drive
drive_auth()

# Grab most recent data file
  # only take first two columns (the third column is a list of metadata)
# data_file <- drive_find(pattern = 'seafood_trade_data_pull')[, 1:2] %>%
#   # set column names to caps for best data practice
#   rename_with(toupper) %>%
#   # isolate date of data creation into separate string
#     # recall that script 1 embeds date of data creation into file name
#   mutate(DATE = str_remove(NAME, 'seafood_trade_data_pull_'),
#          DATE = str_remove(DATE, '.RData'),
#          # format date to find the most recent date
#          date = as.Date(DATE, format = '%m_%d_%y')) %>%
#   # filter for most recent date (max)
#   filter(DATE == max(DATE)) %>%
#   select(NAME)
#   
# # Download the data
# drive_download(data_file$NAME,
#                overwrite = T)
# 
# # Load the data
# load(data_file$NAME)
load('seafood_trade_data_pull_07_09_26.RData')
# clean environment
# rm(data_file)

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
            'FL-E', 'FLORIDA-EAST', 'FLORIDA', 'PR', 'PUERTO RICO', 'VI', 'U.S. VIRGIN ISLANDS')
gulf <- c('AL', 'ALABAMA', 'LA', 'LOUISIANA', 'MS', 'MISSISSIPPI', 'TX', 'TEXAS',
          'FL-W', 'FLORIDA-WEST')
# We are adding a Great Lakes region that is city-based, not state-based like
  # the FEUS. State exceptions include OH, MI, MN and WI, which are considered great
  # lake states
grlake <- c('OH', 'OHIO', 'MI', 'MICHIGAN', 'MINNESOTA', 'WISCONSIN')
# great lakes cities are defined as cities within 75 miles of the nearest great
  # lake
grlake_cities <- great_lakes_cities %>%
  filter(!is.na(MILES_TO_LAKE)) %>%
  filter(MILES_TO_LAKE != '200 +') %>%
  filter(MILES_TO_LAKE != '200+') %>%
  mutate(MILES_TO_LAKE = as.numeric(MILES_TO_LAKE)) %>%
  filter(MILES_TO_LAKE <= 75)

##########################
### DATA SUMMARIZATION ###
##########################
# TODO: extract species from product information
# Exports ----------------------------------------------------------------------
# Data formatting
exports <- export_pull %>%
  # set necessary columns to numeric
  # value and volume need commas removed for coercion
  mutate(YEAR = as.numeric(YEAR)) %>%
  filter(YEAR < 2026) %>%
  # use species_ref to attach species info to products
  left_join(species_ref %>% 
              select(HTS_NUMBER) %>%
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
  select(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE, GROUP_TS,
         PRODUCT_NAME, SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, 
         ECOLOGICAL_CATEGORY) %>%
  group_by(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE, GROUP_TS,
           SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  summarise(EXP_PRODUCT_DIVERSITY = n_distinct(PRODUCT_NAME),
            .groups = 'drop')

# Second piece: summarise value and volume of exports by year, 
  # country name (exported to), customs district (exported from)
exports_price_smry <- exports %>%
  select(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE, GROUP_TS,
         VALUE_USD, EXP_VALUE_2024USD, VOLUME_KG, CONVERTED_VOLUME, 
         SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  group_by(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE, GROUP_TS,
           SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  mutate(EXP_AVERAGE_PRICE_PER_KG = VALUE_USD / VOLUME_KG,
         EXP_AVERAGE_2024PRICE_PER_KG = EXP_VALUE_2024USD / VOLUME_KG)

# Now combine to form one summary sheet
exports_smry <- full_join(exports_products_smry, exports_price_smry)


# Imports ----------------------------------------------------------------------
# Data formatting
imports <- import_pull %>%
  mutate(YEAR = as.numeric(YEAR)) %>%
  filter(YEAR < 2026) %>%
  left_join(species_ref %>% 
              select(HTS_NUMBER) %>%
              distinct()) %>%
  left_join(trade_map %>%
              select(SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, 
                     ECOLOGICAL_CATEGORY, HTS_NUMBER) %>%
              distinct()) %>%
  arrange(YEAR, COUNTRY_NAME) %>%
  left_join(def_index %>% select(YEAR, INDEX)) %>%
  mutate(IMP_VALUE_2024USD = VALUE_USD * INDEX) %>%
  select(-INDEX) %>%
  left_join(conversion_factors %>%
              mutate(HTS_NUMBER = as.character(HTS_NUMBER))) %>%
  mutate(CF = ifelse(is.na(CF), 1, CF),
         CONVERTED_VOLUME = VOLUME_KG * CF)
  

# Data summarizing
imports_products_smry <- imports %>%
  select(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE, GROUP_TS,
         PRODUCT_NAME, SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, 
         ECOLOGICAL_CATEGORY) %>%
  group_by(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE, GROUP_TS,
           SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  summarise(IMP_PRODUCT_DIVERSITY = n_distinct(PRODUCT_NAME),
            .groups = 'drop')

imports_price_smry <- imports %>%
  select(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE, GROUP_TS,
         VALUE_USD, VOLUME_KG, CONVERTED_VOLUME, IMP_VALUE_2024USD,
         SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  group_by(YEAR, CONTINENT, COUNTRY_NAME, US_CUSTOMS_DISTRICT, STATE, GROUP_TS,
           SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
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
                         'Great Lakes', REGION),
         REGION = ifelse(is.na(REGION), 'No Region Assigned', REGION)) 
# The resulting data frame includes import and export data attached to each
  # US Custom's District and Country of Origin or Export, with species data, 
  # for every year from 2004 - 2024


# Grabbing confidential data from processed products ---------------------------
# confidential data in this case only exists in processed products data
# Data is considered confidential if there are less than 3 (i.e., 1 or 2) records
  # in a group. Because our classification hierarchy has four levels, and because 
  # not all products provide information up to the species name, we must isolate
  # confidentiality at each level of the hierarchy. Also, because the user may
  # or may not investigate region, this must be done both at the regional and 
  # outside the regional level. This will be many steps, but the fundamentals are:
  # 1) Determine what would be confidential in the data presently. For those products,
  # change their product form to be OTHER so that they may aggregate.
  # 2) Determine what would be confidential after adjusting product forms. For
  # those products, remove species classification level by level.
  # 3) Determine what would be confidential.
products <- left_join(pp_processed, pp_map) %>%
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
                         'Great Lakes', REGION),
         REGION = ifelse(is.na(REGION), 'No Region Assigned', REGION),
         # NEW_PRODUCT_FORM will store updated product conditions so that
          # PRODUCT_FORM can retain prior, more specific data
         NEW_PRODUCT_FORM = PRODUCT_FORM,
         # OLD_ levels of the classification hierarchy will store initial
           # species classifications that may be set to NA for confidentiality
         OLD_SPECIES_NAME = SPECIES_NAME,
         OLD_SPECIES_GROUP = SPECIES_GROUP,
         OLD_SPECIES_CATEGORY = SPECIES_CATEGORY,
         OLD_ECOLOGICAL_CATEGORY = ECOLOGICAL_CATEGORY,
         # CONFIDENTIAL is a placeholder for labeling data as confidential
         CONFIDENTIAL = NA) 

# Our first step is to identify what products would currently be confidential,
  # then adjust their product form to OTHER so that they may aggregate and no
  # longer be confidential. We will use a function.
overwrite_prodform <- function(data, cols = '', region = '') {
  # here, data should be processed product data as formatted above 
  # cols is a vector of columns to group by and is defaulted as an empty string;
    # acceptable inputs are columns that exist in the inputted data
    # ORDER MATTERS: the last column included in the vector should be the finest
    # level of resolution within the species hierarchy
    # This is because we need to remove any NAs in that column such that the
    # data only includes products with 
  # region is defaulted as an empty string; acceptable inputs are specific
    # regions
  
  # if cols is empty (i.e., no desired columns to group by), set cols to be 
    # NEW_PRODUCT_FORM 
  if ('' %in% cols) {
    cols <- c('NEW_PRODUCT_FORM')
  }
  # Identify what the last column in cols is (this should be the lowest level
    # of the classification hierarchy) and set it as object of type quosure
    # to work in dplyr pipe
  level_filter <- cols[length(cols)]
  level_filter <- as.symbol(level_filter)
  level_filter <- rlang::enquo(level_filter)
  
  # there are three possible ways to munge the data:
  # 1) if no region is provided
  if (region == '') {
    data %>%
      # all inputted columns and non-negotiable columns
      # all_of() allows us to use a vector of strings in a dplyr pipe
      select(YEAR, NEW_PRODUCT_FORM, all_of(cols), PLANT_STREET) %>%
      # Some plant addresses are blank or NA, so we don't worry about those for 
        # confidentiality
      # We also don't want any NAs of the finest level of classification - this
        # ensures that we aren't accidentally marking confidential data that
        # is nonspecific to the desired classification level
      # In the event that no columns are provided, this will be NEW_PRODUCT_FORM
        # which will ensure that we leave out products without a provided condition
      filter(PLANT_STREET != '',
             !is.na(PLANT_STREET),
             !is.na(!!level_filter)) %>%
      # remove duplicates from the data so plants are NOT double-counted
      distinct() %>%
      # group by all columns except plant street so we count the number of plants
        # for our desired group of columns
      group_by(across(c(-PLANT_STREET))) %>%
      count() %>%
      # filter for combos that have fewer than 3 plants 
      filter(n < 3) %>%
      ungroup() %>%
      # join back to the original data so now there is an extra column of 'n'
        # this column will have a number for any product found to have less than
        # 3 plants which process said product, and NA for any others
      right_join(data) %>%
      # we now want to overwrite NEW_PRODUCT_FORM to be OTHER for any products
        # listed at less than 3 plants
      # In the event a product was listed at less than 3 plants for one group
        # and more than 3 for another, we do not want to lose that work and overwrite
        # back to the previous product form, so we also check to see if CONFIDENTIAL
        # is NA or not to preserve prior confidentiality checks
      mutate(NEW_PRODUCT_FORM = ifelse(!is.na(n), 'OTHER', 
                                       ifelse(!is.na(CONFIDENTIAL), 'OTHER', PRODUCT_FORM)),
             # we update CONFIDENTIAL to 1 if the product was found to be
              # confidential in the current grouping
             CONFIDENTIAL = ifelse(!is.na(n), 1, CONFIDENTIAL)) %>%
      # remove the 'n' column so now data is same structure as inputted
      select(!n)
    
    # 2) if region is North Pacific 
  } else if (region == 'North Pacific') {
    # the North Pacific is unique to other regions in that there are only
      # 2 plant addresses provided, and one of the two is blank
    # this means there is only one real address provided, which makes selecting
      # confidential records more complicated
    # we perform the same process as above save for an important distinction:
      # we select confidential records as those with n == 1 AND the plant
      # address is not blank (this means the product was only processed at
      # the plant with an associated address)
    data %>%
      filter(REGION == 'North Pacific') %>%
      select(YEAR, NEW_PRODUCT_FORM, all_of(cols), PLANT_STREET) %>%
      # We need the plants of blank addresses to be included so only filter out
        # the level_filter
      filter(!is.na(!!level_filter)) %>%
      distinct() %>%
      group_by(across(c(-PLANT_STREET))) %>%
      # we need the plant address to know which products were only processed
        # at the plant with a provided address, so we use n() instead of count()
      mutate(n = n()) %>%
      # only retain products processed at 1 plant AND with a provided address
      filter(n == 1,
             PLANT_STREET != '') %>%
      # we add a column for the region so that upon joining back to the data,
        # it only joins to products within that specific region, not all products
        # of that combination
      mutate(REGION = 'North Pacific') %>%
      # remove plant street for joining
      select(!PLANT_STREET) %>%
      # should only join on region, year, new product form, and selected columns
      right_join(data) %>%
      mutate(NEW_PRODUCT_FORM = ifelse(!is.na(n), 'OTHER',
                                       ifelse(!is.na(CONFIDENTIAL), 'OTHER', PRODUCT_FORM)),
             CONFIDENTIAL = ifelse(!is.na(n), 1, CONFIDENTIAL)) %>%
      select(!n)
    
    # 3) Regions that are not the North Pacific
  } else {
    # this is a hybrid of 1) and 2), where the only difference from 1) is that
      # we filter for the inputted region and add the column for region later
      # for joining
    data %>%
      filter(REGION == region) %>%
      select(YEAR, NEW_PRODUCT_FORM, all_of(cols), PLANT_STREET) %>%
      filter(PLANT_STREET != '',
             !is.na(PLANT_STREET),
             !is.na(!!level_filter)) %>%
      distinct() %>%
      group_by(across(c(-PLANT_STREET))) %>%
      count() %>%
      filter(n < 3) %>%
      ungroup() %>%
      mutate(REGION = region) %>%
      right_join(data) %>%
      mutate(NEW_PRODUCT_FORM = ifelse(!is.na(n), 'OTHER', 
                                       ifelse(!is.na(CONFIDENTIAL), 'OTHER', PRODUCT_FORM)),
             CONFIDENTIAL = ifelse(!is.na(n), 1, CONFIDENTIAL)) %>%
      select(!n)
  }
}
# the end result of the above function is a dataset identical in structure to
  # the inputted data, with some changes made to NEW_PRODUCT_FORM and CONFIDENTIAL
# the function is designed where it can be used in a pipe and no prior effort
  # is lost

# pipe for overwriting product forms to OTHER
overwritten_products <- products %>%
  # first identify data that is confidential without aggregation (i.e., 
    # product conditions that are only processed at 1 or 2 plants)
  overwrite_prodform() %>%
  # FIRST SECTION: Each level of the classification hierarchy without region
  overwrite_prodform('ECOLOGICAL_CATEGORY') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY')) %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP')) %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME')) %>%
  # SECOND SECTION: Each level of the classification hierarchy for EACH region
  # Pacific
  overwrite_prodform(region = 'Pacific') %>%
  overwrite_prodform('ECOLOGICAL_CATEGORY', region = 'Pacific') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'Pacific') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'Pacific') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'Pacific') %>%
  # North Pacific
  overwrite_prodform(region = 'North Pacific') %>%
  overwrite_prodform('ECOLOGICAL_CATEGORY', region = 'North Pacific') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'North Pacific') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'North Pacific') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'North Pacific') %>%
  # West Pacific
  overwrite_prodform(region = 'West Pacific') %>%
  overwrite_prodform('ECOLOGICAL_CATEGORY', region = 'West Pacific') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'West Pacific') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'West Pacific') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'West Pacific') %>%
  # New England
  overwrite_prodform(region = 'New England') %>%
  overwrite_prodform('ECOLOGICAL_CATEGORY', region = 'New England') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'New England') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'New England') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'New England') %>%
  # Mid-Atlantic
  overwrite_prodform(region = 'Mid-Atlantic') %>%
  overwrite_prodform('ECOLOGICAL_CATEGORY', region = 'Mid-Atlantic') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'Mid-Atlantic') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'Mid-Atlantic') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'Mid-Atlantic') %>%
  # South Atlantic
  overwrite_prodform(region = 'South Atlantic') %>%
  overwrite_prodform('ECOLOGICAL_CATEGORY', region = 'South Atlantic') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'South Atlantic') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'South Atlantic') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'South Atlantic') %>%
  # Gulf
  overwrite_prodform(region = 'Gulf') %>%
  overwrite_prodform('ECOLOGICAL_CATEGORY', region = 'Gulf') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'Gulf') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'Gulf') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'Gulf') %>%
  # Great Lakes
  overwrite_prodform(region = 'Great Lakes') %>%
  overwrite_prodform('ECOLOGICAL_CATEGORY', region = 'Great Lakes') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'Great Lakes') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'Great Lakes') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'Great Lakes') %>%
  # No Region Assigned
  overwrite_prodform(region = 'No Region Assigned') %>%
  overwrite_prodform('ECOLOGICAL_CATEGORY', region = 'No Region Assigned') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'No Region Assigned') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'No Region Assigned') %>%
  overwrite_prodform(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'No Region Assigned')

test <- overwritten_products %>%
  mutate(POUNDS = ifelse(is.na(POUNDS), 0, POUNDS))

sum(test$POUNDS[which(test$CONFIDENTIAL == 1)]) / sum(test$POUNDS) # 27.3%

# store changed products in separate object
changed_product_forms <- overwritten_products %>%
  filter(CONFIDENTIAL == 1) %>%
  select(!CONFIDENTIAL)

# remove confidential markers
overwritten_products <- overwritten_products %>%
  mutate(CONFIDENTIAL = NA)

# The next step is to identify which products, after attempting to consolidate
  # into less specific product conditions, are confidential. For these, we will
  # attempt to consolidate into less specific species classifications
declassify_species <- function(data, region = '') {
  # data is a dataset of processed products
  # region is an empty character string by default that accepts a string of a 
    # desired region formatted as is in 'data'
  
  # The process is four steps, one for each level of the species hierarchy
  # Each step consists of isolating necessary columns for aggregation, counting
    # how many plants process the product, and overwriting species classifications
    # The distinction in each step lies in the specific level getting isolated
    # and then overwritten. 
  if (region == '') {
    step1 <- data %>%
      select(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
             SPECIES_GROUP, SPECIES_NAME, PLANT_STREET) %>%
      # Because users can't select NA species name, only take products with 
        # a provided species name
      filter(!is.na(SPECIES_NAME)) %>%
      distinct() %>%
      group_by(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
               SPECIES_GROUP, SPECIES_NAME) %>%
      count() %>%
      ungroup() %>%
      filter(n < 3) %>%
      mutate(CONFIDENTIAL = 1) %>%
      # rejoin back to original data with CONFIDENTIAL removed (clean join)
      right_join(data %>% select(!CONFIDENTIAL)) %>%
      # STATE OF ALASKA represnts many processors, so they are not confidential
      # For any confidentially marked products, remove the assigned species name
      mutate(CONFIDENTIAL = ifelse(CITY == 'STATE OF ALASKA', NA, CONFIDENTIAL),
             SPECIES_NAME = ifelse(!is.na(CONFIDENTIAL), NA, SPECIES_NAME)) %>%
      # remove n
      select(!n)
    
    step2 <- step1 %>%
      select(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
             SPECIES_GROUP, SPECIES_NAME, PLANT_STREET) %>%
      # Now we only want products for which there is no species name but a 
        # species group
      filter(is.na(SPECIES_NAME),
             !is.na(SPECIES_GROUP)) %>%
      distinct() %>%
      group_by(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
               SPECIES_GROUP, SPECIES_NAME) %>%
      count() %>%
      ungroup() %>%
      filter(n < 3) %>%
      mutate(CONFIDENTIAL = 1) %>%
      right_join(step1 %>% select(!CONFIDENTIAL)) %>%
      mutate(CONFIDENTIAL = ifelse(CITY == 'STATE OF ALASKA', NA, CONFIDENTIAL),
             SPECIES_GROUP = ifelse(!is.na(CONFIDENTIAL), NA, SPECIES_GROUP)) %>%
      select(!n)
    
    step3 <- step2 %>%
      select(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
             SPECIES_GROUP, SPECIES_NAME, PLANT_STREET) %>%
      # only products for which there is no species name nor group, but a 
        # species category
      filter(is.na(SPECIES_NAME),
             is.na(SPECIES_GROUP),
             !is.na(SPECIES_CATEGORY)) %>%
      distinct() %>%
      group_by(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
               SPECIES_GROUP, SPECIES_NAME) %>%
      count() %>%
      ungroup() %>%
      filter(n < 3) %>%
      mutate(CONFIDENTIAL = 1) %>%
      right_join(step2 %>% select(!CONFIDENTIAL)) %>%
      mutate(CONFIDENTIAL = ifelse(CITY == 'STATE OF ALASKA', NA, CONFIDENTIAL),
             SPECIES_CATEGORY = ifelse(!is.na(CONFIDENTIAL), NA, SPECIES_CATEGORY)) %>%
      select(!n)
    
    step4 <- step3 %>%
      select(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
             SPECIES_GROUP, SPECIES_NAME, PLANT_STREET) %>%
      # only products for which there is no species name, group, nor category,
        # but an ecological category
      filter(is.na(SPECIES_NAME),
             is.na(SPECIES_GROUP),
             is.na(SPECIES_CATEGORY),
             !is.na(ECOLOGICAL_CATEGORY)) %>%
      distinct() %>%
      group_by(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
               SPECIES_GROUP, SPECIES_NAME) %>%
      count() %>%
      ungroup() %>%
      filter(n < 3) %>%
      mutate(CONFIDENTIAL = 1) %>%
      right_join(step3 %>% select(!CONFIDENTIAL)) %>%
      mutate(CONFIDENTIAL = ifelse(CITY == 'STATE OF ALASKA', NA, CONFIDENTIAL),
             ECOLOGICAL_CATEGORY = ifelse(!is.na(CONFIDENTIAL), NA, ECOLOGICAL_CATEGORY)) %>%
      select(!n)
    
  } else {
    # Due to the complexity of the North Pacific processors (STATE OF ALASKA) and
      # the very few products that would be confidential from the NP, the
      # North Pacific is excluded from this process
    if (region == 'North Pacific') {
      return(data)
    }
    
    # Here, the only differences from the steps above are filtering for the
      # desired region in the data
    
    step1 <- data %>%
      filter(REGION == region) %>%
      select(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
             SPECIES_GROUP, SPECIES_NAME, PLANT_STREET) %>%
      filter(!is.na(SPECIES_NAME)) %>%
      distinct() %>%
      group_by(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
               SPECIES_GROUP, SPECIES_NAME) %>%
      count() %>%
      ungroup() %>%
      filter(n < 3) %>%
      mutate(CONFIDENTIAL = 1,
             REGION = region) %>%
      right_join(data %>% select(!CONFIDENTIAL)) %>%
      mutate(SPECIES_NAME = ifelse(!is.na(CONFIDENTIAL), NA, SPECIES_NAME)) %>%
      select(!n)
    
    step2 <- step1 %>%
      filter(REGION == region) %>%
      select(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
             SPECIES_GROUP, SPECIES_NAME, PLANT_STREET) %>%
      filter(is.na(SPECIES_NAME),
             !is.na(SPECIES_GROUP)) %>%
      distinct() %>%
      group_by(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
               SPECIES_GROUP, SPECIES_NAME) %>%
      count() %>%
      ungroup() %>%
      filter(n < 3) %>%
      mutate(CONFIDENTIAL = 1,
             REGION = region) %>%
      right_join(step1 %>% select(!CONFIDENTIAL)) %>%
      mutate(SPECIES_GROUP = ifelse(!is.na(CONFIDENTIAL), NA, SPECIES_GROUP)) %>%
      select(!n)
    
    step3 <- step2 %>%
      filter(REGION == region) %>%
      select(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
             SPECIES_GROUP, SPECIES_NAME, PLANT_STREET) %>%
      filter(is.na(SPECIES_NAME),
             is.na(SPECIES_GROUP),
             !is.na(SPECIES_CATEGORY)) %>%
      distinct() %>%
      group_by(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
               SPECIES_GROUP, SPECIES_NAME) %>%
      count() %>%
      ungroup() %>%
      filter(n < 3) %>%
      mutate(CONFIDENTIAL = 1,
             REGION = region) %>%
      right_join(step2 %>% select(!CONFIDENTIAL)) %>%
      mutate(SPECIES_CATEGORY = ifelse(!is.na(CONFIDENTIAL), NA, SPECIES_CATEGORY)) %>%
      select(!n)
    
    step4 <- step3 %>%
      filter(REGION == region) %>%
      select(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
             SPECIES_GROUP, SPECIES_NAME, PLANT_STREET) %>%
      filter(is.na(SPECIES_NAME),
             is.na(SPECIES_GROUP),
             is.na(SPECIES_CATEGORY),
             !is.na(ECOLOGICAL_CATEGORY)) %>%
      distinct() %>%
      group_by(YEAR, NEW_PRODUCT_FORM, ECOLOGICAL_CATEGORY, SPECIES_CATEGORY,
               SPECIES_GROUP, SPECIES_NAME) %>%
      count() %>%
      ungroup() %>%
      filter(n < 3) %>%
      mutate(CONFIDENTIAL = 1,
             REGION = region) %>%
      right_join(step3 %>% select(!CONFIDENTIAL)) %>%
      mutate(ECOLOGICAL_CATEGORY = ifelse(!is.na(CONFIDENTIAL), NA, ECOLOGICAL_CATEGORY)) %>%
      select(!n)
  }
  
  return(step4)
}

# pipe for declassifying species
declassified_products <- declassify_species(overwritten_products) %>%
  declassify_species('Pacific') %>%
  declassify_species('North Pacific') %>%
  declassify_species('West Pacific') %>%
  declassify_species('New England') %>%
  declassify_species('Mid-Atlantic') %>%
  declassify_species('South Atlantic') %>%
  declassify_species('Gulf') %>%
  declassify_species('No Region Assigned')

# store declassified species in separate object
species_declassified_products <- declassified_products %>%
  filter((is.na(SPECIES_NAME) & !is.na(OLD_SPECIES_NAME)) |
           (is.na(SPECIES_GROUP) & !is.na(OLD_SPECIES_GROUP)) |
           (is.na(SPECIES_CATEGORY) & !is.na(OLD_SPECIES_CATEGORY)) |
           (is.na(ECOLOGICAL_CATEGORY) & !is.na(OLD_ECOLOGICAL_CATEGORY)))


declassified_products <- declassified_products %>%
  mutate(CONFIDENTIAL = NA)
# The last step is to identify which products, after attempting to consolidate
  # into less specific product conditions and species, are confidential
set_confids <- function(data, cols = '', region = '') {
  # this function is nearly identical to overwrite_prodforms
  # data can be raw products data or that formatted by overwrite_prodforms
  # cols are columns to group the data by
  # region is an empty string that accepts specific regions as strings
  
  # the only difference between set_confids and overwrite_prodforms is the 
    # absence of changing product forms to other. Instead, any products 
    # identified to less than 3 plants will have CONFIDENTIAL as 1
  
  if ('' %in% cols) {
    cols <- c('NEW_PRODUCT_FORM')
  }
  
  level_filter <- cols[length(cols)]
  level_filter <- as.symbol(level_filter)
  level_filter <- rlang::enquo(level_filter)
  
  if (region == '') {
    data %>%
      select(YEAR, NEW_PRODUCT_FORM, all_of(cols), PLANT_STREET) %>%
      filter(PLANT_STREET != '',
             !is.na(PLANT_STREET),
             !is.na(!!level_filter)) %>%
      distinct() %>%
      group_by(across(c(-PLANT_STREET))) %>%
      count() %>%
      filter(n < 3) %>%
      ungroup() %>%
      right_join(data) %>%
      mutate(CONFIDENTIAL = ifelse(!is.na(n), 1, CONFIDENTIAL)) %>%
      select(!n)
  } else if (region == 'North Pacific') {
    data %>%
      filter(REGION == 'North Pacific') %>%
      select(YEAR, NEW_PRODUCT_FORM, all_of(cols), PLANT_STREET) %>%
      filter(!is.na(!!level_filter)) %>%
      distinct() %>%
      group_by(across(c(-PLANT_STREET))) %>%
      mutate(n = n()) %>%
      filter(n == 1,
             PLANT_STREET != '') %>%
      ungroup() %>%
      mutate(REGION = 'North Pacific') %>%
      select(!PLANT_STREET) %>%
      right_join(data) %>%
      mutate(CONFIDENTIAL = ifelse(!is.na(n), 1, CONFIDENTIAL)) %>%
      select(!n)
  } else {
    data %>%
      filter(REGION == region) %>%
      select(YEAR, NEW_PRODUCT_FORM, all_of(cols), PLANT_STREET) %>%
      filter(PLANT_STREET != '',
             !is.na(PLANT_STREET),
             !is.na(!!level_filter)) %>%
      distinct() %>%
      group_by(across(c(-PLANT_STREET))) %>%
      count() %>%
      filter(n < 3) %>%
      ungroup() %>%
      mutate(REGION = region) %>%
      right_join(data) %>%
      mutate(CONFIDENTIAL = ifelse(!is.na(n), 1, CONFIDENTIAL)) %>%
      select(!n)
  }
}

# pipe for identifying confidential products
products_marked <- declassified_products %>%
  set_confids() %>%
  # FIRST SECTION: Each level of the classification hierarchy without region
  set_confids('ECOLOGICAL_CATEGORY') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY')) %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP')) %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME')) %>%
  # SECOND SECTION: Each level of the classification hierarchy for EACH region
  # Pacific
  set_confids(region = 'Pacific') %>%
  set_confids('ECOLOGICAL_CATEGORY', region = 'Pacific') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'Pacific') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'Pacific') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'Pacific') %>%
  # North Pacific
  set_confids(region = 'North Pacific') %>%
  set_confids('ECOLOGICAL_CATEGORY', region = 'North Pacific') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'North Pacific') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'North Pacific') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'North Pacific') %>%
  # West Pacific
  set_confids(region = 'West Pacific') %>%
  set_confids('ECOLOGICAL_CATEGORY', region = 'West Pacific') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
              region = 'West Pacific') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'West Pacific') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'West Pacific') %>%
  # New England
  set_confids(region = 'New England') %>%
  set_confids('ECOLOGICAL_CATEGORY', region = 'New England') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'New England') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'New England') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'New England') %>%
  # Mid-Atlantic
  set_confids(region = 'Mid-Atlantic') %>%
  set_confids('ECOLOGICAL_CATEGORY', region = 'Mid-Atlantic') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'Mid-Atlantic') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'Mid-Atlantic') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'Mid-Atlantic') %>%
  # South Atlantic
  set_confids(region = 'South Atlantic') %>%
  set_confids('ECOLOGICAL_CATEGORY', region = 'South Atlantic') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'South Atlantic') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'South Atlantic') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'South Atlantic') %>%
  # Gulf
  set_confids(region = 'Gulf') %>%
  set_confids('ECOLOGICAL_CATEGORY', region = 'Gulf') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'Gulf') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'Gulf') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'Gulf') %>%
  # Great Lakes
  set_confids(region = 'Great Lakes') %>%
  set_confids('ECOLOGICAL_CATEGORY', region = 'Great Lakes') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
                     region = 'Great Lakes') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), region = 'Great Lakes') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'Great Lakes') %>%
  # No Region Assigned
  set_confids(region = 'No Region Assigned') %>%
  set_confids('ECOLOGICAL_CATEGORY', region = 'No Region Assigned') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY'), 
              region = 'No Region Assigned') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY', 
                'SPECIES_GROUP'), region = 'No Region Assigned') %>%
  set_confids(c('ECOLOGICAL_CATEGORY', 'SPECIES_CATEGORY',
                'SPECIES_GROUP', 'SPECIES_NAME'), region = 'No Region Assigned') %>%
  # if there is no provided street address, then a product should not be confidential
  mutate(CONFIDENTIAL = ifelse(CITY == 'STATE OF ALASKA', NA, CONFIDENTIAL))

test <- products_marked %>%
  mutate(POUNDS = ifelse(is.na(POUNDS), 0, POUNDS))

sum(test$POUNDS[which(test$CONFIDENTIAL == 1)]) / sum(test$POUNDS) #2.13%

# store confidential products in separate objects
confidential_products <- products_marked %>%
  filter(CONFIDENTIAL == 1)

# Processed Products -----------------------------------------------------------
# Data formatting
pp_data <- products_marked %>%
  # convert pounds to kilograms in separate column
  mutate(KG = POUNDS * 0.45359237) %>%
  select(YEAR, SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY, 
         NEW_PRODUCT_FORM, POUNDS, DOLLARS, KG, REGION, CONFIDENTIAL) %>%
  rename(PRODUCT_FORM = NEW_PRODUCT_FORM) %>%
  left_join(def_index %>% select(YEAR, INDEX)) %>%
  mutate(DOLLARS = ifelse(is.na(DOLLARS), 0, DOLLARS),
         POUNDS = ifelse(is.na(POUNDS), 0, POUNDS),
         KG = ifelse(is.na(KG), 0, KG),
         DOLLARS_2024 = DOLLARS * INDEX,
         DOLLARS_PER_LB = DOLLARS / POUNDS,
         DOLLARS_PER_KG = DOLLARS / KG,
         DOLLARS_2024_PER_LB = DOLLARS_2024 / POUNDS,
         DOLLARS_2024_PER_KG = DOLLARS_2024 / KG) %>%
  select(-INDEX) %>%
  # mark confidential records' values as 0
  mutate(CONFIDENTIAL = ifelse(is.na(CONFIDENTIAL), 0, CONFIDENTIAL),
         POUNDS = ifelse(CONFIDENTIAL == 1, 0, POUNDS),
         DOLLARS = ifelse(CONFIDENTIAL == 1, 0, DOLLARS),
         KG = ifelse(CONFIDENTIAL == 1, 0, KG),
         DOLLARS_2024 = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_2024),
         # if pounds = 0 also set any rates to 0 (would be Inf or NaN otherwise)
         DOLLARS_PER_LB = ifelse(CONFIDENTIAL == 1 | POUNDS == 0, 0, DOLLARS_PER_LB),
         DOLLARS_PER_KG = ifelse(CONFIDENTIAL == 1 | POUNDS == 0, 0, DOLLARS_PER_KG),
         DOLLARS_2024_PER_LB = ifelse(CONFIDENTIAL == 1 | POUNDS == 0, 0, DOLLARS_2024_PER_LB),
         DOLLARS_2024_PER_KG = ifelse(CONFIDENTIAL == 1 | POUNDS == 0, 0, DOLLARS_2024_PER_KG))

# Commercial Landings ----------------------------------------------------------
# Data formatting
com_landings <- landings_pull %>%
  mutate(YEAR = as.numeric(YEAR),
         POUNDS = as.numeric(gsub(',', '', POUNDS)),
         DOLLARS = as.numeric(gsub(',', '', DOLLARS))) %>%
  # connect groups from map
  left_join(landings_map %>%
              select(NMFS_NAME, TSN, SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
              distinct()) %>%
  left_join(def_index %>% select(YEAR, INDEX)) %>%
  mutate(DOLLARS_2024 = DOLLARS * INDEX,
         KG = POUNDS * 0.45359237,
         METRIC_TONS = KG / 1000,
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
         REGION = ifelse(STATE %in% grlake, 'Great Lakes', REGION),
         REGION = ifelse(is.na(REGION), 'No Region Assigned', REGION))

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
save(list = c('trade_data', 'pp_data', 'com_landings', 'trade_date',
              'landings_date', 'products_date'),
     file = file_name)

# upload to google drive
drive_upload(file_name,
             # IMPORTANT: change path below to match your personal Drive
             # NOTE: no path results in save to Drive location specified
             #       above
             path = 'Seafood Trade Dashboard Project/Seafood Trade Data/',
             overwrite = T)
