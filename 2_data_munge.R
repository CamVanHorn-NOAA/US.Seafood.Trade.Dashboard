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
  # in a group. Because our classification hierarchy has four levels, and because 
  # not all products provide information up to the species name, we must isolate
  # confidentiality at each level of the hierarchy. Also, because the user may
  # or may not investigate region, this must be done both at the regional and 
  # outside the regional level. This will be many steps, but the fundamentals are:
  # 1) Determine what would be confidential in the data presently. For those products,
  # change their product form to be OTHER so that they may aggregate.
  # 2) Determine what would be confidential after adjusting product forms.
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
         # NEW_PRODUCT_FORM will store updated product conditions so that
          # PRODUCT_FORM can retain prior, more specific data
         NEW_PRODUCT_FORM = PRODUCT_FORM,
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
                       'SPECIES_GROUP', 'SPECIES_NAME'), region = 'Great Lakes')

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
