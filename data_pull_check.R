# Validation checks on new data pulls

# What do we need to check for when we pull new data?
  # do column names match up or have they changed?
  # are there new data points that we do not have mapped?
    # new species?
    # new products?
    # new cities?

# libraries
if(!require("tidyverse")) install.packages("tidyverse")

# load the two most recent data pulls (change object names) --------------------
dates <- list.files(path = '~/GitHub/US.Seafood.Trade.Dashboard/', 
           pattern = 'seafood_trade_data_pull_') %>%
  str_remove('seafood_trade_data_pull_') %>%
  str_remove('.RData') %>%
  as.Date(format = '%m_%d_%y')

datas <- list.files(path = '~/GitHub/US.Seafood.Trade.Dashboard/',
                    pattern = 'seafood_trade_data_pull_')[rev(order(dates))][1:2]

load(datas[2])

old_export_pull <- export_pull
old_import_pull <- import_pull
old_landings_pull <- landings_pull
old_products_pull <- pp_processed

cat('Old Data Loading... Done\n')

load(datas[1])

new_export_pull <- export_pull
new_import_pull <- import_pull
new_landings_pull <- landings_pull
new_products_pull <- pp_processed

cat('New Data Loading... Done\n')
rm(def_index, pp_processed, landings_date, products_date, trade_date, 
   export_pull, import_pull, landings_pull)




# do column names match up? ----------------------------------------------------
cat('Checking data structures...\n')
check_names <- function(new_data, old_data, data_type) {
  new_names <- colnames(new_data)
  old_names <- colnames(old_data)
  if (any(!(new_names %in% old_names))) {
    cat("Warning:", data_type, "data structure has changed. See column headers.\n")
  } else {
    cat(data_type, "data structure OK.\n")
  }
}

check_names(new_export_pull, old_export_pull, "Exports")
check_names(new_import_pull, old_import_pull, "Imports")
check_names(new_landings_pull, old_landings_pull, "Landings")
check_names(new_products_pull, old_products_pull, "Processed Products")

cat('Checking data structures... Done.\n\n')
# are there new products? ------------------------------------------------------
cat('Checking for new products...\n')
check_trade <- function(new_data, old_data, data_type) {
  if (!('HTS_NUMBER' %in% colnames(new_data))) {
    return(cat('Error: No HTS field in new', data_type, 'pull. Check column headers.\n'))
  }
  new_hts <- unique(new_data$HTS_NUMBER)
  old_hts <- unique(old_data$HTS_NUMBER)
  
  unmatched_hts <- new_hts[which(!(new_hts %in% old_hts))]
  
  inspect <- new_data[which(new_data$HTS_NUMBER %in% unmatched_hts), ]
  
  if (nrow(inspect != 0)) {
    cat('Warning: Unmatched HTS Codes in', data_type, 'pull. See new_', data_type, '_products.\n')
    return(inspect)
  } else {
    cat('No new products found in', data_type, 'pull.\n')
  }
}

check_products <- function() {
  if (!('PP_CODE' %in% colnames(new_products_pull))) {
    return(cat('Error: No PP_CODE field in new products pull. Check column headers.\n'))
  }
  new_codes <- unique(new_products_pull$PP_CODE)
  old_codes <- unique(old_products_pull$PP_CODE)
  
  unmatched_codes <- new_codes[which(!(new_codes %in% old_codes))]
  
  inspect <- new_products_pull[which(new_products_pull$PP_CODE %in% unmatched_codes), ]
  
  if (nrow(inspect != 0)) {
    cat('Warning: Unmatched PP Codes in processed products pull. See new_processed_products.\n')
  } else {
    cat('No new processed products found.\n')
  }
}

check_landings <- function() {
  if (!('TSN' %in% colnames(new_landings_pull))) {
    return(cat('Error: Check column headers. No TSN found.\n'))
  }
  new_tsn <- unique(new_landings_pull$TSN)
  old_tsn <- unique(old_landings_pull$TSN)
  
  unmatched_tsn <- new_tsn[which(!(new_tsn %in% old_tsn))]
  
  inspect <- new_landings_pull[which(new_landings_pull$TSN %in% unmatched_tsn), ]
  
  if (nrow(inspect != 0)) {
    cat('Warning: Unmatched TSN Codes in Landings pull. See new_landed_species.\n')
    return(inspect)
  } else {
    cat('No new landed species found in Landings pull.\n')
  }
}

new_export_products <- check_trade(new_export_pull, old_export_pull, "Exports")
new_import_products <- check_trade(new_import_pull, old_import_pull, "Imports")
new_landed_species <- check_landings()
new_processed_products <- check_products()

cat('Checking for new products... Done\n\n')
cat('Checking for new ports and plants...\n')
check_customs <- function(new_data, old_data, data_type) {
  if (!('US_CUSTOMS_DISTRICT' %in% colnames(new_data))) {
    return(cat('Error: Check column headers. No US_CUSTOMS_DISTRICT found.\n'))
  }
  
  new_ports <- unique(new_data$US_CUSTOMS_DISTRICT)
  old_ports <- unique(old_data$US_CUSTOMS_DISTRICT)
  
  unmatched_ports <- new_ports[which(!(new_ports %in% old_ports))]
  
  inspect <- new_data[which(new_data$US_CUSTOMS_DISTRICT %in% unmatched_ports), ]
  
  if (nrow(inspect != 0)) {
    cat('Warning: Unmatched Customs Districts in', data_type, 'pull. See new_', data_type, 'ports.\n')
  } else {
    cat('No new Customs Districts found in', data_type, 'pull.\n')
  }
}

new_exports_ports <- check_customs(new_export_pull, old_export_pull, 'Exports')
new_imports_ports <- check_customs(new_import_pull, old_import_pull, 'Imports')

check_plants <- function() {
  if (!('PP_IDNUM' %in% colnames(new_products_pull))) {
    return(cat('Error: Check column headers. No PP_IDNUM found.\n'))
  }
  
  new_plants <- unique(new_products_pull$PP_IDNUM)
  old_plants <- unique(old_products_pull$PP_IDNUM)
  
  unmatched_plants <- new_plants[which(!(new_plants %in% old_plants))]
  
  inspect <- new_products_pull[which(new_products_pull$PP_IDNUM %in% unmatched_plants), ]
  
  if (nrow(inspect != 0)) {
    cat('Warning: Unmatched Processing Plants in new_products_pull. See new_plants.\n')
  } else {
    cat('No new processing plants found.\n')
  }
}

new_plants <- check_plants()
cat('Checking for new ports and plants... Done\n\n')

# Are there products without mapped information? -------------------------------
cat('Checking for mapping gaps...\n')
# trade
new_trade <- bind_rows(new_export_pull %>%
                         left_join(trade_map %>%
                                     select(!PRODUCT_NAME) %>%
                                     distinct()),
                       new_import_pull %>%
                         left_join(trade_map %>%
                                     select(!PRODUCT_NAME) %>%
                                     distinct()))

old_trade <- bind_rows(old_export_pull %>%
                         left_join(trade_map %>%
                                     select(!PRODUCT_NAME) %>%
                                     distinct()),
                       old_import_pull %>%
                         left_join(trade_map %>%
                                     select(!PRODUCT_NAME) %>%
                                     distinct()))

unmapped_hts <- new_trade %>%
  filter(is.na(ECOLOGICAL_CATEGORY)) %>%
  select(HTS_NUMBER, PRODUCT_NAME) %>%
  distinct()

update_trade_map <- bind_rows(trade_map %>%
                                filter(!is.na(ECOLOGICAL_CATEGORY)), 
                              unmapped_hts)

if (nrow(setdiff(trade_map, update_trade_map)) > 0) {
  write.csv(update_trade_map, 'ussd_trade_map.csv')
  
  cat('Warning: New products added to ussd_trade_map.csv that require mapping information. See unmapped_hts and update_trade_map for data.\n')
} else {
  rm(unmapped_hts, update_trade_map)
}

# landings
unmapped_landings <- new_landings_pull %>%
  left_join(landings_map %>%
              distinct()) %>%
  filter(is.na(ECOLOGICAL_CATEGORY)) %>%
  select(TSN) %>%
  distinct()

update_landings_map <- bind_rows(landings_map %>%
                                   filter(!is.na(ECOLOGICAL_CATEGORY)), 
                                 unmapped_landings)

if (nrow(setdiff(landings_map, update_landings_map)) > 0) {
  write.csv(update_landings_map, 'ussd_landings_map.csv')
  
  cat('Warning: New species added to ussd_landings_map.csv that require mapping information. See unmapped_landings and update_landings_map for data.\n')
} else {
  rm(unmapped_landings, update_landings_map)
}

# processed products
pp_names <- read.csv('pp_names_043026.csv')

unmapped_products <- new_products_pull %>%
  left_join(pp_map) %>%
  filter(is.na(ECOLOGICAL_CATEGORY)) %>%
  select(PP_CODE) %>%
  distinct() %>%
  left_join(pp_names %>%
              select(PP_CODE, PP_DSCP))

update_products_map <- bind_rows(pp_map %>%
                                   filter(!is.na(ECOLOGICAL_CATEGORY)), 
                                 unmapped_products)

if (nrow(setdiff(pp_map, update_products_map)) > 0) {
  write.csv(update_products_map, 'ussd_pp_map.csv')
  
  cat('Warning: New products added to ussd_pp_map.csv that require mapping information. See unmapped_products and update_products_map for data.\n')
  rm(pp_names)
} else {
  rm(pp_names, unmapped_products, update_products_map)
}

cat('Checking for mapping gaps... Done\n\n')

# Are there products without assigned regions? ---------------------------------
cat('Checking for unmapped regions...\n')
# regions
norpac <- c('AK', 'ALASKA')
pac <- c('CA', 'CALIFORNIA', 'OR', 'OREGON', 'WA', 'WASHINGTON')
pacisl <- c('HI', 'HAWAII', 'AS', 'CM', 'MP', 'GU')
neweng <- c('CT', 'CONNECTICUT', 'ME', 'MAINE', 'MA', 'MASSACHUSETTS', 'NH', 
            'NEW HAMPSHIRE', 'RI', 'RHODE ISLAND')
midatl <- c('DE', 'DELAWARE', 'MD', 'MARYLAND', 'NJ', 'NEW JERSEY', 'NY',
            'NEW YORK', 'VA', 'VIRGINIA', 'PA', 'PENNSYLVANIA', 'DC',
            'DISTRICT OF COLUMBIA')
souatl <- c('GA', 'GEORGIA', 'NC', 'NORTH CAROLINA', 'SC', 'SOUTH CAROLINA',
            'FL-E', 'FLORIDA-EAST', 'FLORIDA', 'PR', 'PUERTO RICO', 'VI', 
            'U.S. VIRGIN ISLANDS')
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

# trade
new_trade_regions <- new_trade %>%
  left_join(florida_coast_map %>%
              rename(US_CUSTOMS_DISTRICT = PLANT_CITY,
                     FLORIDA_STATE = PLANT_STATE_ABRV) %>%
              select(!c(PLANT_COAST_GEMINI, PLANT_COAST))) %>%
  mutate(STATE = ifelse(!is.na(FLORIDA_STATE), FLORIDA_STATE, STATE),
         # fix US_CUSTOMS_DISTRICT US VIRGIN ISLANDS to STATE for region
         STATE = ifelse(US_CUSTOMS_DISTRICT == 'U.S. VIRGIN ISLANDS',
                        US_CUSTOMS_DISTRICT, STATE)) %>%
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
         REGION = ifelse(is.na(REGION), 'No Region Assigned', REGION)) %>%
  filter(REGION == 'No Region Assigned') %>%
  select(US_CUSTOMS_DISTRICT, STATE) %>%
  distinct()

old_trade_regions <- old_trade %>%
  left_join(florida_coast_map %>%
              rename(US_CUSTOMS_DISTRICT = PLANT_CITY,
                     FLORIDA_STATE = PLANT_STATE_ABRV) %>%
              select(!c(PLANT_COAST_GEMINI, PLANT_COAST))) %>%
  mutate(STATE = ifelse(!is.na(FLORIDA_STATE), FLORIDA_STATE, STATE),
         # fix US_CUSTOMS_DISTRICT US VIRGIN ISLANDS to STATE for region
         STATE = ifelse(US_CUSTOMS_DISTRICT == 'U.S. VIRGIN ISLANDS',
                        US_CUSTOMS_DISTRICT, STATE)) %>%
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
         REGION = ifelse(is.na(REGION), 'No Region Assigned', REGION)) %>%
  filter(REGION == 'No Region Assigned') %>%
  select(US_CUSTOMS_DISTRICT, STATE) %>%
  distinct()


if (nrow(setdiff(new_trade_regions, old_trade_regions)) > 0) {
  cat('Warning: Potential Customs Districts without mapping in trade pull. See new_trade_regions for data.\n')
  rm(new_trade, old_trade, old_trade_regions)
} else {rm(new_trade, old_trade, new_trade_regions, old_trade_regions)}

# landings
new_landings_regions <- new_landings_pull %>%
  # add regions
  mutate(REGION = ifelse(STATE %in% norpac, 'North Pacific', NA),
         REGION = ifelse(STATE %in% pac, 'Pacific', REGION),
         REGION = ifelse(STATE %in% pacisl, 'West Pacific', REGION),
         REGION = ifelse(STATE %in% neweng, 'New England', REGION),
         REGION = ifelse(STATE %in% midatl, 'Mid-Atlantic', REGION),
         REGION = ifelse(STATE %in% souatl, 'South Atlantic', REGION),
         REGION = ifelse(STATE %in% gulf, 'Gulf', REGION),
         REGION = ifelse(STATE %in% grlake, 'Great Lakes', REGION),
         REGION = ifelse(is.na(REGION), 'No Region Assigned', REGION)) %>%
  filter(REGION == 'No Region Assigned') %>%
  select(STATE) %>%
  distinct() %>%
  pull()

old_landings_regions <- old_landings_pull %>%
  # add regions
  mutate(REGION = ifelse(STATE %in% norpac, 'North Pacific', NA),
         REGION = ifelse(STATE %in% pac, 'Pacific', REGION),
         REGION = ifelse(STATE %in% pacisl, 'West Pacific', REGION),
         REGION = ifelse(STATE %in% neweng, 'New England', REGION),
         REGION = ifelse(STATE %in% midatl, 'Mid-Atlantic', REGION),
         REGION = ifelse(STATE %in% souatl, 'South Atlantic', REGION),
         REGION = ifelse(STATE %in% gulf, 'Gulf', REGION),
         REGION = ifelse(STATE %in% grlake, 'Great Lakes', REGION),
         REGION = ifelse(is.na(REGION), 'No Region Assigned', REGION)) %>%
  filter(REGION == 'No Region Assigned') %>%
  select(STATE) %>%
  distinct() %>%
  pull()

if (any(!(new_landings_regions %in% old_landings_regions))) {
  cat('Warning: Potential States without mapping in landings pull. See new_landings_regions for data.\n')
  rm(new_landings_pull, old_landings_pull, old_landings_regions)
} else {rm(new_landings_pull, old_landings_pull, 
           new_landings_regions, old_landings_regions)}

# products
new_products_regions <- new_products_pull %>%
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
         REGION = ifelse(is.na(REGION), 'No Region Assigned', REGION)) %>%
  filter(REGION == 'No Region Assigned') %>%
  select(CITY, STATE) %>%
  distinct()

old_products_regions <- old_products_pull %>%
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
         REGION = ifelse(is.na(REGION), 'No Region Assigned', REGION)) %>%
  filter(REGION == 'No Region Assigned') %>%
  select(CITY, STATE) %>%
  distinct()

if (nrow(setdiff(new_products_regions, old_products_regions)) > 0) {
  cat('Warning: Potential plants without mapping in products pull. See new_products_regions for data.\n')
  rm(new_products_pull, old_products_pull, old_products_regions)
} else {rm(new_products_pull, old_products_pull, new_products_regions, old_products_regions)}

# remove null objects
rm(list = names(which(sapply(globalenv(),is.null))))

cat('Checking for unmapped regions... Done\n\n')
cat('Data Validation Complete. Check environment for data to inspect.\nSee Warnings for information.')

rm(conversion_factors, florida_coast_map, great_lakes_cities,
   grlake_cities, landings_map, new_export_pull, new_import_pull,
   old_export_pull, old_import_pull, pp_map, species_ref, trade_map,
   datas, dates, grlake, gulf, midatl, neweng, norpac, pac, pacisl, souatl,
   check_customs, check_landings, check_names, check_plants, check_products,
   check_trade)