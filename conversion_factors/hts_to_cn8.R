# Attempt to Consolidate Conversion Factor data
# Author: Cameron Van Horn
#         cameron.vanhorn@noaa.gov

# The code below heavily relies on code curated by Kaitlyn Malakoff and Kailin
  # Kroetz (https://github.com/kaitlyn-c-lee/seafood-traceability-design/tree/main)
  # in the file 'Linkage_HTS_CN8.R'
# The purpose here is to match CN-8 codes to HTS codes in NOAA trade data; a 
  # bulk of this effort was performed by Malakoff and Kroetz as mentioned. However,
  # their species attribution is unique to ours, thus some manual effort was 
  # needed to ensure accurate attribution of CN-8 codes to HTS codes. 

# BEFORE RUNNING: 
  # Check if species classifications changed 
  # Check if data from foss changed
# Packages and Data ------------------------------------------------------------
library(tidyverse)
library(readxl)

# Get Export and Import data from FOSS
# Exports
# read csv's
foss_exports_1524 <- read.csv('foss_exports_15-24.csv') %>%
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

foss_exports_0414 <- read.csv('foss_exports_04-14.csv') %>%
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
foss_exports <- bind_rows(foss_exports_0414, foss_exports_1524)

# Imports
# read csv's
foss_imports_1524 <- read.csv('foss_imports_15-24.csv') %>%
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

foss_imports_0414 <- read.csv('foss_imports_04-14.csv') %>%
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
foss_imports <- bind_rows(foss_imports_0414, foss_imports_1524)

rm(foss_exports_0414, foss_exports_1524, foss_imports_0414, foss_imports_1524)


# Get EUMOFA data
eumofa <- read_excel("DM_Annex 7 - CF by CN-8 from 2001 to 2021.xlsx",
                     sheet = "List of CF")
# use most recent cn-8 codes
eumofa <- eumofa %>% 
  filter(Year == '2021') %>%
  # select relevant columns
  select(`CN-8`, `CN-8 product name`, CF, Explanation) %>%
  # remove gaps in Cn-8 codes
  mutate(`CN-8` = str_remove_all(`CN-8`, " "))


# get species mapping sheet
trade_map <- read.csv('trade_data_mapping_sheet.csv') %>%
  mutate(HTS_NUMBER = as.character(HTS_NUMBER))

# Create a Map -----------------------------------------------------------------
# combine imports and exports
trade_data <- rbind(foss_exports, foss_imports) %>%
  # attach species groups
  left_join(trade_map %>%
              # keep only species categories
              select(PRODUCT_NAME, HTS_NUMBER, SPECIES_NAME, SPECIES_GROUP, 
                     SPECIES_CATEGORY, ECOLOGICAL_CATEGORY))

# get map of products/hts codes by classifications
us_hts_cn8 <- trade_data %>%
  select(HTS_NUMBER, PRODUCT_NAME, SPECIES_NAME, SPECIES_GROUP,
         SPECIES_CATEGORY, ECOLOGICAL_CATEGORY) %>%
  distinct() %>%
  mutate(SPECIES_NAME = ifelse(is.na(SPECIES_NAME), '', SPECIES_NAME),
         SPECIES_GROUP = ifelse(is.na(SPECIES_GROUP), '', SPECIES_GROUP),
         SPECIES_CATEGORY = ifelse(is.na(SPECIES_CATEGORY), '', SPECIES_CATEGORY),
         ECOLOGICAL_CATEGORY = ifelse(is.na(ECOLOGICAL_CATEGORY), '', ECOLOGICAL_CATEGORY))

# Attach CN-8 codes to HTS -----------------------------------------------------
# create empty column for cn8 codes
us_hts_cn8$`CN-8` <- ''

# abalone
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'ABALONE', '03078100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'ABALONE' &
                           (str_detect(PRODUCT_NAME, 'FROZEN') | str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE')),
                         '03078300', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'ABALONE' &
                           (str_detect(PRODUCT_NAME, 'PREPAR') | str_detect(PRODUCT_NAME, 'CANNED')),
                         '16055700', `CN-8`))

# anchovy
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'ANCHOVY', '03024200', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'ANCHOVY' &
                           (str_detect(PRODUCT_NAME, 'SALTED')),
                         '03056300', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'ANCHOVY' &
                           (str_detect(PRODUCT_NAME, 'CANNED') | str_detect(PRODUCT_NAME, 'PREPAR')),
                         '16041600', `CN-8`))

# atka mackerel, use mackerel codes
us_hts_cn8 <- us_hts_cn8 %>% 
  mutate(`CN-8` = if_else(SPECIES_NAME == 'ATKA MACKEREL', '03024400', `CN-8`),
         `CN-8` = if_else(SPECIES_NAME == 'ATKA MACKEREL' &
                           str_detect(PRODUCT_NAME, 'FROZEN'),
                         '03035410', `CN-8`))

# bass
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'SEA BASS', '03028410', `CN-8`))

# bonito
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'BONITO', '16041490', `CN-8`))

# butterfish (use mackerel)
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'BUTTERFISH', '03035410', `CN-8`))

# capelin
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_NAME == 'CAPELIN', '03035990', `CN-8`))

# carp
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'CARP', '03027300', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CARP' &
                           str_detect(PRODUCT_NAME, 'FROZEN'),
                         '03032500', `CN-8`))

# species unident., include carp (check 8 digit codes in eumofa)
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(HTS_NUMBER %in% c('304390000'), '03043900', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% c('304510090', '304510190', '304510000', '304510100'), 
                         '03045100', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% c('304690000'), '03046900', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% c('304931005', '304931010', '304931090'),
                         '03049310', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% c('304939000'), '03049390', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% c('305310100', '305310000'), 
                         '03053100', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% c('305440000', '305440100'), 
                         '03054490', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% c('305641000', '305645000', '305640000'),
                         '03056400', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% c('305520000'), '03053100', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% c('304930000'), '03049390', `CN-8`))

# catfish
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'CATFISH', '03027200', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CATFISH' &
                           str_detect(PRODUCT_NAME, 'FROZEN'),
                         '03032400', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CATFISH' &
                           str_detect(PRODUCT_NAME, 'FILLET'), 
                         '03043900', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CATFISH' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'FILLET'),
                         '03046900', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CATFISH' &
                           str_detect(PRODUCT_NAME, 'MEAT'), 
                         '03045100', `CN-8`))

# clam
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'CLAM', '16055600', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CLAM' &
                           (str_detect(PRODUCT_NAME, 'FROZEN') |
                              str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE')),
                         '03077900', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CLAM' &
                           (str_detect(PRODUCT_NAME, 'LIVE') |
                              str_detect(PRODUCT_NAME, 'FRESH')), 
                         '03077100', `CN-8`))

# some unident. species matched using codes
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(HTS_NUMBER %in% c('1605560500'), '16055600', `CN-8`))

# cobia
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'COBIA', '03024600', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'COBIA' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), 
                         '03035600', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% c('305540000'), '03055490', `CN-8`))

# conch
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'CONCH', '03078200', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CONCH' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), 
                         '03078400', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CONCH' &
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), 
                         '03078800', `CN-8`))

# crab
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_CATEGORY == 'CRABS', '03063390', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'CRABS' &
                           str_detect(PRODUCT_NAME, 'FROZEN'),
                         '03061490', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'CRABS' & 
                           (str_detect(PRODUCT_NAME, 'PREPAR') |
                              str_detect(PRODUCT_NAME, 'ATC') |
                              str_detect(PRODUCT_NAME, 'CANNED')),
                         '16051000', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'CRABS' &
                           str_detect(PRODUCT_NAME, 'MEAT'),
                         '16051000', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'CRABS' &
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), 
                         '03069390', `CN-8`))

# snow and king crab
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse((SPECIES_NAME == 'SNOW CRAB' |
                           SPECIES_NAME == 'KING CRAB') &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           !str_detect(PRODUCT_NAME, 'MEAT'), '03061410', `CN-8`))

# crayfish
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'CRAYFISH', '03061910', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CRAYFISH' &
                           str_detect(PRODUCT_NAME, 'PEELED'), '16054000', `CN-8`))

# cuttlefish
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'CUTTLEFISH', '03074290', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CUTTLEFISH' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03074399', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CUTTLEFISH' &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16055400', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CUTTLEFISH' & 
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), '03074980', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% c('1605540500'), '16055400', `CN-8`))

# dolphinfish (in EUMOFA its longfinned tunas)
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'DOLPHINFISH', '03023190', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'DOLPHINFISH' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048990', `CN-8`))

# eels
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'EEL', '03027400', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'EEL' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03032600', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'EEL' &
                           (str_detect(PRODUCT_NAME, 'OIL') |
                              str_detect(PRODUCT_NAME, 'PREPAR')), '16041700', `CN-8`))

# flounder
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'FLOUNDER', '03022980', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'FLOUNDER' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03033910', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'FLOUNDER' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044300', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'FLOUNDER' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03048330', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'FLOUNDER' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045990', `CN-8`))

# halibut
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'HALIBUT', '03022110', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HALIBUT' &
                           str_detect(PRODUCT_NAME, 'ATLANTIC'), '03022130', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HALIBUT' &
                           str_detect(PRODUCT_NAME, 'PACIFIC'), '03022190', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HALIBUT' &
                           str_detect(PRODUCT_NAME, 'ATLANTIC') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03033130', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HALIBUT' &
                           str_detect(PRODUCT_NAME, 'PACIFIC') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03033190', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HALIBUT' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03033110', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HALIBUT' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03048390', `CN-8`))

# plaice
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'PLAICE', '03022200', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'PLAICE' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03033200', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'PLAICE' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03048310', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'PLAICE' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045990', `CN-8`))

# Sole
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'SOLE', '03022300', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SOLE' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03033300', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SOLE' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044300', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SOLE' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03048390', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SOLE' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045990', `CN-8`))

# turbot
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'TURBOT', '03022400', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TURBOT' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03033400', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TURBOT' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TURBOT' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044300', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TURBOT' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03048390', `CN-8`))

# other flatfishes
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_CATEGORY == 'FLATFISHES' &
                           SPECIES_GROUP == '', '03022980', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'FLATFISHES' &
                           SPECIES_GROUP == '' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03033985', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'FLATFISHES' &
                           SPECIES_GROUP == '' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044300', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'FLATFISHES' &
                           SPECIES_GROUP == '' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03048390', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'FLATFISHES' &
                           SPECIES_GROUP == '' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045990', `CN-8`))

# cod
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'COD', '03025190', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'COD' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03036390', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'COD' &
                           str_detect(PRODUCT_NAME, 'ATLANTIC'), '03025110', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'COD' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'ATLANTIC'), '03036310', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'COD' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044410', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'COD' &
                           str_detect(PRODUCT_NAME, 'DRIED'), '03055110', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'COD' &
                           str_detect(PRODUCT_NAME, 'SALTED'), '03055190', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'COD' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03047190', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'COD' &
                           str_detect(PRODUCT_NAME, 'SALTED') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03053219', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'COD' &
                           str_detect(PRODUCT_NAME, 'DRIED') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03053219', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'COD' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045300', `CN-8`),
         `CN-8` = ifelse(ECOLOGICAL_CATEGORY == 'GROUNDFISHES' &
                           str_detect(PRODUCT_NAME, 'SMOKED'), '03054980', `CN-8`))

# cusk
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'CUSK', '03025940', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CUSK' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03036980', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CUSK' &
                           str_detect(PRODUCT_NAME, 'SALTED'), '03056910', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CUSK' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CUSK' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'CUSK' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03047980', `CN-8`))

# haddock
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'HADDOCK', '03025200', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HADDOCK' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03036400', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HADDOCK' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044490', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HADDOCK' &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03047200', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HADDOCK' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03049530', `CN-8`))

# hake (there doesn't seem to be any frozen hake that isn't fillets)
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'HAKE', '03025419', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HAKE' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03036619', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HAKE' &
                           str_detect(PRODUCT_NAME, 'SALTED'), '03056910', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'POLLOCK AND HAKE' &
                           SPECIES_GROUP == '' &
                           str_detect(PRODUCT_NAME, 'SALTED'), '03056910', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HAKE' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044490', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HAKE' &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03047419', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HAKE' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03049550', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HAKE' &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'UROPHYCIS'), '03047490', `CN-8`))
         
# ocean perch
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(str_detect(PRODUCT_NAME, 'OCEAN PERCH'), '03028939', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'OCEAN PERCH') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038939', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'OCEAN PERCH') &
                           str_detect(PRODUCT_NAME, 'ATLANTIC'), '03028931', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'OCEAN PERCH') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044950', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'OCEAN PERCH') &
                           str_detect(PRODUCT_NAME, 'ATLANTIC') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038931', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'OCEAN PERCH') &
                           str_detect(PRODUCT_NAME, 'ATLANTIC') &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03048921', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'OCEAN PERCH') &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03048929', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'OCEAN PERCH') &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03049929', `CN-8`))

# pollock (no canned pollock, but some canned pollock + other fish)
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'POLLOCK', '03025930', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'POLLOCK' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03036950', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'POLLOCK' &
                           str_detect(PRODUCT_NAME, 'ALASKA'), '03025500', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'POLLOCK' &
                           str_detect(PRODUCT_NAME, 'ALASKA') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03036700', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'POLLOCK' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044490', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'POLLOCK' &
                           str_detect(PRODUCT_NAME, 'ALASKA') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03047500', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'POLLOCK' &
                           str_detect(PRODUCT_NAME, 'ALASKA') &
                           (str_detect(PRODUCT_NAME, 'SURIMI') |
                              str_detect(PRODUCT_NAME, 'MINCED')), '03049410', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'POLLOCK' &
                           str_detect(PRODUCT_NAME, 'CANNED'), '16041995', `CN-8`), # none
         `CN-8` = ifelse(SPECIES_GROUP == 'POLLOCK' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03049490', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'POLLOCK' &
                           str_detect(PRODUCT_NAME, 'SALTED') &
                           !str_detect(PRODUCT_NAME, 'FILLET'), '03056910', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'POLLOCK' &
                           str_detect(PRODUCT_NAME, 'FRESH') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044490', `CN-8`))

# whiting and blue whiting
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_NAME == 'BLUE WHITING', '03025600', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'BLUE WHITING' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03036810', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'WHITING' &
                           SPECIES_NAME == '', '03047930', `CN-8`))

# other groundfish
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(ECOLOGICAL_CATEGORY == 'GROUNDFISHES' & 
                           SPECIES_CATEGORY == '', '03025990', `CN-8`),
         `CN-8` = ifelse(ECOLOGICAL_CATEGORY == 'GROUNDFISHES' &
                           SPECIES_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03036990', `CN-8`),
         `CN-8` = ifelse(ECOLOGICAL_CATEGORY == 'GROUNDFISHES' &
                           SPECIES_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044490', `CN-8`),
         `CN-8` = ifelse(ECOLOGICAL_CATEGORY == 'GROUNDFISHES' &
                           SPECIES_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045300', `CN-8`),
         `CN-8` = ifelse(ECOLOGICAL_CATEGORY == 'GROUNDFISHES' &
                           SPECIES_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'SURIMI'), '03049510', `CN-8`),
         `CN-8` = ifelse(ECOLOGICAL_CATEGORY == 'GROUNDFISHES' &
                           SPECIES_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'DRIED'), '03055390', `CN-8`),
         `CN-8` = ifelse(ECOLOGICAL_CATEGORY == 'GROUNDFISHES' &
                           SPECIES_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'DRIED') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03053290', `CN-8`),
         `CN-8` = ifelse(ECOLOGICAL_CATEGORY == 'GROUNDFISHES' &
                           SPECIES_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03047990', `CN-8`))

# grouper
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'GROUPER', '03025190', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'GROUPER' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03036390', `CN-8`))

# herring
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'HERRING', '03024100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HERRING' &
                           str_detect(PRODUCT_NAME, 'SALTED'), '03056100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HERRING' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03035100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HERRING' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03053990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HERRING' &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048600', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HERRING' &
                           str_detect(PRODUCT_NAME, 'SMOKED'), '03054200', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HERRING' &
                           (str_detect(PRODUCT_NAME, 'PICKLED') |
                              str_detect(PRODUCT_NAME, 'KIPPERED') |
                              str_detect(PRODUCT_NAME, 'PREPAR')), '16041299', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HERRING' &
                           (str_detect(PRODUCT_NAME, 'CANNED') |
                              str_detect(PRODUCT_NAME, 'ATC')), '16041291', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'HERRING' &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'PICKLED'), '16041210', `CN-8`))

# horse mackerel jacks
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(str_detect(PRODUCT_NAME, 'JACK,HORSE MACKEREL'), '03024590', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'JACK,HORSE MACKEREL') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03035590', `CN-8`))

# jellyfish
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'JELLYFISH', '03083050', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'JELLYFISH' &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16056300', `CN-8`))

# krill
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'KRILL', '03063990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'KRILL' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03061990', `CN-8`))

# lingcod
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_NAME == 'LINGCOD', '03025190', `CN-8`))

# lobster
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_CATEGORY == 'LOBSTERS', '03063210', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'LOBSTERS' &
                           str_detect(PRODUCT_NAME, 'FRESH'), '03063291', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'LOBSTERS' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03061290', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'LOBSTERS' &
                           (str_detect(PRODUCT_NAME, 'PREPAR') |
                              str_detect(PRODUCT_NAME, 'TAILS') |
                              str_detect(PRODUCT_NAME, 'CANNED')), '16053090', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'LOBSTERS' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '16053010', `CN-8`))

# Homarus lobster
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_CATEGORY == 'LOBSTERS' &
                           str_detect(PRODUCT_NAME, 'HOMARUS'), '03063291', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'LOBSTERS' &
                           str_detect(PRODUCT_NAME, 'HOMARUS') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03061290', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'LOBSTERS' &
                           str_detect(PRODUCT_NAME, 'HOMARUS') &
                           str_detect(PRODUCT_NAME, 'LIVE'), '03063210', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'LOBSTERS' &
                           str_detect(PRODUCT_NAME, 'HOMARUS') &
                           (str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE') |
                              str_detect(PRODUCT_NAME, 'ATC')), '03069290', `CN-8`))

# Norway lobster
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_CATEGORY == 'LOBSTERS' &
                           str_detect(PRODUCT_NAME, 'NORWAY'), '03063400', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'LOBSTERS' &
                           str_detect(PRODUCT_NAME, 'NORWAY') &
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), '03069400', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'LOBSTERS' &
                           str_detect(PRODUCT_NAME, 'NORWAY') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03061500', `CN-8`))

# rock lobster
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_CATEGORY == 'LOBSTERS' &
                           str_detect(PRODUCT_NAME, 'ROCK') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03061190', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'LOBSTERS' &
                           str_detect(PRODUCT_NAME, 'ROCK') &
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), '03069100', `CN-8`))

# mackerel
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'MACKEREL', '03024400', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'MACKEREL' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03035410', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'MACKEREL' &
                           str_detect(PRODUCT_NAME, 'SMOKED'), '03054930', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'MACKEREL' &
                           str_detect(PRODUCT_NAME, 'SALTED'), '03056980', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'MACKEREL' &
                           str_detect(PRODUCT_NAME, 'FILLET DRIED/SALTED/BRINE'), '16041511', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'MACKEREL' &
                           (str_detect(PRODUCT_NAME, 'PREPAR') |
                              str_detect(PRODUCT_NAME, 'CANNED')), '16041590', `CN-8`))

# monkfish
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'MONKFISH', '03028950', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'MONKFISH' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038965', `CN-8`))

# mullet
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'MULLET', '03038990', `CN-8`))

# mussels
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'MUSSEL', '03073190', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'MUSSEL' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03073290', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'MUSSEL' &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16055390',`CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'MUSSEL' &
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), '03073980', `CN-8`))

# nile perch
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_NAME == 'NILE PERCH', '03027900', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'NILE PERCH' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03032900', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'NILE PERCH' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03043300', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'NILE PERCH' &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03046300', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'NILE PERCH' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045100', `CN-8`))

# octopus
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'OCTOPUS', '03075100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'OCTOPUS' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03075200', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'OCTOPUS' &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16055500', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'OCTOPUS' &
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), '03075900', `CN-8`))

# orange roughy
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_NAME == 'ORANGE ROUGHY', '03048990', `CN-8`))

# oysters
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'OYSTER', '03071190', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'OYSTER' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03071200', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'OYSTER' &
                           (str_detect(PRODUCT_NAME, 'PREPAR') |
                              str_detect(PRODUCT_NAME, 'CANNED')), '16055100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'OYSTER' &
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), '03071900', `CN-8`))

# perch nspf
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'PERCH' &
                           str_detect(PRODUCT_NAME, 'NSPF'), '03027900', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'PERCH' &
                           str_detect(PRODUCT_NAME, 'NSPF') &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'PERCH' &
                           str_detect(PRODUCT_NAME, 'NSPF') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03043900', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'PERCH' &
                           str_detect(PRODUCT_NAME, 'NSPF') &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048990', `CN-8`))

# pickerel
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'PICKEREL', '03027900', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'PICKEREL' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'PICKEREL' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03043900', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'PICKEREL' &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048990', `CN-8`))

# pike
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'PIKE', '03027900', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'PIKE' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'PIKE' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'PIKE' &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048990', `CN-8`))

# Rays (which includes skates)
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(str_detect(PRODUCT_NAME, 'RAYS'), '03028200', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'RAYS') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03038200', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'RAYS') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044800', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'RAYS') &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03049700', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'RAYS') &
                           str_detect(PRODUCT_NAME, 'MEAT') &
                           str_detect(PRODUCT_NAME, 'FRESH'), '03045700', `CN-8`))

# sablefish
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_NAME == 'SABLEFISH', '03028990', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'SABLEFISH' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038990', `CN-8`))

# salmon (individual species later)
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_CATEGORY == 'SALMON', '03021900', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SALMON' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03031200', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SALMON' &
                           (str_detect(PRODUCT_NAME, 'FILLET') |
                              str_detect(PRODUCT_NAME, 'STEAKS')), '03044100', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SALMON' &
                           (str_detect(PRODUCT_NAME, 'FILLET') |
                              str_detect(PRODUCT_NAME, 'STEAKS')) &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048100', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SALMON' &
                           str_detect(PRODUCT_NAME, 'SALTED'), '03056950', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SALMON' &
                           str_detect(PRODUCT_NAME, 'SMOKED'), '03054100', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SALMON' &
                           str_detect(PRODUCT_NAME, 'CANNED'), '16041100', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SALMON' &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16041100', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SALMON' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045200', `CN-8`))

# atlantic/danube salmon
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'ATLANTIC SALMON', '03021400', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'ATLANTIC SALMON' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03031300', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'ATLANTIC SALMON' &
                           (str_detect(PRODUCT_NAME, 'FILLET') |
                              str_detect(PRODUCT_NAME, 'STEAK')), '03044100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'ATLANTIC SALMON' &
                           (str_detect(PRODUCT_NAME, 'FILLET') |
                              str_detect(PRODUCT_NAME, 'STEAK')) &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'ATLANTIC SALMON' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045200', `CN-8`))

# sockeye salmon
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_NAME == 'SOCKEYE SALMON' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03031100', `CN-8`))

# sardine
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'SARDINE', '03024330', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SARDINE' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03035330', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SARDINE' &
                           str_detect(PRODUCT_NAME, 'CANNED'), '16041311', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SARDINE' &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16041311', `CN-8`))

# sauger
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_NAME == 'SAUGER', '03038910', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'SAUGER' &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048910', `CN-8`))

# scallops
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'SCALLOP', '03072100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SCALLOP' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03072290', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SCALLOP' &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16055200', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SCALLOP' &
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), '03072900', `CN-8`))

# scorpionfish (use sea bass)
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'SCORPIONFISH', '03038490', `CN-8`))

# sea bass
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'SEA BASS', '03028490', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SEA BASS' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038490', `CN-8`))

# sea cucumber
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'SEA CUCUMBER', '03081100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SEA CUCUMBER' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03081200', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SEA CUCUMBER' &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16056100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SEA CUCUMBER' &
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), '03081900', `CN-8`))

# sea urchin
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'SEA URCHIN', '03082100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SEA URCHIN' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03082200', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SEA URCHIN' &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16056200', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SEA URCHIN' &
                           str_detect(PRODUCT_NAME, 'DIRED/SALTED/BRINE'), '03082900', `CN-8`))

# seabream
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(str_detect(PRODUCT_NAME, 'SEABREAM'), '03028590', `CN-8`))

# shad
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(str_detect(PRODUCT_NAME, 'SHAD,STURGEON FRESH'), '03028990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'SHAD,STURGEON FROZEN'), '03038990', `CN-8`))

# shark
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_CATEGORY == 'SHARKS', '03028180', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHARKS' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038190', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHARKS' &
                           str_detect(PRODUCT_NAME, 'DOGFISH'), '03028115', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHARKS' &
                           str_detect(PRODUCT_NAME, 'DOGFISH') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038115', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHARKS' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03049690', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHARKS' &
                           str_detect(PRODUCT_NAME, 'FIN'), '03029200', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHARKS' &
                           str_detect(PRODUCT_NAME, 'FIN') &
                           str_detect(PRODUCT_NAME, 'DRIED'), '03057100', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHARKS' &
                           str_detect(PRODUCT_NAME, 'FIN') &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16041800', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHARKS' &
                           str_detect(PRODUCT_NAME, 'FIN') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03039200', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% c('304880000'), '03048819', `CN-8`))

# shrimp
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_CATEGORY == 'SHRIMP', '03063690', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHRIMP' &
                           str_detect(PRODUCT_NAME, 'COLD-WATER'), '03063590', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHRIMP' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03061799', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHRIMP' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'COLD-WATER'), '03061699', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHRIMP' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'WARM-WATER'), '03061799', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHRIMP' &
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), '03069590', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHRIMP' &
                           (str_detect(PRODUCT_NAME, 'ATC') |
                              str_detect(PRODUCT_NAME, 'CANNED')), '16052900', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'SHRIMP' &
                           (str_detect(PRODUCT_NAME, 'PREPAR') |
                              str_detect(PRODUCT_NAME, 'BREADED')), '16052190', `CN-8`))

# smelts
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'SMELT', '03028990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SMELT' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038990', `CN-8`))

# snail
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'SNAIL', '03079100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SNAIL' &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16055900', `CN-8`))

# snapper
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'SNAPPER', '03028990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SNAPPER' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038990', `CN-8`))

# squid
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'SQUID', '03074220', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SQUID' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03074338', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SQUID' &
                           (str_detect(PRODUCT_NAME, 'PREPAR') |
                              str_detect(PRODUCT_NAME, 'CANNED')), '16055400', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SQUID' &
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), '03074940', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SQUID' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03074940', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SQUID' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'LOLIGO PEALEI'), '03074333', `CN-8`))

# swordfish
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'SWORDFISH', '03024700', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SWORDFISH' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03035700', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SWORDFISH' &
                           (str_detect(PRODUCT_NAME, 'FILLET') |
                              str_detect(PRODUCT_NAME, 'STEAKS')), '03044500', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SWORDFISH' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           (str_detect(PRODUCT_NAME, 'FILLET') |
                              str_detect(PRODUCT_NAME, 'STEAKS')), '03048400', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SWORDFISH' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045400', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'SWORDFISH' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03049100', `CN-8`))

# tilapia
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'TILAPIA', '03027100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TILAPIA' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03032300', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TILAPIA' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03043100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TILAPIA' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045100', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TILAPIA' &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03046100', `CN-8`))

# toothfish
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'TOOTHFISH', '03028300', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TOOTHFISH' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038300', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TOOTHFISH' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044600', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TOOTHFISH' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045500', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TOOTHFISH' &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048500', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TOOTHFISH' &
                           str_detect(PRODUCT_NAME, 'MEAT') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03049200', `CN-8`))

# trout
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'TROUT', '03021110', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TROUT' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03031410', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TROUT' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044290', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TROUT' &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048250', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'TROUT' &
                           str_detect(PRODUCT_NAME, 'SMOKED'), '03054300', `CN-8`))

# albacore tuna
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_NAME == 'ALBACORE TUNA', '03023190', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'ALBACORE TUNA' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03034190', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'ALBACORE TUNA' &
                           str_detect(PRODUCT_NAME, 'ATC'), '16041448', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'ALBACORE TUNA' &
                           str_detect(PRODUCT_NAME, 'LOINS'), '16041931', `CN-8`)) # no loins anywhere

# bigeye tuna
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_NAME == 'BIGEYE TUNA', '03023490', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'BIGEYE TUNA' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03034490', `CN-8`))

# bluefin tuna
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(str_detect(PRODUCT_NAME, 'TUNA BLUEFIN'), '03023519', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'TUNA BLUEFIN') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03034518', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'TUNA BLUEFIN') &
                           str_detect(PRODUCT_NAME, 'SOUTHERN'), '03023690', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'TUNA BLUEFIN') &
                           str_detect(PRODUCT_NAME, 'PACIFIC'), '03023599', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'TUNA BLUEFIN') &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'PACIFIC'), '03034599', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'TUNA BLUEFIN') &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'SOUTHERN'), '03034690', `CN-8`))

# other tuna
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_CATEGORY == 'TUNAS' &
                           SPECIES_GROUP == '', '03023980', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'TUNAS' &
                           SPECIES_GROUP == '' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03034985', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'TUNAS' &
                           SPECIES_GROUP == '' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03049999', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'TUNAS' &
                           SPECIES_GROUP == '' &
                           str_detect(PRODUCT_NAME, 'FROZEN') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03048700', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'TUNAS' &
                           SPECIES_GROUP == '' &
                           (str_detect(PRODUCT_NAME, 'ATC') |
                              str_detect(PRODUCT_NAME, 'A.T.C')), '16041441', `CN-8`),
         `CN-8` = ifelse(SPECIES_CATEGORY == 'TUNAS' &
                           SPECIES_GROUP == '' &
                           str_detect(PRODUCT_NAME, 'LOINS'), '16041931', `CN-8`), # no loins anywhere
         `CN-8` = ifelse(SPECIES_CATEGORY == 'TUNAS' &
                           SPECIES_GROUP == '' &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16041448', `CN-8`))

# skipjack tuna
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_NAME == 'SKIPJACK TUNA', '03023390', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'SKIPJACK TUNA' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03034390', `CN-8`))

# yellowfin tuna
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_NAME == 'YELLOWFIN TUNA', '03023290', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'YELLOWFIN TUNA' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03034290', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'YELLOWFIN TUNA' &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16041438', `CN-8`)) # no prepar

# whitefish
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'WHITEFISH', '03028990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'WHITEFISH' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'WHITEFISH' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'WHITEFISH' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045990', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'WHITEFISH' &
                           str_detect(PRODUCT_NAME, 'MEAT') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03049999', `CN-8`))

# wolffish (red fish)
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_GROUP == 'WOLFFISH', '03049950', `CN-8`),
         `CN-8` = ifelse(SPECIES_GROUP == 'WOLFFISH' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048929', `CN-8`))

# yellow perch
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(SPECIES_NAME == 'YELLOW PERCH', '03043900', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'YELLOW PERCH' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03046900', `CN-8`),
         `CN-8` = ifelse(SPECIES_NAME == 'YELLOW PERCH' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045990', `CN-8`))

# other molluscs
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(str_detect(PRODUCT_NAME, 'MOLLUSCS NSPF'), '03079100', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'MOLLUSCS NSPF') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03079200', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'MOLLUSCS NSPF') &
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), '03079900', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'MOLLUSCS NSPF') &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16055900', `CN-8`))

# other crustaceans
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(str_detect(PRODUCT_NAME, 'CRUSTACEANS NSPF'), '03063990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'CRUSTACEANS NSPF') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03061990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'CRUSTACEANS NSPF') &
                           str_detect(PRODUCT_NAME, 'DIRED/SALTED/BRINE'), '03069990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'CRUSTACEANS NSPF') &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16054000', `CN-8`))

# other aquatic inverts
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(str_detect(PRODUCT_NAME, 'AQUATIC INVERTEBRATES'), '03089090', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'AQUATIC INVERTEBRATES') &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16056900', `CN-8`))

# other shellfish
# check what is classified as other shellfish

# cockles
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(str_detect(PRODUCT_NAME, 'COCKLE'), '03077100', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'COCKLE') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03077900', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'COCKLE') &
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), '03077900', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'COCKLE') &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16055600', `CN-8`))

# fish nspf
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FISH NSPF') &
                           ECOLOGICAL_CATEGORY == '', '03028990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FISH NSPF') &
                           ECOLOGICAL_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FISH NSPF') &
                           ECOLOGICAL_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FISH NSPF') &
                           ECOLOGICAL_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FISH NSPF') &
                           ECOLOGICAL_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'SMOKED'), '03054980', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FISH NSPF') &
                           ECOLOGICAL_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'DRIED'), '03055985', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FISH NSPF') &
                           ECOLOGICAL_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'SALTED'), '03056980', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FISH NSPF') &
                           ECOLOGICAL_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'DRIED/SALTED/BRINE'), '03053990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FISH NSPF') &
                           ECOLOGICAL_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'SURIMI'), '03049910', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FISH NSPF') &
                           ECOLOGICAL_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FISH NSPF') &
                           ECOLOGICAL_CATEGORY == '' &
                           (str_detect(PRODUCT_NAME, 'MEAT') |
                              str_detect(PRODUCT_NAME, 'MINCED')) &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03049999', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FISH NSPF') &
                           ECOLOGICAL_CATEGORY == '' &
                           str_detect(PRODUCT_NAME, 'PREPAR'), '16042090', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FISH NSPF') &
                           ECOLOGICAL_CATEGORY == '' &
                           (str_detect(PRODUCT_NAME, 'ATC') |
                              str_detect(PRODUCT_NAME, 'CANNED')), '16041997', `CN-8`))

# marine fish nspf
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(str_detect(PRODUCT_NAME, 'MARINE FISH NSPF'), '03028990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'MARINE FISH NSPF') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'MARINE FISH NSPF') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'MARINE FISH NSPF') &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'MARINE FISH NSPF') &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'MARINE FISH NSPF') &
                           str_detect(PRODUCT_NAME, 'MEAT') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03049999', `CN-8`))

# freshwater fish nspf
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FRESHWATER FISH NSPF'), '03028910', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FRESHWATER FISH NSPF') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038910', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FRESHWATER FISH NSPF') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044910', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FRESHWATER FISH NSPF') &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045910', `CN-8`), 
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FRESHWATER FISH NSPF') &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048910', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'FRESHWATER FISH NSPF') &
                           str_detect(PRODUCT_NAME, 'MEAT') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03049921', `CN-8`))

# pike, pickerel, pike perch, yellow pike
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(HTS_NUMBER %in% c('302895025'), '03028910', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'PIKE PERCH'), '03028990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'PIKE PERCH') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03038990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'PIKE PERCH') &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'PIKE PERCH') &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03048990', `CN-8`),
         `CN-8` = ifelse(str_detect(PRODUCT_NAME, 'PIKE PERCH') &
                           str_detect(PRODUCT_NAME, 'MEAT'), '03045990', `CN-8`))

# remaining HTS codes
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(HTS_NUMBER %in% c('1605900500'), '16055900', `CN-8`))

# cetacea, use other fish codes
us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(HTS_NUMBER %in% c('208400000', '208400100'), '03045990', `CN-8`), 
         `CN-8` = ifelse(HTS_NUMBER %in% c('210920100'), '03054980', `CN-8`))


# Find HTS not yet assigned to CN8 ---------------------------------------------
# isolate HTS unassigned
free_hts <- us_hts_cn8 %>%
  filter(`CN-8` == '') %>%
  select(HTS_NUMBER) %>%
  distinct()

# pull hts to species groups from hts_to_species_group.R
hts_species_groups <- read.csv('C:/Users/cameron.vanhorn/Documents/GitHub/seafood-traceability-design/species_group_hts.csv') %>%
  # convert numeric to character
  mutate(HTS.Number = as.character(HTS.Number)) %>%
  rename(HTS_NUMBER = HTS.Number,
         SPECIES_GROUP = species_group) %>%
  select(!c(X, Product.Name)) %>%
  distinct()

hts_species_leftovers <- left_join(free_hts, hts_species_groups) %>%
  left_join(trade_data %>% select(HTS_NUMBER, PRODUCT_NAME) %>% distinct()) %>%
  filter(!is.na(SPECIES_GROUP))


# Assign CN8 to leftover HTS ---------------------------------------------------
# bass
bass_hts <- hts_species_leftovers %>%
  filter(str_detect(SPECIES_GROUP, 'bass')) %>%
  pull(HTS_NUMBER)

us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(HTS_NUMBER %in% bass_hts, '03028410', `CN-8`))

# bonito
bonito_hts <- hts_species_leftovers %>%
  filter(str_detect(SPECIES_GROUP, 'bonito')) %>%
  pull(HTS_NUMBER)

us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(HTS_NUMBER %in% bonito_hts, '16041490', `CN-8`))

# cusk
cusk_hts <- hts_species_leftovers %>%
  filter(str_detect(SPECIES_GROUP, 'cusk')) %>%
  pull(HTS_NUMBER)

us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(HTS_NUMBER %in% cusk_hts &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03036980', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% cusk_hts &
                           str_detect(PRODUCT_NAME, 'SALTED'), '03056910', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% cusk_hts &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044990', `CN-8`))

# hake
hake_hts <- hts_species_leftovers %>%
  filter(str_detect(SPECIES_GROUP, 'hake')) %>%
  pull(HTS_NUMBER)

us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(HTS_NUMBER %in% hake_hts, '03025419', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% hake_hts &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03036619', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% hake_hts &
                           str_detect(PRODUCT_NAME, 'FILLET'), '03044490', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% hake_hts &
                           str_detect(PRODUCT_NAME, 'FILLET') &
                           str_detect(PRODUCT_NAME, 'FROZEN'), '03047419', `CN-8`))

# herring
herring_hts <- hts_species_leftovers %>%
  filter(str_detect(SPECIES_GROUP, 'herring')) %>%
  pull(HTS_NUMBER)

us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(HTS_NUMBER %in% herring_hts, '03024100', `CN-8`))

# shark
shark_hts <- hts_species_leftovers %>%
  filter(str_detect(SPECIES_GROUP, 'shark')) %>%
  pull(HTS_NUMBER)

us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(HTS_NUMBER %in% shark_hts, '16041800', `CN-8`))

# shrimp
shrimp_hts <- hts_species_leftovers %>%
  filter(str_detect(SPECIES_GROUP, 'shrimp')) %>%
  pull(HTS_NUMBER)

us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(HTS_NUMBER %in% shrimp_hts, '03063690', `CN-8`),
         `CN-8` = ifelse(HTS_NUMBER %in% shrimp_hts &
                           str_detect(PRODUCT_NAME, 'ATC'), '16052900', `CN-8`))

# whiting
whiting_hts <- hts_species_leftovers %>%
  filter(str_detect(SPECIES_GROUP, 'whiting')) %>%
  pull(HTS_NUMBER)

us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(HTS_NUMBER %in% whiting_hts, '03047930', `CN-8`))

# other shellfish
shellfish_hts <- hts_species_leftovers %>%
  filter(str_detect(SPECIES_GROUP, 'shellfish')) %>%
  pull(HTS_NUMBER)

us_hts_cn8 <- us_hts_cn8 %>%
  mutate(`CN-8` = ifelse(HTS_NUMBER %in% shellfish_hts, '16042005', `CN-8`))


# Attach Conversion Factors, export data ---------------------------------------
conversion_factors <- left_join(us_hts_cn8, eumofa %>% select(`CN-8`, CF))

write.csv(conversion_factors, 'conversion_factors.csv')