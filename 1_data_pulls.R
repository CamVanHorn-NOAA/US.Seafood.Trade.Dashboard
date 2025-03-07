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
# drive_download('foss_exports_15-24.csv',
#                overwrite = T)
# drive_download('foss_exports_04-14.csv',
#                overwrite = T)
# drive_download('foss_imports_15-24.csv',
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

##################################
### LOAD DOWNLOADED DATA FILES ###
##################################
# Some csv's are downloaded staright from FOSS without any prior modification
# I.e., their headers are setup improperly and need adjustment

# Exports ----------------------------------------------------------------------
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
                             HTS_NUMBER))

foss_exports_0414 <- read.csv('foss_exports_04-14.csv') %>%
  setNames(.[1, ]) %>%
  rename_with( ~ toupper(gsub(' ', '_', .x, fixed = T))) %>%
  rename_with( ~ toupper(gsub('(', '', .x, fixed = T))) %>%
  rename_with( ~ toupper(gsub(')', '', .x, fixed = T))) %>%
  .[-1, ] %>%
  mutate(HTS_NUMBER = ifelse(str_sub(HTS_NUMBER, 1, 1) == '0',
                             str_sub(HTS_NUMBER, 2, -1),
                             HTS_NUMBER))

# combine data (stack)
foss_exports <- bind_rows(foss_exports_0414, foss_exports_1524)

# Imports ----------------------------------------------------------------------
# read csv's
foss_imports_1524 <- read.csv('foss_imports_15-24.csv') %>%
  setNames(.[1, ]) %>%
  rename_with( ~ toupper(gsub(' ', '_', .x, fixed = T))) %>%
  rename_with( ~ toupper(gsub('(', '', .x, fixed = T))) %>%
  rename_with( ~ toupper(gsub(')', '', .x, fixed = T))) %>%
  .[-1, ] %>%
  mutate(HTS_NUMBER = ifelse(str_sub(HTS_NUMBER, 1, 1) == '0',
                             str_sub(HTS_NUMBER, 2, -1),
                             HTS_NUMBER))

foss_imports_0414 <- read.csv('foss_imports_04-14.csv') %>%
  setNames(.[1, ]) %>%
  rename_with( ~ toupper(gsub(' ', '_', .x, fixed = T))) %>%
  rename_with( ~ toupper(gsub('(', '', .x, fixed = T))) %>%
  rename_with( ~ toupper(gsub(')', '', .x, fixed = T))) %>%
  .[-1, ] %>%
  mutate(HTS_NUMBER = ifelse(str_sub(HTS_NUMBER, 1, 1) == '0',
                             str_sub(HTS_NUMBER, 2, -1),
                             HTS_NUMBER))

# combine data (stack)
foss_imports <- bind_rows(foss_imports_0414, foss_imports_1524)

# Processed Products & Species Metadata ----------------------------------------
# read csv's
foss_pp_1523 <- read.csv('foss_pp_15-23.csv') %>%
  setNames(.[1, ]) %>%
  rename_with( ~ toupper(gsub(' ', '_', .x, fixed = T))) %>%
  .[-1, ]

foss_pp_0414 <- read.csv('foss_pp_04-14.csv') %>%
  setNames(.[1, ]) %>%
  rename_with( ~ toupper(gsub(' ', '_', .x, fixed = T))) %>%
  .[-1, ]

# combine data (stack)
foss_pp <- bind_rows(foss_pp_0414, foss_pp_1523)

# read csv of Species metadata
pp_landings_map <- read.csv('pp_com_landings_mapping.csv') %>%
  select(!X)

# Commercial Landings ----------------------------------------------------------
# read csv's
foss_com_1523 <- read.csv('foss_com_landings_15-23.csv') %>%
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
foss_com_landings <- bind_rows(foss_com_0414, foss_com_1523)

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
save(list = c('foss_exports', 'foss_imports', 'foss_pp', 'def_index',
              'species_ref', 'foss_com_landings', 'pp_landings_map',
              'trade_map', 'landings_map'),
     file = file_name)

# upload to google drive
drive_upload(file_name,
             # IMPORTANT: change path below to match your personal Drive
              # NOTE: no path results in save to Drive location specified
              #       above
             path = 'Seafood Trade Dashboard Project/Seafood Trade Data/',
             overwrite = T)
