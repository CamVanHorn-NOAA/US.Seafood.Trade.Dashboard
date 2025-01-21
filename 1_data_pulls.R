# Script 1: Pull Data for Seafood Dashboard
# Data sources include the fisheries one-stop shop (FOSS, 
  # https://www.fisheries.noaa.gov/foss/f?p=215:2:742090237412:::::) and SQL
  # databases. 
  # TODO: ingest processed products data
  # TODO: link to SQL database for processed product download
# Contact: Cameron Van Horn
#          cameron.vanhorn@noaa.gov



# Load Packages ----------------------------------------------------------------
if(!require("googledrive")) install.packages("googledrive")
if(!require("dplyr"))   install.packages("dplyr")

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
# drive_download('foss_exports_15-24.csv')
# drive_download('foss_exports_04-14.csv')
# drive_download('foss_imports_15-24.csv')
# drive_download('foss_imports_04-14.csv')


##################################
### LOAD DOWNLOADED DATA FILES ###
##################################
# The csv's are downloaded staright from FOSS without any prior modification
# I.e., their headers are setup improperly and need adjustment

# Exports ----------------------------------------------------------------------
# read csv's
foss_exports_1524 <- read.csv('foss_exports_15-24.csv') %>%
  # use setNames from 'stats' to assign first row values as column names
  setNames(.[1, ]) %>%
  # remove first row
  .[-1, ]

foss_exports_0414 <- read.csv('foss_exports_04-14.csv') %>%
  setNames(.[1, ]) %>%
  .[-1, ]

# combine data (stack)
foss_exports <- bind_rows(foss_exports_0414, foss_exports_1524)

# Imports ----------------------------------------------------------------------
# read csv's
foss_imports_1524 <- read.csv('foss_imports_15-24.csv') %>%
  setNames(.[1, ]) %>%
  .[-1, ]

foss_imports_0414 <- read.csv('foss_imports_04-14.csv') %>%
  setNames(.[1, ]) %>%
  .[-1, ]

# combine data (stack)
foss_imports <- bind_rows(foss_imports_0414, foss_imports_1524)

#####################
### SAVE THE DATA ###
#####################
# create the save file name
# preserve the date of data creation for reference between data files
file_name <- paste0('seafood_trade_data_pull_', 
                    format(Sys.Date(), '%m_%d_%y'), 
                    '.RData')

# create the file
save(list = c('foss_exports', 'foss_imports'),
     file = file_name)

# upload to google drive
drive_upload(file_name,
             # IMPORTANT: change path below to match your personal Drive
              # NOTE: no path results in save to Drive location specified
              #       above
             path = 'Seafood Trade Dashboard Project/Seafood Trade Data/')
