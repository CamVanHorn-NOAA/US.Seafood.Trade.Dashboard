# Script 1: Pull Data for Seafood Dashboard
# Data sources include the fisheries one-stop shop (FOSS, 
  # https://www.fisheries.noaa.gov/foss/f?p=215:2:742090237412:::::) and SQL
  # databases. 
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
# We can run drive_find(n_max = XX) to obtain access credentials and link
  # the R session to a google drive account
drive_find(n_max = 25)
# the output is simply named files in your google drive

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
### READ DOWNLOADED DATA FILES ###
##################################
# The csv's are downloaded staright from FOSS without any prior modification
# I.e., their headers are setup improperly and need adjustment

# read csv
foss_exports_1524 <- read.csv('foss_exports_15-24.csv') %>%
  # use setNames from 'stats' to assign first row values as column names
  setNames(.[1,]) %>%
  # remove first row
  .[-1, ]