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

