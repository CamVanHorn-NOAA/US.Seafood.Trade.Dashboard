# Script 3: Visualize Seafood Trade Data
# See 2_data_munge for data curation and 1_data_pulls for data sourcing

# Contact: Cameron Van Horn
#          cameron.vanhorn@noaa.gov

#########################
### PACKAGES AND DATA ###
#########################
# Packages ---------------------------------------------------------------------
if(!require("googledrive")) install.packages("googledrive")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("scales")) install.packages("scales")

# Pull Data --------------------------------------------------------------------
# Authorize link to google drive
drive_auth()

# Grab most recent data file
# only take first two columns (the third column is a list of metadata)
data_file <- drive_find(pattern = 'seafood_trade_data_munge')[, 1:2] %>%
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

###############
### EXPORTS ###
###############
# Comparing Export Value through time (Real 2023 USD) --------------------------
# Format Data
exp_value_yr <- trade_data %>%
  # we only need two columns, aggregate based on these columns
  select(YEAR, EXP_VALUE_2023USD) %>%
  mutate(EXP_VALUE_2023USD = ifelse(is.na(EXP_VALUE_2023USD), 0, 
                                    EXP_VALUE_2023USD)) %>%
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  mutate(EXP_VALUE_2023USD_MILLIONS = EXP_VALUE_2023USD / 1000000)

# Plot the data
ggplot(data = exp_value_yr,
       aes(x = factor(YEAR),
           y = EXP_VALUE_2023USD_MILLIONS)) +
  geom_col(fill = 'black') +
  coord_cartesian(ylim = c(4000, 8000)) +
  scale_x_discrete(breaks = c(2004, 2008, 2012, 2016, 2020, 2023),
                   limits = factor(2004:2023)) +
  scale_y_continuous(breaks = c(4000, 5000, 6000, 7000, 8000),
                     labels = label_currency(suffix = 'M')) +
  labs(x = 'Year',
       y = 'Total Exports (Real 2023 USD)') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))
  
  # Comparing Export Volume through time
  # Comparing Export Price (Real 2023 USD/KG) through time
  # Comparing export value/volume/price by customs district
  # Comparing export value/volume/price by country
  
# Imports ----------------------------------------------------------------------
  # Comparing Import value through time (Real 2023 USD)
  # Comparing Import volume through time 
  # Comparing Import price (Real 2023 USD/KG) through time
  # comparing import value/volume/price by customs district
  # Comparing import value/volume/price by country

# Trade Balances ---------------------------------------------------------------
  # Comparing Value balance through time (Real 2023 USD)
  # comparing volume balance through time 
  # comparing price (Real 2023 USD/KG) balance through time
  # comparing value/volume/price balance by customs district
  # comparing value/volume/price balance by customs district

# Processed Products ----------------------------------------------------------- 
  # Compare Value of domestic processed products through time
  # compare volume of domestic processed products through time
  # compare price of domestic processed products through time
  # compare top prices of species/product types through time
  # compare top species sold by volume through time
  # compare top species sold by value through time
  # compare top product types sold by volume through time
  # compare top product types sold by value through time