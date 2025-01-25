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

# Plot Save Function -----------------------------------------------------------
save_plot <- function(plot) {
  ggsave(filename = paste0('Plots/', as.character(substitute(plot)), '.png'),
         plot = plot,
         width = 9,
         height = 6)
}

###############
### EXPORTS ###
###############
# Comparing Export Value through time (Real 2023 USD) --------------------------
# Format Data
exp_value_yr_data <- trade_data %>%
  # we only need two columns, aggregate based on these columns
  select(YEAR, EXP_VALUE_2023USD) %>%
  mutate(EXP_VALUE_2023USD = ifelse(is.na(EXP_VALUE_2023USD), 0, 
                                    EXP_VALUE_2023USD)) %>%
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  # convert USD to Million USD
  mutate(EXP_VALUE_2023USD_MILLIONS = EXP_VALUE_2023USD / 1000000)

# Plot the data
exp_value_yr <- 
  ggplot(data = exp_value_yr_data,
         aes(x = factor(YEAR),
             y = EXP_VALUE_2023USD_MILLIONS)) +
    geom_col(fill = 'black') +
    coord_cartesian(ylim = c(4000, 8000)) +
    scale_x_discrete(breaks = c(2004, 2008, 2012, 2016, 2020, 2023),
                     limits = factor(2004:2023)) +
    scale_y_continuous(breaks = c(4000, 5000, 6000, 7000, 8000),
                       labels = label_currency(suffix = 'M')) +
    labs(x = 'Year',
         y = 'Total Export Value (Real 2023 USD)') +
    theme_bw() +
    theme(axis.text = element_text(size = 10))

# View the plot
exp_value_yr

# Save the plot
save_plot(exp_value_yr)


# Comparing Export Volume through time -----------------------------------------
# Format the data
exp_volume_yr_data <- trade_data %>%
  select(YEAR, EXP_VOLUME_KG) %>%
  mutate(EXP_VOLUME_KG = ifelse(is.na(EXP_VOLUME_KG), 0,
                                EXP_VOLUME_KG)) %>%
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  # convert kg to metric tons
  mutate(EXP_VOLUME_MT = EXP_VOLUME_KG / 1000)
  
# Plot the data
exp_volume_yr <- 
  ggplot(data = exp_volume_yr_data,
         aes(x = factor(YEAR),
             y = EXP_VOLUME_MT)) + 
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4)) +
  scale_y_continuous(label = comma) +
  labs(x = 'Year',
       y = 'Total Export Volume (Metric Tons)') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))

# View the plot
exp_volume_yr

# Save the plot
save_plot(exp_volume_yr)

# Comparing Export Price (Real 2023 USD/KG) through time -----------------------
# Format the data
exp_price_yr_data <- trade_data %>%
  select(YEAR, EXP_VALUE_2023USD, EXP_VOLUME_KG) %>%
  mutate(EXP_VOLUME_KG = ifelse(is.na(EXP_VOLUME_KG), 0,
                                EXP_VOLUME_KG),
         EXP_VALUE_2023USD = ifelse(is.na(EXP_VALUE_2023USD), 0,
                                    EXP_VALUE_2023USD)) %>%
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  mutate(EXP_PRICE_USD_PER_KG = EXP_VALUE_2023USD / EXP_VOLUME_KG )

# Plot the data
exp_price_yr <- 
  ggplot(data = exp_price_yr_data,
         aes(x = factor(YEAR),
             y = EXP_PRICE_USD_PER_KG)) +
  geom_col(fill = 'black') +
  coord_cartesian(ylim = c(3, 4.75)) +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2023)) +
  scale_y_continuous(labels = label_currency(suffix = '/kg'),
                     breaks = c(3, 3.50, 4, 4.50)) +
  labs(x = 'Year',
       y = 'Average Price') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))

# View the plot
exp_price_yr

# Save the plot
save_plot(exp_price_yr)
  
  # Comparing export value/volume/price by customs district
  # Comparing export value/volume/price by country
  
###############
### IMPORTS ###
###############
# Comparing Import value through time (Real 2023 USD) --------------------------
# Format the Data
imp_value_yr_data <- trade_data %>%
  select(YEAR, IMP_VALUE_2023USD) %>%
  mutate(IMP_VALUE_2023USD = ifelse(is.na(IMP_VALUE_2023USD), 0,
                                    IMP_VALUE_2023USD)) %>%
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  mutate(IMP_VALUE_2023USD_MILLIONS = IMP_VALUE_2023USD / 1000000)

# Plot the data
imp_value_yr <- 
  ggplot(data = imp_value_yr_data,
         aes(x = factor(YEAR),
             y = IMP_VALUE_2023USD_MILLIONS)) +
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2023)) +
  scale_y_continuous(labels = label_currency(suffix = 'M')) +
  labs(x = 'Year',
       y = 'Total Import Value (Real 2023 USD)') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))

# View the plot
imp_value_yr

# Save the plot
save_plot(imp_value_yr)

# Comparing Import volume through time -----------------------------------------
# Format the data
imp_volume_yr_data <- trade_data %>%
  select(YEAR, IMP_VOLUME_KG) %>%
  mutate(IMP_VOLUME_KG = ifelse(is.na(IMP_VOLUME_KG), 0,
                                IMP_VOLUME_KG)) %>%
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  mutate(IMP_VOLUME_MT = IMP_VOLUME_KG / 1000)

# Plot the data
imp_volume_yr <- 
  ggplot(data = imp_volume_yr_data,
         aes(x = factor(YEAR),
             y = IMP_VOLUME_MT)) +
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2024)) +
  scale_y_continuous(labels = comma) +
  labs(x = 'Year',
       y = 'Total Import Volume (Metric Tons)') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))

# View the plot
imp_volume_yr

# Save the plot
save_plot(imp_volume_yr)

# Comparing Import price (Real 2023 USD/KG) through time -----------------------
# Format the data
imp_price_yr_data <- full_join(imp_value_yr_data, imp_volume_yr_data) %>%
  mutate(IMP_PRICE_USD_PER_KG = IMP_VALUE_2023USD / IMP_VOLUME_KG)

# Plot the data
imp_price_yr <- 
  ggplot(data = imp_price_yr_data,
         aes(x = factor(YEAR),
             y = IMP_PRICE_USD_PER_KG)) +
  geom_col(fill = 'black') +
  coord_cartesian(ylim = c(6.50, 10)) +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2023)) +
  scale_y_continuous(labels = label_currency(suffix = '/kg'),
                     breaks = c(6.50, 7, 7.50, 8, 8.50, 9, 9.50, 10)) +
  labs(x = 'Year',
       y = 'Average Price') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))

# View the plot
imp_price_yr

# Save the plot
save_plot(imp_price_yr)
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
