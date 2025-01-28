# Script 3: Visualize Seafood Trade Data
# See 2_data_munge for data curation and 1_data_pulls for data sourcing

# Contact: Cameron Van Horn
#          cameron.vanhorn@noaa.gov

# TODO: get nmfspalette package installed (must be on govt machine)
#############################
##### PACKAGES AND DATA #####
#############################
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

# Data formatting (Trade data) -------------------------------------------------
# Aggregate trade data by year
trade_data_yr <- trade_data %>%
  # select only columns that we need to compare trade data across years
  select(YEAR, EXP_VALUE_2023USD, EXP_VOLUME_KG, 
         IMP_VALUE_2023USD, IMP_VOLUME_KG) %>%
  # replace NAs with 0s for summation
  mutate(EXP_VALUE_2023USD = ifelse(is.na(EXP_VALUE_2023USD), 0,
                                    EXP_VALUE_2023USD),
         IMP_VALUE_2023USD = ifelse(is.na(IMP_VALUE_2023USD), 0,
                                    IMP_VALUE_2023USD),
         EXP_VOLUME_KG = ifelse(is.na(EXP_VOLUME_KG), 0,
                                EXP_VOLUME_KG),
         IMP_VOLUME_KG = ifelse(is.na(IMP_VOLUME_KG), 0,
                                IMP_VOLUME_KG)) %>%
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  # calculate price and convert value to millions, kg to metric tons
  mutate(EXP_PRICE_USD_PER_KG = EXP_VALUE_2023USD / EXP_VOLUME_KG,
         IMP_PRICE_USD_PER_KG = IMP_VALUE_2023USD / IMP_VOLUME_KG,
         EXP_VALUE_2023USD_MILLIONS = EXP_VALUE_2023USD / 1000000,
         IMP_VALUE_2023USD_MILLIONS = IMP_VALUE_2023USD / 1000000,
         EXP_VALUE_2023USD_BILLIONS = EXP_VALUE_2023USD / 1000000000,
         IMP_VALUE_2023USD_BILLIONS = IMP_VALUE_2023USD / 1000000000,
         EXP_VOLUME_MT = EXP_VOLUME_KG / 1000,
         IMP_VOLUME_MT = IMP_VOLUME_KG / 1000)

# Data formatting (Processed products) -----------------------------------------
# Aggregate processed products by year
pp_data_yr <- pp_data %>%
  select(YEAR, KG, DOLLARS_2023, DOLLARS_2023_PER_KG) %>%
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  mutate(MT = KG / 1000,
         DOLLARS_2023_BILLIONS = DOLLARS_2023 / 1000000000,
         DOLLARS_2023_PER_KG = DOLLARS_2023 / KG)
  
###################
##### EXPORTS #####
###################
# Comparing Export Value through time (Real 2023 USD) --------------------------
# Make the plot
exp_value_yr <- 
  ggplot(data = trade_data_yr,
         aes(x = factor(YEAR),
             y = EXP_VALUE_2023USD_BILLIONS)) +
    geom_col(fill = 'black') +
    coord_cartesian(ylim = c(4, 8)) +
    scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                     limits = factor(2004:2023)) +
    scale_y_continuous(breaks = seq(4, 8, by = 1),
                       labels = label_currency(suffix = 'B')) +
    labs(x = 'Year',
         y = 'Total Export Value (Real 2023 USD)') +
    theme_bw() +
    theme(axis.text = element_text(size = 10))

# View the plot
exp_value_yr

# Save the plot
save_plot(exp_value_yr)


# Comparing Export Volume through time -----------------------------------------
# Make the plot
exp_volume_yr <- 
  ggplot(data = trade_data_yr,
         aes(x = factor(YEAR),
             y = EXP_VOLUME_MT)) + 
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4)) +
  scale_y_continuous(label = comma) +
  coord_cartesian(ylim = c(750000, 2000000)) +
  labs(x = 'Year',
       y = 'Total Export Volume (Metric Tons)') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))

# View the plot
exp_volume_yr

# Save the plot
save_plot(exp_volume_yr)

# Comparing Export Price (Real 2023 USD/KG) through time -----------------------
# Make the plot
exp_price_yr <- 
  ggplot(data = trade_data_yr,
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
  
###################
##### IMPORTS #####
###################
# Comparing Import value through time (Real 2023 USD) --------------------------
# Make the plot
imp_value_yr <- 
  ggplot(data = trade_data_yr,
         aes(x = factor(YEAR),
             y = IMP_VALUE_2023USD_BILLIONS)) +
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2023)) +
  scale_y_continuous(labels = label_currency(suffix = 'B')) +
  coord_cartesian(ylim = c(10, 35)) +
  labs(x = 'Year',
       y = 'Total Import Value (Real 2023 USD)') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))

# View the plot
imp_value_yr

# Save the plot
save_plot(imp_value_yr)

# Comparing Import volume through time -----------------------------------------
# Make the plot
imp_volume_yr <- 
  ggplot(data = trade_data_yr,
         aes(x = factor(YEAR),
             y = IMP_VOLUME_MT)) +
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2024)) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(ylim = c(1750000, 3500000)) +
  labs(x = 'Year',
       y = 'Total Import Volume (Metric Tons)') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))

# View the plot
imp_volume_yr

# Save the plot
save_plot(imp_volume_yr)

# Comparing Import price (Real 2023 USD/KG) through time -----------------------
# Make the plot
imp_price_yr <- 
  ggplot(data = trade_data_yr,
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

##########################
##### TRADE BALANCES #####
##########################
# Comparing value balance through time (Real 2023 USD) -------------------------
# Format the data
# We need the data formatted such that there are factored groups of imports,
  # exports, and trade balance
# This will involve pivoting the data to be long, using the column headers
  # as a guide for factor names
value_balance_yr_data <- trade_data_yr %>%
  mutate(EXP_VALUE_2023USD_BILLIONS = EXP_VALUE_2023USD_MILLIONS / 1000,
         IMP_VALUE_2023USD_BILLIONS = IMP_VALUE_2023USD_MILLIONS / 1000,
         TRADE_BALANCE = 
           EXP_VALUE_2023USD_BILLIONS - IMP_VALUE_2023USD_BILLIONS) %>%
  select(YEAR, EXP_VALUE_2023USD_BILLIONS, IMP_VALUE_2023USD_BILLIONS,
         TRADE_BALANCE) %>%
  rename(EXPORTS = EXP_VALUE_2023USD_BILLIONS,
         IMPORTS = IMP_VALUE_2023USD_BILLIONS) %>%
  pivot_longer(cols = c(EXPORTS, IMPORTS, TRADE_BALANCE)) %>%
  mutate(name = as.factor(name))

# Plot the data
value_balance_yr <- 
  ggplot(data = value_balance_yr_data,
         aes(x = as.factor(YEAR),
             y = value)) +
  geom_bar(aes(fill = name), 
           stat = 'identity',
           position = 'dodge') +
  labs(x = '',
       y = 'Billions (2023 Dollars)',
       fill = '') +
  scale_fill_discrete(labels = c('Exports',
                                 'Imports',
                                 'Trade Balance')) +
  scale_x_discrete(limits = factor(2004:2023)) +
  scale_y_continuous(labels = label_currency(),
                     breaks = seq(-30, 35, by = 5)) +
  geom_hline(yintercept = 0, color = 'black') +
  theme_minimal() +
  theme(legend.position = 'top',
        axis.line.y = element_line(color = 'black'),
        axis.text.x = element_text(vjust = 13.9, 
                                   angle = 45,
                                   hjust = 5.3),
        plot.background = element_rect(fill = 'white'),
        panel.grid = element_blank())

# View the plot
value_balance_yr

# Save the plot
save_plot(value_balance_yr)
  
# Comparing volume balance through time ----------------------------------------
# Format the data
volume_balance_yr_data <- trade_data_yr %>%
  mutate(TRADE_BALANCE = EXP_VOLUME_MT - IMP_VOLUME_MT) %>%
  rename(EXPORTS = EXP_VOLUME_MT, 
         IMPORTS = IMP_VOLUME_MT) %>%
  select(YEAR, EXPORTS, IMPORTS, TRADE_BALANCE) %>%
  pivot_longer(cols = c(EXPORTS, IMPORTS, TRADE_BALANCE)) %>%
  mutate(name = as.factor(name))

# Plot the data
volume_balance_yr <- 
  ggplot(data = volume_balance_yr_data,
         aes(x = as.factor(YEAR),
             y = value)) +
  geom_bar(aes(fill = name),
           stat = 'identity',
           position = 'dodge') +
  labs(x = '',
       y = 'Metric Tons',
       fill = '') +
  scale_fill_discrete(labels = c('Exports',
                                 'Imports',
                                 'Trade Balance')) +
  scale_x_discrete(limits = factor(2004:2023)) +
  scale_y_continuous(labels = comma,
                     breaks = seq(-2000000, 3500000, by = 500000)) +
  geom_hline(yintercept = 0, color = 'black') +
  theme_minimal() +
  theme(legend.position = 'top',
        axis.line.y = element_line(color = 'black'),
        axis.text.x = element_text(vjust = 12.45,
                                   angle = 45,
                                   hjust = 4.8),
        plot.background = element_rect(fill = 'white'),
        panel.grid = element_blank())

# View the plot
volume_balance_yr

# Save the plot
save_plot(volume_balance_yr)

# Comparing price (Real 2023 USD/KG) balance through time ----------------------
# Format the data
price_balance_yr_data <- trade_data_yr %>%
  mutate(TRADE_BALANCE = EXP_PRICE_USD_PER_KG - IMP_PRICE_USD_PER_KG) %>%
  rename(EXPORTS = EXP_PRICE_USD_PER_KG,
         IMPORTS = IMP_PRICE_USD_PER_KG) %>%
  select(YEAR, EXPORTS, IMPORTS, TRADE_BALANCE) %>%
  pivot_longer(cols = c(EXPORTS, IMPORTS, TRADE_BALANCE)) %>%
  mutate(name = as.factor(name))

# Plot the data
price_balance_yr <- 
  ggplot(data = price_balance_yr_data,
         aes(x = as.factor(YEAR),
             y = value)) + 
  geom_bar(aes(fill = name),
           stat = 'identity',
           position = 'dodge') +
  labs(x = '',
       y = 'Metric Tons',
       fill = '') +
  scale_fill_discrete(labels = c('Exports',
                                 'Imports',
                                 'Trade Balance')) +
  scale_x_discrete(limits = factor(2004:2023)) +
  scale_y_continuous(labels = label_currency(suffix = '/kg'),
                     breaks = seq(-6, 10, by = 2)) +
  geom_hline(yintercept = 0, color = 'black') +
  theme_minimal() + 
  theme(legend.position = 'top',
        axis.line.y = element_line(color = 'black'),
        axis.text.x = element_text(vjust = 12,
                                   angle = 45,
                                   hjust = 4.65),
        plot.background = element_rect(fill = 'white'),
        panel.grid = element_blank())

# View the plot
price_balance_yr

# Save the plot
save_plot(price_balance_yr)
  # comparing value/volume/price balance by customs district
  # comparing value/volume/price balance by country 

###############################
###### PROCESSED PRODUCTS #####
###############################
# Compare Value of domestic processed products through time --------------------
# Make the plot
pp_value_yr <- 
  ggplot(data = pp_data_yr,
         aes(x = factor(YEAR),
             y = DOLLARS_2023_BILLIONS)) +
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2023)) +
  coord_cartesian(ylim = c(7.5, 17.5)) +
  scale_y_continuous(labels = label_currency(suffix = 'B')) +
  labs(x = 'Year',
       y = 'Total Value (Real 2023 USD)') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))

# View the plot
pp_value_yr

# Save the plot
save_plot(pp_value_yr)

# Compare volume of domestic processed products through time -------------------
# Make the plot
pp_volume_yr <- 
  ggplot(data = pp_data_yr,
         aes(x = factor(YEAR),
             y = MT)) +
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2023)) +
  coord_cartesian(ylim = c(1500000, 3000000)) +
  scale_y_continuous(labels = comma) +
  labs(x = 'Year',
       y = 'Total Volume (MT)') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))

# View the plot
pp_volume_yr

# Save the plot
save_plot(pp_volume_yr)

# Compare price of domestic processed products through time --------------------
pp_price_yr <- 
  ggplot(data = pp_data_yr,
         aes(x = factor(YEAR),
             y = DOLLARS_2023_PER_KG)) +
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2023)) +
  scale_y_continuous(labels = label_currency(suffix = '/kg')) +
  coord_cartesian(ylim = c(3, 6)) +
  labs(x = 'Year',
       y = 'Average Price (Real 2023 USD)') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))

# View the plot
pp_price_yr

# Save the plot
save_plot(pp_price_yr)

  # compare top prices of species/product types through time
  # compare top species sold by volume through time
  # compare top species sold by value through time
  # compare top product types sold by volume through time
  # compare top product types sold by value through time
