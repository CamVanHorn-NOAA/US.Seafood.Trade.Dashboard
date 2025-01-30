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

# Data formatting (Trade data by year) -----------------------------------------
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

##############################
##### PROCESSED PRODUCTS #####
##############################
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

###############################
##### SPECIES GROUP TRADE #####
###############################
# We are interested in evaluating some of the metrics calculated above with 
  # respect to specific species of interest
# Due to many species being available in our trade data, it is best to 
  # create generic functions to filter the data for a given species and 
  # plot the species-specific data
# Data filtering function ------------------------------------------------------
# The below function is meant to work within dplyr pipes, specifically those
  # used in this script that incorporate our trade data, not processed products
# The output of this function is the trade_table filtered for the specified
  # species of interest
# The function accounts for the complex structure of our data as there are
  # three columns that contain species information
# GROUP_NAME contains more specific species information, but also relies on
  # species groups provided in GROUP_TS (for example, atlantic salmon are 
  # listed as 'ATLANTIC' in GROUP_NAME and 'SALMON' in GROUP_TS)
  # These kinds of distinctions can make indexing difficult
# Therefore, this function searches both columns for the species term provided
# If the species name is not found, the user is directed via error message to
  # provide a more generic species name or search the data for acceptable
  # species names to call in the function
filter_species <- function(trade_table, species_name) {
  # provide trade_table as a data_frame with GROUP_NAME and GROUP_TS as fields
  # provide species_name as a vector of class character
  
  # store unique species names for each field that contains species names in 
    # separate vectors
  group_name <- unique(trade_table$GROUP_NAME)
  group_ts <- unique(trade_table$GROUP_TS)
  
  # if species_name is provided in lower case, coerces to upper case
  species_name <- toupper(species_name)
  
  # create flags for the function to filter properly
  use_group_name <- ifelse(species_name %in% group_name, 'Y', 'N')
  use_group_ts <- ifelse(species_name %in% group_ts, 'Y', 'N')
  
  # if the provied species_name is not found in either field, give error
  if (use_group_name == 'N' & use_group_ts == 'N') {
    stop("The species you provided is either too specific or not available.
         Try 'unique(your_data$your_column)' to find acceptable calls")
  }
  
  # if the provided species_name is found in the more generic field (TS),
    # regardless of it being in the more specific field, use the generic field
    # as a filter
  if (use_group_ts == 'Y') {
    filtered_data <- trade_table %>%
      filter(GROUP_TS == species_name)
  } 
  
  # if the provided species_name is only found in the more specific field,
    # use the specific field as a filter
  else if (use_group_ts == 'N' & use_group_name == 'Y') {
    filtered_data <- trade_table %>%
      filter(GROUP_NAME == species_name)
  }
  
  return(filtered_data)
}


# Data summarizing function ----------------------------------------------------
# The below function works in tandem with the filter_species fxn above
# These functions are separate in case of interest in viewing the data filtered
  # but not summarized
# The function summarizes data grouped by year and species group, and requires
  # the input data to be trade_data, or formatted similarly to such
summarize_yr_spp <- function(trade_table, species_name) {
  # trade_table is a data frame, either trade_data provided in script 2 or
    # another data frame with comparable nomenclature and purpose
  # species_name is a vector of class character and should not be specific
  
  # set species_name to upper case
  species_name <- toupper(species_name)
  
  # we must specify which group we want to summarize the data around
  # this effort is similar to that done in filter_species
  # To coerce the group to operate in dplyr pipe, first we must designate the
    # object returned in the ifelse() fxn as type symbol (or 'name')
    # NOTE: there is no error output here if the species provided is unavailable
    # because it would be redundant with that provided in filter_species
  which_group <- as.symbol(ifelse(species_name %in% unique(trade_table$GROUP_TS),
                                  'GROUP_TS', 'GROUP_NAME'))
  
  # Because we are using a dplyr pipe in a custom function, using our objects
    # in dplyr functions can become tricky. Effectively, because we need to 
    # define a field (i.e. 'GROUP_TS') with which to index a table inside this 
    # function, we must make this object of type 'quosure' (see rlang)
      # NOTE: later, we call these objects using the bang-bang (!!) in dplyr
      # pipes and functions
  which_group <- rlang::enquo(which_group)
  
  # dplyr pipe to summarize the data grouped by year and species
  summarized_data <- trade_table %>%
    # use the filter_species fxn created above
    filter_species(species_name) %>%
    # select only columns that we need to compare trade data across years
      # NOTE: we coerce which_group to class symbol with sym() to operate in
      # the dplyr pipe
    select(YEAR, !!which_group, EXP_VALUE_2023USD, EXP_VOLUME_KG, 
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
    # group by year and the field specified prior
    group_by(YEAR, !!which_group) %>%
    # sum all columns of numeric type, drop groups
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    # calculate price and convert value to millions and billions, 
      # kg to metric tons
    mutate(EXP_PRICE_USD_PER_KG = EXP_VALUE_2023USD / EXP_VOLUME_KG,
           IMP_PRICE_USD_PER_KG = IMP_VALUE_2023USD / IMP_VOLUME_KG,
           EXP_VALUE_2023USD_MILLIONS = EXP_VALUE_2023USD / 1000000,
           IMP_VALUE_2023USD_MILLIONS = IMP_VALUE_2023USD / 1000000,
           EXP_VALUE_2023USD_BILLIONS = EXP_VALUE_2023USD / 1000000000,
           IMP_VALUE_2023USD_BILLIONS = IMP_VALUE_2023USD / 1000000000,
           EXP_VOLUME_MT = EXP_VOLUME_KG / 1000,
           IMP_VOLUME_MT = IMP_VOLUME_KG / 1000)
  
  return(summarized_data)
}

# Plot function ----------------------------------------------------------------
# Because there are several species that the U.S. imports and exports, it is
  # useful to automate visualizing these trade metrics
# The plot_trade function provided below creates simple visualizations of 
  # export value, volume, price, and trade balances through time of 
  # provided data

# data requires summarized data by year, 
# plot_format requires one of four inputs, and outputs an error if other input
  # is provided
plot_trade <- function(data, plot_format, export = F, import = F) {
  
  # to make calling for balance output easier, give error if both exports and
    # imports are set to T that directs to set each to F
  if (export == T & import == T) {
    stop('Export and import cannot both be true, set one to false. If interested in balance, leave both as false.')
  }
  
  # set terms for export
  if (export == T) {
    # we need two values: a shortform embedded in column names, and a long form
      # that will be used in labels
    shortform <- 'EXP'
    longform <- 'Export'
  }
  
  # set terms for import
  if (import == T) {
    shortform <- 'IMP'
    longform <- 'Import'
  }
  
  # set plot_format to upper case to be flexible for user input
  plot_format <- toupper(plot_format)
  
  # stop function if plot_format is provided incorrectly
  if (!(plot_format %in% c('VALUE', 'VOLUME', 'PRICE', 'BALANCE'))) {
    stop('acceptable plot_format inputs include \"Value\", \"Volume\", \"Price\", and \"Balance\"')
  }
  
  # set vectors and objects for 'value' input
  if (plot_format == 'VALUE') {
    # set column header for value as object of type quosure to operate in ggplot
    y <- as.symbol(paste0(shortform, '_VALUE_2023USD_BILLIONS'))
    y <- rlang::enquo(y)
    # set y-axis tick labels to be of type currency (prefix = $) labeled in bil. 
    label <- label_currency(suffix = 'B')
    # set y-axis label to reflect 'Value' input
    ylab <- paste0('Total ', longform, ' Value (Real 2023 USD)')
  }
  
  # set vectors and objects for 'volume' input
  if (plot_format == 'VOLUME') {
    y <- as.symbol(paste0(shortform, '_VOLUME_MT'))
    y <- rlang::enquo(y)
    # comma adds commas to values in thousands, etc.
    label <- comma
    ylab <- paste0('Total ', longform, ' Volume (Metric Tons)')
  }
  
  # set vectors and objects for 'price' input
  if (plot_format == 'PRICE') {
    y <- as.symbol(paste0(shortform, '_PRICE_USD_PER_KG'))
    y <- rlang::enquo(y)
    label <- label_currency(suffix = '/kg')
    ylab <- paste0('Average ', longform, ' Price (Real 2023 USD)')
  }
  
  # the plots for value, volume, and price are similar enough to use one plot
    # format for each and interchange necessary values, specified above
  if (plot_format %in% c('VALUE', 'VOLUME', 'PRICE')) {
    plot <- 
      ggplot(data = data,
             aes(x = factor(YEAR),
                 y = !!y)) + 
      geom_col(fill = 'black') +
      scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                       limits = factor(2004:2023)) +
      scale_y_continuous(labels = label) +
      labs(x = 'Year',
           y = ylab) +
      theme_bw() +
      theme(axis.text = element_text(size = 10))
  } else {
    # plotting trade balance data requires a different type of plot and 
      # reformatting of the data
    balance_data <- data %>%
      rename(EXPORTS = EXP_VALUE_2023USD_BILLIONS,
             IMPORTS = IMP_VALUE_2023USD_BILLIONS) %>%
      select(YEAR, EXPORTS, IMPORTS) %>%
      # calculate trade balance
      mutate(TRADE_BALANCE = EXPORTS - IMPORTS) %>%
      # pivot longer creates a column of 'names' that we factor to create
        # a plot of several bars for each year (export, import, balances)
      pivot_longer(cols = c(EXPORTS, IMPORTS, TRADE_BALANCE)) %>%
      mutate(name = as.factor(name))
    
    plot <- 
      ggplot(data = balance_data,
             aes(x = factor(YEAR),
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
      scale_y_continuous(labels = label_currency()) +
      geom_hline(yintercept = 0, color = 'black') +
      theme_minimal() +
      theme(legend.position = 'top',
            axis.line.y = element_line(color = 'black'),
            # the placing of the years is finnicky, adjust as needed
            # best to use a standardized plot save output (size-wise) so these
              # can be static
            axis.text.x = element_text(vjust = 13.9, 
                                       angle = 45,
                                       hjust = 5.3),
            plot.background = element_rect(fill = 'white'),
            panel.grid = element_blank())
  }
  
  return(plot)
}


# Salmon (all) -----------------------------------------------------------------
# Format the data
salmon_data <- summarize_yr_spp(trade_data, 'SALMON')

# Plot value
salmon_value_yr <- 
  ggplot(data = salmon_data,
         aes(x = factor(YEAR),
             y = EXP_VALUE_2023USD_BILLIONS)) +
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2023)) +
  scale_y_continuous(labels = label_currency(suffix = 'B')) +
  labs(x = 'Year',
       y = 'Total Export Value (Real 2023 USD)') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))

# Value plot
salmon_value_yr

# Plot volume
salmon_volume_yr <- 
  ggplot(data = salmon_data,
         aes(x = factor(YEAR),
             y = EXP_VOLUME_MT)) +
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2023)) +
  scale_y_continuous(labels = comma) +
  labs(x = 'Year',
       y = 'Total Export Volume (Metric Tons)') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))

# Volume plot
salmon_volume_yr

# Plot price
salmon_price_yr <- 
  ggplot(data = salmon_data,
         aes(x = factor(YEAR),
             y = EXP_PRICE_USD_PER_KG)) +
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2023)) +
  scale_y_continuous(labels = label_currency(suffix = '/kg')) +
  labs(x = 'Year',
       y = 'Average Export Price (Real 2023 USD)') +
  theme_bw() +
  theme(axis.text = element_text(size = 10))

# Price plot
salmon_price_yr
