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
if(!require("ggh4x")) install.packages("ggh4x")
if(!require("RColorBrewer")) install.packages("RColorBrewer")
if(!require("nmfspalette")) install.packages("nmfspalette")

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
  select(YEAR, EXP_VALUE_2024USD, EXP_VOLUME_KG, 
         IMP_VALUE_2024USD, IMP_VOLUME_KG) %>%
  # replace NAs with 0s for summation
  mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD), 0,
                                    EXP_VALUE_2024USD),
         IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD), 0,
                                    IMP_VALUE_2024USD),
         EXP_VOLUME_KG = ifelse(is.na(EXP_VOLUME_KG), 0,
                                EXP_VOLUME_KG),
         IMP_VOLUME_KG = ifelse(is.na(IMP_VOLUME_KG), 0,
                                IMP_VOLUME_KG)) %>%
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  # calculate price and convert value to millions, kg to metric tons
  mutate(EXP_PRICE_USD_PER_KG = EXP_VALUE_2024USD / EXP_VOLUME_KG,
         IMP_PRICE_USD_PER_KG = IMP_VALUE_2024USD / IMP_VOLUME_KG,
         EXP_VALUE_2024USD_MILLIONS = EXP_VALUE_2024USD / 1000000,
         IMP_VALUE_2024USD_MILLIONS = IMP_VALUE_2024USD / 1000000,
         EXP_VALUE_2024USD_BILLIONS = EXP_VALUE_2024USD / 1000000000,
         IMP_VALUE_2024USD_BILLIONS = IMP_VALUE_2024USD / 1000000000,
         EXP_VOLUME_MT = EXP_VOLUME_KG / 1000,
         IMP_VOLUME_MT = IMP_VOLUME_KG / 1000)

# Data formatting (Processed products) -----------------------------------------
# Aggregate processed products by year
pp_data_yr <- pp_data %>%
  select(YEAR, KG, DOLLARS_2024, DOLLARS_2024_PER_KG) %>%
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), sum),
            .groups = 'drop') %>%
  mutate(MT = KG / 1000,
         DOLLARS_2024_BILLIONS = DOLLARS_2024 / 1000000000,
         DOLLARS_2024_PER_KG = DOLLARS_2024 / KG)
  
###################
##### EXPORTS #####
###################
# Comparing Export Value through time (Real 2024 USD) --------------------------
# Make the plot
exp_value_yr <- 
  ggplot(data = trade_data_yr,
         aes(x = factor(YEAR),
             y = EXP_VALUE_2024USD_BILLIONS)) +
    geom_col(fill = 'black') +
    coord_cartesian(ylim = c(4, 8)) +
    scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                     limits = factor(2004:2024)) +
    scale_y_continuous(breaks = seq(4, 8, by = 1),
                       labels = label_currency(suffix = 'B')) +
    labs(x = 'Year',
         y = 'Total Export Value (Real 2024 USD)') +
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

# Comparing Export Price (Real 2024 USD/KG) through time -----------------------
# Make the plot
exp_price_yr <- 
  ggplot(data = trade_data_yr,
         aes(x = factor(YEAR),
             y = EXP_PRICE_USD_PER_KG)) +
  geom_col(fill = 'black') +
  coord_cartesian(ylim = c(3, 4.75)) +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2024)) +
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
# Comparing Import value through time (Real 2024 USD) --------------------------
# Make the plot
imp_value_yr <- 
  ggplot(data = trade_data_yr,
         aes(x = factor(YEAR),
             y = IMP_VALUE_2024USD_BILLIONS)) +
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2024)) +
  scale_y_continuous(labels = label_currency(suffix = 'B')) +
  coord_cartesian(ylim = c(10, 35)) +
  labs(x = 'Year',
       y = 'Total Import Value (Real 2024 USD)') +
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

# Comparing Import price (Real 2024 USD/KG) through time -----------------------
# Make the plot
imp_price_yr <- 
  ggplot(data = trade_data_yr,
         aes(x = factor(YEAR),
             y = IMP_PRICE_USD_PER_KG)) +
  geom_col(fill = 'black') +
  coord_cartesian(ylim = c(6.50, 10.50)) +
  scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                   limits = factor(2004:2024)) +
  scale_y_continuous(labels = label_currency(suffix = '/kg'),
                     breaks = c(6.50, 7, 7.50, 8, 8.50, 9, 9.50, 10, 10.50)) +
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
# Comparing value balance through time (Real 2024 USD) -------------------------
# Format the data
# We need the data formatted such that there are factored groups of imports,
  # exports, and trade balance
# This will involve pivoting the data to be long, using the column headers
  # as a guide for factor names
value_balance_yr_data <- trade_data_yr %>%
  mutate(EXP_VALUE_2024USD_BILLIONS = EXP_VALUE_2024USD_MILLIONS / 1000,
         IMP_VALUE_2024USD_BILLIONS = IMP_VALUE_2024USD_MILLIONS / 1000,
         TRADE_BALANCE = 
           EXP_VALUE_2024USD_BILLIONS - IMP_VALUE_2024USD_BILLIONS) %>%
  select(YEAR, EXP_VALUE_2024USD_BILLIONS, IMP_VALUE_2024USD_BILLIONS,
         TRADE_BALANCE) %>%
  rename(EXPORTS = EXP_VALUE_2024USD_BILLIONS,
         IMPORTS = IMP_VALUE_2024USD_BILLIONS) %>%
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
       y = 'Billions (Real 2024 Dollars)',
       fill = '') +
  scale_fill_discrete(labels = c('Exports',
                                 'Imports',
                                 'Trade Balance')) +
  coord_axes_inside(labels_inside = T) +
  scale_x_discrete(limits = factor(2004:2024)) +
  scale_y_continuous(labels = label_currency(),
                     breaks = seq(-30, 35, by = 5)) +
  geom_hline(yintercept = 0, color = 'black') +
  theme_minimal() +
  theme(legend.position = 'top',
        axis.line.y = element_line(color = 'black'),
        axis.text.x = element_text(hjust = 0.8,
                                   size = 8),
        axis.title.y = element_text(vjust = 13),
        plot.background = element_rect(fill = 'white',
                                       color = 'white'),
        panel.grid = element_blank(),
        plot.margin = margin(5.5, 5.5, 5.5, 50.5, 'points'))

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
  coord_axes_inside(labels_inside = T) +
  scale_x_discrete(limits = factor(2004:2024)) +
  scale_y_continuous(labels = comma,
                     breaks = seq(-2000000, 3500000, by = 500000)) +
  geom_hline(yintercept = 0, color = 'black') +
  theme_minimal() +
  theme(legend.position = 'top',
        axis.line.y = element_line(color = 'black'),
        axis.text.x = element_text(hjust = 0.8,
                                   size = 8),
        axis.title.y = element_text(vjust = 23),
        plot.background = element_rect(fill = 'white',
                                       color = 'white'),
        panel.grid = element_blank(),
        plot.margin = margin(5.5, 5.5, 5.5, 50.5, 'points'))

# View the plot
volume_balance_yr

# Save the plot
save_plot(volume_balance_yr)

# Comparing price (Real 2024 USD/KG) balance through time ----------------------
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
       y = 'Average Price (Real 2024 USD)',
       fill = '') +
  scale_fill_discrete(labels = c('Exports',
                                 'Imports',
                                 'Trade Balance')) +
  scale_x_discrete(limits = factor(2004:2024)) +
  scale_y_continuous(labels = label_currency(suffix = '/kg'),
                     breaks = seq(-6, 10, by = 2)) +
  coord_axes_inside(labels_inside = T) +
  geom_hline(yintercept = 0, color = 'black') +
  theme_minimal() + 
  theme(legend.position = 'top',
        axis.line.y = element_line(color = 'black'),
        axis.text.x = element_text(hjust = 0.8,
                                   size = 8),
        axis.title.y = element_text(vjust = 17),
        plot.background = element_rect(fill = 'white',
                                       color = 'white'),
        panel.grid = element_blank(),
        plot.margin = margin(5.5, 5.5, 5.5, 50.5, 'points'))

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
             y = DOLLARS_2024_BILLIONS)) +
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2023, by = 4),
                   limits = factor(2004:2023)) +
  coord_cartesian(ylim = c(7.5, 17.5)) +
  scale_y_continuous(labels = label_currency(suffix = 'B')) +
  labs(x = 'Year',
       y = 'Total Value (Real 2024 USD)') +
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
  scale_x_discrete(breaks = seq(2004, 2023, by = 4),
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
             y = DOLLARS_2024_PER_KG)) +
  geom_col(fill = 'black') +
  scale_x_discrete(breaks = seq(2004, 2023, by = 4),
                   limits = factor(2004:2023)) +
  scale_y_continuous(labels = label_currency(suffix = '/kg')) +
  coord_cartesian(ylim = c(3, 6)) +
  labs(x = 'Year',
       y = 'Average Price (Real 2024 USD)') +
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
    select(YEAR, !!which_group, EXP_VALUE_2024USD, EXP_VOLUME_KG, 
           IMP_VALUE_2024USD, IMP_VOLUME_KG) %>%
    # replace NAs with 0s for summation
    mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD), 0,
                                      EXP_VALUE_2024USD),
           IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD), 0,
                                      IMP_VALUE_2024USD),
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
    mutate(EXP_PRICE_USD_PER_KG = EXP_VALUE_2024USD / EXP_VOLUME_KG,
           IMP_PRICE_USD_PER_KG = IMP_VALUE_2024USD / IMP_VOLUME_KG,
           EXP_VALUE_2024USD_MILLIONS = EXP_VALUE_2024USD / 1000000,
           IMP_VALUE_2024USD_MILLIONS = IMP_VALUE_2024USD / 1000000,
           EXP_VALUE_2024USD_BILLIONS = EXP_VALUE_2024USD / 1000000000,
           IMP_VALUE_2024USD_BILLIONS = IMP_VALUE_2024USD / 1000000000,
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
    y <- as.symbol(paste0(shortform, '_VALUE_2024USD_BILLIONS'))
    y <- rlang::enquo(y)
    # set y-axis tick labels to be of type currency (prefix = $) labeled in bil. 
    label <- label_currency(suffix = 'B')
    # set y-axis label to reflect 'Value' input
    ylab <- paste0('Total ', longform, ' Value (Real 2024 USD)')
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
    ylab <- paste0('Average ', longform, ' Price (Real 2024 USD)')
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
                       limits = factor(2004:2024)) +
      scale_y_continuous(labels = label) +
      labs(x = 'Year',
           y = ylab) +
      theme_bw() +
      theme(axis.text = element_text(size = 10))
  } else {
    # plotting trade balance data requires a different type of plot and 
      # reformatting of the data
    balance_data <- data %>%
      rename(EXPORTS = EXP_VALUE_2024USD_BILLIONS,
             IMPORTS = IMP_VALUE_2024USD_BILLIONS) %>%
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
           y = 'Billions (Real 2024 USD)',
           fill = '') +
      scale_fill_discrete(labels = c('Exports',
                                     'Imports',
                                     'Trade Balance')) +
      # coord_axes_inside from ggh4x moves the axis tick labels so that they
        # reflect a prototypical coordinate plane (x-axis tick labels on y = 0)
      # this creates an issue where the y-axis title overlaps with y-axis ticks,
        # so we must manually set plot margins to coerce standardized plots and
        # shift the y-axis title to the left
      coord_axes_inside(labels_inside = T) +
      scale_x_discrete(limits = factor(2004:2024)) +
      scale_y_continuous(labels = label_currency()) +
      geom_hline(yintercept = 0, color = 'black') +
      theme_minimal() +
      theme(legend.position = 'top',
            axis.line.y = element_line(color = 'black'),
            axis.text.x = element_text(hjust = 0.8,
                                       size = 8),
            # shift y-axis title to the left
            axis.title.y = element_text(vjust = 14),
            plot.background = element_rect(fill = 'white',
                                           color = 'white'),
            panel.grid = element_blank(),
            # standardize plot size 
            plot.margin = margin(5.5, 5.5, 5.5, 30.5, 'points'))
  }
  
  return(plot)
}


# Salmon (all) -----------------------------------------------------------------
# Format the data
salmon_data <- summarize_yr_spp(trade_data, 'SALMON')

### Exports
# Plot Value
salmon_exp_value_yr <- plot_trade(salmon_data, 'value', export = T)

# View plot
salmon_exp_value_yr

# Save plot
save_plot(salmon_exp_value_yr)

# Plot Volume
salmon_exp_volume_yr <- plot_trade(salmon_data, 'volume', export = T)

# View plot
salmon_exp_volume_yr

# Save plot
save_plot(salmon_exp_volume_yr)

# Plot Price
salmon_exp_price_yr <- plot_trade(salmon_data, 'price', export = T)

# View plot
salmon_exp_price_yr

# Save plot
save_plot(salmon_exp_price_yr)

### Imports
# Plot Value
salmon_imp_value_yr <- plot_trade(salmon_data, 'value', import = T)

# View plot
salmon_imp_value_yr

# Save plot
save_plot(salmon_imp_value_yr)

# Plot Volume
salmon_imp_volume_yr <- plot_trade(salmon_data, 'volume', import = T)

# View plot
salmon_imp_volume_yr

# Save plot
save_plot(salmon_imp_volume_yr)

# Plot Price
salmon_imp_price_yr <- plot_trade(salmon_data, 'price', import = T)

# View plot
salmon_imp_price_yr

# Save plot
save_plot(salmon_imp_price_yr)

### Trade Balance
# Plot Balance
salmon_balance_yr <- plot_trade(salmon_data, 'balance')

# View plot
salmon_balance_yr

# Save plot
save_plot(salmon_balance_yr)

# Tuna (all) -------------------------------------------------------------------
# Format the data
tuna_data <- summarize_yr_spp(trade_data, 'TUNA')

### Exports
# Plot Value
tuna_exp_value_yr <- plot_trade(tuna_data, 'value', export = T)

# View plot
tuna_exp_value_yr

# Save plot
save_plot(tuna_exp_value_yr)

# Plot Volume
tuna_exp_volume_yr <- plot_trade(tuna_data, 'volume', export = T)

# View plot
tuna_exp_volume_yr

# Save plot
save_plot(tuna_exp_volume_yr)

# Plot Price
tuna_exp_price_yr <- plot_trade(tuna_data, 'price', export = T)

# View plot
tuna_exp_price_yr

# Save plot
save_plot(tuna_exp_price_yr)

### Imports
# Plot Value
tuna_imp_value_yr <- plot_trade(tuna_data, 'value', import = T)

# View plot
tuna_imp_value_yr

# Save plot
save_plot(tuna_imp_value_yr)

# Plot Volume
tuna_imp_volume_yr <- plot_trade(tuna_data, 'volume', import = T)

# View plot
tuna_imp_volume_yr

# Save plot
save_plot(tuna_imp_volume_yr)

# Plot Price
tuna_imp_price_yr <- plot_trade(tuna_data, 'price', import = T)

# View plot
tuna_imp_price_yr

# Save plot
save_plot(tuna_imp_price_yr)

### Trade Balance
# Plot Balance
tuna_balance_yr <- plot_trade(tuna_data, 'balance')

# View plot
tuna_balance_yr

# Save plot
save_plot(tuna_balance_yr)

# Cod (all) --------------------------------------------------------------------
# Format the data
cod_data <- summarize_yr_spp(trade_data, 'COD')

### Exports
# Plot Value
cod_exp_value_yr <- plot_trade(cod_data, 'value', export = T)

# View plot
cod_exp_value_yr

# Save plot
save_plot(cod_exp_value_yr)

# Plot Volume
cod_exp_volume_yr <- plot_trade(cod_data, 'volume', export = T)

# View plot
cod_exp_volume_yr

# Save plot
save_plot(cod_exp_volume_yr)

# Plot Price
cod_exp_price_yr <- plot_trade(cod_data, 'price', export = T)

# View plot
cod_exp_price_yr

# Save plot
save_plot(cod_exp_price_yr)

### Imports
# Plot Value
cod_imp_value_yr <- plot_trade(cod_data, 'value', import = T)

# View plot
cod_imp_value_yr

# Save plot
save_plot(cod_imp_value_yr)

# Plot Volume
cod_imp_volume_yr <- plot_trade(cod_data, 'volume', import = T)

# View plot
cod_imp_volume_yr

# Save plot
save_plot(cod_imp_volume_yr)

# Plot Price
cod_imp_price_yr <- plot_trade(cod_data, 'price', import = T)

# View plot
cod_imp_price_yr

# Save plot
save_plot(cod_imp_price_yr)

### Trade Balance
# Plot Balance
cod_balance_yr <- plot_trade(cod_data, 'balance')

# View plot
cod_balance_yr

# Save plot
save_plot(cod_balance_yr)

# Pollock (all) ----------------------------------------------------------------
# Format the data
pollock_data <- summarize_yr_spp(trade_data, 'POLLOCK')

### Exports
# Plot Value
pollock_exp_value_yr <- plot_trade(pollock_data, 'value', export = T)

# View plot
pollock_exp_value_yr

# Save plot
save_plot(pollock_exp_value_yr)

# Plot Volume
pollock_exp_volume_yr <- plot_trade(pollock_data, 'volume', export = T)

# View plot
pollock_exp_volume_yr

# Save plot
save_plot(pollock_exp_volume_yr)

# Plot Price
pollock_exp_price_yr <- plot_trade(pollock_data, 'price', export = T)

# View plot
pollock_exp_price_yr

# Save plot
save_plot(pollock_exp_price_yr)

### Imports
# Plot Value
pollock_imp_value_yr <- plot_trade(pollock_data, 'value', import = T)

# View plot
pollock_imp_value_yr

# Save plot
save_plot(pollock_imp_value_yr)

# Plot Volume
pollock_imp_volume_yr <- plot_trade(pollock_data, 'volume', import = T)

# View plot
pollock_imp_volume_yr

# Save plot
save_plot(pollock_imp_volume_yr)

# Plot Price
pollock_imp_price_yr <- plot_trade(pollock_data, 'price', import = T)

# View plot
pollock_imp_price_yr

# Save plot
save_plot(pollock_imp_price_yr)

### Trade Balance
# Plot Balance
pollock_balance_yr <- plot_trade(pollock_data, 'balance')

# View plot
pollock_balance_yr

# Save plot
save_plot(pollock_balance_yr)


# Shrimp (all) -----------------------------------------------------------------
# Format the data
shrimp_data <- summarize_yr_spp(trade_data, 'SHRIMP')

### Exports
# Plot Value
shrimp_exp_value_yr <- plot_trade(shrimp_data, 'value', export = T)

# View plot
shrimp_exp_value_yr

# Save plot
save_plot(shrimp_exp_value_yr)

# Plot Volume
shrimp_exp_volume_yr <- plot_trade(shrimp_data, 'volume', export = T)

# View plot
shrimp_exp_volume_yr

# Save plot
save_plot(shrimp_exp_volume_yr)

# Plot Price
shrimp_exp_price_yr <- plot_trade(shrimp_data, 'price', export = T)

# View plot
shrimp_exp_price_yr

# Save plot
save_plot(shrimp_exp_price_yr)

### Imports
# Plot Value
shrimp_imp_value_yr <- plot_trade(shrimp_data, 'value', import = T)

# View plot
shrimp_imp_value_yr

# Save plot
save_plot(shrimp_imp_value_yr)

# Plot Volume
shrimp_imp_volume_yr <- plot_trade(shrimp_data, 'volume', import = T)

# View plot
shrimp_imp_volume_yr

# Save plot
save_plot(shrimp_imp_volume_yr)

# Plot Price
shrimp_imp_price_yr <- plot_trade(shrimp_data, 'price', import = T)

# View plot
shrimp_imp_price_yr

# Save plot
save_plot(shrimp_imp_price_yr)

### Trade Balance
# Plot Balance
shrimp_balance_yr <- plot_trade(shrimp_data, 'balance')

# View plot
shrimp_balance_yr

# Save plot
save_plot(shrimp_balance_yr)

# Scallops (all) ---------------------------------------------------------------
# Format the data
scallop_data <- summarize_yr_spp(trade_data, 'SCALLOPS')

### Exports
# Plot Value
scallop_exp_value_yr <- plot_trade(scallop_data, 'value', export = T)

# View plot
scallop_exp_value_yr

# Save plot
save_plot(scallop_exp_value_yr)

# Plot Volume
scallop_exp_volume_yr <- plot_trade(scallop_data, 'volume', export = T)

# View plot
scallop_exp_volume_yr

# Save plot
save_plot(scallop_exp_volume_yr)

# Plot Price
scallop_exp_price_yr <- plot_trade(scallop_data, 'price', export = T)

# View plot
scallop_exp_price_yr

# Save plot
save_plot(scallop_exp_price_yr)

### Imports
# Plot Value
scallop_imp_value_yr <- plot_trade(scallop_data, 'value', import = T)

# View plot
scallop_imp_value_yr

# Save plot
save_plot(scallop_imp_value_yr)

# Plot Volume
scallop_imp_volume_yr <- plot_trade(scallop_data, 'volume', import = T)

# View plot
scallop_imp_volume_yr

# Save plot
save_plot(scallop_imp_volume_yr)

# Plot Price
scallop_imp_price_yr <- plot_trade(scallop_data, 'price', import = T)

# View plot
scallop_imp_price_yr

# Save plot
save_plot(scallop_imp_price_yr)

### Trade Balance
# Plot Balance
scallop_balance_yr <- plot_trade(scallop_data, 'balance')

# View plot
scallop_balance_yr

# Save plot
save_plot(scallop_balance_yr)

# Lobsters (all) ---------------------------------------------------------------
# For now, this in inclusive of ALL lobster imports/exports, not just American
# Format the data
# NOTE: 'LOBSTER' yields an unspecified subset of lobster data
#       'LOBSTERS' includes all lobster trade data
lobster_data <- summarize_yr_spp(trade_data, 'LOBSTERS') 

### Exports
# Plot Value
lobster_exp_value_yr <- plot_trade(lobster_data, 'value', export = T)

# View plot
lobster_exp_value_yr

# Save plot
save_plot(lobster_exp_value_yr)

# Plot Volume
lobster_exp_volume_yr <- plot_trade(lobster_data, 'volume', export = T)

# View plot
lobster_exp_volume_yr

# Save plot
save_plot(lobster_exp_volume_yr)

# Plot Price
lobster_exp_price_yr <- plot_trade(lobster_data, 'price', export = T)

# View plot
lobster_exp_price_yr

# Save plot
save_plot(lobster_exp_price_yr)

### Imports
# Plot Value
lobster_imp_value_yr <- plot_trade(lobster_data, 'value', import = T)

# View plot
lobster_imp_value_yr

# Save plot
save_plot(lobster_imp_value_yr)

# Plot Volume
lobster_imp_volume_yr <- plot_trade(lobster_data, 'volume', import = T)

# View plot
lobster_imp_volume_yr

# Save plot
save_plot(lobster_imp_volume_yr)

# Plot Price
lobster_imp_price_yr <- plot_trade(lobster_data, 'price', import = T)

# View plot
lobster_imp_price_yr

# Save plot
save_plot(lobster_imp_price_yr)

### Trade Balance
# Plot Balance
lobster_balance_yr <- plot_trade(lobster_data, 'balance')

# View plot
lobster_balance_yr

# Save plot
save_plot(lobster_balance_yr)


# Crabs (all) ------------------------------------------------------------------
# Format the data
crab_data <- summarize_yr_spp(trade_data, 'CRABS')

### Exports
# Plot Value
crab_exp_value_yr <- plot_trade(crab_data, 'value', export = T)

# View plot
crab_exp_value_yr

# Save plot
save_plot(crab_exp_value_yr)

# Plot Volume
crab_exp_volume_yr <- plot_trade(crab_data, 'volume', export = T)

# View plot
crab_exp_volume_yr

# Save plot
save_plot(crab_exp_volume_yr)

# Plot Price
crab_exp_price_yr <- plot_trade(crab_data, 'price', export = T)

# View plot
crab_exp_price_yr

# Save plot
save_plot(crab_exp_price_yr)

### Imports
# Plot Value
crab_imp_value_yr <- plot_trade(crab_data, 'value', import = T)

# View plot
crab_imp_value_yr

# Save plot
save_plot(crab_imp_value_yr)

# Plot Volume
crab_imp_volume_yr <- plot_trade(crab_data, 'volume', import = T)

# View plot
crab_imp_volume_yr

# Save plot
save_plot(crab_imp_volume_yr)

# Plot Price
crab_imp_price_yr <- plot_trade(crab_data, 'price', import = T)

# View plot
crab_imp_price_yr

# Save plot
save_plot(crab_imp_price_yr)

### Trade Balance
# Plot Balance
crab_balance_yr <- plot_trade(crab_data, 'balance')

# View plot
crab_balance_yr

# Save plot
save_plot(crab_balance_yr)


#########################################
##### MULTILATERAL LOWE TRADE INDEX #####
#########################################
# Function to calculate MLTI ---------------------------------------------------
calculate_mlti <- function(species, exports = F, imports = F) {
  # This function calculates the Multilateral Lowe Trade Index, which serves to
    # compare year by year changes in trade among top trading nations
    # The nations selected for reference has no effect on comparisons among
    # nations or across years (Fissel et al. 2023)
  # The function inputs are a given species and whether the user seeks the index
    # for exports or imports
  # The function relies on the 'filter_species' function to operate
  
  # First, set value and volume variables to be for exports or imports
  # We use symbols and quosures to embed these terms for use in dplyr
    # see prior functions and rlang for details
  
  if (exports == F & imports == F) {
    stop('Please set either "exports" or "imports" to "T"')
  }
  
  which_value <- as.symbol(ifelse(exports == T, 'EXP_VALUE_2024USD',
                                  'IMP_VALUE_2024USD'))
  which_volume <- as.symbol(ifelse(exports == T, 'EXP_VOLUME_KG',
                                   'IMP_VOLUME_KG'))
  which_value <- rlang::enquo(which_value)
  which_volume <- rlang::enquo(which_volume)
  
  # filter the data for the target species AND for imports or exports
  spp_data <- trade_data %>%
    filter_species(species) %>%
    filter(is.na(!!which_value) == F)
  
  # Because filtering for species depends upon values in two columns,
    # we must specify which column we are to keep for data curation 
  # this does not impact calculations, but simply how the data is presented
  which_group <- as.symbol(ifelse(species %in% unique(spp_data$GROUP_TS),
                                  'GROUP_TS', 'GROUP_NAME'))
  which_group <- rlang::enquo(which_group)
  
  # To calculate the MLTI, we need to calculate the simple average price
  # We must summarize the value and volume of the given species' products
    # within all countries for each year. Then we calculate the price per
    # country per year
  summary_spp_data <- spp_data %>%
    select(YEAR, COUNTRY_NAME, !!which_group, !!which_value,
           !!which_volume) %>%
    group_by(YEAR, COUNTRY_NAME, !!which_group) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    # If, for whatever reason, there is a volume of 0, filter out to preserve
      # calculations (can't have a 0 in denominator)
      # SEE: Netherlands, 2023, Tuna, Imports
    filter(!!which_volume > 0) %>%
    mutate(PRICE = !!which_value / !!which_volume)
  
  # Part of the equation for calculating simple average price involves counting
    # the number of countries and the number of years 
  total_years <- length(unique(summary_spp_data$YEAR))
  total_countries <- length(unique(summary_spp_data$COUNTRY_NAME))
  
  
  # Now we can calculate the simple average price
  # First, sum prices across time for all countries (one value per country)
  # Then sum all of these prices
  average_price <- summary_spp_data %>%
    select(!c(YEAR, !!which_value, !!which_volume)) %>%
    group_by(COUNTRY_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    summarise(across(where(is.numeric), sum)) 
  
  # divide this calculated sum by the product of total years and total countries
  average_price <- average_price$PRICE / (total_years * total_countries)
  
  # We are only interested in the MLTI for the top 9 trading nations for either
    # imports or exports
  # Extract the top 9 trading countries of the most recent year (since we are
    # interested in temporal comparisons for presently relevant nations) by
    # summarizing value within each nation that traded for the most recent year
    # and subsetting for the top 9 summed values
  top9 <- summary_spp_data %>%
    filter(YEAR == 2024) %>%
    group_by(COUNTRY_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    arrange(-!!which_value) %>%
    top_n(9, !!which_value)
  
  # The MLTI calculation requires a base nation to compare other countries 
    # against; it makes sense, therefore, for this base nation to be somewhere
    # in the middle of the distribution.
  # We also want the base nation's value to be of the first year of the dataset
    # so that our comparisons flow chronologically
  # Because we are interested in the top 9 nations, the fifth nation in the list
    # is conveniently the middle nation and will be our base
  base_country <- top9$COUNTRY_NAME[5]
  # in some cases, this selected country may not have been a trading partner
    # in 2004, thus we must check to confirm before proceeding
  # create a list of trading nations in 2004
  trade_nations <- summary_spp_data %>%
    filter(YEAR == 2004) %>%
    select(COUNTRY_NAME) %>%
    distinct() 
  
  # if the selected country was not a trading partner, select the next highest
    # value trading partner as it is more likely they were a trading partner
    # than a lower value trading partner
  if (base_country %in% trade_nations$COUNTRY_NAME) {} else {
    base_country <- top9$COUNTRY_NAME[4]
    # in case this is also not present, take next country
    if (base_country %in% trade_nations$COUNTRY_NAME) {} else {
      base_country <- top9$COUNTRY_NAME[3]
    }
  } 
  
  base_country_q <- summary_spp_data %>%
    filter(YEAR == 2004,
           COUNTRY_NAME == base_country) %>%
    # We choose a base country exclusively to calculate a q_index, which is the
      # foundation for calculating MLTI
    # Q is the product of a nation's trading volume for a given year by the 
      # simple average price for that product by all trading nations
    # In other words, Q standardizes the value of a nations trade such that
      # we compare fluctuations in VOLUME over time 
    mutate(Q_INDEX = !!which_volume * average_price)
  
  # All calculations of MLTI for each country in each year will be a ratio
    # of the given country's given year's volume of trading to the index base
  index_base <- base_country_q$Q_INDEX
  
  # calculate MLTI for each top 9 country for all years
  # the output will be a dataframe of the MLTI value for the top 9 countries
    # from 2004-2024
  mlti_data <- summary_spp_data %>%
    filter(COUNTRY_NAME %in% top9$COUNTRY_NAME) %>%
    mutate(Q_INDEX = !!which_volume * average_price) %>%
    select(YEAR, COUNTRY_NAME, Q_INDEX) %>%
    mutate(MLTI = Q_INDEX / index_base)
  
  return(mlti_data)
}

# Function to plot MLTI --------------------------------------------------------
plot_mlti <- function(mlti_data, exports = F, imports = F) {
  # this function creates facet wrapped plots of MLTI indices generated by the
    # calculate_mlti function described above
  # the operation of this function is simple and does not require much 
    # explanation
  if (exports == F & imports == F) {
    stop('Please set "exports" or "imports" to "T"')
  }
  
  label <- ifelse(exports == T, 'Export', 'Import')
  
  ggplot(data = mlti_data,
         aes(x = factor(YEAR),
             y = MLTI)) +
    geom_point() +
    facet_wrap( ~ factor(COUNTRY_NAME), nrow = 3) +
    scale_x_discrete(breaks = seq(2006, 2022, by = 4)) +
    geom_hline(yintercept = 1, color = 'black') +
    labs(x = '',
         y = paste0('Multilateral ', label, ' Quantity Index')) +
    theme_bw() +
    theme(axis.text = element_text(size = 15),
          axis.title.y = element_text(size = 20),
          strip.text = element_text(size = 15,
                                    color = 'white'),
          strip.background = element_rect(fill = 'black'))
}

# Function to save MLTI plots --------------------------------------------------
save_mlti_plot <- function(plot) {
  ggsave(filename = paste0('Plots/', as.character(substitute(plot)), '.png'),
         plot = plot,
         width = 12,
         height = 11)
}

# Salmon (all) -----------------------------------------------------------------
### Exports
# Make the data
salmon_export_mlti_data <- calculate_mlti('SALMON', exports = T)

# Plot the data
salmon_export_mlti <- plot_mlti(salmon_export_mlti_data, exports = T)

# View the plot
salmon_export_mlti


### Imports
# Make the data
salmon_import_mlti_data <- calculate_mlti('SALMON', imports = T)

# Plot the data
salmon_import_mlti <- plot_mlti(salmon_import_mlti_data, imports = T)

# View the plot
salmon_import_mlti

# Tuna (all) -------------------------------------------------------------------
### Exports
# Make the data
tuna_export_mlti_data <- calculate_mlti('TUNA', exports = T)

# Plot the data
tuna_export_mlti <- plot_mlti(tuna_export_mlti_data, exports = T)

# View the plot
tuna_export_mlti


### Imports
# Make the data
tuna_import_mlti_data <- calculate_mlti('TUNA', imports = T)

# Plot the data
tuna_import_mlti <- plot_mlti(tuna_import_mlti_data, imports = T)

# View the plot
tuna_import_mlti

# Cod (all) --------------------------------------------------------------------
### Exports
# Make the data
cod_export_mlti_data <- calculate_mlti('COD', exports = T)

# Plot the data
cod_export_mlti <- plot_mlti(cod_export_mlti_data, exports = T)

# View the plot
cod_export_mlti


### Imports
# Make the data
cod_import_mlti_data <- calculate_mlti('COD', imports = T)

# Plot the data
cod_import_mlti <- plot_mlti(cod_import_mlti_data, imports = T)

# View the plot
cod_import_mlti

# Pollock (all) ----------------------------------------------------------------
### Exports
# Make the data
pollock_export_mlti_data <- calculate_mlti('POLLOCK', exports = T)

# Plot the data
pollock_export_mlti <- plot_mlti(pollock_export_mlti_data, exports = T)

# View the plot
pollock_export_mlti


### Imports
# Make the data
pollock_import_mlti_data <- calculate_mlti('POLLOCK', imports = T)

# Plot the data
pollock_import_mlti <- plot_mlti(pollock_import_mlti_data, imports = T)

# View the plot
pollock_import_mlti

# Shrimp (all) -----------------------------------------------------------------
### Exports
# Make the data
shrimp_export_mlti_data <- calculate_mlti('SHRIMP', exports = T)

# Plot the data
shrimp_export_mlti <- plot_mlti(shrimp_export_mlti_data, exports = T)

# View the plot
shrimp_export_mlti


### Imports
# Make the data
shrimp_import_mlti_data <- calculate_mlti('SHRIMP', imports = T)

# Plot the data
shrimp_import_mlti <- plot_mlti(shrimp_import_mlti_data, imports = T)

# View the plot
shrimp_import_mlti

# Scallops (all) ---------------------------------------------------------------
### Exports
# Make the data
scallops_export_mlti_data <- calculate_mlti('SCALLOPS', exports = T)

# Plot the data
scallops_export_mlti <- plot_mlti(scallops_export_mlti_data, exports = T)

# View the plot
scallops_export_mlti


### Imports
# Make the data
scallops_import_mlti_data <- calculate_mlti('SCALLOPS', imports = T)

# Plot the data
scallops_import_mlti <- plot_mlti(scallops_import_mlti_data, imports = T)

# View the plot
scallops_import_mlti

# Lobsters (all) ---------------------------------------------------------------
### Exports
# Make the data
lobsters_export_mlti_data <- calculate_mlti('LOBSTERS', exports = T)

# Plot the data
lobsters_export_mlti <- plot_mlti(lobsters_export_mlti_data, exports = T)

# View the plot
lobsters_export_mlti


### Imports
# Make the data
lobsters_import_mlti_data <- calculate_mlti('LOBSTERS', imports = T)

# Plot the data
lobsters_import_mlti <- plot_mlti(lobsters_import_mlti_data, imports = T)

# View the plot
lobsters_import_mlti

# Crabs (all) ------------------------------------------------------------------
### Exports
# Make the data
crabs_export_mlti_data <- calculate_mlti('CRABS', exports = T)

# Plot the data
crabs_export_mlti <- plot_mlti(crabs_export_mlti_data, exports = T)

# View the plot
crabs_export_mlti


### Imports
# Make the data
crabs_import_mlti_data <- calculate_mlti('CRABS', imports = T)

# Plot the data
crabs_import_mlti <- plot_mlti(crabs_import_mlti_data, imports = T)

# View the plot
crabs_import_mlti

############################
##### HERFINDAHL INDEX #####
############################
# To calculate the herfindahl index, we must calculate the share of both import
  # and export value for each trading country with the U.S.
# So, for example, if Germany traded $50M of scallops in value with the U.S., 
  # and Canada traded $50M of scallops, and no one else traded scallops, 
  # each country's share of trade value is 50%. The Index takes the square of 
  # this share for each country, sums the squares, and divides by 10,000 all for
  # a given year. 
# Function to calculate HI -----------------------------------------------------
calculate_hi <- function(species) {
  # the function is very simple in execution as it is mainly simple calculations
  # To make plotting the data easier (so that exports could be compared to 
    # imports), the function outputs both export and import data in one df
  # This makes the only necessary input the species of interest
  
  hi_data <- trade_data %>%
    # relies on filter_species function created above
    filter_species(species) %>%
    # retain only year, country, and value fields 
    select(YEAR, COUNTRY_NAME, EXP_VALUE_2024USD, IMP_VALUE_2024USD) %>%
    # we cannot sum properly if there are NAs, so coerce NAs to be 0
    mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD) == T,
                                      0, EXP_VALUE_2024USD),
           IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD) == T,
                                      0, IMP_VALUE_2024USD)) %>%
    # the Index relies on share of value for each trading nation, so we must
      # group by year and the trading country
    group_by(YEAR, COUNTRY_NAME) %>%
    # sums both import and export values for each country in each year
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    # now we don't need the country specification anymore for these calculations
      # so only group by year
    group_by(YEAR) %>%
    # the following mutations are sequential calculations necessary to calculate
      # HI; we only retain the final produced columns EXP_HI and IMP_HI
    # Step 1: Sum export and import values of all nations in each year
    mutate(TOTAL_EXP_VALUE_YR = sum(EXP_VALUE_2024USD),
           TOTAL_IMP_VALUE_YR = sum(IMP_VALUE_2024USD),
           # Step 2: Calculate each nation's share of trade value in each year
           PROPORT_EXP_VALUE = EXP_VALUE_2024USD / TOTAL_EXP_VALUE_YR,
           PROPORT_IMP_VALUE = IMP_VALUE_2024USD / TOTAL_IMP_VALUE_YR,
           # Step 3: Square the proportions generated in step 2
           PROPORT_EXP_SQUARED = PROPORT_EXP_VALUE^2,
           PROPORT_IMP_SQUARED = PROPORT_IMP_VALUE^2,
           # Step 4: sum the squared proportions for each year so that there is
            # one value per year
           EXP_HI = sum(PROPORT_EXP_SQUARED),
           IMP_HI = sum(PROPORT_IMP_SQUARED)) %>%
    select(YEAR, EXP_HI, IMP_HI) %>%
    # retains the one value per year
    distinct()
  
}

# Function to plot HI ----------------------------------------------------------
plot_hi <- function(hi_data) {
  # the function operates similarly to other plot functions created here
  # the input is the data created by the calculate_hi function
  
  # To plot both exports and imports for comparison of HI, we must format
    # the provided data by pivoting it longer
  format_hi_data <- hi_data %>%
    # rename the columns so that the labels created in pivot_longer are correct
    rename(EXPORTS = EXP_HI,
           IMPORTS = IMP_HI) %>%
    pivot_longer(cols = c(EXPORTS, IMPORTS))
  # the product of pivot_longer is 3 columns
    # YEAR
    # name, or the labels 'EXPORTS' and 'IMPORTS'
    # value, or the HI values assigned to either 'EXPORTS' or 'IMPORTS'
  
  ggplot(data = format_hi_data,
         aes(x = as.factor(YEAR),
             y = value)) +
    geom_line(aes(group = name, 
                  colour = name),
              linewidth = 1.5) +
    # add points for clarity in plot
    geom_point(size = 2,
               color = 'black') +
    # rename terms for plot cleanliness
    scale_color_discrete(name = '',
                         labels = c('Exports', 'Imports')) +
    labs(x = '',
         y = 'Herfindahl Index (HI)',
         # title the plot with the species presented, scrub the info from the
          # name of the data provided to reduce function inputs
         title = toupper(as.character(gsub('_hi_data', 
                                           '', 
                                           substitute(hi_data))))) +
    scale_x_discrete(breaks = seq(2004, 2024, by = 4)) +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.position = 'bottom',
          plot.title = element_text(size = 18))
  
}

# Salmon (all) -----------------------------------------------------------------
# Make the data
salmon_hi_data <- calculate_hi('SALMON')

# Plot the data
salmon_hi <- plot_hi(salmon_hi_data)

# View the plot
salmon_hi


# Tuna (all) -------------------------------------------------------------------
# Make the data
tuna_hi_data <- calculate_hi('TUNA')

# Plot the data
tuna_hi <- plot_hi(tuna_hi_data)

# View the plot
tuna_hi

# Cod (all) --------------------------------------------------------------------
# Make the data
cod_hi_data <- calculate_hi('COD')

# Plot the data
cod_hi <- plot_hi(cod_hi_data)

# View the plot
cod_hi

# Pollock (all) ----------------------------------------------------------------
# Make the data
pollock_hi_data <- calculate_hi('POLLOCK')

# Plot the data
pollock_hi <- plot_hi(pollock_hi_data)

# View the plot
pollock_hi

# Shrimp (all) -----------------------------------------------------------------
# Make the data
shrimp_hi_data <- calculate_hi('SHRIMP')

# Plot the data
shrimp_hi <- plot_hi(shrimp_hi_data)

# View the plot
shrimp_hi

# Scallops (all) ---------------------------------------------------------------
# Make the data
scallop_hi_data <- calculate_hi('SCALLOPS')

# Plot the data
scallop_hi <- plot_hi(scallop_hi_data)

# View the plot
scallop_hi

# Lobsters (all) ---------------------------------------------------------------
# Make the data
lobster_hi_data <- calculate_hi('LOBSTERS')

# Plot the data
lobster_hi <- plot_hi(lobster_hi_data)

# View the plot
lobster_hi

# Crabs (all) ------------------------------------------------------------------
# Make the data
crab_hi_data <- calculate_hi('CRABS')

# Plot the data
crab_hi <- plot_hi(crab_hi_data)

# View the plot
crab_hi

#############################
##### PRODUCTION VOLUME #####
#############################
# Data filtering function ------------------------------------------------------
# The function's purpose is to simply summarize data from FOSS's Processed 
  # Products dataset by year
summarize_pp_yr_spp <- function(product_data, species) {
  # product_data is FOSS Processed Products data formatted in script 2
  # species is a string for the species of interest
  product_data %>%
    # because processed product 'SPECIES' can be specific, only extract rows
      # whose SPECIES string includes that specified in 'species' call
    filter(str_detect(SPECIES, species)) %>%
    # retain only columns of interest
    select(YEAR, PRODUCT_NAME, KG, DOLLARS_2024) %>%
    group_by(YEAR, PRODUCT_NAME) %>%
    # sum volume and value by product condition (PRODUCT_NAME) through time
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    # format value and volume to a higher magnitude value 
    mutate(MT = KG / 1000,
           BILLIONS_2024USD = DOLLARS_2024 / 1000000000)
}

# Set Colors for Plot ----------------------------------------------------------
# use colors provided by packages 'nmfspalettes'
colors <- c(nmfs_palettes[['coral']][6:3], 
            nmfs_palettes[['waves']][6:2], 
            nmfs_palettes[['crustacean']][c(6, 4, 2)],
            nmfs_colors[42:39])

# the names specified below will track to PRODUCT_NAME in data
# They are organized here in the same order and line as 'colors' above
names(colors) <- levels(factor(levels = c(
  'FILLETS', 'STEAKS', 'SURIMI', 'SHUCKED MEATS',
  'CANNED', 'OIL', 'DRESSED', 'SMOKED (EXCL. CANNED)', 'CHOWDERS',
  'FISH STICKS', 'BREADED SHRIMP', 'CAKES/PATTIES',
  'OTHER*', 'OTHER INDUSTRIAL', 'MEAL', 'FISH PORTIONS')))

# Plot function ----------------------------------------------------------------
# the function operates in parts
  # first: identify product conditions that are rare 
    # the purpose here is to condense them into the already present condition
      # 'other' (which we relabel with an asterisk) to reduce clutter in plot
  # second: apply this nomenclature change to rare product conditions in a 
    # new dataset 
  # third: calculate aggregate volume processed per year per species so that
    # we can have species-specific ylimits embedded in the function
  # fourth: create the plot
plot_spp_pp <- function(processed_product_data, species) {
  # processed_product_data is the dataset created by summarize_pp_yr_spp
  # species is a string of the species we are visualizing
  
  low_prop_types <- processed_product_data %>%
    select(MT, PRODUCT_NAME) %>%
    group_by(PRODUCT_NAME) %>%
    # aggregate the total volume per product condition
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    # calculate the proportion of total volume for each product condition
    mutate(TOTAL_VOLUME = sum(MT),
           VOLUME_SHARE = MT / TOTAL_VOLUME) %>%
    # retain production conditions who occupy less than 2% of total production
    filter(VOLUME_SHARE < 0.02) %>%
    # pull() returns the values as a vector
    pull(PRODUCT_NAME)
  
  new_data <- processed_product_data %>%
    # rename low proportion conditions and 'OTHER' as 'OTHER*'
    # the * denotes that these are special conditions
    mutate(PRODUCT_NAME = ifelse(PRODUCT_NAME %in% c('OTHER', low_prop_types),
                                 'OTHER*', PRODUCT_NAME)) %>%
    # set PRODUCT_NAME as a factor for the plot
    # convert to thousand metric tons 
    mutate(PRODUCT_NAME = factor(PRODUCT_NAME),
           THOUSAND_MT = MT / 1000)
  
  # find max production volume for any given year
  yr_volume <- new_data %>%
    select(YEAR, THOUSAND_MT) %>%
    group_by(YEAR) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') 
  
  plot <- ggplot(data = new_data,
                 aes(x = factor(YEAR),
                     y = THOUSAND_MT,
                     fill = PRODUCT_NAME)) +
    geom_col(position = 'stack') +
    scale_fill_manual(values = colors,
                      name = 'Product Condition') +
    labs(x = '',
         y = 'Volume (Thousand Metric Tons)',
         fill = 'Product Condition',
         title = paste0('Domestic Processed ', species, ' Products')) +
    # set the max y-limit as 10 more than the greatest volume per year
    scale_y_continuous(limits = c(0, max(yr_volume$THOUSAND_MT) + 10), 
                       expand = c(0, 0)) +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 15))
  
}

# Salmon (all) -----------------------------------------------------------------
# Make the data
salmon_pp_data <- summarize_pp_yr_spp(pp_data, 'SALMON')

# Plot the data
salmon_pp <- plot_spp_pp(salmon_pp_data, 'Salmon')

# View the plot
salmon_pp

# Tuna (all) -------------------------------------------------------------------
# Make the data
tuna_pp_data <- summarize_pp_yr_spp(pp_data, 'TUNA')

# Plot the data
tuna_pp <- plot_spp_pp(tuna_pp_data, 'Tuna')

# View the plot
tuna_pp

# Cod (all) --------------------------------------------------------------------
# Make the data
cod_pp_data <- summarize_pp_yr_spp(pp_data, 'COD')

# Plot the data
cod_pp <- plot_spp_pp(cod_pp_data, 'Cod')

# View the plot
cod_pp

# Pollock (all) ----------------------------------------------------------------
# Make the data
pollock_pp_data <- summarize_pp_yr_spp(pp_data, 'POLLOCK')

# Plot the data
pollock_pp <- plot_spp_pp(pollock_pp_data, 'Pollock')

# View the plot
pollock_pp

# Shrimp (all) -----------------------------------------------------------------
# Make the data
shrimp_pp_data <- summarize_pp_yr_spp(pp_data, 'SHRIMP')

# Plot the data
shrimp_pp <- plot_spp_pp(shrimp_pp_data, 'Shrimp')

# View the plot
shrimp_pp

# Scallops (all) ---------------------------------------------------------------
# Make the data
scallop_pp_data <- summarize_pp_yr_spp(pp_data, 'SCALLOPS')

# Plot the data
scallop_pp <- plot_spp_pp(scallop_pp_data, 'Scallop')

# View the plot
scallop_pp

# Lobsters (all) ---------------------------------------------------------------
# Make the data
lobster_pp_data <- summarize_pp_yr_spp(pp_data, 'LOBSTER')

# Plot the data
lobster_pp <- plot_spp_pp(lobster_pp_data, 'Lobster')

# View the plot
lobster_pp

# Crabs (all) ------------------------------------------------------------------
# Make the data
crab_pp_data <- summarize_pp_yr_spp(pp_data, 'CRAB')

# Plot the data
crab_pp <- plot_spp_pp(crab_pp_data, 'Crab')

# View the plot
crab_pp

# TODOS ------------------------------------------------------------------------
# TODO: Export/Import Volume Ratio
# TODO: Net Exports
# TODO: Net exports to top 5 countries
# TODO: Real Export Effective Exchange Rate Index (foreign currency per dollar)
  # IMPORTANT: Cannot achieve since dataset available in Fissel et al. 2023 is
  # no longer available online
# TODO: US Share of global trade value activity
# TODO: Export and import value growth of the US and rest of the world
# TODO: US Apparent Consumption
# TODO: Apparent consumption relative to US production
# TODO: Unexported US production relative to apparent consumption
