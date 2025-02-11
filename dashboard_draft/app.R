# Developing a proof-of-concept dashboard for U.S. seafood trade metrics
# Contact: Cameron Van Horn
#          cameron.vanhorn@noaa.gov


library(shiny)
library(bslib)
library(tidyverse)
library(scales)
library(ggh4x)

# Pull Data 
load('seafood_trade_data_munge_01_31_25.RData')

# Store functions
filter_species <- function(trade_table, species_name) {

  group_name <- unique(trade_table$GROUP_NAME)
  group_ts <- unique(trade_table$GROUP_TS)
  
  species_name <- toupper(species_name)
  
  use_group_name <- ifelse(species_name %in% group_name, 'Y', 'N')
  use_group_ts <- ifelse(species_name %in% group_ts, 'Y', 'N')
  
  if (use_group_name == 'N' & use_group_ts == 'N') {
    stop("The species you provided is either too specific or not available.
         Try 'unique(your_data$your_column)' to find acceptable calls")
  }
  
  if (use_group_ts == 'Y') {
    filtered_data <- trade_table %>%
      filter(GROUP_TS == species_name)
  } 
  
  else if (use_group_ts == 'N' & use_group_name == 'Y') {
    filtered_data <- trade_table %>%
      filter(GROUP_NAME == species_name)
  }
  
  return(filtered_data)
}
summarize_yr_spp <- function(trade_table, species_name) {
  if (species_name == '') {
    return(NULL)
    stop()
  }
  species_name <- toupper(species_name)

  which_group <- as.symbol(ifelse(species_name %in% unique(trade_table$GROUP_TS),
                                  'GROUP_TS', 'GROUP_NAME'))
  
  which_group <- rlang::enquo(which_group)
  
  summarized_data <- trade_table %>%
    filter_species(species_name) %>%
    select(YEAR, !!which_group, EXP_VALUE_2024USD, EXP_VOLUME_KG, 
           IMP_VALUE_2024USD, IMP_VOLUME_KG) %>%
    mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD), 0,
                                      EXP_VALUE_2024USD),
           IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD), 0,
                                      IMP_VALUE_2024USD),
           EXP_VOLUME_KG = ifelse(is.na(EXP_VOLUME_KG), 0,
                                  EXP_VOLUME_KG),
           IMP_VOLUME_KG = ifelse(is.na(IMP_VOLUME_KG), 0,
                                  IMP_VOLUME_KG)) %>%
    group_by(YEAR, !!which_group) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
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
plot_trade <- function(data, plot_format, export = F, import = F) {
  if (is.null(data)) {
    stop()
  }
  if (export == T & import == T) {
    stop('Export and import cannot both be true, set one to false. If interested in balance, leave both as false.')
  }
  
  if (export == T) {
    shortform <- 'EXP'
    longform <- 'Export'
  }
  
  if (import == T) {
    shortform <- 'IMP'
    longform <- 'Import'
  }
  
  plot_format <- toupper(plot_format)
  
  if (plot_format == 'VALUE') {
    y <- as.symbol(paste0(shortform, '_VALUE_2024USD_BILLIONS'))
    y <- rlang::enquo(y)
    label <- label_currency(suffix = 'B')
    ylab <- paste0('Total ', longform, ' Value (Real 2024 USD)')
  }
  
  if (plot_format == 'VOLUME') {
    y <- as.symbol(paste0(shortform, '_VOLUME_MT'))
    y <- rlang::enquo(y)
    label <- comma
    ylab <- paste0('Total ', longform, ' Volume (Metric Tons)')
  }
  
  if (plot_format == 'PRICE') {
    y <- as.symbol(paste0(shortform, '_PRICE_USD_PER_KG'))
    y <- rlang::enquo(y)
    label <- label_currency(suffix = '/kg')
    ylab <- paste0('Average ', longform, ' Price (Real 2024 USD)')
  }
  
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
    balance_data <- data %>%
      rename(EXPORTS = EXP_VALUE_2024USD_BILLIONS,
             IMPORTS = IMP_VALUE_2024USD_BILLIONS) %>%
      select(YEAR, EXPORTS, IMPORTS) %>%
      mutate(TRADE_BALANCE = EXPORTS - IMPORTS) %>%
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
      coord_axes_inside(labels_inside = T) +
      scale_x_discrete(limits = factor(2004:2024)) +
      scale_y_continuous(labels = label_currency()) +
      geom_hline(yintercept = 0, color = 'black') +
      theme_minimal() +
      theme(legend.position = 'top',
            legend.text = element_text(size = 15),
            axis.line.y = element_line(color = 'black'),
            axis.text.x = element_text(hjust = 0.8,
                                       size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(vjust = 14,
                                        size = 15),
            plot.background = element_rect(fill = 'white',
                                           color = 'white'),
            panel.grid = element_blank(),
            plot.margin = margin(5.5, 5.5, 5.5, 40.5, 'points'))
  }
  
  return(plot)
}

# Define UI 
ui <- page_sidebar(
  sidebar = sidebar(
    selectInput(
      'species',
      label = 'Select species group',
      choices = c('', 'Cod', 'Crabs', 'Lobsters', 'Pollock', 
                  'Salmon', 'Scallops', 'Shrimp', 'Tuna'))
    ),
  fluidRow(
    plotOutput('balance')
  ),
  fluidRow(
    column(6,
           plotOutput('exp_volume')),
    column(6,
           plotOutput('imp_volume'))
  )
  )

# Define server logic 
server <- function(input, output, session) {
  
  output$balance <- renderPlot({
    plot_trade(summarize_yr_spp(trade_data, input$species), 'BALANCE')
  })
  
  output$exp_volume <- renderPlot({
    plot_trade(summarize_yr_spp(trade_data, input$species), 'VOLUME', 
               export = T)
  })
  
  output$imp_volume <- renderPlot({
    plot_trade(summarize_yr_spp(trade_data, input$species), 'VOLUME',
               import = T)
  })

}

# Run the app
shinyApp(ui = ui, server = server)