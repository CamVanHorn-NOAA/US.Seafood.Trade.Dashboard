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
filter_species <- function(data, species) {
  species_names <- unique(data$SPECIES_NAME)
  species_groups <- unique(data$SPECIES_GROUP)
  species_categories <- unique(data$SPECIES_CATEGORY)
  ecology_categories <- unique(data$ECOLOGICAL_CATEGORY)
  
  species <- toupper(species)
  
  locate_group <- 
    ifelse(species %in% ecology_categories, 
           'ECOLOGICAL_CATEGORY',
           ifelse(species %in% species_categories, 
                  'SPECIES_CATEGORY',
                  ifelse(species %in% species_groups, 
                         'SPECIES_GROUP',
                         ifelse(species %in% species_names, 
                                'SPECIES_NAME',
                                'UNAVAILABLE'))))
  
  if (locate_group == 'UNAVAILABLE') {
    stop("The species you provided is either too specific or not available.
         Try 'unique(your_data$your_column) to find acceptable calls.")
  } 
  
  group <- as.symbol(locate_group)
  group <- rlang::enquo(group)
  
  new_data <- data %>%
    filter(!!group == species)
  
  return(new_data)
  
}
summarize_trade_yr_spp <- function(trade_table, species) {
  
  species <- toupper(species)
  
  which_group <- as.symbol(
    ifelse(species %in% unique(trade_table$ECOLOGICAL_CATEGORY), 
           'ECOLOGICAL_CATEGORY',
           ifelse(species %in% unique(trade_table$SPECIES_CATEGORY), 
                  'SPECIES_CATEGORY',
                  ifelse(species %in% unique(trade_table$SPECIES_GROUP), 
                         'SPECIES_GROUP',
                         'SPECIES_NAME')))
  )
  
  group <- rlang::enquo(which_group)
  
  summarized_data <- trade_table %>%
    filter_species(species) %>%
    select(YEAR, !!group, EXP_VALUE_2024USD, EXP_VOLUME_KG, 
           IMP_VALUE_2024USD, IMP_VOLUME_KG) %>%
    mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD), 0,
                                      EXP_VALUE_2024USD),
           IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD), 0,
                                      IMP_VALUE_2024USD),
           EXP_VOLUME_KG = ifelse(is.na(EXP_VOLUME_KG), 0,
                                  EXP_VOLUME_KG),
           IMP_VOLUME_KG = ifelse(is.na(IMP_VOLUME_KG), 0,
                                  IMP_VOLUME_KG)) %>%
    group_by(YEAR, !!group) %>%
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
  
  if (export == T & import == T) {
    data <- data %>%
      mutate(NET_VALUE_2024USD_BILLIONS = 
               EXP_VALUE_2024USD_BILLIONS - IMP_VALUE_2024USD_BILLIONS,
             NET_VOLUME_MT = EXP_VOLUME_MT - IMP_VOLUME_MT,
             NET_PRICE = EXP_PRICE_USD_PER_KG - IMP_PRICE_USD_PER_KG)
    
    shortform <- 'NET'
    longform <- 'Net Export'
  }
  
  if (export == T & import == F) {
    shortform <- 'EXP'
    longform <- 'Export'
  }
  
  if (import == T & export == F) {
    shortform <- 'IMP'
    longform <- 'Import'
  }
  
  plot_format <- toupper(plot_format)
  
  if (!(plot_format %in% c('VALUE', 'VOLUME', 'PRICE', 'BALANCE', 'RATIO'))) {
    stop('acceptable plot_format inputs include \"Value\", \"Volume\", \"Price\",  \"Balance\", and \"Ratio\"')
  }
  
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
  } else if (plot_format == 'RATIO') {
    data$GROUP <- 'group'
    
    plot <- 
      ggplot(data = data, 
             aes(x = factor(YEAR),
                 y = (EXP_VOLUME_MT / IMP_VOLUME_MT))) +
      geom_line(aes(group = GROUP),
                color = 'black',
                linewidth = 1.5) +
      geom_point(color = 'black',
                 size = 2) +
      scale_x_discrete(breaks = seq(2004, 2024, by = 4),
                       limits = factor(2004:2024)) +
      labs(x = '', 
           y = 'Export / Import Volume Ratio') +
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
            axis.line.y = element_line(color = 'black'),
            axis.text.x = element_text(hjust = 0.8,
                                       size = 8),
            axis.title.y = element_text(vjust = 14),
            plot.background = element_rect(fill = 'white',
                                           color = 'white'),
            panel.grid = element_blank(),
            plot.margin = margin(5.5, 5.5, 5.5, 30.5, 'points'))
  }
  
  return(plot)
}

# Define UI 
ui <- fluidPage(
  selectInput(
             'species',
             label = 'Select species group',
             choices = c('', 'Cod', 'Crabs', 'Lobsters', 'Pollock', 
                         'Salmon', 'Scallops', 'Shrimp', 'Tuna')
           ),
  tabsetPanel(
             tabPanel('Trade Balance (Value)',
                      plotOutput('balance')
                      ),
             tabPanel('Trade Volume',
                      fluidRow(
                        plotOutput('exp_volume')
                      ),
                      fluidRow(
                        plotOutput('imp_volume')
                      )),
             )
           )

# Define server logic 
server <- function(input, output, session) {
  
  output$balance <- renderPlot({
    req(input$species != '')
    plot_trade(summarize_yr_spp(trade_data, input$species), 'BALANCE')
  })
  
  output$exp_volume <- renderPlot({
    req(input$species != '')
    plot_trade(summarize_yr_spp(trade_data, input$species), 'VOLUME', 
               export = T)
  })
  
  output$imp_volume <- renderPlot({
    req(input$species != '')
    plot_trade(summarize_yr_spp(trade_data, input$species), 'VOLUME',
               import = T)
  })

}

# Run the app
shinyApp(ui = ui, server = server)