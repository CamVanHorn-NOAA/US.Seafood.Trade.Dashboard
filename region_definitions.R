#########################
#' *REGION DEFINITIONS* #
#########################
# Definitions for regional attributions to trade, landings, and processed
  # products data used in the NOAA Fisheries U.S. Seafood Dashboard
  # (https://github.com/CamVanHorn-NOAA/US.Seafood.Trade.Dashboard)

# These regional assignments derive from the FEUS deifnitions found here
  # https://s3.amazonaws.com/media.fisheries.noaa.gov/2024-11/FEUS-2022-SPO248B.pdf

# North Pacific
norpac <- c('AK', 'ALASKA')

# Pacific Coast
pac <- c('CA', 'CALIFORNIA', 'OR', 'OREGON', 'WA', 'WASHINGTON')

# Pacific Islands
pacisl <- c('HI', 'HAWAII', 'AS', 'CM', 'MP', 'GU')

# New England
neweng <- c('CT', 'CONNECTICUT', 'ME', 'MAINE', 'MA', 'MASSACHUSETTS', 'NH', 
            'NEW HAMPSHIRE', 'RI', 'RHODE ISLAND')

# Mid-Atlantic
midatl <- c('DE', 'DELAWARE', 'MD', 'MARYLAND', 'NJ', 'NEW JERSEY', 'NY',
            'NEW YORK', 'VA', 'VIRGINIA', 'PA', 'PENNSYLVANIA', 'DC')

# South Atlantic
souatl <- c('GA', 'GEORGIA', 'NC', 'NORTH CAROLINA', 'SC', 'SOUTH CAROLINA',
            'FL-E', 'FLORIDA-EAST', 'FLORIDA', 'PR', 'PUERTO RICO', 'VI', 'U.S. VIRGIN ISLANDS')

# Gulf Region
gulf <- c('AL', 'ALABAMA', 'LA', 'LOUISIANA', 'MS', 'MISSISSIPPI', 'TX', 'TEXAS',
          'FL-W', 'FLORIDA-WEST')

# We are adding a Great Lakes region that is city-based, not state-based like
# the FEUS. State exceptions include OH, MI, MN and WI, which are considered great
# lake states
grlake <- c('OH', 'OHIO', 'MI', 'MICHIGAN', 'MINNESOTA', 'WISCONSIN')

# great lakes cities are defined as cities within 75 miles of the nearest great
  # lake (for the purposes of the NOAA Fisheries U.S. Seafood Dashboard)
grlake_cities <- great_lakes_cities %>%
  filter(!is.na(MILES_TO_LAKE)) %>%
  filter(MILES_TO_LAKE != '200 +') %>%
  filter(MILES_TO_LAKE != '200+') %>%
  mutate(MILES_TO_LAKE = as.numeric(MILES_TO_LAKE)) %>%
  filter(MILES_TO_LAKE <= 75)