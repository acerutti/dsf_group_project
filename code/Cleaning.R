################################################################################
#
# Script 2: Data Cleaning
# 20.11.2021
#
################################################################################

## Variable Selection ----------------------------------------------------------
library(tidyverse)

load("data/rent_listings_raw.RData")

# We first check which variables do not contain any data. I filtered out any 
# variables which had roughly more than 50% NAs

# NAs per variable
lapply(D, function(x) table(is.na(x)))

# preliminary selection of important variables
keep_for_now <- c("rent_full", "area", "home_type", "GDENAMK", "KTKZ", "zipcode", "GKODE", "GKODN", "PLZNAME",
                  "descr", "floors", "furnished", "lat", "lon", "date","month", "quarter_general", # furnished kept for tokenization
                  "msregion", "rooms",  "year_built", "newly_built", "balcony", # check if newly_built is year_built == 2019
                  "Micro_rating_new", "Micro_rating_NoiseAndEmission_new", "Micro_rating_Accessibility_new",
                  "Micro_rating_DistrictAndArea_new", "Micro_rating_SunAndView_new",
                  "Micro_rating_ServicesAndNature_new",
                  "dist_to_haltst", "dist_to_highway", "dist_to_school_1",
                  "dist_to_train_stat", "apoth_pix_count_km2", "restaur_pix_count_km2", "superm_pix_count_km2")


D_selection <- D[,keep_for_now]



###############
# Notes on variables
# address cannot be used anymore
# area usable is not specified in most cases, use area
# balcony is unclear: NA comes from not specifying in description or no balcony?
# cabletv: keep until clear what it is
# does floors mean on which floor it is?
# what does msregion mean? MSRegion are defined regions by the BFS. Each region
      # is homogenous. https://www.bfs.admin.ch/bfs/de/home/statistiken/raum-umwelt/nomenklaturen/msreg.assetdetail.415729.html
 
# newly_built might have the same problem as the apartment features - if it's 
#         not specified it's not got a 1
# check if zipcode is PLZ4! They are the same!
    # which(na.omit(D)$zipcode != na.omit(D)$PLZ4)
# the two columns with wgh_avg_... are identical - only keep one


# do avg values correspond to gemeinde? (e.g. anteil_efh, avg_size_household)
# dist_to_main_stat?
#

lapply(D, function(x) table(is.na(x)))

D_selection <- replace(D_selection,"TRUE",1)
#taking all variables with 1 or TRUE, converting it into 1s and 0s
temp <- apply(D_selection, 2, unique)


bin <- c("balcony", "furnished")


D_selection[,bin] = apply(D_selection[,bin], 2, function(x) replace_na(x,0))


D_clean <- D_selection %>%
  select(-c(year_built,descr)) %>%
  na.omit(D_selection)

apply(D_selection, 2, function(x) sum(is.na(x)))



