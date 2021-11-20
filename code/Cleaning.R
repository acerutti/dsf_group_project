################################################################################
#
# Script 1: Preliminary Data Cleaning
#
#
################################################################################
# Clean the environment
rm(list = ls())

# Libraries
library(tidyverse)

# Set working directory
setwd("~/Desktop/dsf_group_project")

# Loading the data
# Ale
D <- read_csv("data/copy_data.csv", col_names = T)

# Jason
# D <- read_csv("Downloads/Copy of rent_listings_100k_2019_geoloc_with_microdata.csv", col_names = T)



## Resoving Parsing Failures ---------------------------------------------------

# parsing failures
problems <- problems()

# we correct by reading in the raw file and changing 1.0 to 1
# Ale
raw <- read_file("data/copy_data.csv")

# Jason
# raw <- read_file("Project/Copy of rent_listings_100k_2019_geoloc_with_microdata.csv")

# correction
raw1 <- gsub(",1.0,", ",1,", raw)

# Write this new data to a csv file
# Ale
write_file(raw1, file = "data/rent_listings_raw.csv")

# Jason
#write_file(raw1, file = "Project/rent_listings_raw.csv")

# parsing and saving as RData file
# Ale
D <- read_csv("data/rent_listings_raw.csv", col_names = T)

# Jason
# D <- read_csv("Project/rent_listings_raw.csv", col_names = T)

# Get the problems another dime
problems <- problems() # we ignore the problematic cases for now

# Save dataframe + problems 
# Ale
save(D, problems, file = "data/rent_listings_raw.RData")

# Jason
# save(D, problems, file = "Project/rent_listings_raw.RData")


## Variable Selection ----------------------------------------------------------

# We first check which variables do not contain any data. I filtered out any 
# variables which had roughly more than 50% NAs

# NAs per variable
lapply(D, function(x) table(is.na(x)))

# preliminary selection of important variables
unimportant <- c("Url", "appartments", "area_useable", "basement", "bath", 
                 "bath_tube", "bright", "building_plot", "ceiling", "cheminee",
                 "contact_name", "contact_tel", "cornerhome", "date_available",
                 "date_inactive", "dishwasher", "dryer", "exit_ramp", "first_time",
                 "furnished", "garden_m2", "gardenshed", "heating_air",
                 "heating_earth", "heating_electro", "heating_far", "heating_gas",
                 "heating_oil", "heating_pellets", "index", "kids_friendly", 
                 "laundry", "level_0", "manlift", "middle_house", "minergie",
                 "new_building", "number", "oldbuilding", "oven", "parking_indoor",
                 "parking_outside", "pets", "playground", "pool", "price", "price_m2",
                 "public_transport", "quarter_general", "quiet", "raised_groundfloor",
                 "rent_annual", "rent_annual_m2", "rent_day", "sale", "shared_flat",
                 "shopping", "shower", "size_land", "source", "sunny", "terrace",
                 "toilets", "topstorage", "veranda", "water", "wheelchair", "year",
                 "ind", "ind_add", "PLZZ", "PLZ4", "wgh_avg_sonnenklasse_per_egid...115",
                 "dist_to_lake", "geb_wohnnutz_total", "max_guar_down_speed", 
                 "cabletv", "distance_highway", "distance_kindergarden",
                 "distance_primary", "distance_secondary", "elevator" )

keep_for_now <- c("GDENAMK", "GDENR", "KTKZ", "address", "area", "balcony",
                  "date", "descr", "floors", "home_type", "lat", "lon", "month",
                  "msregion", "municipality", "quarter_specific", "rent_brutto",
                  "rent_full", "rent_nk", "rooms", "scraping_date", "street",
                  "year_built", "zipcode", "GKODE", "GKODN", "PLZNAME",
                  "STRNAME", "Micro_rating_new", "Micro_rating_NoiseAndEmission_new",
                  "Micro_rating_DistrictAndArea_new", "Micro_rating_SunAndView_new",
                  "Micro_rating_ServicesAndNature_new", "Anteil_auslaend", "Avg_age",
                  "Avg_size_household", "Noise_max", "anteil_efh", "apoth_pix_count_km2",
                  "avg_anzhl_geschosse", "avg_bauperiode", "dist_to_4G", "dist_to_5G",
                  "dist_to_haltst", "dist_to_highway", "dist_to_school_1",
                  "dist_to_train_stat", "restaur_pix_count_km2", "super_pix_count_km2",
                  "wgh_avg_sonnenklasse_per_egid...136", "rent_m2_pix_avg_km2",
                  "rent_m2_pix_avg", "dist_to_river")


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






