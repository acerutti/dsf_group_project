################################################################################
#
# DSF Group Project
# Script 2: Data Cleaning
# 20.11.2021
#
################################################################################

library(tidyverse)
library(xlsx)
load("data/rent_listings_raw.RData")

## Variable Selection ----------------------------------------------------------

# We first check which variables do not contain any data. I filtered out any 
# variables which had roughly more than 50% NAs

# NAs per variable
lapply(D, function(x) table(is.na(x)))

# preliminary selection of important variables
keep_for_now <- c("rent_full", "area", "home_type", "GDENAMK", "GDENR", "KTKZ", "zipcode", 
                  "GKODE", "GKODN", "PLZNAME", "descr", "floors", "furnished", 
                  "lat", "lon", "date","month", "quarter_general", # furnished kept for tokenization
                  "msregion", "rooms",  "year_built", "newly_built", "balcony", # check if newly_built is year_built == 2019
                  "Micro_rating_new", "Micro_rating_NoiseAndEmission_new", 
                  "Micro_rating_Accessibility_new", "Micro_rating_DistrictAndArea_new", 
                  "Micro_rating_SunAndView_new", "Micro_rating_ServicesAndNature_new",
                  "dist_to_haltst", "dist_to_highway", "dist_to_school_1",
                  "dist_to_train_stat", "apoth_pix_count_km2", 
                  "restaur_pix_count_km2", "superm_pix_count_km2")
################################################################################
# Notes on variables
################################################################################
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
################################################################################


##### Sunday, 21.11.2021 Cleaning ----------------------------------------------
       

## FILTERING METHOD APPROACH ------
# We filter outliers as defined by the boxplot system. This means we first
# compute the ranges of admissible results for every amr, and then drop the 
# observations which are defined as outliers for every amr. Note: we only drop
# bottom outliers as we have issues with observations with lowest rent_full


# Legend of Arbeitsmarktregionen (amr)
legend <- as_tibble(read.xlsx("data/legend.xlsx", sheetIndex = 1, header = T))
legend$MS.Regionen <- as.double(legend$MS.Regionen)
legend$Arbeitsmarktregionen.2018 <- as.double(legend$Arbeitsmarktregionen.2018)
legend$Arbeitsmarktgrossregionen.2018 <- as.double(legend$Arbeitsmarktgrossregionen.2018)
legend$BFS.Gde.nummer <- as.double(legend$BFS.Gde.nummer)

legend <- legend %>%
  select(MS.Regionen, Arbeitsmarktregionen.2018, Arbeitsmarktgrossregionen.2018, BFS.Gde.nummer)

# Arbeitsmarktregionen Names
amr_names <- read.xlsx("data/legend.xlsx", sheetIndex = 3, startRow = 2, header = T)
amr_names$Code <- as.double(amr_names$Code)

# Combine Arbeitsmarktregionen with its names
legend <- legend %>%
  left_join(amr_names, by = c("Arbeitsmarktregionen.2018" = "Code"))

# Calculate quantiles for the accpetance ranges
ms_quantiles <- D %>% 
  filter(!is.na(area)) %>%
  filter(area >= 25) %>%
  select(all_of(keep_for_now)) %>%
  mutate("rent_m2" = rent_full/area) %>%
  left_join(legend, by = c("msregion" = "MS.Regionen", "GDENR" = "BFS.Gde.nummer")) %>%
  group_by(Arbeitsmarktregionen.2018) %>%
  summarise(median = median(rent_m2), rent_iqr= IQR(rent_m2), q1 = quantile(rent_m2, probs = 0.25), q3 = quantile(rent_m2, probs = 0.75))
  
# Data which is going to be analyzed
data_analyzed <- D %>%
  filter(!is.na(area)) %>%
  filter(area >= 25) %>%
  select(all_of(keep_for_now)) %>%
  left_join(legend, by = c("msregion" = "MS.Regionen", "GDENR" = "BFS.Gde.nummer")) %>%
  mutate("rent_m2" = rent_full/area) %>%
  left_join(ms_quantiles, by = c("Arbeitsmarktregionen.2018" = "Arbeitsmarktregionen.2018")) %>%
  mutate(outlier = ifelse(rent_m2 < q1 - 1.5*rent_iqr, 1, 0)) %>%
  filter(outlier == 0) %>%
  rowid_to_column() %>%
  select(-c("floors", "median", "rent_iqr", "q1", "q3", "outlier"))


data_analyzed[,c("balcony", "furnished")] = apply(data_analyzed[,c("balcony", "furnished")], 2, function(x) replace_na(x,0))


save(list = c("D", "data_analyzed", "problems"), file = "data/rent_listings_raw.RData")
