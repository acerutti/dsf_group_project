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







##### Sunday's work

View(D %>%
       filter(!is.na(area)) %>%
       mutate("rent_m2" = rent_full/area) %>%
       select("rent_full", "area", "rent_m2", "rent_m2_pix_avg_km2", "GDENAMK", "KTKZ", "zipcode", "GKODE", "GKODN", "PLZNAME",
              "descr", "floors", "furnished", "lat", "lon", "date","month", "quarter_general",
              "msregion", "rooms",  "year_built", "newly_built", "balcony",
              "Micro_rating_new", "Micro_rating_NoiseAndEmission_new", "Micro_rating_Accessibility_new",
              "Micro_rating_DistrictAndArea_new", "Micro_rating_SunAndView_new",
              "Micro_rating_ServicesAndNature_new",
              "dist_to_haltst", "dist_to_highway", "dist_to_school_1",
              "dist_to_train_stat", "apoth_pix_count_km2", "restaur_pix_count_km2", "superm_pix_count_km2") %>%
       arrange(rent_full, rent_m2))
       

## FILTERING METHOD ------
# we are going to calculate mean m2 rents for all "arbeitsmarktregionen" and then
# we filter outliers (as measured by something like a boxplot or some standard
# deviations from the mean). We then consider only values within the bounds

# computing arbeitsmarktregionen
# merging ms regions with arbeitsmarktregionen


## legend
library(xlsx)
legend <- read.xlsx("data/legend.xlsx", sheetIndex = 1, header = T)
legend <- as_tibble(legend)
legend$MS.Regionen <- as.double(legend$MS.Regionen)
legend$Arbeitsmarktregionen.2018 <- as.double(legend$Arbeitsmarktregionen.2018)
legend$Arbeitsmarktgrossregionen.2018 <- as.double(legend$Arbeitsmarktgrossregionen.2018)
legend$BFS.Gde.nummer <- as.double(legend$BFS.Gde.nummer)
legend <- legend %>%
  select(MS.Regionen, Arbeitsmarktregionen.2018, Arbeitsmarktgrossregionen.2018, BFS.Gde.nummer)

ms_quantiles <- D %>% # we first compute quantiles and iqr to get rid of other ranges
  filter(!is.na(area)) %>%
  filter(area >= 25) %>%
  mutate("rent_m2" = rent_full/area) %>%
  select("rent_full", "area", "rent_m2", "rent_m2_pix_avg_km2", "GDENAMK", "KTKZ", "zipcode", "GKODE", "GKODN", "PLZNAME",
         "descr", "floors", "furnished", "lat", "lon", "date","month", "quarter_general",
         "msregion", "GDENR", "rooms",  "year_built", "newly_built", "balcony",
         "Micro_rating_new", "Micro_rating_NoiseAndEmission_new", "Micro_rating_Accessibility_new",
         "Micro_rating_DistrictAndArea_new", "Micro_rating_SunAndView_new",
         "Micro_rating_ServicesAndNature_new",
         "dist_to_haltst", "dist_to_highway", "dist_to_school_1",
         "dist_to_train_stat", "apoth_pix_count_km2", "restaur_pix_count_km2", "superm_pix_count_km2") %>%
  left_join(legend, by = c("msregion" = "MS.Regionen", "GDENR" = "BFS.Gde.nummer")) %>%
  group_by(Arbeitsmarktregionen.2018) %>%
  summarise(median = median(rent_m2), rent_iqr= IQR(rent_m2), q1 = quantile(rent_m2, probs = 0.25), q3 = quantile(rent_m2, probs = 0.75))
  



data_analyzed <- D %>% # data of regions analysed
  filter(!is.na(area)) %>%
  filter(area >= 25) %>%
  mutate("rent_m2" = rent_full/area) %>%
  select("rent_full", "area", "home_type", "rent_m2", "rent_m2_pix_avg_km2", "GDENAMK", "KTKZ", "zipcode", "GKODE", "GKODN", "PLZNAME",
         "descr", "floors", "furnished", "lat", "lon", "date","month", "quarter_general",
         "msregion", "GDENR", "rooms",  "year_built", "newly_built", "balcony",
         "Micro_rating_new", "Micro_rating_NoiseAndEmission_new", "Micro_rating_Accessibility_new",
         "Micro_rating_DistrictAndArea_new", "Micro_rating_SunAndView_new",
         "Micro_rating_ServicesAndNature_new",
         "dist_to_haltst", "dist_to_highway", "dist_to_school_1",
         "dist_to_train_stat", "apoth_pix_count_km2", "restaur_pix_count_km2", "superm_pix_count_km2") %>%
  left_join(legend, by = c("msregion" = "MS.Regionen", "GDENR" = "BFS.Gde.nummer")) %>%
  left_join(ms_quantiles, by = c("Arbeitsmarktregionen.2018" = "Arbeitsmarktregionen.2018")) %>%
  mutate(outlier = ifelse(rent_m2 < q1 - 1.5*rent_iqr, 1, 0)) %>%
  filter(outlier == 0)

lapply(data_analyzed, function(x) table(is.na(x)))


data_analyzed %>%
  rowid_to_column() %>%
  select(-rent_m2_pix_avg_km2, floors, median, rent_iqr, q1, q3, outlier)
  


data_analyzed[,c("balcony", "furnished")] = apply(data_analyzed[,c("balcony", "furnished")], 2, function(x) replace_na(x,0))

