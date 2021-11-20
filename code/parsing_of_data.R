################################################################################
#
# Script 1: Parsing Data 




###---> WENN ÜBERMITTELT, DATEN/BEZEICHNUNGEN NOCHMAL ÜBERARBEITEN



# 20.11.2021
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

