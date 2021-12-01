################################################################################
# DSF Group Project: Group 10
#
# Preparation: Parsing Data 
# Note: This script is an auxilliary to the main script. The only aim of this
# script is to prepare the obtained csv-file into an RData file for easier use
# We used a separate file as this part uses intermediate data files.
#
# Alessandra Cerutti, Amélie Madrona, Maximilian Bisges, Jason Rosenthal
# 5.12.2021
################################################################################

# Clean the environment
rm(list = ls())

# Libraries
library(tidyverse)

# Set working directory

# Loading the data
D <- read_csv("data_raw.csv", col_names = T)


## Resolving Parsing Failures ---------------------------------------------------

# parsing failures
problems <- problems()

# we correct by reading in the raw file and changing 1.0 to 1
raw <- read_file("data_raw.csv")

# correction
raw1 <- gsub(",1.0,", ",1,", raw)

# Write this new data to a csv file
write_file(raw1, file = "data.csv")

# actual data parsing
D <- read_csv("data.csv", col_names = T)

# Get the problems another dime
problems <- problems() # we ignore the problematic cases for now

# Save dataframe and problems. This is the RData file used for the actual main
# script
save(D, problems, file = "rent_listings.RData")

