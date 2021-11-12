####
# MAIN CODE----
####

# Preparation Steps ----
library(tidyverse)

# Clean the environment
# rm(list = ls())

# read and load csv
setwd("~/Desktop/dsf_group_project")

raw_data <- read.csv("data/data.csv")

# Some Basic Analysis ----
# group by canton
canton <- raw_data %>% group_by(KTKZ) %>% 
  summarise(number = sum(str_count(KTKZ)))

# amount of na pro column
sapply(raw_data, function(x) sum(is.na(x)))


# Check the data types 
varTypes = map_chr(raw_data, class)

varTypes <- tibble(varTypes) # create 
