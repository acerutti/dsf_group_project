####
# MAIN CODE----
####

# Preparation Steps ----
library(tidyverse)


# Clean the environment
rm(list = ls())

# read and load csv
setwd("~/Desktop/dsf_group_project")

data <- read.csv("data/copy_data.csv")


# Some Basic Analysis ----
# group by canton
canton <- data %>% group_by(KTKZ) %>% 
  summarise(number = sum(str_count(KTKZ)))

# amount of na pro column
sapply(data, function(x) sum(is.na(x)))


# Check the data types 
varTypes = map_chr(data, class)

varTypes <- tibble(varTypes) # create 

# average area of an apartment
mean(data$area, na.rm = TRUE)


# Selecting Columns ---
# get only columns with less or equal 50% of na values as the number of row
important_columns <- data[, colSums(is.na(data)) <= nrow(data)*0.5]
colnames(important_columns)

col_test <- data %>% 
  select(GDENAMK, GDENR, KTKZ, address, area, area_useable, balcony, bath_tube)

# Get the smallest zip code 
min_col_GDENAMK <- min(col_test[, "GDENR"], na.rm=T)

# what are the uniques values of the cantons? - if it is 26 then fine!
length(unique(col_test$KTKZ))

# what are the values contained in apartments
unique(data$appartments)
# we see that there are no entries - deselct the column

# how many na values are in the area column? 
na_col_area = sum(is.na(col_test$area))

# get all the uniques values of the area_usable column
unique(data$area_useable)

# how many na values in the balcony column
na_col_balcony = sum(is.na(col_test$balcony))

# no basement selected becuase the percentage of missing values is really high
perc_na_col_basement = (sum(is.na(data$basement))/100000)*100

# percentage of rows with not entered if there is a bath or not in the apartment
perc_na_col_bath = (sum(is.na(data$bath))/100000)*100
# I do not select bath because 99 percentage of the entries do not contain any value

## Jason added this for testing purposes

print("hello")
