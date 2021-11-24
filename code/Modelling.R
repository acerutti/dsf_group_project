################################################################################
#
# DSF Group Project
# Script 5: Modelling
# 24.11.2021
#
################################################################################

rm(list = ls())

library(tidyverse) 
library(caret)              # cross-validation etc.
library(rsample)            # data partitions
library(ranger)             # random forests
library(randomForest)       # random forest 

load("data/rent_listings_raw.RData")


## Approaches ------------------------------------------------------------------

# 1) Easy linear regression models with different features (cross-validated)
# 2) Try Random Forests (cross-validated)
# 3) Try Boosted Trees (cross-validated)


## Preliminary data cleaning ---------------------------------------------------
modelling_vars <- c("rent_full", "area", "home_type", "furnished", "rooms", 
                    "Micro_rating_new", "Label", "balcony")

# data for modelling
D <- data_analyzed %>%
  select(all_of(modelling_vars)) %>%
  mutate(attic = ifelse(home_type %in% c("Dachwohnung", "Attika"), 1, 0))

# onehot encode the Arbeitsmarktregionen
label_encoding <- dummyVars(~ Label, data = test)
onehot <- as.data.frame(predict(label_encoding, test))

# data for analysis
D <- D %>%
  bind_cols(onehot) %>%
  select(-c("Label", "home_type")) %>%
  drop_na()


## OLS -------------------------------------------------------------------------

ols_valset <- function(data, target, features, prop){
  
  dat = data[,c(target, features)]
  dat = na.omit(dat)
  # data split
  split = initial_split(dat, prop = prop)
  data_train = training(split)
  data_test = training(split)
  
  f <- as.formula(noquote(paste(target, "~", ".")))
  reg <- lm(f, data = data_train)
  y_hat <- predict(reg, newdata = data_test)
  
  mae <- mean(abs(data_test[[target]] - y_hat), na.rm = T)
  
  return(mae)
}


## Model 1: all variables, validation set approach
ols_valset(D %>% drop_na(), 
           target = "rent_full", 
           features = setdiff(names(D), "rent_full"), 
           prop = 0.8)













