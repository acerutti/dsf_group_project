###############################################################################*
#
# DSF Group Project
# Script 6: Boosted Decision Trees
# 29.11.2021
#
###############################################################################* 

rm(list = ls())

library(tidyverse) 
library(caret)              # cross-validation etc.
library(rsample)            # data partitions
library(ranger)             # random forests
library(randomForest)       # random forest 
library(janitor)            # use for variable name cleaning


# packages for boosted trees
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(h2o)          # a java-based platform
library(pdp)          # model visualization
library(ggplot2)      # model visualization
library(lime)         # model visualization
library(vip)          # devtools::install_github("koalaverse/vip")

load("data/rent_listings_raw.RData")

###############################################################################*
## Preliminary data cleaning ---------------------------------------------------
###############################################################################*

# Variables which can be used for modelling
modelling_vars <- c("rent_full", "area", "home_type", "furnished", "rooms", 
                    "Label", "balcony")

# Onehot-encode the Arbeitsmarktregionen and home_types
label_encoding <- dummyVars(~ Label, data = data_analyzed)
onehot_label <- as.data.frame(predict(label_encoding, data_analyzed))
hometype_encoding <- dummyVars(~ home_type, data = data_analyzed)
onehot_hometype <- as.data.frame(predict(hometype_encoding, data_analyzed))

# Data for modelling
Dmod <- data_analyzed %>%
  select(all_of(modelling_vars), 
         dplyr::starts_with("dist"),
         dplyr::starts_with("Micro")) %>%
  bind_cols(onehot_label) %>%
  bind_cols(onehot_hometype) %>%
  select(-c("Label", "home_type")) %>%
  drop_na() %>%
  clean_names() # makes titles clean 
# (capitals become lower-case, brackets deleted, etc.) 
# (e.g. Wil (SG) becomes wil_sg)

# Set up data frame where we can push all results of all models to
boosting_results <- data.frame(matrix(ncol = 10, nrow = 20))
names(boosting_results) <- c( "model",  "n.trees", "interaction.depth", "shrinkage",
                             "cv.folds", "training_time", "rmse_cv", "mae_cv", "mae", "mae/mean")



###############################################################################*
### BOOSTING - NOTES
# 
# Data Dmod used for modelling
# We split the data into test and training set with a proportion of 0.8. 
# We set the parameter as variables that can be changed outside of the training
# function so that it is easier to save different training in our table and compare them. 

###############################################################################*

## Training  ------------------------------------------------------
# Create training (80%) and test (20%) 
# Use set.seed for reproducibility

set.seed(123)
Dmod_split <- initial_split(Dmod, prop = .8)
Dmod_train <- training(Dmod_split)
Dmod_test  <- testing(Dmod_split)

# Setting the parameters
# Change here the variable for the training
model_type <- "gbm"
n.trees <- 1600
interaction.depth <- 3 #maximum depth of variable interactions
shrinkage <- 0.01 #similar as learning rate
cv.folds <-  10 # cross validation

# for measuring training time - get the start time
start_time <- Sys.time() 

# fitting of the model
gbm.fit <- gbm(
  formula = rent_full ~ .,
  distribution = "gaussian",
  data = Dmod_train,
  n.trees = n.trees,
  interaction.depth = interaction.depth, 
  shrinkage = shrinkage, 
  cv.folds = cv.folds,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# get the end time
end_time <- Sys.time() 
end_time - start_time # calculate the length of the training - appears in the console

training_time <- difftime(end_time, start_time, units = "mins") #save the training time

# print results
print(gbm.fit)

# get MSE and compute RMSE
sqrt(min(gbm.fit$cv.error))


## Predicting ------------------------------------------------------------------
# predict values for test data
pred <- predict(gbm.fit, n.trees = gbm.fit$n.trees, Dmod_test)



## Assessing Performance of the model ------------------------------------------
# RMSE_CV
rmse_cv <- caret::RMSE(pred, Dmod_test$rent_full)

# MAE_CV
mae_cv <- caret::MAE(pred, Dmod_test$rent_full)

# MAE 
# always to zero because we have cross validatation
mae <- 0

# MAE/MEAN
mae_cv_mean <- caret::MAE(pred, Dmod_test$rent_full)/mean(Dmod_test$rent_full)


##Save the results after each iteration to table  --------------------------------
## Always change the number of the row when saving the results of a new set of parameters 
# and the training. At the moment it is comment out becuase we set the best parameters already
# at the beginning. 

# boosting_results[10, ] <- c(model_type, n.trees, interaction.depth, shrinkage, 
                              #cv.folds, training_time, rmse_cv, mae_cv, mae, mae_cv_mean)

# Omit all the rows that are not filled 
# boosting_results <- na.omit(boosting_results)

# Save the data frame 
# write.csv(boosting_results,"data/boosting_results.csv", row.names = FALSE)


## Plots  ----------------------------------------------------------------------
# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit, method = "cv")

# plot for showing the importance of each variable
vip::vip(gbm.fit)



