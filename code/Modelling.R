###############################################################################*
#
# DSF Group Project
# Script 5: Modelling
# 24.11.2021
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

load("data/rent_listings_raw.RData")


## Approaches ------------------------------------------------------------------

# 1) Linear regression models with different features (cross-validated)
# 2) Random Forests (cross-validated)
# 3) Boosted Trees (cross-validated)

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
ols_results <- data.frame(matrix(ncol = 6, nrow = 5))
names(ols_results) <- c("model_nr", "model_type", "rmse_cv", "mae_cv", "mae", "mae/mean")




###############################################################################*
## OLS -------------------------------------------------------------------------
###############################################################################*

###############################################################################*
#
### OLS - Notes
# General approach: we model different OLS with different specified variables
# we check the errors (mse and mae) of models through cross-validation and a
# validation set approach
###############################################################################*
set.seed(123)



## Model 1: all variables ------------------------------------------------------

# Vars used: area, rooms, apartment types, furnished (yes/no), micro ratings,
# balcony (yes/no), distances to places of interest, arbeitsmarktregionen

# split into training/test
split <- initial_split(Dmod, prop = 0.8)             

# cross-validation split
data_ctrl <- trainControl(method = "cv", number = 10) 

# cross-validation
ols_1_cv <- train(rent_full ~ .,                    # model to fit
                  data = training(split),                        
                  trControl = data_ctrl,           # folds
                  method = "lm",                   # specifying regression model
                  na.action = na.pass)      
rmse_cv <- ols_1_cv$results[["RMSE"]]               # rmse_cv: 363.41
mae_cv <- ols_1_cv$results[["MAE"]]                 # mae_cv: 249.93

# normal model
ols_1 <- lm(rent_full ~ ., data = training(split))


# error (validation set approach)
y_hat <- predict(ols_1, newdata = testing(split))
mae <- MAE(y_hat, testing(split)$rent_full)          # mae: 244.78


mae_mean <- MAE(y_hat, testing(split)$rent_full)/mean(testing(split)$rent_full) # in relation to mean price: 13.97%

ols_results[1,] <- c(1, "OLS", rmse_cv, mae_cv, mae, mae_mean)


## Model 2: Selected variables--------------------------------------------------

# Vars used: area, rooms, furnished (yes/no), balcony (yes/no), micro ratings,
# distances to places of interest

split <- initial_split(
  Dmod %>% 
    select(all_of(setdiff(modelling_vars, c("home_type", "Label"))), 
           micro_rating_new, 
           dplyr::starts_with("dist")
    ),
  prop = 0.8)

# cross-validation split
data_ctrl <- trainControl(method = "cv", number = 10) 

# cross-validation
ols_2_cv <- train(rent_full ~ .,                    # model to fit
                  data = training(split),                        
                  trControl = data_ctrl,           # folds
                  method = "lm",                   # specifying regression model
                  na.action = na.pass)      
rmse_cv <- ols_2_cv$results[["RMSE"]]               # rmse_cv: 478.05
mae_cv <- ols_2_cv$results[["MAE"]]                 # mae_cv: 333.65

# normal model
ols_2 <- lm(rent_full ~ ., data = training(split))

# error (validation set approach)
y_hat <- predict(ols_2, newdata = testing(split))
mae <- MAE(y_hat, testing(split)$rent_full)       # mae: 332.84


mae_mean <- MAE(y_hat, testing(split)$rent_full)/mean(testing(split)$rent_full) # in relation to mean price: 18.96%

ols_results[2,] <- c(2, "OLS", rmse_cv, mae_cv, mae, mae_mean)



## Model 3: "very simple" ------------------------------------------------------

# Vars used: area, rooms, general micro rating, arbeitsmarktregionen

split <- initial_split(
  Dmod %>%
    select(rent_full, area, rooms, micro_rating_new, starts_with("label"))
)

# cross-validation split
data_ctrl <- trainControl(method = "cv", number = 10) 

# cross-validation
ols_3_cv <- train(rent_full ~ .,                    # model to fit
                  data = training(split),                        
                  trControl = data_ctrl,           # folds
                  method = "lm",                   # specifying regression model
                  na.action = na.pass)      
rmse_cv <- ols_3_cv$results[["RMSE"]]               # rmse_cv: 371.89
mae_cv <- ols_3_cv$results[["MAE"]]                 # mae_cv: 256.60

# normal model
ols_3 <- lm(rent_full ~ ., data = training(split))

# error (validation set approach)
y_hat <- predict(ols_3, newdata = testing(split))
mae <- MAE(y_hat, testing(split)$rent_full)         # mae: 256.72


mae_mean <- MAE(y_hat, testing(split)$rent_full)/mean(testing(split)$rent_full) # in relation to mean price: 14.69%

ols_results[3,] <- c(3, "OLS", rmse_cv, mae_cv, mae, mae_mean)


## Model 4: "squared variables" ------------------------------------------------

# Vars used: area, area squared, rooms, rooms squared, apartment types, 
# furnished (yes/no), micro ratings,
# balcony (yes/no), distances to places of interest, arbeitsmarktregionen

split <- initial_split(
  Dmod %>%
    mutate(area_sq = area^2) %>%
    mutate(rooms_sq = rooms^2)
)

# cross-validation split
data_ctrl <- trainControl(method = "cv", number = 10) 

# cross-validation
ols_4_cv <- train(rent_full ~ .,                    # model to fit
                  data = training(split),                        
                  trControl = data_ctrl,           # folds
                  method = "lm",                   # specifying regression model
                  na.action = na.pass)      
rmse_cv <- ols_4_cv$results[["RMSE"]]               # rmse_cv: 362.21
mae_cv <- ols_4_cv$results[["MAE"]]                 # mae_cv: 249.33

# normal model
ols_4 <- lm(rent_full ~ ., data = training(split))

# error (validation set approach)
y_hat <- predict(ols_4, newdata = testing(split))
mae <- MAE(y_hat, testing(split)$rent_full)         # mae: 248.32


mae_mean <- MAE(y_hat, testing(split)$rent_full)/mean(testing(split)$rent_full) # in relation to mean price: 14.12%

ols_results[4,] <- c(4, "OLS", rmse_cv, mae_cv, mae, mae_mean)



## Model 5: all variables including cantons, etc" ------------------------------

# Vars used: area, rooms, apartment types, furnished (yes/no), micro ratings,
# balcony (yes/no), distances to places of interest, arbeitsmarktregionen,
# cantons, arbeitsmarktgrossregionen

split <- initial_split(
  data_analyzed %>%
    select(all_of(modelling_vars), 
           dplyr::starts_with("dist"),
           dplyr::starts_with("Micro"), 
           KTKZ, 
           Arbeitsmarktgrossregionen.2018) %>%
    drop_na()
)

# cross-validation split
data_ctrl <- trainControl(method = "cv", number = 10) 

# cross-validation
ols_5_cv <- train(rent_full ~ .,                    # model to fit
                  data = training(split),                        
                  trControl = data_ctrl,           # folds
                  method = "lm",                   # specifying regression model
                  na.action = na.pass)      
rmse_cv <- ols_5_cv$results[["RMSE"]]               # rmse_cv: 357.00
mae_cv <- ols_5_cv$results[["MAE"]]                 # mae_cv: 245.58

# normal model
ols_5 <- lm(rent_full ~ ., data = training(split))

# error (validation set approach)
y_hat <- predict(ols_5, newdata = testing(split))
mae <- MAE(y_hat, testing(split)$rent_full)         # mae: 249.74


mae_mean <- MAE(y_hat, testing(split)$rent_full)/mean(testing(split)$rent_full) # in relation to mean price

ols_results[5,] <- c(5, "OLS", rmse_cv, mae_cv, mae, mae_mean)





###############################################################################*
## Random Forests --------------------------------------------------------------
###############################################################################*
set.seed(123)
###############################################################################*
### RANDOM FOREST - NOTES
### We model some random forests in this section. We begin with a simple "vanilla"
### random forest with default values from randomForest. We then try some other
### random forest with a reduction in the amount of features. Once we have an overview
### on which model performs best (based on oob errors), we get into hyper-
### parameter tuning for that model
###############################################################################*


## Model 1: All variables, vanilla attempt -------------------------------------

  # Note: we first try a normal "vanilla" random forest with randomForest
  # package. It should serve as a general benchmark on what follows

split <- initial_split(Dmod, prop = 0.8) 
Dmod_train <- analysis(split)
Dmod_test <- assessment(split)

  # we use randomForest package as first result to plot oob errors vs. no. trees
  # Note: randomForest parameters: 
    # mtry = p/3, sample = nrow(Dmod), ntree = 500, node size = 5
rf1 <- randomForest(
  formula = rent_full ~ .,
  data    = Dmod_train
)

plot(rf1)                             # plot oob errors vs. no. of trees:
                                      # after some time mse doesn't improve
                                      # by much

rf1$mse[which.min(rf1$mse)]           # oob mse with 500 trees: 103206
                                        

y_hat <- predict(rf1, Dmod_test) 
MAE(y_hat, Dmod_test$rent_full)       # MAE: 211.77



## Model 2: Selected Variables (same variables as ols model 3) -----------------

  # Vars used: area, rooms, general micro rating, arbeitsmarktregionen
  # Note: we use the ranger package for further models due to its faster
  # implementation. Parameters remain the same as for the default vanilla 
  # approach at the beginning
split <- initial_split(
  Dmod %>%
    select(rent_full, area, rooms, micro_rating_new, dplyr::starts_with("label")),
  prop = 0.8
)
Dmod_train <- training(split)
Dmod_test <- testing(split)

rf2 <- ranger(
  formula       = rent_full ~ .,
  data          = Dmod_train,
  mtry          = 33,                # p/3 as in prev. model
  min.node.size = 5
)

rf2$prediction.error      # oob mse: 115586
MAE(predict(rf2, data = Dmod_test)$prediction, Dmod_test$rent_full)
                          # MAE: 228.40 (better than ols: 256)



## Model 3: Selected Variables (same variables as ols model 2) -----------------

 # Vars used: area, rooms, furnished (yes/no), balcony (yes/no), micro ratings,
 # distances to places of interest
split <- initial_split(
  Dmod %>%
    select(rent_full, area, rooms, furnished, balcony, 
           dplyr::starts_with("micro"), dplyr::starts_with("dist")),
  prop = 0.8
)
Dmod_train <- training(split)
Dmod_test <- testing(split)

rf3 <- ranger(
  formula       = rent_full ~ .,
  data          = Dmod_train,
  mtry          = 4,                # p/3 as in prev. model
  min.node.size = 5
)
rf3$prediction.error               # oob mse: 157119
MAE(predict(rf3, data = Dmod_test)$prediction, Dmod_test$rent_full)
                                   # MAE: 271.66 (better than ols: 332.84)

###############################################################################*
## Hyper-parameter tuning - Random Forest---------------------------------------
###############################################################################*

###############################################################################*
### HYPER-PARAMETER TUNING - NOTES:
# We do a hyper-parameter tuning for a random forest with all the variables in
# modelling_vars, distance, and microlocations
#
# Only run this section if you want to replicate the results!
###############################################################################*

split <- initial_split(Dmod, prop = 0.8)
Dmod_train <- analysis(split)

# we start with a hyper-parameter grid as introduced in the lecture
#hyper_grid <- expand.grid(
#  mtry       = seq(35, 45, by = 2),
#  node_size  = seq(3, 9, by = 2),
#  OOB_RMSE   = 0 # a place to dump results
#  sampe_size = c(.55, .632, .70, .80),
#)

#for(i in 1:nrow(hyper_grid)) {
  
  
#  model <- ranger(
#    formula         = rent_full ~ ., 
#    data            = Dmod_train, 
#    num.trees       = 500,
#    mtry            = hyper_grid$mtry[i],
#    write.forest    = FALSE,
#    min.node.size   = hyper_grid$node_size[i],
#    sample.fraction = hyper_grid$sampe_size[i],
#    oob.error       = TRUE,
#    verbose         = FALSE,
#    seed            = 123
#)

# add OOB error to grid
#hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)

# progress
#print(i)
#}


#hyper_grid %>% arrange(OOB_RMSE)

# Results (OOB_RMSE) don't wary by much (Goes from 320 to 328). 
# Nonetheless, here are the top 10 models


#      mtry  node_size  sampe_size    OOB_RMSE
#1    45         3        0.8         320.4696
#2    41         3        0.8         320.6417
#3    43         3        0.8         320.8151
#4    39         3        0.8         320.9273
#5    45         5        0.8         321.0673
#6    35         3        0.8         321.1452
#7    43         5        0.8         321.2615
#8    41         5        0.8         321.3123
#9    39         5        0.8         321.3756
#10   37         3        0.8         321.3880


## Model 3: Model obtained by hyper-parameter tuning ---------------------------

  # This is the model with the hyper-parameters which are obtained through
  # tuning.
split <- initial_split(Dmod, prop = 0.8)
Dmod_train <- training(split)
Dmod_test <- testing(split)

rf4 <- ranger(
  formula         = rent_full ~ .,
  data            = Dmod_train,
  mtry            = 45,
  min.node.size   = 3,
  sample.fraction = 0.8,
  seed            = 123
)

rf4$prediction.error                         # oob MSE: 101566.4
MAE(predict(rf4, data = Dmod_test)$prediction, Dmod_test$rent_full)
                                             # MAE: 216.43

# MAE is higher than initially obtained random forest. This probably comes 
# from having a smaller sample size of 0.8 instead of the full sample size
# as the default randomForest package provides
