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
model_results <- data.frame(matrix(ncol = 6, nrow = 15))
names(model_results) <- c("model_nr", "model_type", "rmse_cv", "mae_cv", "mae", "mae/mean")





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

model_results[1,] <- c(1, "OLS", rmse_cv, mae_cv, mae, mae_mean)


## Model 2: selected variables with cross validation ---------------------------

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
mae <- MAE(y_hat, testing(split)$rent_full)


mae_mean <- MAE(y_hat, testing(split)$rent_full)/mean(testing(split)$rent_full) # in relation to mean price: 18.96%

model_results[2,] <- c(2, "OLS", rmse_cv, mae_cv, mae, mae_mean)

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

model_results[3,] <- c(3, "OLS", rmse_cv, mae_cv, mae, mae_mean)


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

model_results[4,] <- c(4, "OLS", rmse_cv, mae_cv, mae, mae_mean)



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

model_results[5,] <- c(5, "OLS", rmse_cv, mae_cv, mae, mae_mean)



###############################################################################*
## Random Forests --------------------------------------------------------------
###############################################################################*
set.seed(123)
###############################################################################*
### RANDOM FOREST - NOTES
### Don't use randomForest package. It's too slow! Use ranger instead
###############################################################################*

## Model 1: Selected Variables -------------------------------------------------

# Vars used: area, rooms, arbeitsmarktregionen
split <- initial_split(
  Dmod %>%
    select(rent_full, area, rooms, starts_with("label")),
  prop = 0.8
)
Dmod_train <- analysis(split)
Dmod_test <- assessment(split)


m1 <- ranger(
  formula = rent_full ~ .,
  data    = Dmod_train
)
y_hat <- predict(m1, data = Dmod_test)
MAE(y_hat$predictions, Dmod_test$rent_full) # MAE 250


## Model 2: all variables ------------------------------------------------------

# Vars used: area, rooms, 
split <- initial_split(Dmod)
m2 <- ranger(
  formula = rent_full ~ .,
  data = analysis(split)
)
y_hat <- predict(m2, data = assessment(split))
MAE(y_hat$predictions, assessment(split)$rent_full) # lowest MAE with 230

which.min(m2$prediction.error)


###############################################################################*
## Hyper-parameter tuning ------------------------------------------------------
###############################################################################*

###############################################################################*
### HYPER-PARAMETER TUNING - NOTES:
# We do a hyper-parameter tuning for a random forest with all the variables in
# modelling_vars, distance, and microlocations
#
# Only run this section if you want to replicate the results!
###############################################################################*


# Dmod_train <- analysis(split)

# we start with a hyper-parameter grid as introduced in the lecture
# hyper_grid <- expand.grid(
#  mtry       = seq(20, 30, by = 2),
#  node_size  = seq(3, 9, by = 2),
#  sampe_size = c(.55, .632, .70, .80),
#  OOB_RMSE   = 0 # a place to dump results
#)


# for(i in 1:nrow(hyper_grid)) {
#  
#  
#  model <- ranger(
#    formula         = rent_full ~ ., 
#    data            = Dmod_train, 
#    num.trees       = 500,
#    mtry            = hyper_grid$mtry[i],
#    write.forest    = FALSE,
#    min.node.size   = hyper_grid$node_size[i],
#    sample.fraction = hyper_grid$sampe_size[i],
#    oob.error       = TRUE,
#    verbose         = TRUE,
#    seed            = 123
#  )

# add OOB error to grid
#  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
#}


# hyper_grid %>% arrange(OOB_RMSE)

# Results (OOB_RMSE) don't wary by much. Nonetheless, here are the top 10 models
# NOTE: hyper-parameter tuning was run without setting a seed. Results may
# therefore wary


#      mtry  node_size  sampe_size    OOB_RMSE
#1     30         3      0.800        324.0606
#2     30         5      0.800        324.7636
#3     26         3      0.800        325.0434
#4     28         3      0.800        325.1838
#5     28         5      0.800        325.5639
#6     30         7      0.800        325.7180
#7     26         5      0.800        325.7482
#8     30         3      0.700        326.0298
#9     24         3      0.800        326.2729
#10    28         3      0.700        326.3092


###############################################################################*
## Boosted Decision Trees ------------------------------------------------------
###############################################################################*


###############################################################################*
### BOOSTING - NOTES
# 
# Data Dmod used for modelling
# 
# 
###############################################################################*


## Model 1: all variables ------------------------------------------------------
# Create training (70%) and test (30%) 
# Use set.seed for reproducibility

set.seed(123)
Dmod_split <- initial_split(Dmod, prop = .7)
Dmod_train <- training(Dmod_split)
Dmod_test  <- testing(Dmod_test)

# Training
start_time <- Sys.time() # for measuring training time - get the start time

gbm.fit <- gbm(
  formula = rent_full ~ .,
  distribution = "gaussian",
  data = Dmod_train,
  n.trees = 20000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

end_time <- Sys.time() # get the end time
end_time - start_time # calculate the length of the training - appears in the console

# print results
print(gbm.fit)

# Result for 100 trees: 
# The best cross-validation iteration was 100. There were 120 predictors of which 1 had non-zero influence.

# Result for 1000 trees: 
# The best cross-validation iteration was 1000. There were 120 predictors of which 1 had non-zero influence.

# Result for 2000 trees: 
# The best cross-validation iteration was 2000. There were 120 predictors of which 3 had non-zero influence.

# Result for 4000 trees: 
# Time difference of 7.881699 mins
# The best cross-validation iteration was 4000. There were 120 predictors of which 6 had non-zero influence.

# Result for 20000 trees:
# Time difference of 1.879366 hours
# The best cross-validation iteration was 20000. There were 120 predictors of which 30 had non-zero influence.


# get MSE and compute RMSE
sqrt(min(gbm.fit$cv.error))
## With 100 trees: 660.7521 CHF off the real price
## With 1000 trees: 566.5158 CHF off the real price
## With 2000 trees: 524.6013
## With 4000 trees: 484.7614
## With 20000 trees: 407.6505

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit, method = "cv")


# Plot which variable had the most influence on the rentprice
par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

# Another plot for showing the importance of each variable
# devtools::install_github("koalaverse/vip")

library(vip)
vip::vip(gbm.fit)

