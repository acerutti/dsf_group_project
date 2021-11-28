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
library(janitor)

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
model_results <- data.frame(NULL)


###############################################################################*
## OLS -------------------------------------------------------------------------
###############################################################################*

###############################################################################*
#
### OLS - Notes
# Explain all models
# 
# TO DO
# make models with polynomials
# run cv errors for all
###############################################################################*


## Model 1: all variables ------------------------------------------------------
split <- initial_split(Dmod, prop = 0.8)             # split into training/test

ols_all <- lm(rent_full ~ ., data = training(split))
summary(ols_all)

y_hat <- predict(ols_all, newdata = testing(split))
MAE(y_hat, testing(split)$rent_full) # 249


MAE(y_hat, testing(split)$rent_full)/mean(Dmod$rent_full) # in relation to mean price

# cross-validation
data_ctrl <- trainControl(method = "cv", number = 5) 
ols_all_cv <- train(rent_full ~ .,                    # model to fit
                     data = Dmod,                        
                     trControl = data_ctrl,           # folds
                     method = "lm",                   # specifying regression model
                     na.action = na.pass)      
summary(ols_all_cv)
ols_all_cv$results # results not that incredible. mae is still around 250


## Model 2: selected variables with cross validation -------------------------------------------------

# selection of only some variables to prevent overfitting
split <- initial_split(
  Dmod %>% 
    select(all_of(setdiff(modelling_vars, c("home_type", "Label"))), 
           micro_rating_new, 
           dplyr::starts_with("dist")
           ),
  prop = 0.8)

ols_sel <- lm(rent_full ~ ., data = training(split))
MAE(predict(ols_sel, newdata = testing(split)), testing(split)$rent_full) 
# yields worse results of 330



## Model 3: "very simple" ------------------------------------------------------
split <- initial_split(
  Dmod %>%
    select(rent_full, area, rooms, micro_rating_new, starts_with("label"))
)
ols_simple <- lm(rent_full ~ ., data = training(split))
MAE(predict(ols_simple, newdata = testing(split)), testing(split)$rent_full)

# yields results of 257
# the most simple model which only controls for area, rooms, micro rating, and
# location in amr yields equally good results as the model with all components



## Model 4: "no micro rating" --------------------------------------------------
split <- initial_split(
  Dmod %>%
    select(rent_full, area, rooms, starts_with("label"))
)
ols_simple <- lm(rent_full ~ ., data = training(split))
MAE(predict(ols_simple, newdata = testing(split)), testing(split)$rent_full)

# similar results but a bit less precise

## Model 5: "all variables including cantons, etc." ----------------------------
split <- initial_split(
  data_analyzed %>%
    select(all_of(modelling_vars), dplyr::starts_with("dist"),dplyr::starts_with("Micro"), KTKZ, Arbeitsmarktgrossregionen.2018) %>%
    drop_na()
)

mod_all <- lm(rent_full ~ ., data = analysis(split))
MAE(predict(mod_all, newdata = testing(split)), testing(split)$rent_full)
## doesn't help a lot, worries about overfitting



###############################################################################*
## Random Forests --------------------------------------------------------------
###############################################################################*

###############################################################################*
### RANDOM FOREST - NOTES
### Don't use randomForest package. It's too slow! Use ranger instead
###############################################################################*

## Model 1: Selected Variables as shown ----------------------------------------
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

# results (OOB_RMSE) don't wary by much. Nonetheless, here are the top 10 models

#      mtry  node_size  sampe_size OOB_RMSE
#1     30         3      0.800   324.0606
#2     30         5      0.800   324.7636
#3     26         3      0.800   325.0434
#4     28         3      0.800   325.1838
#5     28         5      0.800   325.5639
#6     30         7      0.800   325.7180
#7     26         5      0.800   325.7482
#8     30         3      0.700   326.0298
#9     24         3      0.800   326.2729
#10    28         3      0.700   326.3092


###############################################################################*
## Boosted Decision Trees ------------------------------------------------------
###############################################################################*


###############################################################################*
### BOOSTING - NOTES
# 
# Hi Alessandra :), please use the Dmod dataset if you model any boosted trees
# the variables there are already onehot encoded and everything else is good
# to go.
###############################################################################*

