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
library(janitor)

load("data/rent_listings_raw.RData")


## Approaches ------------------------------------------------------------------

# 1) Linear regression models with different features (cross-validated)
# 2) Random Forests (cross-validated)
# 3) Boosted Trees (cross-validated)


## Preliminary data cleaning ---------------------------------------------------
modelling_vars <- c("rent_full", "area", "home_type", "furnished", "rooms", 
                    "Label", "balcony")

# onehot encode the Arbeitsmarktregionen
label_encoding <- dummyVars(~ Label, data = data_analyzed)
onehot_label <- as.data.frame(predict(label_encoding, data_analyzed))
hometype_encoding <- dummyVars(~ home_type, data = data_analyzed)
onehot_hometype <- as.data.frame(predict(hometype_encoding, data_analyzed))

# data for modelling
Dmod <- data_analyzed %>%
  select(all_of(modelling_vars), dplyr::starts_with("dist"),dplyr::starts_with("Micro")) %>%
  # mutate(attic = ifelse(home_type %in% c("Dachwohnung", "Attika"), 1, 0)) %>% # unselected for now, prediction based on all home_types
  bind_cols(onehot_label) %>%
  bind_cols(onehot_hometype) %>%
  select(-c("Label", "home_type")) %>%
  drop_na() %>%
  clean_names()




## OLS -------------------------------------------------------------------------





## Model 1: all variables ------------------------------------------------------
split <- initial_split(Dmod, prop = 0.8)             # split into training/test

ols_all <- lm(rent_full ~ ., data = training(split))
summary(ols_all)

y_hat <- predict(ols_all, newdata = testing(split))
MAE(y_hat, testing(split)$rent_full) # 249

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
           Micro_rating_new, 
           dplyr::starts_with("dist")
           ),
  prop = 0.8)

ols_sel <- lm(rent_full ~ ., data = training(split))
MAE(predict(ols_sel, newdata = testing(split)), testing(split)$rent_full) 
# yields worse results of 330



## Model 3: "very simple" ------------------------------------------------------
split <- initial_split(
  Dmod %>%
    select(rent_full, area, rooms, Micro_rating_new, starts_with("Label"))
)
ols_simple <- lm(rent_full ~ ., data = training(split))
MAE(predict(ols_simple, newdata = testing(split)), testing(split)$rent_full)

# yields results of 257
# the most simple model which only controls for area, rooms, micro rating, and
# location in amr yields equally good results as the model with all components



## Model 4: "no micro rating" --------------------------------------------------
split <- initial_split(
  Dmod %>%
    select(rent_full, area, rooms, starts_with("Label"))
)
ols_simple <- lm(rent_full ~ ., data = training(split))
MAE(predict(ols_simple, newdata = testing(split)), testing(split)$rent_full)

# similar results but a bit less precise


## -----------------------------------------------------------------------------


## Random Forests --------------------------------------------------------------

split <- initial_split(
  Dmod %>%
    select(rent_full, area, rooms, starts_with("Label")),
  prop = 0.8
)
Dmod_train <- analysis(split)
Dmod_test <- assessment(split)


m1 <- randomForest(
  formula = rent_full ~ .,
  data    = Dmod_train
)
y_hat <- predict(m1, data = Dmod_test)
MAE(y_hat$predictions, Dmod_test$rent_full)

