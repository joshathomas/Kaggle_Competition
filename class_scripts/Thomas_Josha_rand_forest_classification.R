# {Rand Forest} tuning----------------------------------------------------------


#Load Packages------------------------------------------------------------------ 

library(tidymodels)
library(tidyverse)
library(doParallel)
library(tictoc)

#Register Processing------------------------------------------------------------ 

# Create a cluster object and then register: 
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

#Handle conflicts--------------------------------------------------------------- 

tidymodels_prefer()

#Load Required Objects ---------------------------------------------------------

load("Kaggle_Competition/data_classification/initial_split.rda")
load("Kaggle_Competition/data_classification/base_recipe_classification.rda")

#set.seed-----------------------------------------------------------------------

set.seed(3013)

#Update Recipe------------------------------------------------------------------

# wildfires_recipe <- wildfires_recipe %>%
#   step_interact(wlf ~ all_numeric_predictors()^2)

#no step interact in rand forest

#No interctions with random forest

#Define Model-------------------------------------------------------------------

rand_forest_spec <-
  rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine('randomForest') %>%
  set_mode('classification')

#Check tuning parameters--------------------------------------------------------

# parameters(rand_forest_spec)

# Set-up parameters ------------------------------------------------------------

rand_forest_parameters <- parameters(rand_forest_spec) %>% 
  update(mtry = mtry(range = c(2,10)))#There are 36 total variables which formed the basis of our range

# Define tuning grid------------------------------------------------------------

rand_forest_grid <- grid_regular(rand_forest_parameters, levels = c(8,5))

# Workflow----------------------------------------------------------------------

rand_forest_wflow <- 
  workflow() %>% 
  add_recipe(bank_loan_recipe_class) %>% 
  add_model(rand_forest_spec)

# Tuning/fitting ---------------------------------------------------------------

tic("Random Forest")

#Tune grid----------------------------------------------------------------------

rand_forest_tuned_class <- rand_forest_wflow %>% 
  tune_grid(resamples = bank_loan_folds, grid = rand_forest_grid, control = keep_pred, metrics = bank_loan_metrics)

#Check results------------------------------------------------------------------ 

# autoplot(rand_forest_tuned_class, metric = "roc_auc")

#select best

rand_forest_best_class <- select_best(rand_forest_tuned_class, metric = "roc_auc")


# save runtime info
toc(log = TRUE)

time_log <- tic.log(format = FALSE)

rand_forest_tictoc_class <- tibble(
  model = time_log[[1]]$msg, 
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)


# End parallel processing 

stopCluster(cl)

# Save results & workflow

save(rand_forest_tuned_class, file = "Kaggle_Competition/model_info_classification/rand_forest_classification.rda")

