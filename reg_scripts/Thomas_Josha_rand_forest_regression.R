# {Rand Forest} tuning----------------------------------------------------------


#Load Packages------------------------------------------------------------------ 

library(tidymodels)
library(tidyverse)
library(doParallel)
library(tictoc)

#Register Processing------------------------------------------------------------ 

# Create a cluster object and then register: 
cl <- makePSOCKcluster(6)
registerDoParallel(cl)

#Handle conflicts--------------------------------------------------------------- 

tidymodels_prefer()

#Load Required Objects ---------------------------------------------------------

load("data_regression/initial_split_reg.rda")
load("data_regression/base_recipe_regression.rda")

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
  set_mode('regression')

#Check tuning parameters--------------------------------------------------------

# parameters(rand_forest_spec)

# Set-up parameters ------------------------------------------------------------

rand_forest_parameters <- parameters(rand_forest_spec) %>% 
  update(mtry = mtry(range = c(2,10)))#There are 36 total variables which formed the basis of our range

# Define tuning grid------------------------------------------------------------

rand_forest_grid <- grid_regular(rand_forest_parameters, levels = c(5,5))

# Workflow----------------------------------------------------------------------

rand_forest_wflow <- 
  workflow() %>% 
  add_recipe(bank_loan_recipe_reg) %>% 
  add_model(rand_forest_spec)

# Tuning/fitting ---------------------------------------------------------------

tic("Random Forest")

#Tune grid----------------------------------------------------------------------

rand_forest_tuned <- rand_forest_wflow %>% 
  tune_grid(resamples = bank_loan_folds_reg, grid = rand_forest_grid, control = keep_pred, metrics = bank_loan_metrics)

#Check results------------------------------------------------------------------ 

autoplot(rand_forest_tuned, metric = "rmse")

#select best

rand_forest_best <- select_best(rand_forest_tuned, metric = "rmse")


# save runtime info
toc(log = TRUE)

time_log <- tic.log(format = FALSE)

rand_forest_tictoc <- tibble(
  model = time_log[[1]]$msg, 
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)


# End parallel processing 

stopCluster(cl)

# Save results & workflow

save(rand_forest_tuned, rand_forest_wflow, file = "model_info_regression/rand_forest_regression.rda")
