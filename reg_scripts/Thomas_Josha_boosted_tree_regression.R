# {Boosted Tree Forest} tuning--------------------------------------------------


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

# bank_loan_recipe_reg <- bank_loan_recipe_reg %>%
#   step_interact(bank_loan_train_reg ~ all_numeric_predictors()^2)


#Define Model-------------------------------------------------------------------

boost_tree_spec <-
  boost_tree(trees = tune(),learn_rate = tune(), min_n = tune(), loss_reduction = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

#Check tuning parameters--------------------------------------------------------

# parameters(boost_tree_spec)

# # Set-up parameters ------------------------------------------------------------

boost_tree_parameters <- parameters(boost_tree_spec) %>%
  update(min_n = min_n(range = c(2,20))) #There are 36 total variables which formed the basis of our range

# # Define tuning grid------------------------------------------------------------
# 
# boost_tree_grid <- grid_regular(boost_tree_parameters, levels = c(4))

# Workflow----------------------------------------------------------------------

boost_tree_wflow <- 
  workflow() %>% 
  add_recipe(bank_loan_recipe_reg) %>% 
  add_model(boost_tree_spec)

# Tuning/fitting ---------------------------------------------------------------

tic("Random Forest")

#Tune grid----------------------------------------------------------------------

boost_tree_tuned <- boost_tree_wflow %>%
  tune_grid(resamples = bank_loan_folds_reg, control = keep_pred, metrics = bank_loan_metrics)

#Check results------------------------------------------------------------------ 

autoplot(boost_tree_tuned, metric = "rmse")

#select best

boost_tree_best <- select_best(boost_tree_tuned, metric = "rmse")


# save runtime info
toc(log = TRUE)

time_log <- tic.log(format = FALSE)

boost_tree_tictoc <- tibble(
  model = time_log[[1]]$msg, 
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)


# End parallel processing 

stopCluster(cl)

# Save results & workflow

save(boost_tree_tuned, boost_tree_wflow, file = "model_info_regression/boost_tree_regression.rda")
