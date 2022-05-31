#Bank Loan Regression Basic Recipe


# Load Recipes------------------------------------------------------------------

library(tidyverse)
library(tidymodels)

tidymodels_prefer()

# Load data---------------------------------------------------------------------

load("Kaggle_Competition/data_regression/initial_split_reg.rda")

#Base Recipe--------------------------------------------------------------------

#Use Models

# usemodels::use_earth(money_made_inv~annual_inc + acc_open_past_24mths+ avg_cur_bal + bc_util + delinq_2yrs + delinq_amnt + dti + int_rate + loan_amnt + mort_acc + num_sats + num_tl_120dpd_2m + num_tl_90g_dpd_24m + num_tl_30dpd + out_prncp_inv + pub_rec + pub_rec_bankruptcies + tot_coll_amt + tot_cur_bal + total_rec_late_fee + earliest_cr_line + last_credit_pull_d, data = loan_data, clipboard = TRUE)

bank_loan_recipe_reg <-recipe(money_made_inv ~ annual_inc + acc_open_past_24mths+ avg_cur_bal + bc_util + delinq_2yrs + delinq_amnt + dti + int_rate + loan_amnt + mort_acc + num_sats + num_tl_120dpd_2m + num_tl_90g_dpd_24m + num_tl_30dpd + out_prncp_inv + pub_rec + pub_rec_bankruptcies + tot_coll_amt + tot_cur_bal + total_rec_late_fee + sub_grade + term , data = bank_loan_train_reg) %>% 
  # step_rm(emp_title, id) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  step_novel(all_nominal_predictors()) %>% # assigns a unseen factor level to a factor (variable) that already existed, comes up when you fold the data alot, always put this in 
  step_dummy(all_nominal_predictors()) %>% #not the supervising variable, because the outcome variable should only be one variable , the computer will do the seperation by itself, its a software specific issue
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_zv(all_predictors()) %>% # stands for step zero variance, always put this in recipe
  step_nzv() %>%   #stands for near zero variance, we have several variables that are near zero variance that will provide little information
  step_normalize(all_numeric_predictors()) #you can't normalize dates


#Recipe Check-------------------------------------------------------------------

# bank_loan_recipe_reg %>%
#   prep() %>%
#   bake(new_data = NULL) %>% 
#   view()

#Set Control Grid---------------------------------------------------------------

#This code allows you to get the metrics of all of our models without saving 
# the workflow individually. 

keep_pred <- control_grid(save_pred = TRUE, save_workflow = TRUE)

#Set Metrics Set----------------------------------------------------------------

bank_loan_metrics <- metric_set(
  rmse
)

#Save Recipe--------------------------------------------------------------------

save(bank_loan_recipe_reg, keep_pred, bank_loan_metrics, file = "Kaggle_Competition/data_regression/base_recipe_regression.rda")

#Load Model Results-------------------------------------------------------------

