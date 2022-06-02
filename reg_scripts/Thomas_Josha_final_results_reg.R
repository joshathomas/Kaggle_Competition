# Finalize Prediction tables 

# Load Packages 

library(tidymodels)
library(tidyverse)

# Load Data

load("data_regression/initial_split_reg.rda")
load("model_info_regression/rand_forest_regression.rda")

# Regression Final Results 

#Random Forest

# Extract Workflow

rand_forest_workflow <- extract_workflow(rand_forest_tuned)

# Finalize Workflow

rand_forest_workflow_tuned <- rand_forest_workflow %>%
  finalize_workflow(select_best(rand_forest_tuned, metric = "rmse"))

# Fit Model 

rand_forest_fit <- fit(rand_forest_workflow_tuned, data = bank_loan_train_reg)

# Clean Test Data

bank_loan_test_reg  <- bank_loan_test_reg %>% 
  mutate(earliest_cr_line = lubridate::my(earliest_cr_line), 
         last_credit_pull_d = lubridate::my(last_credit_pull_d))

rand_forest_final_reg <- predict(rand_forest_fit, new_data = bank_loan_test_reg) %>%
  bind_cols(bank_loan_test_reg %>% select(id)) %>%
  mutate(Id = id, Predicted = .pred) %>%
  select(Id, Predicted)


write_csv(rand_forest_final_reg, file = "model_info_regression/rand_forest_reg_results.csv")


# Boosted Tree

# Load Data

load("data_regression/initial_split_reg.rda")
load("model_info_regression/boost_tree_regression.rda")

# Extract Workflow

boost_tree_workflow <- extract_workflow(boost_tree_tuned)

# Finalize Workflow

boost_tree_workflow_tuned <- boost_tree_workflow %>%
  finalize_workflow(select_best(boost_tree_tuned, metric = "rmse"))

# Fit Model 

boost_tree_fit <- fit(boost_tree_workflow_tuned, data = bank_loan_train_reg)

# Clean Test Data

bank_loan_test_reg  <- bank_loan_test_reg %>% 
  select(c(annual_inc,acc_open_past_24mths, avg_cur_bal,bc_util, delinq_2yrs,delinq_amnt, dti, int_rate, loan_amnt, mort_acc, num_sats, num_tl_120dpd_2m, num_tl_90g_dpd_24m, num_tl_30dpd, out_prncp_inv, pub_rec, pub_rec_bankruptcies, tot_coll_amt, tot_cur_bal, total_rec_late_fee, sub_grade, id, term))

boost_tree_final_reg <- predict(boost_tree_fit, new_data = bank_loan_test_reg) %>%
  bind_cols(bank_loan_test_reg %>% select(id)) %>%
  mutate(Id = id, Predicted = .pred) %>%
  select(Id, Predicted)


write_csv(boost_tree_final_reg, file = "model_info_regression/boost_tree_reg_results.csv")

