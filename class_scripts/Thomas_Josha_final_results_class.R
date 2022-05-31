# Finalize Prediction tables 

# Load Packages 

library(tidymodels)
library(tidyverse)

# Load Data

load("data_classification/initial_split_class.rda")
load("model_info_classification/rand_forest_classification.rda")

# Classification Final Results 

# Rand Forest

# Extract Workflow

rand_forest_workflow <- extract_workflow(rand_forest_tuned_class)

# Finalize Workflow

rand_forest_workflow_tuned <- rand_forest_workflow %>%
  finalize_workflow(select_best(rand_forest_tuned_class, metric = "roc_auc"))

# Fit Model 

rand_forest_fit <- fit(rand_forest_workflow_tuned, data = bank_loan_train_class)

# Clean Test Data

bank_loan_test_class  <- bank_loan_test_class %>% 
  mutate(earliest_cr_line = lubridate::my(earliest_cr_line), 
         last_credit_pull_d = lubridate::my(last_credit_pull_d))

rand_forest_final_class <- predict(rand_forest_fit, new_data = bank_loan_test_class) %>%
  bind_cols(bank_loan_test_class %>% select(id)) %>%
  mutate( Id = id, Category = .pred_class) %>%
  select(Id, Category)


write_csv(rand_forest_final_class, file = "model_info_classification/rand_forest_class_results.csv")

# Boost Tree 

# Load Data

load("data_classification/initial_split_class.rda")
load("model_info_classification/boost_tree.rda")

# Extract Workflow

boost_tree_workflow <- extract_workflow(boost_tree_tuned)

# Finalize Workflow

boost_tree_workflow_tuned <- boost_tree_workflow %>%
  finalize_workflow(select_best(boost_tree_tuned, metric = "roc_auc"))

# Fit Model 

boost_tree_fit <- fit(boost_tree_workflow_tuned, data = bank_loan_train_class)

# Clean Test Data

bank_loan_test_class  <- bank_loan_test_class %>% 
  select(c(annual_inc, acc_open_past_24mths, avg_cur_bal, bc_util, delinq_2yrs, delinq_amnt, dti, int_rate, loan_amnt, mort_acc, num_sats, num_tl_120dpd_2m, num_tl_90g_dpd_24m, num_tl_30dpd, out_prncp_inv, pub_rec, pub_rec_bankruptcies, tot_coll_amt, tot_cur_bal, total_rec_late_fee, term, sub_grade, id, initial_list_status))

boost_tree_final_class <- predict(boost_tree_fit, new_data = bank_loan_test_class) %>%
  bind_cols(bank_loan_test_class %>% select(id)) %>%
  mutate( Id = id, Category = .pred_class) %>%
  select(Id, Category)


write_csv(boost_tree_final_class, file = "model_info_classification/boost_tree_class_results.csv")

