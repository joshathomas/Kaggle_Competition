# Bank Loan Classification Competition

# Load Libraries

library(tidyverse)
library(tidymodels)


# Load Data---------------------------------------------------------------------

codebook <- readxl::read_excel("Kaggle_Competition/data_classification/Codebook_cl.xlsx")

bank_loan_test_class <- read_csv("Kaggle_Competition/data_classification/test.csv") %>% 
  janitor::clean_names() %>% 
  mutate(
    earliest_cr_line = lubridate::my(earliest_cr_line),
    last_credit_pull_d = lubridate::my(last_credit_pull_d), 
    addr_state = factor(addr_state,levels = c("CT", "CA", "NY", "NJ", "MO", "CO", "FL", "IL", "TX", "PA", "GA", "MI", "NC", "OH", "TN", "VA","WI", "MD", "AZ", "MA", "MS", "WA", "SC", "RI", "UT", "OK", "OR", "NV", "IN", "MT", "WV", "KS", "DE", "NH", "AL", "LA", "MN", "ME", "AR", "KY", "HI", "NM", "DC", "NE", "AK", "ND", "ID", "WY","VT")), 
    application_type = factor(application_type, levels = c("Individual", "Joint App")), 
    emp_length = factor(emp_length, levels = c("< 1 year", "1 year", "3 years", "2 years","8 years", "6 years",  "4 years" ,"7 years", "5 years", "9 years","10+ years"), ordered = TRUE), 
    grade = factor(grade, levels = c("A", "B", "C", "D", "E", "F", "G")),
    sub_grade = factor(sub_grade, levels= c("C1", "B5", "A2", "B2", "B4", "B3", "E3", "D1", "C5", "B1", "F1", "C4", "A1", "F5", "G4", "A5", "D2","D5", "A3", "C3", "C2", "G2", "E5", "A4", "E1", "E2", "D3", "D4", "F2", "F3", "F4", "E4", "G3", "G1", "G5")),
    home_ownership = factor(home_ownership, levels = c("RENT",  "OWN", "MORTGAGE", "ANY")), 
    delinq_2yrs = factor(delinq_2yrs, delinq_2yrs == 0, delinq_2yrs>0), 
    initial_list_status = factor(initial_list_status, levels = c("w", "f")), 
    term = factor(term, levels = c("36 months", "60 months")), 
    verification_status = factor(verification_status, levels = c("Verified", "Not Verified", "Source Verified"))
  )

bank_loan_test_reg <- read_csv("Kaggle_Competition/data_regression/test.csv") %>% 
  janitor::clean_names()%>% 
  mutate(
    earliest_cr_line = lubridate::my(earliest_cr_line),
    last_credit_pull_d = lubridate::my(last_credit_pull_d), 
    addr_state = factor(addr_state,levels = c("CT", "CA", "NY", "NJ", "MO", "CO", "FL", "IL", "TX", "PA", "GA", "MI", "NC", "OH", "TN", "VA","WI", "MD", "AZ", "MA", "MS", "WA", "SC", "RI", "UT", "OK", "OR", "NV", "IN", "MT", "WV", "KS", "DE", "NH", "AL", "LA", "MN", "ME", "AR", "KY", "HI", "NM", "DC", "NE", "AK", "ND", "ID", "WY","VT")), 
    application_type = factor(application_type, levels = c("Individual", "Joint App")), 
    emp_length = factor(emp_length, levels = c("< 1 year", "1 year", "3 years", "2 years","8 years", "6 years",  "4 years" ,"7 years", "5 years", "9 years","10+ years"), ordered = TRUE), 
    grade = factor(grade, levels = c("A", "B", "C", "D", "E", "F", "G")),
    sub_grade = factor(sub_grade, levels= c("C1", "B5", "A2", "B2", "B4", "B3", "E3", "D1", "C5", "B1", "F1", "C4", "A1", "F5", "G4", "A5", "D2","D5", "A3", "C3", "C2", "G2", "E5", "A4", "E1", "E2", "D3", "D4", "F2", "F3", "F4", "E4", "G3", "G1", "G5")),
    home_ownership = factor(home_ownership, levels = c("RENT",  "OWN", "MORTGAGE", "ANY")), 
    delinq_2yrs = factor(delinq_2yrs, delinq_2yrs == 0, delinq_2yrs>0), 
    initial_list_status = factor(initial_list_status, levels = c("w", "f")), 
    term = factor(term, levels = c("36 months", "60 months")), 
    verification_status = factor(verification_status, levels = c("Verified", "Not Verified", "Source Verified"))
  )


bank_loan_train_reg <- read_csv("Kaggle_Competition/data_regression/train.csv") %>%
  janitor::clean_names()%>% 
  mutate(
    earliest_cr_line = lubridate::my(earliest_cr_line),
    last_credit_pull_d = lubridate::my(last_credit_pull_d), 
    addr_state = factor(addr_state,levels = c("CT", "CA", "NY", "NJ", "MO", "CO", "FL", "IL", "TX", "PA", "GA", "MI", "NC", "OH", "TN", "VA","WI", "MD", "AZ", "MA", "MS", "WA", "SC", "RI", "UT", "OK", "OR", "NV", "IN", "MT", "WV", "KS", "DE", "NH", "AL", "LA", "MN", "ME", "AR", "KY", "HI", "NM", "DC", "NE", "AK", "ND", "ID", "WY","VT")), 
    application_type = factor(application_type, levels = c("Individual", "Joint App")), 
    emp_length = factor(emp_length, levels = c("< 1 year", "1 year", "3 years", "2 years","8 years", "6 years",  "4 years" ,"7 years", "5 years", "9 years","10+ years"), ordered = TRUE), 
    grade = factor(grade, levels = c("A", "B", "C", "D", "E", "F", "G")),
    sub_grade = factor(sub_grade, levels= c("C1", "B5", "A2", "B2", "B4", "B3", "E3", "D1", "C5", "B1", "F1", "C4", "A1", "F5", "G4", "A5", "D2","D5", "A3", "C3", "C2", "G2", "E5", "A4", "E1", "E2", "D3", "D4", "F2", "F3", "F4", "E4", "G3", "G1", "G5")),
    home_ownership = factor(home_ownership, levels = c("RENT",  "OWN", "MORTGAGE", "ANY")), 
    delinq_2yrs = factor(delinq_2yrs, delinq_2yrs == 0, delinq_2yrs>0), 
    initial_list_status = factor(initial_list_status, levels = c("w", "f")), 
    term = factor(term, levels = c("36 months", "60 months")), 
    verification_status = factor(verification_status, levels = c("Verified", "Not Verified", "Source Verified"))
  )


# term, interest rate, loan amount 

bank_loan_train_class <- read_csv("Kaggle_Competition/data_classification/train.csv") %>% 
  janitor::clean_names()%>% 
  mutate(
  earliest_cr_line = lubridate::my(earliest_cr_line),
  last_credit_pull_d = lubridate::my(last_credit_pull_d), 
  hi_int_prncp_pd = factor(hi_int_prncp_pd, levels = c(0,1)), 
  addr_state = factor(addr_state, levels = c("CT", "CA", "NY", "NJ", "MO", "CO", "FL", "IL", "TX", "PA", "GA", "MI", "NC", "OH", "TN", "VA","WI", "MD", "AZ", "MA", "MS", "WA", "SC", "RI", "UT", "OK", "OR", "NV", "IN", "MT", "WV", "KS", "DE", "NH", "AL", "LA", "MN", "ME", "AR", "KY", "HI", "NM", "DC", "NE", "AK", "ND", "ID", "WY","VT")), 
  application_type = factor(application_type, levels = c("Individual", "Joint App")), 
  emp_length = factor(emp_length, levels = c("< 1 year", "1 year", "3 years", "2 years","8 years", "6 years",  "4 years" ,"7 years", "5 years", "9 years","10+ years"), ordered = TRUE), 
  grade = factor(grade, levels = c("A", "B", "C", "D", "E", "F", "G")),
  sub_grade = factor(sub_grade, levels= c("C1", "B5", "A2", "B2", "B4", "B3", "E3", "D1", "C5", "B1", "F1", "C4", "A1", "F5", "G4", "A5", "D2","D5", "A3", "C3", "C2", "G2", "E5", "A4", "E1", "E2", "D3", "D4", "F2", "F3", "F4", "E4", "G3", "G1", "G5")),
  home_ownership = factor(home_ownership, levels = c("RENT",  "OWN", "MORTGAGE", "ANY")), 
  delinq_2yrs = factor(delinq_2yrs, delinq_2yrs == 0, delinq_2yrs>0), 
  initial_list_status = factor(initial_list_status, levels = c("w", "f")), 
  term = factor(term, levels = c("36 months", "60 months")), 
  verification_status = factor(verification_status, levels = c("Verified", "Not Verified", "Source Verified"))
  )


#Data Folds---------------------------------------------------------------------


bank_loan_folds_class <-  bank_loan_train_class %>% 
  vfold_cv(v = 5, repeats = 3, strata = hi_int_prncp_pd) #the more folds (v) the smaller the resamples become

bank_loan_folds_reg <-bank_loan_train_reg %>% 
  vfold_cv(v = 5, repeats = 3, strata = money_made_inv) #the more folds (v) the smaller the resamples become


# EDA --------------------------------------------------------------------------

#We will check the whole training set for missing data and then counduct the rest of the eda on the split training set.  

naniar::gg_miss_var(bank_loan_dat_reg)

naniar::miss_case_table(bank_loan_dat_reg)


naniar::gg_miss_var(bank_loan_dat_class)

naniar::miss_case_table(bank_loan_dat_class)

# There are no missing values in this dataset. 

# Variable class check (i.e. character, numeric, etc.).-------------------------

# get list of variable classes in console

lapply(bank_loan_train_class, class)

# We see numeric variables and characters in our dataset. 

# Handling character variables-------------------------------------------------- 

# application_type = factor 

# earliest_cr_line = date (lubridate package)

# emp_length = factor (it could also be treated as a numeric however there are categories such as <1 year and 10+ years that would be non-specific numbers)

# emp_title = factor (the names themselves may not be usuful but you could use them to categorize job types and industries)

# grade = factor 

# home_ownership = factor

# initial_list_status = factor 

# last_credit_pull_d = date

# purpose = factor

# term = numeric 

# verification_status = factor 


# Variable Spread---------------------------------------------------------------


ggplot(data = bank_loan_train_class, aes(x= hi_int_prncp_pd))+
  geom_histogram(bins = 10) #target variable already dummy coded into 0 and 1

ggplot(data = bank_loan_train_class, aes(x= acc_now_delinq))+
  geom_histogram(bins = 5) #most people are not delinquent on their accounts, those that are, are only deliquent on one account. not normally distributed

ggplot(data = bank_loan_train_class, aes(x= acc_open_past_24mths))+
  geom_histogram(bins = 30) #acc_open_past_24mths is skewed to the right. Most people have open between 0 and 10 accounts in the past 24 months.

moments::skewness(bank_loan_train_class$acc_open_past_24mths, na.rm = TRUE) #confirms positive skew

ggplot(data = bank_loan_train_class, aes(x= sqrt(acc_open_past_24mths)))+
  geom_histogram(bins = 30)#square root helps with the positive skew of the data 

ggplot(data = bank_loan_train_class, aes(x= log10(annual_inc)))+
  geom_histogram(bins = 10) #most people in our dataset have middle class incomes (right-skewed), log10 transformation helps with the distribution some. 

moments::skewness(bank_loan_train_class$annual_inc, na.rm = TRUE) #confirms positive skew

ggplot(data = bank_loan_train_class, aes(x= avg_cur_bal))+
  geom_histogram()# the average current balance of the accounts is skewed to left 

moments::skewness(bank_loan_train_class$annual_inc, na.rm = TRUE) #confirms positive skew

ggplot(data = bank_loan_train_class, aes(x= sqrt(max(bc_util+1)-bc_util)))+
  geom_histogram()#Ratio of total current balance to high credit/credit limit for all bankcard accounts is skewed to the left. 

moments::skewness(bank_loan_train_class$bc_util, na.rm = TRUE)#confirms left skew

ggplot(data = bank_loan_train_class, aes(x= log10(delinq_2yrs)))+
  geom_histogram(bins = 5)#The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years is skewed to the right. 

moments::skewness(bank_loan_train_class$delinq_2yrs, na.rm = TRUE)#confirms right skew

ggplot(data = bank_loan_train_class, aes(x= delinq_amnt))+
  geom_histogram(bins = 2)# The past-due amount owed for the accounts on which the borrower is now delinquent is uniformly 0.

ggplot(data = bank_loan_train_class, aes(x= dti))+
  geom_histogram()# A ratio calculated using the borrower’s total monthly debt payments on the total debt obligations, excluding mortgage and the requested LC loan, divided by the borrower’s self-reported monthly income is normally distributed. 

ggplot(data = bank_loan_train_class, aes(x= sqrt(int_rate)))+
  geom_histogram()# Interest Rate on the loan is slightly skewed to the right

moments::skewness(bank_loan_train_class$int_rate, na.rm = TRUE)#confirms slight right skew

ggplot(data = bank_loan_train_class, aes(x= sqrt(loan_amnt)))+
  geom_histogram()#The listed amount of the loan applied for by the borrower is slightly skewed to the right. 

moments::skewness(bank_loan_train_class$loan_amnt, na.rm = TRUE)#confirms slight right skew

ggplot(data = bank_loan_train_class, aes(x= 1/(mort_acc)))+
  geom_histogram(bins = 15) #Number of mortgage accounts is skewed to the right. 

moments::skewness(bank_loan_train_class$mort_acc, na.rm = TRUE)#confirms right skew

ggplot(data = bank_loan_train_class, aes(x= log10(num_sats)))+
  geom_histogram()# Number of satisfactory accounts is right skew

moments::skewness(bank_loan_train_class$num_sats, na.rm = TRUE)#confirms right skew

ggplot(data = bank_loan_train_class, aes(x= num_tl_120dpd_2m))+
  geom_histogram()#Number of accounts currently 120 days past due (updated in past 2 months) is universally zero

ggplot(data = bank_loan_train_class, aes(x= num_tl_90g_dpd_24m))+
  geom_histogram(bins = 5) #Very few accounts are 90 or more days past due in last 24 months.

ggplot(data = bank_loan_train_class, aes(x= num_tl_30dpd))+
  geom_histogram(bins = 5) #Very few accounts are currently 30 days past due (updated in past 2 months)


ggplot(data = bank_loan_train_class, aes(x= sqrt(out_prncp_inv)))+
  geom_histogram(bins = 10) # The remaining outstanding principal for portion of total amount funded by investors is skewed to the right. The most common value is 0. 

moments::skewness(bank_loan_train_class$out_prncp_inv, na.rm = TRUE)#confirms right skew

ggplot(data = bank_loan_train_class, aes(x= pub_rec))+
  geom_histogram(bins = 5)#Number of derogatory public records is overwhelmingly 0. This most likley will not provide useful information on the majority of accounts. For those with a record it will probably be important. 

ggplot(data = bank_loan_train_class, aes(x= pub_rec_bankruptcies))+
  geom_histogram(bins = 5)#Number of public record bankruptcies is mostly 0 meaning the variable is skewed to the right. 

ggplot(data = bank_loan_train_class, aes(x= log10(tot_coll_amt)))+
  geom_histogram(bins = 10)#Total collection amounts ever owed is skewed to the right. 

moments::skewness(bank_loan_train_class$tot_coll_amt, na.rm = TRUE)#confirms right skew

ggplot(data = bank_loan_train_class, aes(x= log10(tot_cur_bal)))+
  geom_histogram(bins = 20)#Total current balance of all accounts is skewed to the right. 

moments::skewness(bank_loan_train_class$tot_cur_bal, na.rm = TRUE)#confirms right skew

ggplot(data = bank_loan_train_class, aes(x= log10(total_rec_late_fee)))+
  geom_histogram()#Late fees received to date is skewed to the right. 

moments::skewness(bank_loan_train_class$total_rec_late_fee, na.rm = TRUE)#confirms right skew

# Given our overview of the shape of our numeric variables we can colcude that they are largely skewed to the right. We most likely will need to transform them with a log10 or Yeo-Jonhson step in our recipe. 

#Bivariate relationships--------------------------------------------------------

bank_loan_train_numeric <-select_if(bank_loan_train_class, is.numeric) %>% 
  as_tibble()

bank_loan_cor <- cor(bank_loan_train_numeric)

corrplot::corrplot(bank_loan_cor, type = "upper", method = "square", tl.cex = 0.5)

# Our table shows us that most of our variables are not correlated. The variables that are correlated include (but are not limited to) the following hi_int_prncp_pd and out_prncp_inv, acc_now_delinq and num_tl_120dpd_2m, num_tl_30dpd and acc_now_delinq, acc_open_past_24mths and num_stats. 

#Use Models

# usemodels::use_earth(money_made_inv~., data = loan_data, clipboard = TRUE)

#Save Initial Set-Up------------------------------------------------------------

save(bank_loan_train_class, bank_loan_test_class, bank_loan_folds_class,file = "Kaggle_Competition/data_classification/initial_split_class.rda")

save(bank_loan_train_reg, bank_loan_test_reg, bank_loan_folds_reg,file = "Kaggle_Competition/data_regression/initial_split_reg.rda")




