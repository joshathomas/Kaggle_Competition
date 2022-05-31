#notes 5.9.2022

# Data Science Models 

# Ensamble Model

# Ensemble modeling is a process where multiple diverse models are created to predict an outcome, either by using many different modeling algorithms or using different training data sets.

# Example of ensamble model is a Random Forest Model 

# What is the Stacks package? 

# Model stacking is an ensemble technique that involves training a model to combine the outputs of many diverse statistical models, and has been shown to improve predictive performance in a variety of settings. 'stacks' implements a grammar for 'tidymodels'-aligned model stacking.

# Stacks workflow

# 1. Define candidate ensemble members using functionality from rsample, parsnip, workflows, recipes, and tune

# 2. Initialize a data_stack object with stacks()

# 3. Iteratively add candidate ensemble members to the data_stack with add_candidates()

# 4. Evaluate how to combine their predictions with blend_predictions()

# The outputs from each of these candidate ensemble members are highly correlated, so the blend_predictions() function performs regularization to figure out how we can combine the outputs (includes how much to weight each model) from the stack members to come up with a final prediction.

# 5. Fit candidate ensemble members with non-zero stacking coefficients with fit_members()



# 6. Predict on new data with predict()!



# Candidate models are variations within a model class with slightly different parameters. 

# You basically use lasso regression because it tells you what predictors (in our case models) are important. 

