
# This file reads all the input files and joins the data as needed
# Also has commands to understand data
source("read_input.R")

# Preliminary data exploration, however further data analysis was moved to Tableau 
# by saving the dataset to CSV file and load in Tableau
source("data_explore.R")

# Features are defined in this file based on all Prior Order data
source("features.R")

# This files combines all the features, creates the complete training and test datasets
source("create_sets.R")

# Training set exploration of features.
source("features_explore.R")

# Based on feature exploration important features are selected
# Different Models are run and compared before performing on the test data
source("models.R")

