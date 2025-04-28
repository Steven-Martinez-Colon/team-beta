# Edited by Steven Martinez

############## Loading Libraries Function ####################

load_libraries <- function(packages) {
  # Check for missing packages
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # install missing packages
  if(length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
  
  # Load all packages
  lapply(packages, library, character.only = TRUE)
  
  cat("All packages are loaded succesfully.\n")
}


# Loading necessary libraries
load_libraries(c("tidyverse", "lubridate", "stats", "ggplot2", "corrplot", "stringr", "stringi", "class",
                 "tidymodels", "modeldata", "themis", "vip", "baguette", "janitor", "rvest",
                 "yardstick", "gsheet", "caret", "randomForest", "here", "tibble", "dplyr", "ISAR",
                 "tidyr", "mgcv", "teamcolors", "baseballr", "Lahman", "remotes", "ggcorrplot", "broom", "readr",
                 "glmnet", "xgboost", "Matrix", "Metrics", "reshape2", "DMwR2", "smotefamily"))

# Load only the necessary functions from 'car'
library(car, exclude = "select")

# Turning off warning messages
options(warnings = 0)

############################## Loading Dataset ####################################

# Loading dataset
balanced_df <- read_csv("final_data/balanced_rf_data.csv")

# Loading Dataset
balanced_df <- read.csv("images/rf_data.csv", row.names = 1, check.names = F)


################################# Random Forest with Predictors - 4 classes ##############################

# Removing special characters in order to perform Random Forest
balanced_df <- balanced_df %>% rename_with(make.names)

# Set seed
set.seed(123)

# Define your target variable
target_var <- "Team.Success"  

# Create stratified folds
balanced_df$fold <- caret::createFolds(balanced_df[[target_var]], k = 10, list = FALSE)

# Initialize results storage
results_df <- data.frame(Fold = integer(), Validation_Accuracy = numeric(), Cumulative_Avg = numeric(), Training_OOB_Accuracy = numeric())

# List to store importance matrices
importance_list <- list()

# _______________________________________________

# Cross-Validation Loop with SMOTE and Random Forest
#
# Purpose:
#   - Perform 10-fold stratified cross-validation to evaluate 
#     random forest model performance.
#   - Apply SMOTE oversampling only to the training folds to address 
#     class imbalance without introducing data leakage.
#   - Train random forest models on the SMOTE-augmented training data.
#   - Evaluate model performance on real (untouched) validation data.
#   - Record validation accuracy and training OOB (out-of-bag) accuracy 
#     for each fold to monitor for overfitting.
#   - Save fold-specific variable importance scores to later compute 
#     average importance across all folds.
#   - Save predictions across all folds to generate a final 
#     cross-validation confusion matrix.
#
# Key Notes:
#   - SMOTE is applied **inside** the loop, **after** splitting, 
#     and **only** to training data.
#   - Validation data remains untouched to ensure honest evaluation.
#   - Importance scores are aggregated across folds for robust 
#     feature importance analysis.

for (i in 1:10) {
  
  # Split into training and validation
  train_data <- balanced_df %>% filter(fold != i) %>% select(-fold)
  valid_data <- balanced_df %>% filter(fold == i) %>% select(-fold)
  
  # Make sure target variable is a factor
  train_data[[target_var]] <- as.factor(train_data[[target_var]])
  
  # Apply SMOTE only to training data
  smote_result <- SMOTE(X = train_data %>% select(-!!sym(target_var)),
                        target = train_data[[target_var]])
  
  # smote_result$data has the SMOTE'd dataset (features + label as last column)
  train_data_smote <- smote_result$data
  names(train_data_smote)[ncol(train_data_smote)] <- target_var  # Rename target properly
  
  # Make sure target is still a factor after SMOTE
  train_data_smote[[target_var]] <- as.factor(train_data_smote[[target_var]])
  
  # Train random forest on SMOTE data
  rf_model <- randomForest(formula = as.formula(paste(target_var, "~ .")),
                           data = train_data_smote,
                           importance = TRUE,
                           mtry = 8)
  
  # Predict on validation
  predictions <- predict(rf_model, newdata = valid_data)
  
  # Initialize if first fold / This is to keep track of predictions to create a confusion matrix later
  if (i == 1) {
    all_predictions <- data.frame(True = valid_data[[target_var]], Predicted = predictions)
  } else {
    all_predictions <- rbind(all_predictions, 
                             data.frame(True = valid_data[[target_var]], Predicted = predictions))
  }
  
  # Calculate accuracies
  validation_accuracy <- mean(predictions == valid_data[[target_var]])
  training_oob_accuracy <- 1 - rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
  
  # Save variable importance for this fold
  fold_importance <- importance(rf_model)
  fold_importance_df <- data.frame(Variable = rownames(fold_importance), fold_importance)
  importance_list[[i]] <- fold_importance_df
  
  # Add results to dataframe
  results_df <- rbind(results_df, 
                      data.frame(Fold = i, 
                                 Validation_Accuracy = validation_accuracy,
                                 Cumulative_Avg = mean(results_df$Validation_Accuracy, na.rm = TRUE) * (i-1)/i + validation_accuracy/i,
                                 Training_OOB_Accuracy = training_oob_accuracy))
  
  cat("Fold", i, 
      "- Validation Accuracy:", round(validation_accuracy, 4),
      "- Training OOB Accuracy:", round(training_oob_accuracy, 4), "\n")
  
}

# Note:
# SMOTE was applied to training folds inside cross-validation to handle class imbalance.
# Validation folds remained untouched to ensure honest model evaluation.

# _______________________________________________

# Final cross-validation results table
print(results_df)

# Final average accuracy
print(round(mean(results_df$Validation_Accuracy), 4))

# ______________________________________

# Reshape results to long format for plotting.
# Creates 'Type' (Validation vs Training) and 'Accuracy' columns for ggplot.
results_df_long <- results_df %>%
  tidyr::pivot_longer(cols = c(Validation_Accuracy, Training_OOB_Accuracy), 
                      names_to = "Type", 
                      values_to = "Accuracy")

# Plot Training OOB Accuracy vs Validation Accuracy by Fold
# Helps visually check for overfitting.
# Lines close together = No overfitting.
# Large gaps = Potential overfitting.

ggplot(results_df_long, aes(x = Fold, y = Accuracy, color = Type)) +
  geom_line(aes(group = Type)) +
  geom_point(size = 3) +
  labs(
    title = "Cross-Validation Check: Training vs Validation Accuracy",
    subtitle = "Healthy models show lines close together (no overfitting)",
    x = "Fold Number",
    y = "Accuracy",
    color = "Accuracy Type"
  ) +
  ylim(0.78, 1.0) +
  theme_minimal()

# ____________________________________

# Summarizing the predictions in order to create the confusion matrix
conf_matrix_df <- all_predictions %>%
  count(True, Predicted)


# Plot confusion matrix for cross-validation results
ggplot(conf_matrix_df, aes(x = True, y = Predicted)) +
  geom_tile(aes(fill = n), colour = "white") +
  geom_text(aes(label = n), vjust = 0.5, fontface = "bold") +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
  labs(
    title = "Distribution of Team Success Across Cross-Validation Predictions",
    subtitle = "Random Forest Model (10-Fold CV)",
    x = "Actual Team Success",
    y = "Predicted Team Success",
    fill = "Count"
  ) +
  theme_minimal(base_size = 14)

# ____________________________________

# Averaging Variable Importance Across Cross-Validation Folds
#
# - Combine all fold-specific variable importance tables
# - Average MeanDecreaseAccuracy across folds
# - Sort variables by their average importance
# - Plot the top 10 most important variables
#
# Provides a more robust and stable estimate of feature importance
# compared to using a single model's importance values.

# Combine all importance dataframes
all_importance_df <- bind_rows(importance_list, .id = "Fold")

# Now average across folds
average_importance <- all_importance_df %>%
  group_by(Variable) %>%
  summarize(across(where(is.numeric), mean)) %>%
  arrange(desc(MeanDecreaseAccuracy))

# View top variables
head(average_importance)


# Top 10 variables plot
average_importance %>%
  top_n(10, wt = MeanDecreaseAccuracy) %>%
  ggplot(aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Most Important Variables (Cross-Validated)",
       x = "Variable",
       y = "Mean Decrease in Accuracy") +
  theme_minimal()

# ____________________________________

################################# Random Forest with Predictors - Two classes ##############################

# Loading dataset
binary_balanced_df <- read.csv("images/rf_data.csv", row.names = 1, check.names = F)

# Removing special characters in order to perform Random Forest
binary_balanced_df <- binary_balanced_df %>% rename_with(make.names)

# Changing Team Success to be binary (1 = Playoff team, 0 = Didn't make playoff)
binary_balanced_df$Team.Success <- ifelse(binary_balanced_df$Team.Success == 1,0,1)


# Set seed
set.seed(123)

# Define your target variable
target_var <- "Team.Success"

# Create stratified folds
binary_balanced_df$fold <- caret::createFolds(binary_balanced_df[[target_var]], k = 10, list = FALSE)

# Initialize results storage
results_df <- data.frame(Fold = integer(), Validation_Accuracy = numeric(), Cumulative_Avg = numeric(), Training_OOB_Accuracy = numeric())

# List to store importance matrices
importance_list <- list()

# _______________________________________________

# Cross-Validation Loop:
# - Train random forest on training folds
# - Evaluate on validation fold
# - Record validation and OOB training accuracies
# Purpose: Check model performance and monitor for overfitting.

for (i in 1:10) {
  
  # Split into training and validation
  train_data <- binary_balanced_df %>% filter(fold != i) %>% select(-fold)
  valid_data <- binary_balanced_df %>% filter(fold == i) %>% select(-fold)
  
  # Make sure target variable is a factor
  train_data[[target_var]] <- as.factor(train_data[[target_var]])
  
  # Apply SMOTE only to training data
  smote_result <- SMOTE(X = train_data %>% select(-!!sym(target_var)),
                        target = train_data[[target_var]])
  
  # smote_result$data has the SMOTE'd dataset (features + label as last column)
  train_data_smote <- smote_result$data
  names(train_data_smote)[ncol(train_data_smote)] <- target_var  # Rename target properly
  
  # Make sure target is still a factor after SMOTE
  train_data_smote[[target_var]] <- as.factor(train_data_smote[[target_var]])
  
  # Train random forest on SMOTE data
  rf_model <- randomForest(formula = as.formula(paste(target_var, "~ .")),
                           data = train_data_smote,
                           importance = TRUE,
                           mtry = 8)
  
  # Predict on validation
  predictions <- predict(rf_model, newdata = valid_data)
  
  # Initialize if first fold / This is to keep track of predictions to create a confusion matrix later
  if (i == 1) {
    all_predictions <- data.frame(True = valid_data[[target_var]], Predicted = predictions)
  } else {
    all_predictions <- rbind(all_predictions, 
                             data.frame(True = valid_data[[target_var]], Predicted = predictions))
  }
  
  # Calculate accuracies
  validation_accuracy <- mean(predictions == valid_data[[target_var]])
  training_oob_accuracy <- 1 - rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
  
  # Save variable importance for this fold
  fold_importance <- importance(rf_model)
  fold_importance_df <- data.frame(Variable = rownames(fold_importance), fold_importance)
  importance_list[[i]] <- fold_importance_df
  
  # Add results to dataframe
  results_df <- rbind(results_df, 
                      data.frame(Fold = i, 
                                 Validation_Accuracy = validation_accuracy,
                                 Cumulative_Avg = mean(results_df$Validation_Accuracy, na.rm = TRUE) * (i-1)/i + validation_accuracy/i,
                                 Training_OOB_Accuracy = training_oob_accuracy))
  
  cat("Fold", i, 
      "- Validation Accuracy:", round(validation_accuracy, 4),
      "- Training OOB Accuracy:", round(training_oob_accuracy, 4), "\n")
  
}

# _______________________________________________

# Final cross-validation results table
print(results_df)

# Final average accuracy
print(round(mean(results_df$Validation_Accuracy), 4))

# ______________________________________

# Reshape results to long format for plotting.
# Creates 'Type' (Validation vs Training) and 'Accuracy' columns for ggplot.
results_df_long <- results_df %>%
  tidyr::pivot_longer(cols = c(Validation_Accuracy, Training_OOB_Accuracy), 
                      names_to = "Type", 
                      values_to = "Accuracy")

# Plot Training OOB Accuracy vs Validation Accuracy by Fold
# Helps visually check for overfitting.
# Lines close together = No overfitting.
# Large gaps = Potential overfitting.

ggplot(results_df_long, aes(x = Fold, y = Accuracy, color = Type)) +
  geom_line(aes(group = Type)) +
  geom_point(size = 3) +
  labs(
    title = "Cross-Validation Check: Training vs Validation Accuracy",
    subtitle = "Healthy models show lines close together (no overfitting)",
    x = "Fold Number",
    y = "Accuracy",
    color = "Accuracy Type"
  ) +
  ylim(0.9, 1.0) +
  theme_minimal()

# ____________________________________

# Summarizing the predictions in order to create the confusion matrix
conf_matrix_df <- all_predictions %>%
  count(True, Predicted)


# Plot confusion matrix for cross-validation results
ggplot(conf_matrix_df, aes(x = True, y = Predicted)) +
  geom_tile(aes(fill = n), colour = "white") +
  geom_text(aes(label = n), vjust = 0.5, fontface = "bold") +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
  labs(
    title = "Distribution of Team Success Across Cross-Validation Predictions",
    subtitle = "Random Forest Model (5-Fold CV)",
    x = "Actual Team Success",
    y = "Predicted Team Success",
    fill = "Count"
  ) +
  theme_minimal(base_size = 14)

# ____________________________________

# Averaging Variable Importance Across Cross-Validation Folds
#
# - Combine all fold-specific variable importance tables
# - Average MeanDecreaseAccuracy across folds
# - Sort variables by their average importance
# - Plot the top 10 most important variables
#
# Provides a more robust and stable estimate of feature importance
# compared to using a single model's importance values.

# Combine all importance dataframes
all_importance_df <- bind_rows(importance_list, .id = "Fold")

# Now average across folds
average_importance <- all_importance_df %>%
  group_by(Variable) %>%
  summarize(across(where(is.numeric), mean)) %>%
  arrange(desc(MeanDecreaseAccuracy))

# View top variables
head(average_importance)


# Top 10 variables plot
average_importance %>%
  top_n(10, wt = MeanDecreaseAccuracy) %>%
  ggplot(aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Most Important Variables (Cross-Validated)",
       x = "Variable",
       y = "Mean Decrease in Accuracy") +
  theme_minimal()

# ____________________________________












