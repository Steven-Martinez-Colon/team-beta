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

# Loading Dataset
balanced_df <- read.csv("images/rf_data.csv", row.names = 1, check.names = F)

################################# Random Forest with Predictors - 4 classes ##############################

# Removing special characters in order to perform Random Forest
balanced_df <- balanced_df %>% rename_with(make.names)

# Changing Team Success as a factor
#balanced_df$Team.Success <-  as.factor(balanced_df$Team.Success)
#str(balanced_df)

# Loading the calcSplitRatio function from GitHub
source("code/calcSplitRatio-3.R")

# Finding the ideal split ratio
calcSplitRatio(p = 78, balanced_df) # Split data into training (89%) and testing (11%) sets

# Set seed
set.seed(123)

# Stratified split using createDataPartition
train_index <- createDataPartition(balanced_df$`Team.Success`, p = 0.89, list = FALSE)
train <- balanced_df[train_index, ]
test <- balanced_df[-train_index, ]

# Define your target variable
target_var <- "Team.Success"

# Create stratified folds
train$fold <- caret::createFolds(train[[target_var]], k = 10, list = FALSE)

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
  train_data <- train %>% filter(fold != i) %>% select(-fold)
  valid_data <- train %>% filter(fold == i) %>% select(-fold)
  
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

# Final average validation accuracy
print(round(mean(results_df$Validation_Accuracy), 4))

# Final average training accuracy
print(round(mean(results_df$Training_OOB_Accuracy), 4))

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
  geom_text(aes(label = round(MeanDecreaseAccuracy, 1)), 
            hjust = -0.1, size = 4, fontface = "bold") +  # Labels just outside bars
  coord_flip() +
  labs(title = "Top 10 Most Important Variables (Cross-Validated)",
       x = "Variable",
       y = "Mean Decrease in Accuracy") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold")) +
  expand_limits(y = max(average_importance$MeanDecreaseAccuracy) * 1.1)  # Ensure space for text


# ______________ Creating Predictions on Test _______________________

# Predict on test
rf_pred <- predict(rf_model, newdata = test)

# Evaluating random forest model
confusionMatrix(rf_pred, as.factor(test$`Team.Success`))

# Creating a heatmap table for the confusion matrix
rf_conf_matrix <- table(Predicted = rf_pred, Actual = test$`Team.Success`) # Create the table as a matrix
rf_conf_df <- as.data.frame(rf_conf_matrix) # Convert to data frame for ggplot

# Plot heatmap
ggplot(rf_conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
  labs(title = "Distribution of Team Success Across Predictions",
       subtitle = "Random Forest",
       x = "Team Success",
       y = "Predicted",
       fill = "Count") +
  theme_minimal(base_size = 14)

# ______________ ROC Curves ___________________________-

## Create multiclass ROC curve

# Get predicted class probabilities (not class labels)
probs <- as.data.frame(predict(rf_model, newdata = test, type = "prob"))


# List to store ROC data frames and AUC values
roc_dfs <- list()
auc_labels <- c()

# Loop over each class
for (class in levels(test$Team.Success)) {
  # Binary response: 1 if current class, 0 otherwise
  binary_response <- as.factor(ifelse(test$Team.Success == class, class, paste0("not_", class)))
  
  # Predicted probabilities for this class
  predictor <- probs[, class]
  
  # Calculate ROC
  roc_obj <- roc(binary_response, predictor)
  
  # Store ROC curve data
  roc_df <- data.frame(
    Specificity = rev(roc_obj$specificities),
    Sensitivity = rev(roc_obj$sensitivities),
    Class = class
  )
  roc_dfs[[class]] <- roc_df
  
  # Get AUC value and build label
  auc_val <- auc(roc_obj)
  auc_labels <- c(auc_labels, paste0(class, " (AUC = ", round(auc_val, 3), ")"))
}

# Combine ROC curves into one data frame
all_roc_df <- bind_rows(roc_dfs)
all_roc_df$Class <- factor(all_roc_df$Class, levels = levels(test$Team.Success), labels = auc_labels)

# Plot with ggplot2
ggplot(all_roc_df, aes(x = 1 - Specificity, y = Sensitivity, color = Class)) +
  geom_line(size = 1.2) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Multiclass ROC Curve (One-vs-All)",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Class"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

################################# Random Forest with Predictors - Two classes ##############################

# Loading dataset
binary_balanced_df <- read.csv("images/rf_data.csv", row.names = 1, check.names = F)

# Removing special characters in order to perform Random Forest
binary_balanced_df <- binary_balanced_df %>% rename_with(make.names)

# Changing Team Success to be binary (1 = Playoff team, 0 = Didn't make playoff)
binary_balanced_df$Team.Success <- ifelse(binary_balanced_df$Team.Success == 1,0,1)

# Loading the calcSplitRatio function from GitHub
source("code/calcSplitRatio-3.R")

# Finding the ideal split ratio
calcSplitRatio(p = 78, binary_balanced_df) # Split data into training (89%) and testing (11%) sets


# Set seed
set.seed(123)

# Stratified split using createDataPartition
train_index <- createDataPartition(binary_balanced_df$`Team.Success`, p = 0.89, list = FALSE)
train <- binary_balanced_df[train_index, ]
test <- binary_balanced_df[-train_index, ]

# Define your target variable
target_var <- "Team.Success"

# Create stratified folds
train$fold <- caret::createFolds(train[[target_var]], k = 10, list = FALSE)

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
  train_data <- train %>% filter(fold != i) %>% select(-fold)
  valid_data <- train %>% filter(fold == i) %>% select(-fold)
  
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

# Final average validation accuracy
print(round(mean(results_df$Validation_Accuracy), 4))

# Final average training accuracy
print(round(mean(results_df$Training_OOB_Accuracy), 4))

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
  ylim(0.89, 1.0) +
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
  geom_text(aes(label = round(MeanDecreaseAccuracy, 1)), 
            hjust = -0.1, size = 4, fontface = "bold") +  # Labels just outside bars
  coord_flip() +
  labs(title = "Top 10 Most Important Variables (Cross-Validated)",
       x = "Variable",
       y = "Mean Decrease in Accuracy") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold")) +
  expand_limits(y = max(average_importance$MeanDecreaseAccuracy) * 1.1)  # Ensure space for text

# ____________ Prediction on Test Set ________________________

# Predict on test
rf_pred <- predict(rf_model, newdata = test)

# Evaluating random forest model
confusionMatrix(rf_pred, as.factor(test$`Team.Success`))

# Creating a heatmap table for the confusion matrix
rf_conf_matrix <- table(Predicted = rf_pred, Actual = test$`Team.Success`) # Create the table as a matrix
rf_conf_df <- as.data.frame(rf_conf_matrix) # Convert to data frame for ggplot

# Plot heatmap
ggplot(rf_conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
  labs(title = "Distribution of Team Success Across Predictions",
       subtitle = "Random Forest",
       x = "Team Success",
       y = "Predicted",
       fill = "Count") +
  theme_minimal(base_size = 14)

# __________________ ROC Curve on Test Set __________________

# Predict probabilities for the positive class
rf_probs <- predict(rf_model, newdata = test, type = "prob")
positive_class_probs <- rf_probs[, "1"]  # '1' = made playoffs

# Actual labels
actual <- test$Team.Success

# Compute ROC curve using pROC
roc_obj <- pROC::roc(actual, positive_class_probs)

# Extract AUC
auc_val <- pROC::auc(roc_obj)

# Create dataframe for ggplot
roc_df <- data.frame(
  Specificity = rev(roc_obj$specificities),
  Sensitivity = rev(roc_obj$sensitivities),
  FPR = 1 - rev(roc_obj$specificities),
  TPR = rev(roc_obj$sensitivities)
)

# Plot with ggplot2
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "#2c7fb8", size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50") +
  labs(
    title = paste("ROC Curve for Binary Random Forest Model"),
    subtitle = paste("AUC =", round(auc_val, 3)),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal(base_size = 14)








