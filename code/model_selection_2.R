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
                 "glmnet", "xgboost", "Matrix", "Metrics", "reshape2", "DMwR2"))

# Load only the necessary functions from 'car'
library(car, exclude = "select")

# Turning off warning messages
options(warnings = 0)

############################## Loading Dataset ####################################

# Loading dataset
df <- read_csv("final_data/final_dataset.csv")

############################# Removing Year 2020 #####################################

# removing the year 2020 in order to perform PCA again without it
df_no_2020 <- df %>% 
  filter(Year != 2020)

############################# Performing PCA #####################################

# Keep only numeric columns and drop any columns that have NAs
df_numeric <- df_no_2020 %>%
  select(where(is.numeric)) %>%
  select(where(~ !any(is.na(.))))

# 13 columns were dropped because they contained NAs.
# These variables were statistics created more recently so there was data missing from previous years.
# ab.EV | ab.HardH% | ap.EV | ap.HardH% | tf.Rdrs | tf.Rdrs/yr | tf.Rgood | md.Attendance | md.Attend/G | md.Chall
# md.Succ | md.Succ% | pvp.RA9extras

# Scale the data
df_scaled <- scale(df_numeric)

# Run PCA
pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

# Scree plot
screeplot(pca_result, type = "lines", main = "Scree Plot")

# Cumulative proportion of variance explained
summary(pca_result)

# View loadings
loadings <- pca_result$rotation
View(loadings)

# Top 10 contributing variables to PC1
head(sort(abs(loadings[,1]), decreasing = TRUE), 10)

# PC2
head(sort(abs(loadings[,2]), decreasing = TRUE), 10)

# PC3
head(sort(abs(loadings[,3]), decreasing = TRUE), 10)

# PC4
head(sort(abs(loadings[,4]), decreasing = TRUE), 10)


###################### KNN Model Using the first 20 PCs #################

# Use first 20 principal components
pca_df <- as.data.frame(pca_result$x[, 1:20])

# Add the target variable
pca_df$target <- df_no_2020$Team.Success

# Function to find the ideal split ratio
calcSplitRatio <- function(p = NA, df) {
  ## @p  = the number of parameters. by default, if none are provided, the number of columns (predictors) in the dataset are used
  ## @df = the dataframe that will be used for the analysis
  
  ## If the number of parameters isn't supplied, set it to the number of features minus 1 for the target
  if(is.na(p)) {
    p <- ncol(df) -1   ## COMMENT HERE
  }
  
  ## Calculate the ideal number of testing set
  test_N <- (1/sqrt(p))*nrow(df)
  ## Turn that into a testing proportion
  test_prop <- round((1/sqrt(p))*nrow(df)/nrow(df), 2)
  ## And find the training proportion
  train_prop <- 1-test_prop
  
  ## Tell us the results!
  print(paste0("The ideal split ratio is ", train_prop, ":", test_prop, " training:testing"))
  
  ## Return the size of the training set
  return(train_prop)
}

# Finding the ideal split ratio
calcSplitRatio(p = 21, pca_df) # Split data into training (78%) and testing (22%) sets

set.seed(123)  # For reproducibility

# Stratified split using createDataPartition
train_index <- createDataPartition(pca_df$target, p = 0.78, list = FALSE)
train <- pca_df[train_index, ]
test <- pca_df[-train_index, ]

# Set up predictors and target
train_x <- train[, 1:20]
train_y <- train$target
test_x <- test[, 1:20]
test_y <- test$target


# Looking for the best k value
accuracy_scores <- c() # setting up a variable to store the accuracy scores

# Loop to check which k value has the best accuracy
for (k in 1:20) {
  predicted_k <- knn(train = train_x, test = test_x, cl = train_y, k = k)
  acc <- mean(predicted_k == test_y)
  accuracy_scores <- c(accuracy_scores, acc)
}

# Looking at all the accuracy scores for the k values
print(accuracy_scores)

# Best k
best_k <- which.max(accuracy_scores)
best_acc <- max(accuracy_scores)
cat("Best k:", best_k, "\nBest Accuracy:", round(best_acc, 4))

# Plot accuracy vs k (run plot and abline together)
plot(1:20, accuracy_scores, type = "b", pch = 19,
     xlab = "k", ylab = "Accuracy", main = "KNN Accuracy by k")
abline(v = best_k, col = "red", lty = 2)


# Running a knn model using k=9
knn_predicted <- knn(train = train_x, test = test_x, cl = train_y, k = 9)

# Evaluate accuracy
accuracy <- mean(knn_predicted == test_y)
print(accuracy)

# Looking at Confusion matrix
confusionMatrix(knn_predicted, as.factor(test$target))

# Creating a heatmap table for the confusion matrix
conf_matrix <- table(Predicted = knn_predicted, Actual = test_y) # Create the table as a matrix
conf_df <- as.data.frame(conf_matrix) # Convert to data frame for ggplot

# Plot heatmap
ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
  labs(title = "Distribution of Team Success Across Clusters",
       subtitle = "KNN Model",
       x = "Team Success",
       y = "Predicted",
       fill = "Count") +
  theme_minimal(base_size = 14)


# Visualizing PC1 vs PC2 colored by Team Success
pca_plot_df <- data.frame(PC1 = pca_result$x[,1],
                          PC2 = pca_result$x[,2],
                          TeamSuccess = as.factor(df_no_2020$`Team.Success`))

ggplot() +
  geom_point(data = pca_plot_df, aes(x = PC1, y = PC2, color = TeamSuccess), alpha = 0.6) +
  labs(title = "PC1 vs PC2 Colored by Team Success",
       x = "PC1", y = "PC2") +
  theme_minimal()




############################# Random Forest ######################################

# For reproducibility
set.seed(123)

# Train the model
rf_model <- randomForest(as.factor(target) ~ ., data = train, ntree = 500)

# Predict on test
rf_pred <- predict(rf_model, newdata = test)

# Evaluating random forest model
confusionMatrix(rf_pred, as.factor(test$target))

# Creating a heatmap table for the confusion matrix
rf_conf_matrix <- table(Predicted = rf_pred, Actual = test_y) # Create the table as a matrix
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



######################### Binary Playoffs ################################################

# Changing Team Success to be Binary
df$Team.Success <- ifelse(df$Team.Success == 1,0,1)

# Playoffs vs non-playoffs df
df_playoffs_only <- df

############################ PCA - Binary Playoffs ###################################

# removing the year 2020 in order to perform PCA again without it
df_playoffs_only <- df_playoffs_only %>% 
  filter(Year != 2020)

# Keep only numeric columns and drop any columns that have NAs
df_playoffs_only_numeric <- df_playoffs_only %>%
  select(where(is.numeric)) %>%
  select(where(~ !any(is.na(.))))

# Scale the data
df_playoffs_scaled <- scale(df_playoffs_only_numeric)

# Run PCA
pca_result <- prcomp(df_playoffs_scaled, center = TRUE, scale. = TRUE)

# Scree plot
screeplot(pca_result, type = "lines", main = "Scree Plot")

# Cumulative proportion of variance explained
summary(pca_result)

# View loadings
loadings <- pca_result$rotation
View(loadings)

# Top 10 contributing variables to PC1
head(sort(abs(loadings[,1]), decreasing = TRUE), 10)

# PC2
head(sort(abs(loadings[,2]), decreasing = TRUE), 10)

# PC3
head(sort(abs(loadings[,3]), decreasing = TRUE), 10)

# PC4
head(sort(abs(loadings[,4]), decreasing = TRUE), 10)

###################### KNN Model Using the first 20 PCs - Binary Playoffs #################

# Use first 20 principal components
pca_df <- as.data.frame(pca_result$x[, 1:20])

# Add the target variable
pca_df$target <- df_playoffs_only$`Team.Success`

# Function to find the ideal split ratio
calcSplitRatio <- function(p = NA, df) {
  ## @p  = the number of parameters. by default, if none are provided, the number of columns (predictors) in the dataset are used
  ## @df = the dataframe that will be used for the analysis
  
  ## If the number of parameters isn't supplied, set it to the number of features minus 1 for the target
  if(is.na(p)) {
    p <- ncol(df) -1   ## COMMENT HERE
  }
  
  ## Calculate the ideal number of testing set
  test_N <- (1/sqrt(p))*nrow(df)
  ## Turn that into a testing proportion
  test_prop <- round((1/sqrt(p))*nrow(df)/nrow(df), 2)
  ## And find the training proportion
  train_prop <- 1-test_prop
  
  ## Tell us the results!
  print(paste0("The ideal split ratio is ", train_prop, ":", test_prop, " training:testing"))
  
  ## Return the size of the training set
  return(train_prop)
}

# Finding the ideal split ratio
calcSplitRatio(p = 21, pca_df) # Split data into training (78%) and testing (22%) sets

set.seed(123)  # For reproducibility

# Stratified split using createDataPartition
train_index <- createDataPartition(pca_df$target, p = 0.78, list = FALSE)
train <- pca_df[train_index, ]
test <- pca_df[-train_index, ]

# Set up predictors and target
train_x <- train[, 1:20]
train_y <- train$target
test_x <- test[, 1:20]
test_y <- test$target


# Looking for the best k value
accuracy_scores <- c() # setting up a variable to store the accuracy scores

# Loop to check which k value has the best accuracy
for (k in 1:20) {
  predicted_k <- knn(train = train_x, test = test_x, cl = train_y, k = k)
  acc <- mean(predicted_k == test_y)
  accuracy_scores <- c(accuracy_scores, acc)
}

# Looking at all the accuracy scores for the k values
print(accuracy_scores)

# Best k
best_k <- which.max(accuracy_scores)
best_acc <- max(accuracy_scores)
cat("Best k:", best_k, "\nBest Accuracy:", round(best_acc, 4))

# Plot accuracy vs k (run plot and abline together)
plot(1:20, accuracy_scores, type = "b", pch = 19,
     xlab = "k", ylab = "Accuracy", main = "KNN Accuracy by k")
abline(v = best_k, col = "red", lty = 2)


# Running a knn model using k=12
knn_predicted <- knn(train = train_x, test = test_x, cl = train_y, k = 12)

# Evaluate accuracy
accuracy <- mean(knn_predicted == test_y)
print(accuracy)

# Looking at Confusion matrix
confusionMatrix(knn_predicted, as.factor(test$target))

# Creating a heatmap table for the confusion matrix
conf_matrix <- table(Predicted = knn_predicted, Actual = test_y) # Create the table as a matrix
conf_df <- as.data.frame(conf_matrix) # Convert to data frame for ggplot

# Plot heatmap
ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
  labs(title = "Distribution of Team Success Across Predictions",
       subtitle = "KNN Model",
       x = "Team Success",
       y = "Predicted",
       fill = "Count") +
  theme_minimal(base_size = 14)


# Visualizing PC1 vs PC2 colored by Team Success
pca_plot_df <- data.frame(PC1 = pca_result$x[,1],
                          PC2 = pca_result$x[,2],
                          TeamSuccess = as.factor(df_playoffs_only$`Team.Success`))

ggplot() +
  geom_point(data = pca_plot_df, aes(x = PC1, y = PC2, color = TeamSuccess), alpha = 0.6) +
  labs(title = "PC1 vs PC2 Colored by Team Success",
       subtitle = "Playoffs vs Non-Playoffs Teams",
       x = "PC1", y = "PC2") +
  theme_minimal()

############################# Random Forest - Binary Playoffs ######################################

# For reproducibility
set.seed(123)

# Train the model
rf_model <- randomForest(as.factor(target) ~ ., data = train, ntree = 500)

# Predict on test
rf_pred <- predict(rf_model, newdata = test)

# Evaluating random forest model
confusionMatrix(rf_pred, as.factor(test$target))

# Creating a heatmap table for the confusion matrix
rf_conf_matrix <- table(Predicted = rf_pred, Actual = test_y) # Create the table as a matrix
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


################################# Random Forest with Predictors - 4 classes ##############################

# Loading Dataset
rf_data <- read.csv("images/rf_data.csv", row.names = 1, check.names = F)

# Removing special characters in order to perform Random Forest
rf_data <- rf_data %>% rename_with(make.names)

# Finding the ideal split ratio
calcSplitRatio(p = 78, pca_df) # Split data into training (89%) and testing (11%) sets

# For reproducibility
set.seed(123)

# Stratified split using createDataPartition
train_index <- createDataPartition(rf_data$`Team.Success`, p = 0.89, list = FALSE)
train <- rf_data[train_index, ]
test <- rf_data[-train_index, ]

# Set up predictors and target
train_x <- train %>% dplyr::select(-Team.Success)
train_y <- train$`Team.Success`
test_x <- test %>% dplyr::select(-Team.Success)
test_y <- test$`Team.Success`


# For reproducibility
set.seed(123)

# Train the model
rf_model <- randomForest(as.factor(`Team.Success`) ~ ., data = train, ntree = 500, importance = TRUE)

# Predict on test
rf_pred <- predict(rf_model, newdata = test)

# Evaluating random forest model
confusionMatrix(rf_pred, as.factor(test$`Team.Success`))

# Creating a heatmap table for the confusion matrix
rf_conf_matrix <- table(Predicted = rf_pred, Actual = test_y) # Create the table as a matrix
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


# Extract importance
imp <- importance(rf_model)

# Convert to data frame
imp_df <- data.frame(Variable = rownames(imp), imp)

# Sort by MeanDecreaseAccuracy
imp_sorted <- imp_df %>% 
  arrange(desc(MeanDecreaseAccuracy))

# Selecting only MeanDecreaseAccuracy
imp_sorted <- imp_sorted %>% 
  select(MeanDecreaseAccuracy)

# Viewing the top variables
head(imp_sorted)

################################# Random Forest with Predictors - Two classes ##############################

# Loading Dataset
rf_data <- read.csv("images/rf_data.csv", row.names = 1, check.names = F)

rf_data_binary <- rf_data$Team.Success <- ifelse(rf_data$Team.Success == 1,0,1)

# Removing special characters in order to perform Random Forest
rf_data <- rf_data %>% rename_with(make.names)

# Finding the ideal split ratio
calcSplitRatio(p = 78, pca_df) # Split data into training (89%) and testing (11%) sets

# For reproducibility
set.seed(123)

# Stratified split using createDataPartition
train_index <- createDataPartition(rf_data$`Team.Success`, p = 0.89, list = FALSE)
train <- rf_data[train_index, ]
test <- rf_data[-train_index, ]

# Set up predictors and target
train_x <- train %>% dplyr::select(-Team.Success)
train_y <- train$`Team.Success`
test_x <- test %>% dplyr::select(-Team.Success)
test_y <- test$`Team.Success`


# For reproducibility
set.seed(123)

# Train the model
rf_model <- randomForest(as.factor(`Team.Success`) ~ ., data = train, ntree = 500, importance = TRUE)

# Predict on test
rf_pred <- predict(rf_model, newdata = test)

# Evaluating random forest model
confusionMatrix(rf_pred, as.factor(test$`Team.Success`))

# Creating a heatmap table for the confusion matrix
rf_conf_matrix <- table(Predicted = rf_pred, Actual = test_y) # Create the table as a matrix
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


# Extract importance
imp <- importance(rf_model)

# Convert to data frame
imp_df <- data.frame(Variable = rownames(imp), imp)

# Sort by MeanDecreaseAccuracy
imp_sorted <- imp_df %>% 
  arrange(desc(MeanDecreaseAccuracy))

# Selecting only MeanDecreaseAccuracy
imp_sorted <- imp_sorted %>% 
  select(MeanDecreaseAccuracy)

# Viewing the top variables
head(imp_sorted)
























########## The code below still needs some work. It is not working properly ###########
############################# Using SMOTE on KNN #################################

# Make sure target is a factor
train$target <- as.factor(train$target)

# Apply SMOTE using performanceEstimation::smote
set.seed(123)
train_smote <- smote(target ~ ., data = train, perc.over = 600, perc.under = 100)

# Check class distribution after SMOTE
table(train_smote$target)

# Extract predictors and target from SMOTE dataset
train_x_smote <- train_smote[, 1:20]
train_y_smote <- train_smote$target

# Original test set
test_x <- test[, 1:20]
test_y <- test$target

# Looking for the best k value
accuracy_scores <- c() # setting up a variable to store the accuracy scores

# Loop to check which k value has the best accuracy
for (k in 1:20) {
  predicted_k <- knn(train = train_x, test = test_x, cl = train_y, k = k)
  acc <- mean(predicted_k == test_y)
  accuracy_scores <- c(accuracy_scores, acc)
}

# Looking at all the accuracy scores for the k values
print(accuracy_scores)

# Best k
best_k <- which.max(accuracy_scores)
best_acc <- max(accuracy_scores)
cat("Best k:", best_k, "\nBest Accuracy:", round(best_acc, 4))

# Plot accuracy vs k (run plot and abline together)
plot(1:20, accuracy_scores, type = "b", pch = 19,
     xlab = "k", ylab = "Accuracy", main = "KNN Accuracy by k")
abline(v = best_k, col = "red", lty = 2)


# Running a knn model using k=8
predicted_smote <- knn(train = train_x_smote, test = test_x, cl = train_y_smote, k = 9)

# Accuracy
accuracy_smote <- mean(predicted_smote == test_y)
cat("Accuracy after SMOTE:", round(accuracy_smote, 4), "\n")

# Confusion Matrix
conf_matrix_smote <- confusionMatrix(as.factor(predicted_smote), as.factor(test_y))
print(conf_matrix_smote)

