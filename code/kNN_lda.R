## Written by Joseph Annand and Steven Martinez

## INSTRUCTIONS:
## Set working directory to repository in local drive before running script

library(tidyr)
library(dplyr)
library(forcats)
library(stringr)
library(ggplot2)
library(purrr)
library(readr)
library(corrplot)
library(ggcorrplot)
library(themis)
library(recipes)
library(caret)
library(moments)
library(FactoMineR)
library(factoextra)
library(MASS)
source("code/calcSplitRatio-3.R")
library(MVN)
library(class)
library(randomForest)
library(pROC)


## Read data without changing special chars in column names
mlb_data <- read.csv("final_data/final_data.csv", check.names = FALSE)

############################### Clean Data #####################################

## Convert Year to factor
mlb_data$Year <- as.factor(mlb_data$Year)

## COnvert team succes to factor
mlb_data$Team.Success <- as.factor(mlb_data$Team.Success)

## Filter out data from 2020 season
mlb_data <- mlb_data %>% filter(Year != "2020")

## Set rownames equal to concatenation of Team and Year
rownames(mlb_data) <- paste(mlb_data$Tm, mlb_data$Year, sep = "_")

## Drop columns with NA values
no_na_cols <- names(mlb_data)[sapply(mlb_data, function(x) !any(is.na(x)))]
mlb_data <- mlb_data %>% dplyr::select(all_of(no_na_cols))

## Drop non-numeric cols
num_cols <- names(mlb_data)[sapply(mlb_data, is.numeric)]

## Create final MLB data set
mlb_df <- mlb_data %>% dplyr::select(all_of(num_cols))


###################### Remove duplicates #######################################

findDuplicateCols <- function(df) {
  ## Removes columns that have duplicated data
  ## @df = dataframe from which duplicate columns will be removed
  ## returns vector of duplicate columns
  
  # Create a list to hold sets of duplicated columns
  duplicate_sets <- list()
  checked_cols <- rep(FALSE, ncol(df))
  
  for (i in 1:(ncol(df) - 1)) {
    if (checked_cols[i]) next
    
    group <- names(df)[i]
    for (j in (i + 1):ncol(df)) {
      if (!checked_cols[j] && all(df[[i]] == df[[j]], na.rm = TRUE)) {
        group <- c(group, names(df)[j])
        checked_cols[j] <- TRUE
      }
    }
    
    if (length(group) > 1) {
      duplicate_sets[[length(duplicate_sets) + 1]] <- group
      checked_cols[i] <- TRUE
    }
  }
  
  # Print the sets
  duplicate_sets
  
  # Extract all non-first elements from each group in duplicate_sets
  duplicates_to_remove <- unlist(lapply(duplicate_sets, function(group) group[-1]))
  
  # View the result
  return(duplicates_to_remove)
}


mlb_df <- mlb_df %>% dplyr::select(-all_of(findDuplicateCols(mlb_df)))

################## Find sets of highly correlated variables ####################


findCorrelatedSets <- function(df, threshold=0.9) {
  ## Find all non-normally distributed variables and apply Box Cox transformations
  ## @df = dataframe
  ## threshold = correlation coefficient cutoff value
  ## returns list of sets of correlated predictor variables
  
  # Set threshold
  threshold <- 0.9
  # Compute correlation matrix using pairwise complete observations
  cor_matrix <- cor(df, use = "pairwise.complete.obs")
  
  # Find indices of highly correlated pairs
  high_corr_pairs <- which(abs(cor_matrix) > threshold & lower.tri(cor_matrix), arr.ind = TRUE)
  
  # Build a list of sets
  high_corr_sets <- list()
  visited <- rep(FALSE, ncol(cor_matrix))
  
  # Loop through each pair
  for (i in seq_len(nrow(high_corr_pairs))) {
    # Get cor_matrix index of first variable in the pair
    row <- high_corr_pairs[i, 1]
    # Get cor_matrix index of second variable in the pair
    col <- high_corr_pairs[i, 2]
    # Get first variable name with colnames of cor_matrix and index in colnames
    var1 <- colnames(cor_matrix)[row]
    # Get second variable name with colnames of cor_matrix and index in colnames
    var2 <- colnames(cor_matrix)[col]
    
    # Try to find an existing set that contains one of the variables
    found_set <- FALSE
    for (j in seq_along(high_corr_sets)) {
      # if var1 or var2 already exist in a set, add the other var in the set
      if (var1 %in% high_corr_sets[[j]] || var2 %in% high_corr_sets[[j]]) {
        high_corr_sets[[j]] <- unique(c(high_corr_sets[[j]], var1, var2))
        # set found_set to TRUE to exit loop for this pair
        found_set <- TRUE
        break
      }
    }
    # if no set is found, then create a new set with the two variables
    if (!found_set) {
      high_corr_sets[[length(high_corr_sets) + 1]] <- c(var1, var2)
    }
  }
  
  return(high_corr_sets)
}



# # Export high_corr_sets to text file
# sink("images/highly_correlated_sets.txt")
# for (i in seq_along(findCorrelatedSets(mlb_df))) {
#   cat(paste0("Set ", i, ":\n"))
#   cat(paste(high_corr_sets[[i]], collapse = ", "), "\n\n")
# }
# sink()

####################### Remove duplicates and highly cor var ###################

# Get list of unique correlated predictors
high_corr_var <- unique(unlist(findCorrelatedSets(mlb_df)))

# Manually choose predictors to keep from highly correlated sets
keep_high_corr <- c("ab.rOBA","sbm.OPS+","ab.ISO","ab.SO%","ab.BB%","ab.LD%","ab.GB/FB",
                    "ab.WPA","sp.WHIP","tf.DefEff","ap.HR%","ap.SO%","ap.BB%","ap.GB/FB",
                    "ap.RE24","tf.PO","tf.A","tf.Fld%","tf.Rtot/yr","md.Attend/G",
                    "md.BPF","md.Est. Payroll","pvb.PA","pvb.dWAR","Rrep","pvp.WAR",
                    "sbm.lgOPS","sbm.BA","sbm.PwrSpd")

# Get correlated variables that will not be kept
remove_high_corr <- setdiff(high_corr_var, keep_high_corr)

# "remove high correlation" mlb_df
mlb_rhc_df <- mlb_df %>% dplyr::select(-all_of(remove_high_corr))


###################### Perform Arithmetic Transformation #######################

transform_mlb <- mlb_rhc_df

shapiro_pvalues <- sapply(transform_mlb, function (col) {
  shapiro.test(col)$p.value
})

shapiro_results <- data.frame(
  Variable = names(shapiro_pvalues),
  P_Value = shapiro_pvalues
)

## Get non-normal columns with Shapiro-Wilks p-value < 0.05
non_normal_cols <- names(shapiro_pvalues[shapiro_pvalues < 0.05])

## Apply Box-Cox transformation to non-normal cols
for (colname in non_normal_cols) {
  col <- transform_mlb[[colname]]
  
  # Shift if necessary (Box-Cox requires > 0)
  if (any(col <= 0, na.rm = TRUE)) {
    min_val <- min(col, na.rm = TRUE)
    col <- col + abs(min_val) + 1
  }
  
  # Create temporary df for lm fitting
  df_temp <- data.frame(y = col)
  
  # Fit linear model (Box-Cox requires a model)
  # Using dummy response variable
  lm_model <- lm(y ~ 1, df_temp)
  
  # Apply Box-Cox transformation
  bc <- boxcox(lm_model, lambda = seq(-5, 5, 0.1), plotit = FALSE)
  lambda_opt <- bc$x[which.max(bc$y)]
  transformed_col <- if (lambda_opt == 0) log(col) else (col^lambda_opt - 1) / lambda_opt
  
  # Replace original column with transformed values
  transform_mlb[[colname]] <- transformed_col
}

mlb_df <- transform_mlb

############################## Train/test split ################################

## Split data into training and testing sets
getSplitRatio <- function(df) {
  ## @df = data frame to determine optimal train-test split ratio
  ## returns proportion of data set to include in training set
  
  ## use calcSplitRatio function to determine optimal split
  calcRatio <- calcSplitRatio(df = df)
  
  ## if function returns an optimal split with less than 50% in training set
  ## return 0.50 so that training set is not less than half the data set
  
  if (calcRatio < 0.50) {
    return(0.50)
  } else {
    return(calcRatio)
  }
}

############################## LDA Assumptions #################################


## Check multivariate normality assumption
mvn_result <- mvn(data = mlb_df, mvnTest = "royston", tol = 1e-57)
mvn_result$multivariateNormality


############################### Four Category ##################################

## Add Team Success back to data frame
mlb_df$Team.Success <- mlb_data$Team.Success

## Four category response LDA
lda_model_4 <- lda(Team.Success ~ ., data = mlb_df[,-c(32,33)])
lda_values_4 <- predict(lda_model_4)$x


mlb_lda_4 <- as.data.frame(lda_values_4)
mlb_lda_4$Team.Success <- mlb_df$Team.Success

ratio <- getSplitRatio(mlb_lda_4)

# Set the seed for reproducibility
set.seed(123)

# Create stratified train-test split
train_index <- createDataPartition(mlb_lda_4$Team.Success, p = ratio, list = FALSE)

# Split the data into training and test sets
train_data <- mlb_lda_4[train_index, ]
test_data <- mlb_lda_4[-train_index, ]


## Rename levels in Team.Success so train() can read them
levels(train_data$Team.Success)[levels(train_data$Team.Success) == "1"] <- "missed_po"
levels(train_data$Team.Success)[levels(train_data$Team.Success) == "2"] <- "made_po"
levels(train_data$Team.Success)[levels(train_data$Team.Success) == "3"] <- "runner_up"
levels(train_data$Team.Success)[levels(train_data$Team.Success) == "4"] <- "ws_winner"

levels(train_data$Team.Success)

# Define trainControl with SMOTE
ctrl <- trainControl(
  method = "cv",           # k-fold cross-validation
  number = 10,              # 10 folds
  sampling = "smote", # apply SMOTE inside each fold
  classProbs = TRUE,
  savePredictions = "final"
)

# Train KNN model
set.seed(500)
knn_model_multi <- train(
  Team.Success ~ ., 
  data = train_data,
  method = "knn",
  trControl = ctrl,
  preProcess = c("center", "scale"),  # scale predictors before KNN
  tuneLength = 10                     # search over 10 different K values
)

print(knn_model_multi)
plot(knn_model_multi)

# Extract results
train_results_multi <- knn_model_multi$results

# Plot
ggplot(train_results_multi, aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = train_results_multi$k[which.max(train_results_multi$Accuracy)],
             color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = seq(min(train_results_multi$k), max(train_results_multi$k),
                                  by = 2)) + 
  labs(title = "k-value Tuning for k-Nearest Neighbors (LDA)",
       x = "Number of Neighbors (k)",
       y = "Validation Accuracy") +
  theme_classic()


## Rename levels in Team.Success so they match training data
levels(test_data$Team.Success)[levels(test_data$Team.Success) == "1"] <- "missed_po"
levels(test_data$Team.Success)[levels(test_data$Team.Success) == "2"] <- "made_po"
levels(test_data$Team.Success)[levels(test_data$Team.Success) == "3"] <- "runner_up"
levels(test_data$Team.Success)[levels(test_data$Team.Success) == "4"] <- "ws_winner"

levels(test_data$Team.Success)

# Scale numeric data of test set
test_x <- test_data %>% dplyr::select(-Team.Success)
test_x <- as.data.frame(scale(test_x))
test_y <- test_data$Team.Success

test_data <- test_x %>% mutate(Team.Success = test_y)

# Predict test set response with trained cross-validated kNN
predictions <- predict(knn_model_multi, newdata = test_data)

# Create detailed confusion matrix
confusionMatrix(predictions, test_data$Team.Success)

# Creating a heatmap table for the confusion matrix
conf_matrix <- table(Predicted = predictions, Actual = test_data$Team.Success) # Create the table as a matrix
conf_df <- as.data.frame(conf_matrix) # Convert to data frame for ggplot

# Plot heatmap
ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
  labs(title = "10-fold cross-validated kNN with LDA",
       subtitle = "Multiclass Team Success Response",
       x = "Actual Team Success",
       y = "Predicted",
       fill = "Count") +
  theme_minimal(base_size = 14)

## Create multiclass ROC curve

# Get predicted class probabilities (not class labels)
probs <- predict(knn_model_multi, newdata = test_data, type = "prob")


# List to store ROC data frames and AUC values
roc_dfs <- list()
auc_labels <- c()

# Loop over each class
for (class in levels(test_data$Team.Success)) {
  # Binary response: 1 if current class, 0 otherwise
  binary_response <- as.factor(ifelse(test_data$Team.Success == class, class, paste0("not_", class)))
  
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
all_roc_df$Class <- factor(all_roc_df$Class, levels = levels(test_data$Team.Success), labels = auc_labels)

# Plot with ggplot2
ggplot(all_roc_df, aes(x = 1 - Specificity, y = Sensitivity, color = Class)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  geom_line(size = 1.2) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "ROC Curve (One-vs-All)",
    subtitle = "Multiclass kNN Model with LDA Results",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Class"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")



############################# Binary Response ##################################

## Binary category response LDA

mlb_df$Team.Success <- ifelse(mlb_df$Team.Success == "1","0","1")
mlb_df$Team.Success <- as.factor(mlb_df$Team.Success)

lda_model_2 <- lda(Team.Success ~ ., data = mlb_df[,-c(32,33)])
lda_values_2 <- predict(lda_model_2)$x

mlb_lda_2 <- as.data.frame(lda_values_2)
mlb_lda_2$Team.Success <- mlb_df$Team.Success

ratio <- getSplitRatio(mlb_lda_2)

# Set the seed for reproducibility
set.seed(321)

# Create stratified train-test split
train_index <- createDataPartition(mlb_lda_2$Team.Success, p = ratio, list = FALSE)

# Split the data into training and test sets
train_data <- mlb_lda_2[train_index, ]
test_data <- mlb_lda_2[-train_index, ]

## Rename levels in Team.Success so train() can read them
levels(train_data$Team.Success)[levels(train_data$Team.Success) == "0"] <- "missed_po"
levels(train_data$Team.Success)[levels(train_data$Team.Success) == "1"] <- "made_po"

levels(train_data$Team.Success)

# Define trainControl with SMOTE
ctrl <- trainControl(
  method = "cv",           # k-fold cross-validation
  number = 10,              # 10 folds
  sampling = "smote", # apply SMOTE inside each fold
  classProbs = TRUE,
  savePredictions = "final"
)

# Train KNN model
set.seed(501)
knn_model_binary <- train(
  Team.Success ~ ., 
  data = train_data,
  method = "knn",
  trControl = ctrl,
  preProcess = c("center", "scale"),  # scale predictors before KNN
  tuneLength = 10                     # search over 10 different K values
)

print(knn_model_binary)
plot(knn_model_binary)

# Extract results
train_results_binary <- knn_model_binary$results

# Plot
ggplot(train_results_binary, aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = train_results_binary$k[which.max(train_results_binary$Accuracy)],
             color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = seq(min(train_results_binary$k), max(train_results_binary$k),
                                  by = 2)) + 
  labs(title = "k-value Tuning for k-Nearest Neighbors (LDA)",
       x = "Number of Neighbors (k)",
       y = "Validation Accuracy") +
  theme_classic()

## Rename levels in Team.Success so they match training data
levels(test_data$Team.Success)[levels(test_data$Team.Success) == "0"] <- "missed_po"
levels(test_data$Team.Success)[levels(test_data$Team.Success) == "1"] <- "made_po"
levels(test_data$Team.Success)

# Scale numeric data of test set
test_x <- test_data %>% dplyr::select(-Team.Success)
test_x <- as.data.frame(scale(test_x))
# Get test response data
test_y <- test_data$Team.Success

# Set test_data to scaled predictors and response
test_data <- test_x %>% mutate(Team.Success = test_y)

# Predict response using 10-fold CV kNN model
predictions <- predict(knn_model_binary, newdata = test_data)

# Looking at Confusion matrix
confusionMatrix(predictions, test_data$Team.Success)

# Creating a heatmap table for the confusion matrix
conf_matrix <- table(Predicted = predictions, Actual = test_y) # Create the table as a matrix
conf_df <- as.data.frame(conf_matrix) # Convert to data frame for ggplot

# Plot heatmap
ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
  labs(title = "10-fold cross-validated kNN with LDA",
       subtitle = "Binary Team Success Response",
       x = "Team Success",
       y = "Predicted",
       fill = "Count") +
  theme_minimal(base_size = 14)


## Create multiclass ROC curve

# Get predicted class probabilities (not class labels)
probs <- predict(knn_model_binary, newdata = test_data, type = "prob")


# List to store ROC data frames and AUC values
roc_dfs <- list()
auc_labels <- c()

# Loop over each class
for (class in levels(test_data$Team.Success)) {
  # Binary response: 1 if current class, 0 otherwise
  binary_response <- as.factor(ifelse(test_data$Team.Success == class, class, paste0("not_", class)))
  
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
all_roc_df$Class <- factor(all_roc_df$Class, levels = levels(test_data$Team.Success), labels = auc_labels)

# Plot with ggplot2
ggplot(all_roc_df %>% filter(Class == "made_po (AUC = 0.973)"),
       aes(x = 1 - Specificity, y = Sensitivity, color = Class)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  geom_line(size = 1.2) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "ROC Curve",
    subtitle = "Binary kNN with LDA results",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  annotate(geom = "text", x=0.25, y=0.75, label = "AUC = 0.973")
