## Written by Joseph Annand and Steven Martinez


library(tidyr)
library(dplyr)
library(forcats)
library(stringr)
library(ggplot2)
library(purrr)
library(readr)
library(corrplot)
library(ggcorrplot)
library(caret)
library(moments)
library(FactoMineR)
library(factoextra)
library(MASS)
source("code/calcSplitRatio-3.R")
library(MVN)
library(class)

dataset_folder <- paste(getwd(),"/final_data",sep="")

data_file <- paste(dataset_folder,"/final_dataset.csv", sep = "")

## Read data without changing special chars in column names
mlb_data <- read.csv(data_file, check.names = FALSE)

############################### Clean Data #####################################

## Convert Year to factor
mlb_data$Year <- as.factor(mlb_data$Year)

## Change Team.Success from four possible values to two
## 0 = missed playoffs, 1 = made the playoffs
## mlb_data$Team.Success <- ifelse(mlb_data$Team.Success == 1,0,1)
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



# Export high_corr_sets to text file
sink("images/highly_correlated_sets.txt")
for (i in seq_along(findCorrelatedSets(mlb_df))) {
  cat(paste0("Set ", i, ":\n"))
  cat(paste(high_corr_sets[[i]], collapse = ", "), "\n\n")
}
sink()

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


###################### Transformations Function ################################


performBoxCox <- function(df) {
  ## Find all non-normally distributed variables and apply Box Cox transformations
  ## @df = dataframe with predictors to apply transformations
  ## returns dataframe with transformed predictors
  
  shapiro_pvalues <- sapply(df, function (col) {
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
    col <- df[[colname]]
    
    # Shift if necessary (Box-Cox requires > 0)
    if (any(col <= 0, na.rm = TRUE)) {
      min_val <- min(col, na.rm = TRUE)
      col <- col + abs(min_val) + 1
    }
    
    # Create temporary df for lm fitting
    df_temp <- data.frame(y = col)
    
    # Fit linear model (Box-Cox requires a model)
    # Using dummy response variable
    lm_model <- lm(y ~ 1, data = df_temp)

    # Apply Box-Cox transformation
    bc <- boxcox(lm_model, lambda = seq(-5, 5, 0.1), plotit = FALSE)
    lambda_opt <- bc$x[which.max(bc$y)]
    transformed_col <- if (lambda_opt == 0) log(col) else (col^lambda_opt - 1) / lambda_opt

    # Replace original column with transformed values
    df[[colname]] <- transformed_col
  }
  
  return(df_temp)
  
}

# mlb_df <- performBoxCox(mlb_df)

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

############################## LDA Assumptions #################################


## Check multivariate normality assumption
mvn_result <- mvn(data = mlb_df, mvnTest = "royston", tol = 1e-57)
mvn_result$multivariateNormality


############################### Four Category ##################################

## Add Team Success back to data frame
mlb_df$Team.Success <- mlb_data$Team.Success

write.csv(mlb_df, file = "images/rf_data.csv")

## Four category response LDA
lda_model_4 <- lda(Team.Success ~ ., data = mlb_df[,-c(32,33)])
lda_values_4 <- predict(lda_model_4)$x


mlb_lda_4 <- as.data.frame(lda_values_4)
mlb_lda_4$Team.Success <- mlb_df$Team.Success
# mlb_lda_4$Year <- substr(rownames(mlb_lda_4), nchar(rownames(mlb_lda_4)) - 3,
#                         nchar(rownames(mlb_lda_4)))

## Train-test split
# time_slices <- createTimeSlices(1:nrow(mlb_df),
#                                 initialWindow = floor(0.8 * nrow(mlb_df)),
#                                 horizon = 0)

## Split data into training (80%) and testing (20%) sets

# Set the seed for reproducibility
set.seed(123)

# Create stratified train-test split (80% training, 20% test)
train_index <- createDataPartition(mlb_lda_4$Team.Success, p = 0.8, list = FALSE)

# Split the data into training and test sets
train_data <- mlb_df[train_index, ]
test_data <- mlb_df[-train_index, ]

# Define the predictor variables (excluding the response variable)
train_x <- train_data %>% dplyr::select(-Team.Success)
train_y <- train_data$Team.Success

# Define the predictor variables for the test set
test_x <- test_data %>% dplyr::select(-Team.Success)
test_y <- test_data$Team.Success

# Scale train_x and test_x
train_x <- scale(train_x)
test_x <- scale(test_x)

# Looking for the best k value
accuracy_scores <- c() # setting up a variable to store the accuracy scores

# Loop to check which k value has the best accuracy
for (k_value in 1:20) {
  predicted_k <- knn(train = train_data, test = test_data, cl = train_y, k = k_value)
  acc <- mean(predicted_k == test_y)
  accuracy_scores <- c(accuracy_scores, acc)
}

# Best k
best_k <- which.max(accuracy_scores)
best_acc <- max(accuracy_scores)
cat("Best k:", best_k, "\nBest Accuracy:", round(best_acc, 4))

# Plot accuracy vs k (run plot and abline together)
plot(1:20, accuracy_scores, type = "b", pch = 19,
     xlab = "k", ylab = "Accuracy",
     main = "Accuracy by k-value for KNN with LDA",
     sub = "Four Category Team Success Response")
abline(v = best_k, col = "red", lty = 2)

# Running a knn model using k=9
predicted <- knn(train = train_x, test = test_x, cl = train_y, k = best_k)

# Evaluate accuracy
accuracy <- mean(predicted == test_y)
print(accuracy)

# Looking at Confusion matrix
table(Predicted = predicted, Actual = test_y)

# Creating a heatmap table for the confusion matrix
conf_matrix <- table(Predicted = predicted, Actual = test_y) # Create the table as a matrix
conf_df <- as.data.frame(conf_matrix) # Convert to data frame for ggplot

# Plot heatmap
ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
  labs(title = "Confusion Matrix for KNN with LDA",
       subtitle = "Four Category Team Success Response",
       x = "Team Success",
       y = "Predicted",
       fill = "Count") +
  theme_minimal(base_size = 14)

############################# Binary Response ##################################

## Binary category response LDA

mlb_df$Team.Success <- ifelse(mlb_df$Team.Success == 1,0,1)

lda_model_2 <- lda(Team.Success ~ ., data = mlb_df[,-c(32,33)])
lda_values_2 <- predict(lda_model_2)$x

mlb_lda_2 <- as.data.frame(lda_values_2)
mlb_lda_2$Team.Success <- mlb_df$Team.Success

# Set the seed for reproducibility
set.seed(321)

# Create stratified train-test split (80% training, 20% test)
train_index <- createDataPartition(mlb_lda_2$Team.Success, p = 0.8, list = FALSE)

# Split the data into training and test sets
train_data <- mlb_df[train_index, ]
test_data <- mlb_df[-train_index, ]

# Define the predictor variables (excluding the response variable)
train_x <- train_data %>% dplyr::select(-Team.Success)
train_y <- train_data$Team.Success

# Define the predictor variables for the test set
test_x <- test_data %>% dplyr::select(-Team.Success)
test_y <- test_data$Team.Success

# Scale train_x and test_x
train_x <- scale(train_x)
test_x <- scale(test_x)

# Looking for the best k value
accuracy_scores <- c() # setting up a variable to store the accuracy scores

# Loop to check which k value has the best accuracy
for (k_value in 1:20) {
  predicted_k <- knn(train = train_data, test = test_data, cl = train_y, k = k_value)
  acc <- mean(predicted_k == test_y)
  accuracy_scores <- c(accuracy_scores, acc)
}

# Best k
best_k <- which.max(accuracy_scores)
best_acc <- max(accuracy_scores)
cat("Best k:", best_k, "\nBest Accuracy:", round(best_acc, 4))

# Plot accuracy vs k (run plot and abline together)
plot(1:20, accuracy_scores, type = "b", pch = 19,
     xlab = "k", ylab = "Accuracy",
     main = "Accuracy by k-value for KNN with LDA",
     sub = "Binary Category Team Success Response")
abline(v = best_k, col = "red", lty = 2)

# Running a knn model using k=9
predicted <- knn(train = train_x, test = test_x, cl = train_y, k = best_k)

# Evaluate accuracy
accuracy <- mean(predicted == test_y)
print(accuracy)

# Looking at Confusion matrix
table(Predicted = predicted, Actual = test_y)

# Creating a heatmap table for the confusion matrix
conf_matrix <- table(Predicted = predicted, Actual = test_y) # Create the table as a matrix
conf_df <- as.data.frame(conf_matrix) # Convert to data frame for ggplot

# Plot heatmap
ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
  labs(title = "Confusion Matrix for KNN with LDA",
       subtitle = "Binary Team Success Response",
       x = "Team Success",
       y = "Predicted",
       fill = "Count") +
  theme_minimal(base_size = 14)
