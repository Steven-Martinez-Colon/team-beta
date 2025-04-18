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
                 "glmnet", "xgboost", "Matrix", "Metrics", "reshape2"))

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

# Split data into training (80%) and testing (20%) sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(pca_df), 0.8 * nrow(pca_df))
train <- pca_df[train_indices, ]
test <- pca_df[-train_indices, ]


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


# Running a knn model using k=11
predicted <- knn(train = train_x, test = test_x, cl = train_y, k = 11)

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
  labs(title = "Distribution of Team Success Across Clusters",
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
  geom_point(data = subset(pca_plot_df, TeamSuccess == 4),
             aes(x = PC1, y = PC2),
             color = "black", size = 2, shape = 21, stroke = 1) +
  labs(title = "PC1 vs PC2 Colored by Team Success",
       subtitle = "World Series Winners Highlighted",
       x = "PC1", y = "PC2") +
  theme_minimal()


############################# Joey's Code ######################################

head(df) # testing

# Looking at Confusion matrix
table(Predicted = predicted, Actual = test_y)












































