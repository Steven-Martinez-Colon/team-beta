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
load_libraries(c("tidyverse", "lubridate", "stats", "ggplot2", "corrplot", "stringr", "stringi",
                 "tidymodels", "modeldata", "themis", "vip", "baguette", "janitor", "rvest",
                 "yardstick", "gsheet", "caret", "randomForest", "here", "tibble", "dplyr", "ISAR", "tidyr", "mgcv",
                 "teamcolors", "baseballr", "Lahman", "remotes", "ggcorrplot", "broom", "readr", "glmnet", "xgboost", "Matrix", "Metrics"))

# Load only the necessary functions from 'car'
library(car, exclude = "select")

# Turning off warning messages
options(warnings = 0)


############################## Loading Dataset ####################################

# Loading dataset
df <- read_csv("final_data/final_dataset.csv")


############################# Performing PCA #####################################

# Keep only numeric columns and drop any columns that have NAs
df_numeric <- df %>%
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

# Top contributing variables to PC1
head(sort(abs(loadings[,1]), decreasing = TRUE), 7)

# PC2
head(sort(abs(loadings[,2]), decreasing = TRUE), 7)

# PC3
head(sort(abs(loadings[,3]), decreasing = TRUE), 7)

# PC4
head(sort(abs(loadings[,4]), decreasing = TRUE), 8)


###################### K-Means #################

# Using the first four PCs for the K-Means Model
pca_data <- as.data.frame(pca_result$x[, 1:4])

# Using the Elbow Method to pick a k value
wss <- vector()
for (k in 1:10) {
  km <- kmeans(pca_data, centers = k, nstart = 25)
  wss[k] <- km$tot.withinss
}

# Looking at the plot
plot(1:10, wss, type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal k")

# Performing kmeans
set.seed(42)
kmeans_result <- kmeans(pca_data, centers = 4, nstart = 25)

# Add cluster labels to your data
pca_data$Cluster <- as.factor(kmeans_result$cluster)
pca_data$TeamSuccess <- as.factor(df$Team.Success)  # Adding the team success variable
pca_data$Tm <- as.factor(df$Tm)  # Adding the team names
pca_data$Year <- as.factor(df$Year)   # Adding year

# Table to see the distribution of team success in each cluster
table(Cluster = pca_data$Cluster, TeamSuccess = pca_data$TeamSuccess)


# Create the table as a data frame
table_data <- as.data.frame.matrix(table(Cluster = pca_data$Cluster,
                                         TeamSuccess = pca_data$TeamSuccess))

# Convert to grob
table_plot <- tableGrob(table_data)





# Filter for WS winners (TeamSuccess == 4)
ws_winners <- subset(pca_data, TeamSuccess == 4)

# Looking at the k-means clustering with WS winners highlighted
ggplot() +
  geom_point(data = pca_data, aes(x = PC1, y = PC2, color = Cluster), alpha = 0.6) +
  geom_point(data = subset(pca_data, TeamSuccess == 4),
             aes(x = PC1, y = PC2),
             color = "black", size = 2, shape = 21, stroke = 1) +
  labs(title = "K-Means Clustering on PCA-Reduced Data",
       subtitle = "World Series Winners Highlighted",
       x = "PC1", y = "PC2") +
  theme_minimal()



# Making a clean dataset in order to find the means of the variables in each cluster
df_clean <- df_numeric
df_clean$Cluster <- pca_data$Cluster

# Grouping by cluster and getting the mean for the variables
cluster_means <- df_clean %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

View(cluster_means)

# Writing the cluster summery in a csv file
write.csv(cluster_means, "cluster_means.csv", row.names = FALSE)


































