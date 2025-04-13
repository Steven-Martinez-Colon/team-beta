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


###################### K-Means #################

# Using the first four PCs for the K-Means Model
pca_data <- as.data.frame(pca_result$x[, 1:20])

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
kmeans_result <- kmeans(pca_data, centers = 3, nstart = 25)

# Add cluster labels to your data
pca_data$Cluster <- as.factor(kmeans_result$cluster)
pca_data$TeamSuccess <- as.factor(df_no_2020$Team.Success)  # Adding the team success variable
pca_data$Tm <- as.factor(df_no_2020$Tm)  # Adding the team names
pca_data$Year <- as.factor(df_no_2020$Year)   # Adding year

# Writing the pca data in a csv file
write.csv(pca_data, "pca_data.csv", row.names = FALSE)

# Table to see the distribution of team success in each cluster
table(Cluster = pca_data$Cluster, TeamSuccess = pca_data$TeamSuccess)


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



####### Making a table to show distribution of team success across clusters #######
library(ggplot2)
library(reshape2)

# Create the table as a matrix
conf_matrix <- table(Cluster = pca_data$Cluster, TeamSuccess = pca_data$TeamSuccess)

# Convert to data frame for ggplot
conf_df <- as.data.frame(conf_matrix)
colnames(conf_df) <- c("Cluster", "TeamSuccess", "Count")

# Make sure factors are ordered
conf_df$Cluster <- factor(conf_df$Cluster)
conf_df$TeamSuccess <- factor(conf_df$TeamSuccess)

# Plot heatmap
ggplot(conf_df, aes(x = TeamSuccess, y = Cluster, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
  labs(title = "Distribution of Team Success Across Clusters",
       x = "Team Success",
       y = "Cluster",
       fill = "Count") +
  theme_minimal(base_size = 14)



















# Ignore this bottom code.
# It is not needed for our analysis. It was just an exploration.


############################### Looking at Runner Ups #######################

# Table to see the distribution of team success in each cluster
table(Cluster = pca_data$Cluster, TeamSuccess = pca_data$TeamSuccess)

# Looking at the k-means clustering with WS Runner-ups highlighted
ggplot() +
  geom_point(data = pca_data, aes(x = PC1, y = PC2, color = Cluster), alpha = 0.6) +
  geom_point(data = subset(pca_data, TeamSuccess == 3),
             aes(x = PC1, y = PC2),
             color = "black", size = 2, shape = 21, stroke = 1) +
  labs(title = "K-Means Clustering on PCA-Reduced Data",
       subtitle = "World Series Runner-Ups Highlighted",
       x = "PC1", y = "PC2") +
  theme_minimal()

# Finding out who is the one runner-up team in cluster 1
print(
  pca_data %>% 
    filter(Cluster == 1 & `TeamSuccess` == 3) %>% 
    select(Tm,
           Year)
)


# Creating a dataset for the runner-up teams
runner_up_df <- df_no_2020 %>% 
  filter(`Team.Success` == 3)

# Top 10 contributing variables to PC1
head(sort(abs(loadings[,1]), decreasing = TRUE), 10)


# Rank teams by spW
print(
  runner_up_df %>%
  arrange(desc(`sp.W`)) %>%
  select(Tm, Year, `sp.W`),
  n = 24)

# Rank teams by spW-L%
print(
  runner_up_df %>%
    arrange(desc(`sp.W-L%`)) %>%
    select(Tm, Year, `sp.W-L%`),
  n = 24)

# Rank teams by pvb.RAR
print(
  runner_up_df %>%
    arrange(desc(`pvb.RAR`)) %>%
    select(Tm, Year, `pvb.RAR`),
  n = 24)

# Rank teams by ab.RE24
print(
  runner_up_df %>%
    arrange(desc(`ab.RE24`)) %>%
    select(Tm, Year, `ab.RE24`),
  n = 24)

# Rank teams by pvb.WAR
print(
  runner_up_df %>%
    arrange(desc(`pvb.WAR`)) %>%
    select(Tm, Year, `pvb.WAR`),
  n = 24)

# Rank teams by pvb.WAA
print(
  runner_up_df %>%
    arrange(desc(`pvb.WAA`)) %>%
    select(Tm, Year, `pvb.WAA`),
  n = 24)

# Rank teams by pvb.oRAR
print(
  runner_up_df %>%
    arrange(desc(`pvb.oRAR`)) %>%
    select(Tm, Year, `pvb.oRAR`),
  n = 24)

# Rank teams by sbm.OWn%
print(
  runner_up_df %>%
    arrange(desc(`sbm.OWn%`)) %>%
    select(Tm, Year, `sbm.OWn%`),
  n = 24)

# Rank teams by pvb.RAA
print(
  runner_up_df %>%
    arrange(desc(`pvb.RAA`)) %>%
    select(Tm, Year, `pvb.RAA`),
  n = 24)

# Looking at team record for the year 2014
print(
  df_no_2020 %>% 
    arrange(desc(`sp.W`)) %>% 
    filter(Year == 2014) %>% 
    select(Tm, `sp.W`, `sp.L`),
  n=30
)


print(
  df_no_2020 %>% 
    arrange(desc(`sp.W`)) %>% 
    filter(Year == 2005) %>% 
    select(Tm, `sp.W`, `sp.L`),
  n=30
)




################################## PCA No Year Variable #############################

# removing the Year variable
df_no_year <- df_no_2020 %>% 
  select(-Year)


# Keep only numeric columns and drop any columns that have NAs
df_numeric_no_year <- df_no_year %>%
  select(where(is.numeric)) %>%
  select(where(~ !any(is.na(.))))

# Scale the data
df_scaled_no_year <- scale(df_numeric_no_year)

# Run PCA
pca_result_no_year <- prcomp(df_scaled_no_year, center = TRUE, scale. = TRUE)

# Scree plot
screeplot(pca_result_no_year, type = "lines", main = "Scree Plot")

# Cumulative proportion of variance explained
summary(pca_result_no_year)

# View loadings
loadings_no_year <- pca_result_no_year$rotation
View(loadings_no_year)

# Top 10 contributing variables to PC1
head(sort(abs(loadings_no_year[,1]), decreasing = TRUE), 10)

# PC2
head(sort(abs(loadings_no_year[,2]), decreasing = TRUE), 10)

# PC3
head(sort(abs(loadings_no_year[,3]), decreasing = TRUE), 10)

# PC4
head(sort(abs(loadings_no_year[,4]), decreasing = TRUE), 10)


###################### K-Means No Year Variable #################

# Using the first four PCs for the K-Means Model
pca_data_no_year <- as.data.frame(pca_result_no_year$x[, 1:20])

# Using the Elbow Method to pick a k value
wss <- vector()
for (k in 1:10) {
  km <- kmeans(pca_data_no_year, centers = k, nstart = 25)
  wss[k] <- km$tot.withinss
}

# Looking at the plot
plot(1:10, wss, type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal k")

# Performing kmeans
set.seed(42)
kmeans_result_no_year <- kmeans(pca_data_no_year, centers = 3, nstart = 25)

# Add cluster labels to your data
pca_data_no_year$Cluster <- as.factor(kmeans_result_no_year$cluster)
pca_data_no_year$TeamSuccess <- as.factor(df_no_2020$Team.Success)  # Adding the team success variable
pca_data_no_year$Tm <- as.factor(df_no_2020$Tm)  # Adding the team names
pca_data_no_year$Year <- as.factor(df_no_2020$Year)   # Adding year

# Table to see the distribution of team success in each cluster
table(Cluster = pca_data_no_year$Cluster, TeamSuccess = pca_data_no_year$TeamSuccess)


# Filter for WS winners (TeamSuccess == 4)
ws_winners_no_year <- subset(pca_data_no_year, TeamSuccess == 4)

# Looking at the k-means clustering with WS winners highlighted
ggplot() +
  geom_point(data = pca_data_no_year, aes(x = PC1, y = PC2, color = Cluster), alpha = 0.6) +
  geom_point(data = subset(pca_data_no_year, TeamSuccess == 4),
             aes(x = PC1, y = PC2),
             color = "black", size = 2, shape = 21, stroke = 1) +
  labs(title = "K-Means Clustering on PCA-Reduced Data",
       subtitle = "World Series Winners Highlighted",
       x = "PC1", y = "PC2") +
  theme_minimal()


# Making a clean dataset in order to find the means of the variables in each cluster
df_clean_no_year <- df_numeric_no_year
df_clean_no_year$Cluster <- pca_data_no_year$Cluster

# Grouping by cluster and getting the mean for the variables
cluster_means_no_year <- df_clean_no_year %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

View(cluster_means_no_year)













