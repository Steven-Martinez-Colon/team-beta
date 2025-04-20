## Written by Joseph Annand


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

dataset_folder <- paste(getwd(),"/final_data",sep="")

data_file <- paste(dataset_folder,"/final_dataset.csv", sep = "")

## Read data without changing special chars in column names
mlb_data <- read.csv(data_file, check.names = FALSE)

############################### Clean Data #####################################

## Convert Year to factor
mlb_data$Year <- as.factor(mlb_data$Year)

## Change Team.Success from four possible values to two
## 0 = missed playoffs, 1 = made the playoffs
mlb_data$Team.Success <- ifelse(mlb_data$Team.Success == 1,0,1)
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


###################### Correlation #############################################

# Create a list to hold sets of duplicated columns
duplicate_sets <- list()
checked_cols <- rep(FALSE, ncol(mlb_df))

for (i in 1:(ncol(mlb_df) - 1)) {
  if (checked_cols[i]) next
  
  group <- names(mlb_df)[i]
  for (j in (i + 1):ncol(mlb_df)) {
    if (!checked_cols[j] && all(mlb_df[[i]] == mlb_df[[j]], na.rm = TRUE)) {
      group <- c(group, names(mlb_df)[j])
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
duplicates_to_remove



# Set threshold
threshold <- 0.9
# Compute correlation matrix using pairwise complete observations
cor_matrix <- cor(mlb_df %>% dplyr::select(-duplicates_to_remove),
                  use = "pairwise.complete.obs")

# Find indices of highly correlated pairs
high_corr_pairs <- which(abs(cor_matrix) > threshold & lower.tri(cor_matrix), arr.ind = TRUE)

# Build a list of sets
high_corr_sets <- list()
visited <- rep(FALSE, ncol(cor_matrix))

# Lopp through each pair
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

# View result
high_corr_sets

# Export high_corr_sets to text file
sink("images/highly_correlated_sets.txt")
for (i in seq_along(high_corr_sets)) {
  cat(paste0("Set ", i, ":\n"))
  cat(paste(high_corr_sets[[i]], collapse = ", "), "\n\n")
}
sink()

