
library(tidyr)
library(dplyr)
library(stringr)
library(codebookr)
library(readr)

dataset_folder <- paste(getwd(),"/datasets",sep="")

all_csv <- list.files(path = dataset_folder, pattern = "\\.csv$", full.names = TRUE)

csv_list <- lapply(all_csv, read.csv)
names(csv_list) <- tools::file_path_sans_ext(basename(all_csv))

################################## Functions ###################################

# Function to rename columns based on first row values
rename_columns <- function(df) {
  cols_to_rename <- colnames(df)[colnames(df) != "Year"]  # Identify columns to rename
  
  colnames(df)[colnames(df) != "Year"] <- as.character(df[1, cols_to_rename])  # Rename to first value in the column
  
  # Handle NA or empty string column names
  colnames(df)[is.na(colnames(df))] <- paste0("missing", which(is.na(colnames(df))))
  colnames(df)[colnames(df) == ""] <- paste0("missing",which(colnames(df) == ""))
  
  return(df[-1, ])  # Remove first row after renaming
}


drop_na_cols <- function(df) {
  df <- df[, colSums(is.na(df)) < nrow(df)]
}


get_unique_columns <- function(df_list) {
  # Extract column names from all dataframes and combine them into one vector
  all_columns <- unlist(lapply(df_list, colnames))
  
  # Get unique column names
  unique_columns <- unique(all_columns)
  
  return(unique_columns)
}


# Function to add missing columns to any dataframe in df_list
add_missing_columns <- function(df_list, columns_to_add) {
  # Iterate through each dataframe in the list
  df_list <- lapply(df_list, function(df) {
    # Check for columns that are missing
    missing_columns <- setdiff(columns_to_add, colnames(df))
    
    # Add missing columns with NA values
    for (col in missing_columns) {
      df[[col]] <- NA  # You can change `NA` to any default value if needed
    }
    
    return(df)
  })
  
  return(df_list)
}


get_common_columns <- function(df_list) {
  common_cols <- Reduce(intersect, lapply(df_list, colnames))
  return(common_cols)
}


remove_non_observations <- function(df) {
  
  bad_values <- c("Tm","","League Average")
  
  df <- df %>%
    filter(!(Tm %in% bad_values))
  
  return(df)
}

################################################################################

current_csv <- csv_list[[1]]

# Find all indices where the first col = "Tm"
# Use this to split the csv file into different dataframes
tm_indices <- which(current_csv[[1]] == "Tm")

# Initialize a list to store the split dataframes
df_list <- list()

# Loop through "Tm" indices to extract dataframes for each season
for (i in seq_along(tm_indices)) {
  start_idx <- tm_indices[i]  # Start from "Tm"
  
  # Determine end index (either next "Tm" or end of dataframe)
  end_idx <- ifelse(i < length(tm_indices), tm_indices[i + 1] - 1, nrow(current_csv))
  
  # Extract the subset and store in list
  df_list[[paste0("df_", i)]] <- current_csv[start_idx:end_idx, ]
}


# Filter out dataframes with less than 2 rows
# Column names are written again at the end of each season so this line removes those rows
df_list <- df_list[sapply(df_list, nrow) >= 2]


# Apply rename_columns function to every dataframe in the list
df_list <- lapply(df_list, rename_columns)

# Drop columns that have all NA values
df_list <- lapply(df_list, drop_na_cols)


# Get vector of all unique column names to pass through the add_missing_columns function
columns_to_add <- get_unique_columns(df_list)

# Apply the function to the list of dataframes
df_list <- add_missing_columns(df_list, columns_to_add)


# Get the common columns between all dataframes to use in full join
common_columns <- get_common_columns(df_list)

# Perform a full join on all dataframes based on the common columns
df_full_joined <- Reduce(function(x, y) full_join(x, y, by = common_columns), df_list)

df_full_joined <- remove_non_observations(df_full_joined)

csv_list[[1]] <- df_full_joined

write.csv(df_full_joined, file = paste(getwd(),"/cleaned_data/","cleaned_",names(csv_list)[1],".csv",sep = ""))

names(csv_list)[1]


################################################################################

current_csv <- csv_list[[2]]

# Find all indices where the first col = "Tm"
# Use this to split the csv file into different dataframes
tm_indices <- which(current_csv[[1]] == "Tm")

# Initialize a list to store the split dataframes
df_list <- list()

# Loop through "Tm" indices to extract dataframes for each season
for (i in seq_along(tm_indices)) {
  start_idx <- tm_indices[i]  # Start from "Tm"
  
  # Determine end index (either next "Tm" or end of dataframe)
  end_idx <- ifelse(i < length(tm_indices), tm_indices[i + 1] - 1, nrow(current_csv))
  
  # Extract the subset and store in list
  df_list[[paste0("df_", i)]] <- current_csv[start_idx:end_idx, ]
}


# Filter out dataframes with less than 2 rows
# Column names are written again at the end of each season so this line removes those rows
df_list <- df_list[sapply(df_list, nrow) >= 2]


# Apply rename_columns function to every dataframe in the list
df_list <- lapply(df_list, rename_columns)

# Drop columns that have all NA values
df_list <- lapply(df_list, drop_na_cols)


# Get vector of all unique column names to pass through the add_missing_columns function
columns_to_add <- get_unique_columns(df_list)

# Apply the function to the list of dataframes
df_list <- add_missing_columns(df_list, columns_to_add)


# Get the common columns between all dataframes to use in full join
common_columns <- get_common_columns(df_list)

# Perform a full join on all dataframes based on the common columns
df_full_joined <- Reduce(function(x, y) full_join(x, y, by = common_columns), df_list)

df_full_joined <- remove_non_observations(df_full_joined)

csv_list[[2]] <- df_full_joined

write.csv(df_full_joined, file = paste(getwd(),"/cleaned_data/","cleaned_",names(csv_list)[2],".csv",sep = ""))

names(csv_list)[2]

################################################################################


for (i in 3:length(csv_list)) {
  csv_list[[i]] <- remove_non_observations(csv_list[[i]])
  write.csv(csv_list[[i]], file = paste(getwd(),"/cleaned_data/","cleaned_",names(csv_list)[i],".csv",sep = ""))
}


################################################################################


