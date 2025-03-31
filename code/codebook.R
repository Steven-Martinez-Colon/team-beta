
library(tidyr)
library(dplyr)
library(stringr)
library(codebookr)

dataset_folder <- paste(getwd(),"/datasets",sep="")

all_csv <- list.files(path = dataset_folder, pattern = "\\.csv$", full.names = TRUE)

csv_list <- lapply(all_csv, read.csv)


test <- csv_list[[1]]
View(test)

colnames(test) <- as.character(test[1,])
test <- test[-1,]

colnames(test)[colnames(test) == "2000"] <- "year"


############################# Account for NA columns ###########################

# Identify columns with blank names or containing "NA"
na_cols <- which(is.na(colnames(test)))


# Function to find first character value in a column
find_first_character_value <- function(column) {
  char_value <- column[sapply(column, is.character)][1]  # Get first character entry
  if (!is.na(char_value) && !is.null(char_value)) {
    return(char_value)
  } else {
    return(NA)  # Return NA if no character value found
  }
}

# Loop through identified columns and rename them
for (idx in na_cols) {
  new_name <- find_first_character_value(test[[idx]])
  if (!is.na(new_name)) {
    colnames(test)[idx] <- new_name  # Rename column
  }
}

###############################################################################


tm_indices <- which(test[[1]] == "Tm")

# Initialize a list to store the split dataframes
df_list <- list()

# Loop through "Tm" indices to extract subsets
for (i in seq_along(tm_indices)) {
  start_idx <- tm_indices[i]  # Start from "Tm"
  
  # Determine end index (either next "Tm" or end of dataframe)
  end_idx <- ifelse(i < length(tm_indices), tm_indices[i + 1] - 1, nrow(test))
  
  # Extract the subset and store in list
  df_list[[paste0("df_", i)]] <- test[start_idx:end_idx, ]
}

View(df_list[[1]])
View(df_list[[2]])
View(df_list[[3]])

View(df_list[["df_1"]])

# Filter out dataframes with less than 2 rows
df_list <- df_list[sapply(df_list, nrow) >= 2]


# Function to rename columns based on first row values
rename_columns <- function(df) {
  cols_to_rename <- colnames(df)[colnames(df) != "Year"]  # Identify columns to rename
  colnames(df)[colnames(df) != "Year"] <- as.character(df[1, cols_to_rename])  # Rename
  return(df[-1, ])  # Remove first row after renaming
}

# Apply function to every dataframe in the list
df_list <- lapply(df_list, rename_columns)

View(df_list[[23]])

