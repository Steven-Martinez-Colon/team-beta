
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