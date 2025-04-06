
library(tidyr)
library(dplyr)
library(forcats)
library(stringr)
library(ggplot2)
library(purrr)
library(readr)


dataset_folder <- paste(getwd(),"/cleaned_data",sep="")

all_csv <- list.files(path = dataset_folder, pattern = "\\.csv$", full.names = TRUE)

csv_list <- lapply(all_csv, function(file) read.csv(file, check.names = FALSE))

names(csv_list) <- tools::file_path_sans_ext(basename(all_csv))

###############################################################################

# create abbreviations for which csv file each column in full dataset came from
abbreviations <- c("ab","ap","tf","md","pvb","pvp","sbm","sb","sp")

# Remove index rows with no column name
for (i in 1:length(csv_list)) {
  working_csv <- csv_list[[i]]
  working_csv <- working_csv[,names(working_csv) != ""]
  csv_list[[i]] <- working_csv
}

# Add abbreviation to columns in each csv
for (i in 1:length(csv_list)) {
  working_csv <- csv_list[[i]]
  for (col in colnames(working_csv)) {
    # do not change the Tm and year columns so that full_join can be performed
    if (col != "Tm" & col != "Year") {
      new_col <- paste(abbreviations[i],col,sep = ".")
      names(working_csv)[names(working_csv) == col] <- new_col
    }
  }
  csv_list[[i]] <- working_csv
}


################################################################################


full_dataset <- csv_list %>% reduce(full_join, by =c("Tm","Year"))

colnames(full_dataset)

write.csv(full_dataset, paste(dataset_folder,"/all_data.csv", sep=""))
