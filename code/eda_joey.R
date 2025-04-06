
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

full_dataset <- csv_list %>% reduce(full_join, by =c("Tm","Year"))

colnames(full_dataset)
