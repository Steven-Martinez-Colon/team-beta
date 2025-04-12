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
source("code/calcSplitRatio-3.R")

dataset_folder <- paste(getwd(),"/final_data",sep="")

data_file <- paste(dataset_folder,"/final_dataset.csv", sep = "")

## Read data without changing special chars in column names
mlb_data <- read.csv(data_file, check.names = FALSE)

## Convert Tema and Year to factors
mlb_data$Year <- as.factor(mlb_data$Year)
mlb_data$Team.Success <- as.factor(mlb_data$Team.Success)


############################### Clean Data #####################################

## Set rownames equal to concatenation of Team and Year
rownames(mlb_data) <- paste(mlb_data$Tm, mlb_data$Year, sep = "_")

## Filter out data from 2020 season
mlb_data <- mlb_data %>% filter(Year != "2020")

## Drop columns with NA values
no_na_cols <- names(mlb_data)[sapply(mlb_data, function(x) !any(is.na(x)))]
mlb_data <- mlb_data %>% select(all_of(no_na_cols))

## Drop non-numeric cols
num_cols <- names(mlb_data)[sapply(mlb_data, is.numeric)]
mlb_data <- mlb_data %>% select(all_of(num_cols))


################################# PCA ##########################################



############################### Split Data #####################################

calcSplitRatio(df = mlb_data)

############################### Encoding #######################################




###################### Imputation of Missing Values  ###########################




###################### Perform Arithmetic Transformation #######################



###################### Normalize, center, and/or scale #########################


