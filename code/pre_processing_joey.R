
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

mlb_data <- read.csv(data_file, check.names = FALSE)

mlb_data$Year <- as.factor(mlb_data$Year)
mlb_data$Team.Success <- as.factor(mlb_data$Team.Success)


############################### Split Data #####################################

calcSplitRatio(df = mlb_data)


############################### Encoding #######################################




###################### Imputation of Missing Values  ###########################




###################### Perform Arithmetic Transformation #######################



###################### Normalize, center, and/or scale #########################


