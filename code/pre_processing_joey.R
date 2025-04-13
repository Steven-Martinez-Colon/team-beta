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
source("code/calcSplitRatio-3.R")

dataset_folder <- paste(getwd(),"/final_data",sep="")

data_file <- paste(dataset_folder,"/final_dataset.csv", sep = "")

## Read data without changing special chars in column names
mlb_data <- read.csv(data_file, check.names = FALSE)

## Convert Tema and Year to factors
mlb_data$Year <- as.factor(mlb_data$Year)
mlb_data$Team.Success <- as.factor(mlb_data$Team.Success)

## Create dataframe of CPI values for 2000-2024
## https://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/
cpi_table <- data.frame(
  "Year" = as.factor(2000:2024),
  "cpi" = c(172.2,177.1,179.9,184.0,188.9,195.3,201.6,207.3,215.303,214.537,
            218.056,224.939,229.594,232.957,236.736,237.017,240.007,245.120,
            251.107,255.657,258.811,270.970,292.655,304.702,313.689)
)

## Merge mlb_data with cpi_table by year
mlb_data <- merge(mlb_data, cpi_table, by="Year")

## Adjust for inflation
base_cpi <- cpi_table$cpi[cpi_table$Year == "2024"]

## Get salary and payroll columns
salary_cols <- grep("Salary|Payroll", colnames(mlb_data), value = T)

## Create columns adjusted for for inflation
for (column in salary_cols) {
  new_col <- paste0(column,"adj")
  mlb_data[new_col] <- mlb_data[column] * (base_cpi / mlb_data$cpi)
}

############################### Clean Data #####################################

## Remove salary cols not adjusted for inflation
mlb_data <- mlb_data %>% select(!all_of(salary_cols))

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

## https://www.datacamp.com/tutorial/pca-analysis-r
## https://bryanhanson.github.io/LearnPCA/articles/Vig_07_Functions_PCA.html

## normalize numeric data
# scaled_data <- scale(mlb_data)

## PCA
data.pca <- princomp(mlb_data, cor = T)
summary(data.pca)

## View loadings
View(data.pca$loadings[,1:75])

## Scree plot
fviz_eig(data.pca, addlabels = T, ncp = 20)

## Biplot with cos2
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = T)

############################### Split Data #####################################

calcSplitRatio(df = mlb_data)

############################### Encoding #######################################




###################### Imputation of Missing Values  ###########################




###################### Perform Arithmetic Transformation #######################



###################### Normalize, center, and/or scale #########################


