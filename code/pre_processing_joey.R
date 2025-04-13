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


###################### Pre-PCA Feature Engineering #############################

## Convert Year to factor
mlb_data$Year <- as.factor(mlb_data$Year)

## Change Team.Success from four possible values to two
## 0 = missed playoffs, 1 = made the playoffs
mlb_data$Team.Success <- ifelse(mlb_data$Team.Success == 1,0,1)
mlb_data$Team.Success <- as.factor(mlb_data$Team.Success)

## Check how observations are distributed among new response classes
freq_table <- table(mlb_data$Team.Success)                # Frequency count
percent_table <- prop.table(freq_table) * 100  # Convert to percentages

## Create summary table of response class distribution
summary_table <- data.frame(
  Team.Success = names(freq_table),
  Frequency = as.vector(freq_table),
  Percentage = round(as.vector(percent_table), 2)
)

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

## Remove cpi and salary cols not adjusted for inflation
mlb_data <- mlb_data %>% select(!all_of(c(cpi,salary_cols)))

## Filter out data from 2020 season
mlb_data <- mlb_data %>% filter(Year != "2020")

## Set rownames equal to concatenation of Team and Year
rownames(mlb_data) <- paste(mlb_data$Tm, mlb_data$Year, sep = "_")

## Drop columns with NA values
no_na_cols <- names(mlb_data)[sapply(mlb_data, function(x) !any(is.na(x)))]
mlb_data <- mlb_data %>% select(all_of(no_na_cols))

## Drop non-numeric cols
num_cols <- names(mlb_data)[sapply(mlb_data, is.numeric)]

## Create final MLB data set
mlb_df <- mlb_data %>% select(all_of(num_cols)) %>% mutate(Team.Success = mlb_data$Team.Success)

## Create dataset for PCA
pca_mlb_data <- mlb_data %>% select(all_of(num_cols))


################################# PCA ##########################################

## https://www.datacamp.com/tutorial/pca-analysis-r
## https://bryanhanson.github.io/LearnPCA/articles/Vig_07_Functions_PCA.html

## normalize numeric data
# scaled_data <- scale(mlb_data)

## PCA
data.pca <- princomp(pca_mlb_data, cor = T)
summary(data.pca)

## View loadings
View(data.pca$loadings[,1:75])

## Scree plot
fviz_eig(data.pca, addlabels = T, ncp = 20)

## Biplot with cos2
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = T)

## Create dataframe with first 20 principal components
pc_df <- as.data.frame(data.pca$scores[,1:20])
pc_df$Team.Success <- mlb_data$Team.Success

############################### Split Data #####################################

## Calculate ideal train:test split ratio for PCA scores data
ratio <- calcSplitRatio(df = pc_df) ## 0.78:0.22

## Get training index and stratify by response
set.seed(123)
train_index <- createDataPartition(pc_df$Team.Success, p = ratio, list = F)

pc_train <- pc_df[train_index, ]
pc_test <- pc_df[-train_index, ]




############################### Encoding #######################################



###################### Imputation of Missing Values  ###########################

any(is.na(pc_train)) # should be FALSE
any(is.na(pc_test)) # should be FALSE

###################### Perform Arithmetic Transformation #######################


###################### Normalize, center, and/or scale #########################


