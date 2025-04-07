
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

dataset_folder <- paste(getwd(),"/final_data",sep="")

data_file <- paste(dataset_folder,"/final_dataset.csv", sep = "")

mlb_data <- read.csv(data_file, check.names = FALSE)

################################################################################

mlb_data$Year <- as.factor(mlb_data$Year)
mlb_data$Team.Success <- as.factor(mlb_data$Team.Success)

numeric_mlb_cols <- names(mlb_data)[sapply(mlb_data, is.numeric)]

cols_without_na <- names(mlb_data)[sapply(mlb_data, function(x) !any(is.na(x)))]

cor_data <- mlb_data %>% select(cols_without_na)
num_cor_cols <- names(cor_data)[sapply(cor_data, is.numeric)]

################################################################################

cor_matrix <- cor(cor_data %>% select(all_of(num_cor_cols)), use = "pairwise.complete.obs")

# create heatmap of correlation values with corrplot
corrplot::corrplot(cor_matrix, method = "color", tl.cex = 0.6, order = "hclust")

# create heat map of correlation values with ggcorrplot
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = FALSE)

high_corr <- findCorrelation(cor_matrix, cutoff = 0.8, names = TRUE)
print(high_corr)

remove_high_corr <- cor_data[, !names(cor_data) %in% high_corr]

rhc_cor_matrix <- cor(remove_high_corr %>% select(any_of(num_cor_cols)),
                      use = "pairwise.complete.obs")

corrplot::corrplot(rhc_cor_matrix, method = "color", tl.cex = 0.6, order = "hclust")

############## Debugging standard deviation is zero error from cor() ###########

zero_sd_cols <- sapply(mlb_data, function(x) is.numeric(x) && sd(x, na.rm = TRUE) == 0)
names(mlb_data)[zero_sd_cols]


bad_cols <- sapply(mlb_data, function(x) is.numeric(x) && length(unique(na.omit(x))) <= 1)
names(mlb_data)[bad_cols]


col_sd <- apply(mlb_data[,numeric_cols], 2, function(x) sd(x, na.rm = TRUE))

View(data.frame(column = numeric_cols,
                sd = col_sd))
