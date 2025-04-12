## Written by Joseph Annand


## Load libraries
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
initial_corrplot <- corrplot::corrplot(cor_matrix, method = "color", tl.cex = 0.6, tl.pos = "n")

# create heat map of correlation values with ggcorrplot
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = FALSE)

high_corr <- findCorrelation(cor_matrix, cutoff = 0.8, names = TRUE)
print(high_corr)

remove_high_corr <- cor_data[, !names(cor_data) %in% high_corr]

rhc_cor_matrix <- cor(remove_high_corr %>% select(any_of(num_cor_cols)),
                      use = "pairwise.complete.obs")

rhc_corrplot <- corrplot::corrplot(rhc_cor_matrix, method = "color", tl.cex = 0.6, tl.pos = "n")


################################################################################

pca_cols <- c("sbm.RC","sb.TB","sb.R","sb.RBI","sb.H","sb.LOB","pvb.PA","sp.L","sp.W-L%",
              "pvp.R","sp.R","ap.RE24","sp.ER","pvb.WAA","pvp.RA9","sp.ERA","ap.OPS","sp.RA/G",
              "tf.RA/G","sp.FIP","ap.SLG","sp.#P","tf.#Fld","sb.#Bat","ab.SO%","sb.SO","sb.SH","sbm.lgBA")

cor_matrix <- cor(cor_data %>% select(all_of(pca_cols)), use = "pairwise.complete.obs")

pca_corrplot <- corrplot(cor_matrix, method = "color", tl.cex = 0.6, tl.col = "black")

pca_cols <- c("Tm","Year","Team.Success",pca_cols)
pca_data <- cor_data %>% select(all_of(pca_cols))

################################################################################

freq_table <- table(pca_data$Team.Success)                # Frequency count
percent_table <- prop.table(freq_table) * 100  # Convert to percentages

# Combine into a single data frame
summary_table <- data.frame(
  Team.Success = names(freq_table),
  Frequency = as.vector(freq_table),
  Percentage = round(as.vector(percent_table), 2)
)

print(summary_table)

write_csv(summary_table, paste(getwd(),"/images/summary_table.csv", sep=""))

# Example: response is your response variable (factor or integer), and df is your data frame
summary_stats <- pca_data %>%
  group_by(Team.Success) %>%
  summarise(across(where(is.numeric), list(
    mean = ~mean(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    iqr = ~IQR(.x, na.rm = TRUE),
    skew = ~skewness(.x, na.rm = TRUE),
    kurt = ~kurtosis(.x, na.rm = TRUE)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(-Team.Success, names_to = c("Variable", "Statistic"), names_sep = "_") %>%
  pivot_wider(names_from = Statistic, values_from = value)

write.csv(summary_stats, paste(getwd(),"/images/summary_stats.csv", sep=""))

############## Debugging standard deviation is zero error from cor() ###########

zero_sd_cols <- sapply(mlb_data, function(x) is.numeric(x) && sd(x, na.rm = TRUE) == 0)
names(mlb_data)[zero_sd_cols]


bad_cols <- sapply(mlb_data, function(x) is.numeric(x) && length(unique(na.omit(x))) <= 1)
names(mlb_data)[bad_cols]


col_sd <- apply(mlb_data[,numeric_cols], 2, function(x) sd(x, na.rm = TRUE))

View(data.frame(column = numeric_cols,
                sd = col_sd))
