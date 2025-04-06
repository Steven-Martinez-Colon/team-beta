############## Loading Libraries Function ####################

load_libraries <- function(packages) {
  # Check for missing packages
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # install missing packages
  if(length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
  
  # Load all packages
  lapply(packages, library, character.only = TRUE)
  
  cat("All packages are loaded succesfully.\n")
}


# Loading necessary libraries
load_libraries(c("tidyverse", "lubridate", "stats", "ggplot2", "corrplot", "stringr", "stringi",
                 "tidymodels", "modeldata", "themis", "vip", "baguette", "janitor", "rvest",
                 "yardstick", "gsheet", "caret", "randomForest", "here", "tibble", "dplyr", "ISAR", "tidyr", "mgcv",
                 "teamcolors", "baseballr", "Lahman", "remotes", "ggcorrplot", "broom", "readr", "glmnet", "xgboost", "Matrix", "Metrics"))

# Load only the necessary functions from 'car'
library(car, exclude = "select")

# Turning off warning messages
options(warnings = 0)

####################### Loading Cleaned Data ###########################

# Loading Cleaned Data
df <- read_csv("cleaned_data/all_data.csv")

# Looking at structure
str(df)

# Removing % symbols and converting columns to numeric
df <- df %>%
  mutate(across(
    where(~ all(grepl("%", .[!is.na(.)]))), # removing % symbol
    ~ as.numeric(gsub("%", "", .)) / 100    # converting column to numeric and a proportion
  ))

# Making sure % sign was removed and columns are numeric
str(df)

# Removing firs index column
df <- df[,-1]

# Removing $ and commas from payroll variable and coverting to numeric - pvb.Salary
df <- df %>%
  mutate(`pvb.Salary` = as.numeric(gsub("[\\$,]", "", `pvb.Salary`)))

# md.Est. Payroll
df <- df %>%
  mutate(`md.Est. Payroll` = as.numeric(gsub("[\\$,]", "", `md.Est. Payroll`)))

# pvp.Salary
df <- df %>% 
  mutate(`pvp.Salary` = as.numeric(gsub("[\\$,]", "", `pvp.Salary`)))

# Looking at the structure one more time to make sure the dataset is good to go
str(df)


# Save to CSV as backup
write.csv(df, "final_dataset.csv", row.names = FALSE)
















