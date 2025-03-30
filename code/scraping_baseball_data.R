############# Loading Libraries Function ####################

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
load_libraries(c("tidyverse", "lubridate", "stats", "ggplot2", "corrplot", "stringr", "stringi", "purr",
                 "tidymodels", "modeldata", "themis", "vip", "baguette", "janitor", "rvest",
                 "yardstick", "gsheet", "caret", "randomForest", "here", "tibble", "dplyr", "ISAR", "tidyr", "mgcv",
                 "teamcolors", "baseballr", "Lahman", "remotes", "ggcorrplot", "broom", "readr", "glmnet",
                 "xgboost", "Matrix", "Metrics", "codebookr"))

# Load only the necessary functions from 'car'
library(car, exclude = "select")

# Turning off warning messages
options(warnings = 0)

############################# Scraping Team Standard Batting Data ################################

# Function to scrape "Team Standard Batting" data for a given year
scrape_batting_data <- function(year) {
  # Construct the URL for the given year
  url <- paste0("https://www.baseball-reference.com/leagues/majors/", year, ".shtml")
  
  # Read the HTML content of the page
  webpage <- read_html(url)
  
  # Extract the HTML content as a string
  html_content <- as.character(webpage)
  
  # Remove all HTML comments
  html_content_no_comments <- gsub("<!--|-->", "", html_content)
  
  # Parse the cleaned HTML content
  cleaned_html <- read_html(html_content_no_comments)
  
  # Extract the "Team Standard Batting" table
  batting_table <- cleaned_html %>%
    html_node("#teams_standard_batting") %>%
    html_table()
  
  # Add a column for the year
  batting_table$Year <- year
  
  return(batting_table)
}


# Initialize an empty list to store data frames
batting_data_list <- list()

# Loop through each year from 2000 to 2024
for (year in 2000:2024) {
  message("Scraping data for year: ", year)
  Sys.sleep(3) # Pause to not get locked out of scraping from website
  year_data <- scrape_batting_data(year)
  batting_data_list[[as.character(year)]] <- year_data
}

# Combine all yearly data frames into one
batting_data <- bind_rows(batting_data_list)

# Save the data to a CSV file
write.csv(batting_data, "Team_Standard_Batting_2000_2024.csv", row.names = FALSE)


############################# Scraping Team Standard Pitching Data ################################


# Function to scrape "Team Standard Pitching" data for a given year
scrape_pitching_data <- function(year) {
  # Construct the URL for the given year
  url <- paste0("https://www.baseball-reference.com/leagues/majors/", year, ".shtml")
  
  # Read the HTML content of the page
  webpage <- read_html(url)
  
  # Extract the HTML content as a string
  html_content <- as.character(webpage)
  
  # Remove all HTML comments
  html_content_no_comments <- gsub("<!--|-->", "", html_content)
  
  # Parse the cleaned HTML content
  cleaned_html <- read_html(html_content_no_comments)
  
  # Extract the "Team Standard Pitching" table
  pitching_table <- cleaned_html %>%
    html_node("#teams_standard_pitching") %>%
    html_table()
  
  # Add a column for the year
  pitching_table$Year <- year
  
  return(pitching_table)
}


# Initialize an empty list to store data frames
pitching_data_list <- list()

# Loop through each year from 2000 to 2024
for (year in 2000:2024) {
  message("Scraping data for year: ", year)
  Sys.sleep(3) # Pause to not get locked out of scraping from website
  year_data <- scrape_pitching_data(year)
  pitching_data_list[[as.character(year)]] <- year_data
}

# Combine all yearly data frames into one
pitching_data <- bind_rows(pitching_data_list)

# Save the data to a CSV file
write.csv(pitching_data, "Team_Standard_Pitching_2000_2024.csv", row.names = FALSE)















































































