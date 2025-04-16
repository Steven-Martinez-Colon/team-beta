# Steven Martinez

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


############################# Scraping Team Advanced Batting Data ################################

# Function to scrape "Team Advanced Batting" data for a given year
scrape_advanced_batting_data <- function(year) {
  # Construct the URL for the given year
  url <- paste0("https://www.baseball-reference.com/leagues/majors/", year, "-advanced-batting.shtml")
  
  # Read the HTML content of the page
  webpage <- read_html(url)
  
  # Extract the HTML content as a string
  html_content <- as.character(webpage)
  
  # Remove all HTML comments
  html_content_no_comments <- gsub("<!--|-->", "", html_content)
  
  # Parse the cleaned HTML content
  cleaned_html <- read_html(html_content_no_comments)
  
  # Extract the "Team Advanced Batting" table
  advanced_batting_table <- cleaned_html %>%
    html_node("#teams_advanced_batting") %>%
    html_table()
  
  # Add a column for the year
  advanced_batting_table$Year <- year
  
  return(advanced_batting_table)
}


# Initialize an empty list to store data frames
advanced_batting_data_list <- list()

# Loop through each year from 2000 to 2024
for (year in 2000:2024) {
  message("Scraping data for year: ", year)
  Sys.sleep(3) # Pause to not get locked out of scraping from website
  year_data <- scrape_advanced_batting_data(year)
  advanced_batting_data_list[[as.character(year)]] <- year_data
}

# Combine all yearly data frames into one
advanced_batting_data <- bind_rows(advanced_batting_data_list)

# Save the data to a CSV file
write.csv(advanced_batting_data, "Team_Advanced_Batting_2000_2024.csv", row.names = FALSE)


############################# Scraping Team Advanced Pitching Data ################################

# Function to scrape "Team Advanced Pitching" data for a given year
scrape_advanced_pitching_data <- function(year) {
  # Construct the URL for the given year
  url <- paste0("https://www.baseball-reference.com/leagues/majors/", year, "-advanced-pitching.shtml")
  
  # Read the HTML content of the page
  webpage <- read_html(url)
  
  # Extract the HTML content as a string
  html_content <- as.character(webpage)
  
  # Remove all HTML comments
  html_content_no_comments <- gsub("<!--|-->", "", html_content)
  
  # Parse the cleaned HTML content
  cleaned_html <- read_html(html_content_no_comments)
  
  # Extract the "Team Advanced Pitching" table
  advanced_pitching_table <- cleaned_html %>%
    html_node("#teams_advanced_pitching") %>%
    html_table()
  
  # Add a column for the year
  advanced_pitching_table$Year <- year
  
  return(advanced_pitching_table)
}


# Initialize an empty list to store data frames
advanced_pitching_data_list <- list()

# Loop through each year from 2000 to 2024
for (year in 2000:2024) {
  message("Scraping data for year: ", year)
  Sys.sleep(3) # Pause to not get locked out of scraping from website
  year_data <- scrape_advanced_pitching_data(year)
  advanced_pitching_data_list[[as.character(year)]] <- year_data
}

# Combine all yearly data frames into one
advanced_pitching_data <- bind_rows(advanced_pitching_data_list)

# Save the data to a CSV file
write.csv(advanced_pitching_data, "Team_Advanced_Pitching_2000_2024.csv", row.names = FALSE)


############################# Scraping Team Fielding Data ################################

# Function to scrape "Team Standard Pitching" data for a given year
scrape_fielding_data <- function(year) {
  # Construct the URL for the given year
  url <- paste0("https://www.baseball-reference.com/leagues/majors/", year, "-standard-fielding.shtml")
  
  # Read the HTML content of the page
  webpage <- read_html(url)
  
  # Extract the HTML content as a string
  html_content <- as.character(webpage)
  
  # Remove all HTML comments
  html_content_no_comments <- gsub("<!--|-->", "", html_content)
  
  # Parse the cleaned HTML content
  cleaned_html <- read_html(html_content_no_comments)
  
  # Extract the "Team Standard Fielding" table
  fielding_table <- cleaned_html %>%
    html_node("#teams_standard_fielding") %>%
    html_table()
  
  # Add a column for the year
  fielding_table$Year <- year
  
  return(fielding_table)
}


# Initialize an empty list to store data frames
fielding_data_list <- list()

# Loop through each year from 2000 to 2024
for (year in 2000:2024) {
  message("Scraping data for year: ", year)
  Sys.sleep(3) # Pause to not get locked out of scraping from website
  year_data <- scrape_fielding_data(year)
  fielding_data_list[[as.character(year)]] <- year_data
}

# Combine all yearly data frames into one
fielding_data <- bind_rows(fielding_data_list)

# Save the data to a CSV file
write.csv(fielding_data, "Team_Fielding_2000_2024.csv", row.names = FALSE)


############################# Scraping Team Sabermetric Batting Data ################################

# Function to scrape "Team Sabermetric Batting" data for a given year
scrape_sabermetric_batting_data <- function(year) {
  # Construct the URL for the given year
  url <- paste0("https://www.baseball-reference.com/leagues/majors/", year, "-sabermetric-batting.shtml")
  
  # Read the HTML content of the page
  webpage <- read_html(url)
  
  # Extract the HTML content as a string
  html_content <- as.character(webpage)
  
  # Remove all HTML comments
  html_content_no_comments <- gsub("<!--|-->", "", html_content)
  
  # Parse the cleaned HTML content
  cleaned_html <- read_html(html_content_no_comments)
  
  # Extract the "Team Sabermetric Batting" table
  sabermetric_table <- cleaned_html %>%
    html_node("#teams_sabermetric_batting") %>%
    html_table()
  
  # Add a column for the year
  sabermetric_table$Year <- year
  
  return(sabermetric_table)
}


# Initialize an empty list to store data frames
sabermetric_data_list <- list()

# Loop through each year from 2000 to 2024
for (year in 2000:2024) {
  message("Scraping data for year: ", year)
  Sys.sleep(3) # Pause to not get locked out of scraping from website
  year_data <- scrape_sabermetric_batting_data(year)
  sabermetric_data_list[[as.character(year)]] <- year_data
}

# Combine all yearly data frames into one
sabermetric_batting_data <- bind_rows(sabermetric_data_list)

# Save the data to a CSV file
write.csv(sabermetric_batting_data, "Team_Sabermetric_Batting_2000_2024.csv", row.names = FALSE)


############################# Scraping Team Player Value - Batters Data ################################

# Function to scrape "Team Player Value Batters" data for a given year
scrape_player_value_batters_data <- function(year) {
  # Construct the URL for the given year
  url <- paste0("https://www.baseball-reference.com/leagues/majors/", year, "-value-batting.shtml")
  
  # Read the HTML content of the page
  webpage <- read_html(url)
  
  # Extract the HTML content as a string
  html_content <- as.character(webpage)
  
  # Remove all HTML comments
  html_content_no_comments <- gsub("<!--|-->", "", html_content)
  
  # Parse the cleaned HTML content
  cleaned_html <- read_html(html_content_no_comments)
  
  # Extract the "Team Player Value--Batters" table
  team_player_value_batters_table <- cleaned_html %>%
    html_node("#teams_value_batting") %>%
    html_table()
  
  # Add a column for the year
  team_player_value_batters_table$Year <- year
  
  return(team_player_value_batters_table)
}


# Initialize an empty list to store data frames
team_player_value_data_list <- list()

# Loop through each year from 2000 to 2024
for (year in 2000:2024) {
  message("Scraping data for year: ", year)
  Sys.sleep(3) # Pause to not get locked out of scraping from website
  year_data <- scrape_player_value_batters_data(year)
  team_player_value_data_list[[as.character(year)]] <- year_data
}

# Combine all yearly data frames into one
team_player_value_batters_data <- bind_rows(team_player_value_data_list)

# Save the data to a CSV file
write.csv(team_player_value_batters_data, "Team_Player_Value_Batters_2000_2024.csv", row.names = FALSE)


############################# Scraping Team Player Value - Pitchers Data ################################

# Function to scrape "Team Player Value Pitchers" data for a given year
scrape_player_value_pitchers_data <- function(year) {
  # Construct the URL for the given year
  url <- paste0("https://www.baseball-reference.com/leagues/majors/", year, "-value-pitching.shtml")
  
  # Read the HTML content of the page
  webpage <- read_html(url)
  
  # Extract the HTML content as a string
  html_content <- as.character(webpage)
  
  # Remove all HTML comments
  html_content_no_comments <- gsub("<!--|-->", "", html_content)
  
  # Parse the cleaned HTML content
  cleaned_html <- read_html(html_content_no_comments)
  
  # Extract the "Team Player Value--Pitchers" table
  team_player_value_pitchers_table <- cleaned_html %>%
    html_node("#teams_value_pitching") %>%
    html_table()
  
  # Add a column for the year
  team_player_value_pitchers_table$Year <- year
  
  return(team_player_value_pitchers_table)
}


# Initialize an empty list to store data frames
team_player_value_pitchers_data_list <- list()

# Loop through each year from 2000 to 2024
for (year in 2000:2024) {
  message("Scraping data for year: ", year)
  Sys.sleep(3) # Pause to not get locked out of scraping from website
  year_data <- scrape_player_value_pitchers_data(year)
  team_player_value_pitchers_data_list[[as.character(year)]] <- year_data
}

# Combine all yearly data frames into one
team_player_value_pitchers_data <- bind_rows(team_player_value_pitchers_data_list)

# Save the data to a CSV file
write.csv(team_player_value_pitchers_data, "Team_Player_Value_Pitchers_2000_2024.csv", row.names = FALSE)


























































