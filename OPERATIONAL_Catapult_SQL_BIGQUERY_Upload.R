library(bigrquery)
library(httr)
library(jsonlite)
library(tidyverse)
library(future)
library(furrr)

# Step 1: Set up authentication
Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/catapulttestproject-3cb794e3de65.json") 

# Step 2: Retrieve Dataframes of Clean Data
input_dir <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSVs_Clean"
input_dir2 = "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSV_OUTPUTS"

athletes <- read.csv(file.path(input_dir, "df_all_athletes_clean.csv"))
activities <- read.csv(file.path(input_dir, "df_activities_clean.csv"))
periods <- read.csv(file.path(input_dir, "df_all_periods_clean.csv"))
stats_activities <- read.csv(file.path(input_dir, "df_aggregate_activity_stats_clean.csv"))
stats_period <- read.csv(file.path(input_dir, "df_aggregate_period_stats_clean.csv"))
catapult_calendar <- read.csv(file.path(input_dir, "df_catapult_calandar.csv"))
stats_act_percentages = read.csv(file.path(input_dir2, "aggregate_activity_stats_against_averages.csv"))
game_averages = read.csv(file.path(input_dir2, "game_pivoted_averages.csv"))
practice_averages = read.csv(file.path(input_dir2, "practice_pivoted_averages.csv"))
team_totals_inseason = read.csv(file.path(input_dir2, "team_inseason_totals.csv"))
athlete_totals_inseason = read.csv(file.path(input_dir2, "athlete_inseason_totals.csv"))

# Function to check for invalid column names and remove duplicates
check_column_names <- function(df) {
  # Replace invalid characters
  invalid_cols <- names(df)[grepl("[^a-zA-Z0-9_]", names(df))]
  if (length(invalid_cols) > 0) {
    print("Invalid column names detected, renaming...")
    colnames(df) <- gsub("[^a-zA-Z0-9_]", "_", names(df))
  }

  # Check for duplicate column names and remove them
  col_names <- colnames(df)
  duplicated_names <- duplicated(col_names)

  if (any(duplicated_names)) {
    print("Duplicate column names detected, removing duplicates...")
    df <- df[, !duplicated(col_names)]
  }
  
  return(df)
}

#validate column names
athletes <- check_column_names(athletes)
activities <- check_column_names(activities)
periods <- check_column_names(periods)
stats_activities <- check_column_names(stats_activities)
stats_period <- check_column_names(stats_period)
catapult_calendar <- check_column_names(catapult_calendar)
practice_averages <- check_column_names(practice_averages)
game_averages <- check_column_names(game_averages)
stats_act_percentages <- check_column_names(stats_act_percentages)
athlete_totals_inseason <- check_column_names(athlete_totals_inseason)
team_totals_inseason <- check_column_names(team_totals_inseason)

# Step 3: Connect to Google BigQuery
project_id <- "catapulttestproject"
dataset <- "Cpult_test"  # Dataset name only, not fully qualified

# Create a named list of tables
upload_frame <- list(
  athletes = athletes,
  activities = activities,
  periods = periods,
  stats_activities = stats_activities,
  stats_period = stats_period,
  catapult_calendar = catapult_calendar,
  practice_averages = practice_averages,
  game_averages = game_averages,
  stats_act_percentages = stats_act_percentages,
  athlete_totals_inseason = athlete_totals_inseason,
  team_totals_inseason = team_totals_inseason
)

# Step 4: Upload each data frame to BigQuery
for (table_name in names(upload_frame)) {
  # Construct the fully qualified table name
  full_table_name <- paste(project_id, dataset, table_name, sep = ".")
  
  # Upload the data frame to BigQuery
  bq_table_upload(full_table_name, upload_frame[[table_name]], write_disposition = "WRITE_TRUNCATE")
  
  # Print a message to indicate that the upload is complete
  cat("Uploaded", table_name, "to BigQuery table", full_table_name, "\n")
}