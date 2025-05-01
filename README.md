# Catapult
Project for Custom Catapult API,Cleaning, and Aggregation

 
Contents
Data Workflow	3
API Calls	4
API Calling – First Run	8
	10
Dictionary of Important Components	10
Functions	10
Key Dataframes	10
Thresholds and Parameters	10
Output Files	11
API Calling – RERun	11
Data Cleaning	13
Steps	13
	15
Dictionary of Important Components	15
Functions	15
Thresholds	15
Key Dataframes	16
Data Aggregation and Calculations	16
Data Upload to BigQuery	16
Installation Process – Changeable Factors	19
1. Overview	19
2. Prerequisites	19
3. Installation Steps	20
5.	Relational Table Visualization	25

 
Data Workflow
1.	API Call To Pull Data
a.	First Run = Script: “OPERATIONAL_Catapult__FirstRun_API_Scipt_V3.R”
b.	All Following Runs = “OPERATIONAL_Catapult__ReRUN_API_Scipt_V1.R”
2.	Data Cleaning
a.	Script: “OPERATIONAL_Catapult_Cleaning_V3.R”
3.	Aggregate and Calculate Useful Data
a.	Script: “OPERATIONAL_Catapult_Aggregate_Data_Calculations.R”
4.	Upload to Bigquery
a.	Script: “OPERATIONAL_Catapult_SQL_BIGQUERY_Upload” 
 
 
API Calls
API Call and Transfer To Data Frame
R Code	Steps
#fetch activities
message("Fetching activities...")
activites_url = "https://connect-us.catapultsports.com/api/v6/activities"
activities_data = tryCatch({
  fetch_data(activites_url)
},error = function(e) { 
  print(paste("Error fetching Activites Data:",e$message)) 
  NULL
})
df_activities = if (!is.null(activities_data)) as.data.frame(activities_data) else data.frame()
# Rename column 'id' to 'activity_id'
if ("id" %in% names(df_activities)) {
  colnames(df_activities)[colnames(df_activities) == "id"] <- "activity_id"
}
df_activities = df_activities %>% 
  mutate(
    start_date = as.Date.POSIXct(start_time),
    end_date = as.Date.POSIXct(end_time),
    activity_date = start_date
  )
rm(activities_data ) #clean up redundancy 
	1.	Message – fetching activities
2.	Set the URL for the API
3.	Set a temporary function with trycatch that calls fetch_data(specific URL)
a.	Automated debugging with error statement
4.	Set dataframe 
a.	Only If data exists set dataframe
b.	Immediate needed cleaning here
5.	Remove redundancies
Fetch Data Function – Generic
R Code	Steps
# Define function to fetch data from API
fetch_data <- function(url) {
  response <- VERB("GET", url, add_headers(.headers = headers), content_type("application/octet-stream"))
  
  if (status_code(response) == 200) {
    raw_content <- content(response, "text", encoding = "UTF-8")
    if (nchar(raw_content) > 0) {
      return(fromJSON(raw_content, flatten = TRUE))
    } else {
      print(paste("Warning: Empty API response from:", url))
      return(NULL)
    }
  } else {
    print(paste("Error fetching data from:", url, "Status code:", status_code(response)))
    return(NULL)
  }
}
	1.	Define as Function with the parameter URL
2.	Combine the necessary components for the API call (URL, headers, content type)
3.	If statement
a.	If the call works properly encode the raw text at UTF-8
b.	Flatten the the JSON response 
c.	OTHERWISE – IF ERROR = warning statement

CatapultR – Aggregate Stats
R Code	Steps
# Load period slugs
period_slugs <- read.csv("C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/Foundational_CSVs/slugs_periods.csv",
                         header = FALSE, stringsAsFactors = FALSE)

# Convert to character vector
period_slugs <- as.character(period_slugs[[1]])

# Function to fetch stats for a single period
fetch_period_stats <- function(period_id) {
  max_retries <- 5  # Maximum number of retries
  retry_delay <- 1  # Initial delay in seconds
  
  for (retry in 1:max_retries) {
    tryCatch({
      # Rate limiting: Pause for retry_delay seconds
      if (api_request_count > 0) {
        message(sprintf("Pausing for %d seconds to avoid rate limiting...", retry_delay))
        Sys.sleep(retry_delay)
      }
      api_request_count <<- api_request_count + 1  # Increment counter
      
      data <- ofCloudGetStatistics(
        catapultr_token, 
        params = period_slugs, 
        filters = list(
          name = "period_id",
          comparison = "=",
          values = period_id
        )
      )
      
      # Handle empty or malformed responses
      if (is.null(data) || nrow(data) == 0 || length(data) == 0) {
        missing_data_log[[length(missing_data_log) + 1]] <- data.frame(period_id = period_id, error = "No data returned or empty response")
        return(NULL)
      }
      
      # Enforce consistent data types to prevent bind_rows() errors
      data <- data %>%
        mutate(across(where(is.numeric), as.character),  # Convert all numeric to character
               period_name = as.character(period_name))  # Ensure period_name is always character
      
      return(data)
      
    }, error = function(e) {
      if (grepl("status_code = 429", e$message)) {
        # Exponential backoff
        retry_delay <<- retry_delay * 2
        message(sprintf("Rate limit hit. Retrying in %d seconds...", retry_delay))
      } else {
        message(sprintf("Error fetching data for period_id: %s. Error: %s", period_id, e$message))
        missing_data_log[[length(missing_data_log) + 1]] <- data.frame(period_id = period_id, error = e$message)
        return(NULL)
      }
    })
  }

# Fetch all period statistics using map()
df_period_stats_list <- map(period_id_list, fetch_period_stats)

df_period_stats_list <- df_period_stats_list %>%
  map(parse_method_b) %>%
  bind_rows()

# Remove NULL values
df_period_stats_list <- compact(df_period_stats_list)

# Combine all results into a single data frame
df_period_stats <- df_period_stats_list %>%
  map(~ mutate(., period_name = as.character(period_name))) %>%  # Ensure period_name is character
  bind_rows()	1.	Load and create a list of slugs. This is the catapult name for the parameters we want to save.  
2.	Set rate limiter for to prevent API rate limit errors (429 Errors)
3.	Set function to pull activity data for each athlete in each activity.
a.	Filters is what differentiates period vs activity version of this function.
4.	 Handle empty returns
5.	Bind rows
6.	Use map to run loop through all activities
7.	Parse with method B
Parse Method B	Steps
parse_method_b <- function(data) {
  data %>%
    mutate(
      across(where(is.list), ~ map_chr(.x, function(item) {
        if (is.null(item) || length(item) == 0) {
          return(NA_character_)  # Handle NULL and empty lists
        }
        if (is.data.frame(item)) {
          return(jsonlite::toJSON(item, auto_unbox = TRUE, null = "null"))  # Convert data frames to JSON
        }
        if (is.list(item)) {
          return(paste(unlist(item, use.names = FALSE), collapse = ", "))  # Flatten lists
        }
        as.character(item)  # Convert other types to character
      }))
    ) %>%
    mutate(across(where(is.numeric), as.character))  # Convert numeric columns to character
}	1.	If there is a list – map the characters, 
2.	Convert data frames to json
3.	Flatten all responses
4.	Convert everything to string type

 
API Calling – First Run
1. Load Libraries
•	Load necessary libraries: httr, jsonlite, tidyverse, and catapultR.
2. Set Date Range
•	Define start_date and end_date for filtering data (currently end_date is empty).
3. Set API Token
•	Define auth_token for API authentication.
•	Create API headers with the token.
•	Rebuild the token for catapultR using ofCloudCreateToken.
4. Define Helper Functions
•	fetch_data(url): Fetches data from the API.
o	Step 1: Send a GET request to the specified URL.
o	Step 2: Check if the response status code is 200 (success).
o	Step 3: Parse the JSON response and return it.
o	Step 4: Handle errors or empty responses.
•	parse_method_b(data): Parses nested lists in the data.
o	Step 1: Convert nested lists to JSON or flatten them.
o	Step 2: Convert numeric columns to character.
•	parse_method_c(data): Unnests nested lists into a long format.
o	Step 1: Unnest all nested columns.
o	Step 2: Convert numeric columns to character.
5. Fetch Athletes Data
•	Step 1: Set the URL for fetching athlete data.
•	Step 2: Use fetch_data to retrieve athlete data.
•	Step 3: Convert the response to a dataframe.
•	Step 4: Rename the id column to athlete_id.
•	Step 5: Clean up redundant data.
6. Fetch Activities Data
•	Step 1: Set the URL for fetching activity data.
•	Step 2: Use fetch_data to retrieve activity data.
•	Step 3: Convert the response to a dataframe.
•	Step 4: Rename the id column to activity_id.
•	Step 5: Handle nested activity_athletes by converting them to comma-separated strings.
•	Step 6: Convert time-related columns to appropriate formats.
•	Step 7: Clean up redundant data.
7. Fetch Periods Data
•	Step 1: Set the URL for fetching period data.
•	Step 2: Use fetch_data to retrieve period data.
•	Step 3: Convert the response to a dataframe.
•	Step 4: Rename the id column to period_id.
•	Step 5: Convert nested lists to comma-separated strings.
•	Step 6: Convert time-related columns to appropriate formats.
•	Step 7: Clean up redundant data.
8. Fetch Period Aggregate Data
•	Step 1: Load period slugs from a CSV file.
•	Step 2: Extract valid period IDs from df_periods.
•	Step 3: Define a function fetch_period_stats to fetch stats for each period.
o	Step 3.1: Handle rate limiting with exponential backoff.
o	Step 3.2: Fetch data using ofCloudGetStatistics.
o	Step 3.3: Handle empty or malformed responses.
•	Step 4: Fetch stats for all periods using map.
•	Step 5: Parse the fetched data using parse_method_b.
•	Step 6: Combine all results into a single dataframe.
•	Step 7: Save missing data logs to a CSV file if errors occur.
9. Fetch Summary GPS Data for Activities
•	Step 1: Fetch parameters (metrics) from the API.
•	Step 2: Convert the response to a dataframe.
•	Step 3: Extract slugs for needed parameters from a CSV file.
•	Step 4: Extract valid activity IDs from df_activities.
•	Step 5: Define a function fetch_activity_stats to fetch stats for each activity.
o	Step 5.1: Handle rate limiting with exponential backoff.
o	Step 5.2: Fetch data using ofCloudGetStatistics.
o	Step 5.3: Handle empty or malformed responses.
•	Step 6: Fetch stats for all activities using map.
•	Step 7: Parse the fetched data using parse_method_b.
•	Step 8: Combine all results into a single dataframe.
•	Step 9: Save missing data logs to a CSV file if errors occur.
10. Save Data to CSV
•	Step 1: Define the output directory.
•	Step 2: Save each dataframe to a CSV file if data is available:
o	df_all_athletes_raw.csv
o	df_activities_raw.csv
o	df_periods_raw.csv
o	df_aggregate_activity_stats_raw.csv
o	df_aggregate_period_stats_raw.csv
•	Step 3: Print a summary message indicating the number of files successfully saved.
________________________________________
 
Dictionary of Important Components
Functions
•	fetch_data(url): Fetches data from the API and handles errors.
•	parse_method_b(data): Parses nested lists in the data.
•	parse_method_c(data): Unnests nested lists into a long format.
•	fetch_period_stats(period_id): Fetches stats for a single period with retry logic.
•	fetch_activity_stats(activity_id): Fetches stats for a single activity with retry logic.
Key Dataframes
•	df_all_athletes: Contains athlete data fetched from the API.
•	df_activities: Contains activity data fetched from the API.
•	df_periods: Contains period data fetched from the API.
•	df_activities_stats: Contains aggregated stats for activities.
•	df_period_stats: Contains aggregated stats for periods.
Thresholds and Parameters
•	Rate Limiting:
o	Max Retries: 5.
o	Retry Delay: Starts at 1 second and doubles on each retry.
•	Data Validation:
o	Empty Responses: Logged and skipped.
o	Malformed Data: Logged and skipped.
Output Files
•	CSV Files:
o	df_all_athletes_raw.csv
o	df_activities_raw.csv
o	df_periods_raw.csv
o	df_aggregate_activity_stats_raw.csv
o	df_aggregate_period_stats_raw.csv
•	Error Logs:
o	periods_error_log_YYYY-MM-DD.csv
o	activities_error_log_YYYY-MM-DD.csv
API Calling – RERun
*Denotes Current Differences Between Scripts*
1. Date Filtering for New Data
•	First Script (Historical Pull):
o	No date filtering is applied. The script pulls all historical data available from the API.
•	Second Script (Incremental Pull):
o	Step 1: Reads the existing df_activities_raw.csv file to determine the last recorded activity date (last_activity_date).
o	Step 2: Sets from as the timestamp of last_activity_date and to as the current date (Sys.Date()).
o	Step 3: Checks if last_activity_date is equal to the current date. If true, the script stops because there is no new data.
o	Step 4: Fetches only new activities and periods that occurred between from and to.
________________________________________
2. Fetching New Activities and Periods
•	First Script (Historical Pull):
o	Fetches all activities and periods without any date filtering.
•	Second Script (Incremental Pull):
o	Step 1: Uses ofCloudGetActivities with from and to parameters to fetch only new activities.
o	Step 2: Iterates through the new activity IDs to fetch corresponding periods using ofCloudGetPeriods.
o	Step 3: Stores the new activity and period IDs in new_activites and new_periods, respectively.
________________________________________
3. Fetching Aggregated Stats for New Data
•	First Script (Historical Pull):
o	Fetches aggregated stats for all activities and periods in the dataset.
•	Second Script (Incremental Pull):
o	Step 1: Fetches aggregated stats only for new activities (new_activites) and new periods (new_periods).
o	Step 2: Uses ofCloudGetStatistics with filters for activity_id and period_id to fetch stats for the new data.
________________________________________
4. Appending New Data to Existing Files
•	First Script (Historical Pull):
o	Overwrites existing CSV files with the newly fetched data.
•	Second Script (Incremental Pull):
o	Step 1: Defines a helper function append_to_csv to append new data to existing CSV files.
o	Step 2: Appends new aggregated stats for activities and periods to the existing df_aggregate_activity_stats_raw.csv and df_aggregate_period_stats_raw.csv files, respectively.
________________________________________
5. Handling Venue Data
•	First Script (Historical Pull):
o	Does not fetch or handle venue data.
•	Second Script (Incremental Pull):
o	Step 1: Fetches venue data using the fetch_data function.
o	Step 2: Saves the venue data to df_venues.csv.
________________________________________
6. Error Handling for Rate Limiting
•	First Script (Historical Pull):
o	Uses a basic retry mechanism with exponential backoff for rate limiting.
•	Second Script (Incremental Pull):
o	Step 1: Enhances the retry mechanism with more detailed logging and error handling.
o	Step 2: Tracks API request counts (api_request_count) to manage rate limiting more effectively.
________________________________________
7. Progress Messaging
•	First Script (Historical Pull):
o	Provides basic progress messages for each step.
•	Second Script (Incremental Pull):
o	Step 1: Adds more detailed progress messages, such as "Fetching Aggregated Stats Data From CatapultR" and "Period Stat Fetching Completed."
o	Step 2: Includes a check to stop the script if no new data is found (last_activity_date == today).
________________________________________
8. Output Files
•	First Script (Historical Pull):
o	Saves the following files:
	df_all_athletes_raw.csv
	df_activities_raw.csv
	df_periods_raw.csv
	df_aggregate_activity_stats_raw.csv
	df_aggregate_period_stats_raw.csv
•	Second Script (Incremental Pull):
o	Saves the following files:
	df_all_athletes_raw.csv
	df_activities_raw.csv
	df_periods_raw.csv
	df_aggregate_activity_stats_raw.csv (appended)
	df_aggregate_period_stats_raw.csv (appended)
	df_venues.csv (new file)
Summary of Key Differences
Feature	First Script (Historical Pull)	Second Script (Incremental Pull)
Date Filtering	None (pulls all historical data)	Filters data from last_activity_date to Sys.Date()
Fetching New Data	Fetches all activities and periods	Fetches only new activities and periods
Aggregated Stats	Fetches stats for all activities/periods	Fetches stats only for new activities/periods
Appending Data	Overwrites existing CSV files	Appends new data to existing CSV files
Venue Data	Not fetched	Fetches and saves venue data
Error Handling	Basic retry mechanism	Enhanced retry mechanism with logging
Progress Messaging	Basic progress messages	Detailed progress messages
Output Files	5 CSV files	6 CSV files (includes df_venues.csv)
Data Cleaning
Steps
1. Load Libraries
•	Load necessary libraries: tidyverse, hms, and openxlsx.
2. Define Output Directory
•	Set the input directory where raw CSV files are stored.
3. Define Helper Function
•	check_column_names(df):
o	Step 1: Replace invalid characters in column names with underscores.
o	Step 2: Remove duplicate column names from the dataframe.
4. Clean Athletes Data
•	Step 1: Load df_all_athletes_raw.csv.
•	Step 2: Perform data cleaning:
o	Add full_name by concatenating first_name and last_name.
o	Set status based on the jersey column.
o	Set gender to "Male".
o	Convert created_at, modified_at, and date_of_birth_date to Date format.
o	Calculate velocity_max_MPH and velocity_max_m_s from velocity_max.
•	Step 3: Check for duplicates in full_name and athlete_id and remove them.
•	Step 4: Validate column names using check_column_names.
5. Clean Activities Data
•	Step 1: Load df_activities_raw.csv.
•	Step 2: Perform data cleaning:
o	Classify sessions as "Remote", "Team", or "Small Group/RTP" based on athlete_count.
o	Convert start_time, end_time, modified_at, start_date, end_date, and activity_date to Date format.
o	Filter out activities with 0 athletes.
•	Step 3: Check for duplicates in activity_id and name and remove them.
•	Step 4: Filter activities with 0 periods and 0 athletes.
•	Step 5: Create a list of unique activities.
•	Step 6: Validate column names using check_column_names.
6. Clean Periods Data
•	Step 1: Load df_periods_raw.csv.
•	Step 2: Perform data cleaning:
o	Convert created_at, modified_at, start_date_time, end_date_time, start_date, end_date, start_time, and end_time to appropriate date/time formats.
o	Calculate period_duration in seconds.
•	Step 3: Check for duplicates in period_id and remove them.
•	Step 4: Create a table of unique periods with average duration and occurrence count.
•	Step 5: Validate column names using check_column_names.
7. Clean Aggregate Activities Stats
•	Step 1: Load df_aggregate_activity_stats_raw.csv.
•	Step 2: Perform data cleaning:
o	Convert start_time, end_time, field_time, total_duration, and date to appropriate formats.
o	Rename date to activity_date.
•	Step 3: Calculate thresholds for max_vel, max_effort_acceleration, and max_effort_deceleration based on athlete performance.
•	Step 4: Filter out invalid rows based on calculated thresholds.
•	Step 5: Validate column names using check_column_names.
8. Clean Aggregate Period Stats
•	Step 1: Load df_aggregate_period_stats_raw.csv.
•	Step 2: Perform data cleaning:
o	Convert start_time, end_time, field_time, total_duration, and date to appropriate formats.
o	Rename date to activity_date.
•	Step 3: Filter out periods with durations over 3 hours or less than 15 seconds.
•	Step 4: Calculate thresholds for max_vel, max_effort_acceleration, and max_effort_deceleration based on athlete performance.
•	Step 5: Filter out invalid rows based on calculated thresholds.
•	Step 6: Validate column names using check_column_names.
9. Build Catapult Calendar
•	Step 1: Create a calendar dataframe from df_activities.
•	Step 2: Add columns for year, quarter, semester, month, week, day_of_month, and day_of_week.
•	Step 3: Calculate in_season_week based on the start date of the season and Memorial Day.
•	Step 4: Classify sessions as "Pre-Season", "In-Season", or "Off-Season".
•	Step 5: Validate column names using check_column_names.
10. Build Relational Table
•	Step 1: Create a list of cleaned dataframes.
•	Step 2: Extract column names from each dataframe and count occurrences across dataframes.
•	Step 3: Create a relational table of column names and their occurrences.
•	Step 4: Validate column names using check_column_names.
11. Save Cleaned Data
•	Step 1: Save cleaned dataframes to CSV files in the output directory.
•	Step 2: Save duplicate logs to an Excel file.
•	Step 3: Print a summary message indicating the number of files successfully saved.
________________________________________
 
Dictionary of Important Components
Functions
•	check_column_names(df): Validates and cleans column names in a dataframe.
•	memorial_day(year): Calculates Memorial Day for a given year.
Thresholds
•	Athlete Data:
o	velocity_max_MPH: Converted from velocity_max using a factor of 2.236936.
o	velocity_max_m_s: Directly taken from velocity_max.
•	Activities Data:
o	session_class: Classified based on athlete_count:
	"Remote" if athlete_count <= (max_session_athletes * 0.25).
	"Team" if athlete_count >= (median_session_athletes * 0.5).
	"Small Group/RTP" otherwise.
•	Aggregate Activities Stats:
o	max_allowable_distance: avg_dist + (sd_dist * 3).
o	min_allowable_distance: avg_dist - (sd_dist * 3).
o	min_allowable_time: avg_time - (sd_time * 3).
o	athletes_top_allowable_speed: max_velocity_performed + (sd_velocity_performed * 1.25).
o	athletes_top_allowable_accel: max_accel_performed + (sd_accel_performed * 1.5).
o	athletes_top_allowable_decel: max_decel_performed - (sd_decel_performed * 1).
o	high_speed_efforts < 24 (~+2.5 SDs) per session,
o	sprint_efforts < 12 per session. 
•	Aggregate Period Stats:
o	max_period_dur: period_median_dur + (period_sd_dur * 3).
o	distance_per_min: Filtered to be greater than 7.5.
Key Dataframes
•	df_athletes: Cleaned athlete data.
•	df_activities: Cleaned activity data.
•	df_periods: Cleaned period data.
•	df_stats_activites: Cleaned aggregate activity stats.
•	df_stats_period: Cleaned aggregate period stats.
•	df_catapult_calendar: Calendar data with session types.
•	df_column_names: Relational table of column names across dataframes.
Data Aggregation and Calculations


Data Upload to BigQuery 
1.	Set Up Authentication
•	Uses the bigrquery library to authenticate with Google Cloud using a service account key stored in a JSON file (catapulttestproject-3cb794e3de65.json).
________________________________________
2. Load Cleaned Data from CSV Files
•	Reads cleaned data from CSV files stored in two directories:
o	input_dir: Contains cleaned data files (e.g., df_all_athletes_clean.csv, df_activities_clean.csv).
o	input_dir2: Contains additional processed data files (e.g., aggregate_activity_stats_against_averages.csv, game_pivoted_averages.csv).
•	Loads the following dataframes:
o	athletes, activities, periods, stats_activities, stats_period, catapult_calendar, stats_act_percentages, game_averages, practice_averages, team_totals_inseason, athlete_totals_inseason.
________________________________________
3. Validate Column Names
•	Defines a helper function check_column_names to:
o	Replace invalid characters in column names with underscores (_).
o	Remove duplicate column names.
•	Applies this function to all loaded dataframes to ensure column names are valid and unique.
________________________________________
4. Connect to Google BigQuery
•	Specifies the Google Cloud project ID (catapulttestproject) and dataset name (Cpult_test).
•	Creates a named list (upload_frame) containing all dataframes to be uploaded.
________________________________________
5. Upload Data to BigQuery
•	Iterates through each dataframe in upload_frame:
o	Constructs the fully qualified BigQuery table name (e.g., catapulttestproject.Cpult_test.athletes).
o	Uses bq_table_upload to upload the dataframe to the corresponding BigQuery table.
o	Sets the write_disposition to WRITE_TRUNCATE, which overwrites the existing table if it exists.
o	Prints a confirmation message after each upload.
________________________________________
 
Key Features of the Code
•	Authentication: Uses a service account key for secure access to Google Cloud.
•	Data Validation: Ensures column names are valid and unique before uploading.
•	Batch Upload: Uploads multiple dataframes to BigQuery in a loop.
•	Overwrite Mode: Replaces existing tables in BigQuery with the new data.
________________________________________
Output
•	The script uploads the following tables to Google BigQuery:
1.	athletes
2.	activities
3.	periods
4.	stats_activities
5.	stats_period
6.	catapult_calendar
7.	practice_averages
8.	game_averages
9.	stats_act_percentages
10.	athlete_totals_inseason
11.	team_totals_inseason
 
Installation Process – Changeable Factors
This documentation explains how to install and configure the R-based Catapult API call system. It assumes you are familiar with R but are setting up this multi-script API call system for the first time on a new computer.
1. Overview
The system consists of several R scripts that:
•	Fetch Data via API: Scripts like OPERATIONAL_Catapult__FirstRun_API_Scipt_V3.r and OPERATIONAL_Catapult__ReRUN_API_Scipt_V1.R call the Catapult API to download raw data (e.g., athletes, activities, and periods).
•	Aggregate and Clean Data: Scripts such as OPERATIONAL_Catapult_Aggregate_Data_Calculations.R, OPERATIONAL_Catapult_Cleaning_V2.R, and OPERATIONAL_Catapult_Cleaning_V3.R transform and clean the raw data.
•	Upload to BigQuery: The OPERATIONAL_Catapult_SQL_BIGQUERY_Upload.R script handles authentication and uploads cleaned data to Google BigQuery.
2. Prerequisites
1.	Create Catapult API authentication Token. 
a.	API module must be turned on.
b.	Create an API token.
c.	Save the API token.
d.	Input API token directly or update reference to API token.
i.	Token is needed in First Run and Rerun scripts.
2.	Set-up BigQuery for data connection and upload. 
3.	Determine needed parameters.
i.	Parameters slugs must be saved. This is the catapult specific metric name in their system for each metric that is utilized in Openfield.
b.	Create list of slugs in excel file named needed_slugs and period_slugs. [Save as CSV]
c.	Prerequisites:
i.	Total duration as total_duration
ii.	Total distance as total_distance
iii.	Top speed as max_vel
iv.	Max acceleration rate as max_effort_acceleration
v.	Max deceleration rate as max_effort_deceleration
vi.	Activity ID as activity_id
vii.	Activity Name as activity_name
viii.	Athlete Name as athlete_name
ix.	Athlete ID as athlete_id
x.	Date as date
xi.	Starting date of activity as start_date
xii.	Ending date of activity as end_date
xiii.	Day of the week as day_name
xiv.	Starting Time as start_time
xv.	Ending time as end_time
d.	Period slugs added prerequisites:
i.	Period ID as period_id
ii.	Period name as period_name
1.	*These must be added to identify the periods when aggregate periods stats script runs.*
4.	Set-Up Same Calander File. 
a.	Game Calander saves all dates of games from team schedule on the website. 
b.	This imports all games to classify practice versus game sessions. 
c.	Copy and paste all games from grid format schedule directly into csv “Mlax_Game_Cal” in folder Foundational.
3. Installation Steps
3.1. Set Up Your Working Directory
1.	Create a Project Folder:
Create a new directory (or RStudio project) on your computer to host the scripts and associated CSV/Excel files.
2.	Organize Folders:
o	Set up file structure the same as the following:
1.	Scripts/ – Place all the R scripts (e.g., the API call scripts, cleaning scripts, upload scripts) here.
2.	CSVs_Raw/ – For raw data outputs from the API.
3.	CSVs_Clean/ – For cleaned and processed data.
4.	CSV_OUTPUTS/ – For final outputs (e.g., data compared against averages).
5.	Foundational_CSVs/ – For any baseline CSV/Excel files required by the scripts.
6.	Credentials/ – For storing API tokens and BigQuery JSON credential file.
 
 
 
3.2. Install Required R Packages
Open R or RStudio and run the following commands to install all necessary libraries:
###START R CODE###
# Install packages from CRAN
install.packages(c("httr", "jsonlite", "tidyverse", "lubridate", "readxl", "openxlsx", "bigrquery", "future", "furrr", "hms", "progress"))

# Install catapultR if it is not on CRAN; for example, if it’s available on GitHub:
# install.packages("devtools")
# devtools::install_github("yourusername/catapultR")
###END R CODE###
3.3. Configure API and Credential Settings
1.	API Token:
In the scripts (for example, OPERATIONAL_Catapult__FirstRun_API_Scipt_V3.r and OPERATIONAL_Catapult__ReRUN_API_Scipt_V1.R), locate the section where the API token is defined. Replace the placeholder token with your valid token:
###START R CODE###
auth_token = "YOUR_REAL_API_TOKEN"
###END R CODE###
2.	BigQuery Credentials:
In the OPERATIONAL_Catapult_SQL_BIGQUERY_Upload.R script, set the environment variable for BigQuery:
1.	Adjust the file path to point to where you have stored your JSON credentials file.
###START R CODE###
Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = "C:/path/to/your/credentials/catapulttestproject-3cb794e3de65.json")
###END R CODE###
3.	File Paths:
Update any file paths in the scripts (for reading/writing CSVs or Excel files) to match your working directory structure. For example, modify:
1.	Change the first part – prior to “/CSV_Raw” to the path utilized for this project. 
###START R CODE###
input_dir <- "C:/Users/YourName/YourProject/CSVs_Raw"
###END R CODE###
3.4. Running the Scripts
It is recommended to run the scripts in sequence:
1.	Initial API Data Fetch:
Run OPERATIONAL_Catapult__FirstRun_API_Scipt_V3.r to fetch the initial set of data from the Catapult API.
Tip: Open the script in RStudio and source it (using Source button or source("path/to/OPERATIONAL_Catapult__FirstRun_API_Scipt_V3.r")).
2.	Data Re-Run (if needed):
Use OPERATIONAL_Catapult__ReRUN_API_Scipt_V1.R if you need to update or refresh the data.
3.	Data Aggregation & Cleaning:
Process the raw CSVs using the cleaning scripts (OPERATIONAL_Catapult_Cleaning_V2.R and OPERATIONAL_Catapult_Cleaning_V3.R) and run the aggregation calculations in OPERATIONAL_Catapult_Aggregate_Data_Calculations.R. These scripts will generate cleaned datasets and perform summary calculations.
4.	BigQuery Upload:
Finally, run OPERATIONAL_Catapult_SQL_BIGQUERY_Upload.R to upload the final cleaned datasets to Google BigQuery. Make sure your BigQuery project, dataset, and table names are correctly configured in the script.
3.5. Troubleshooting & Tips
•	Missing Packages: If any package fails to install, check your R version or install dependencies individually.
•	File Path Issues: Confirm that all file paths in the scripts are updated to your local directories.
•	API Errors:
o	Ensure that your API token is valid and not expired.
o	Look at the printed error messages in the console; many scripts have tryCatch error handlers that log messages.
•	BigQuery Upload:
o	Verify that your credentials JSON file is correctly pointed to and that you have permission to write to the specified BigQuery dataset.
•	Script Order: It is important to run the scripts in the order outlined above to ensure that raw data is fetched, cleaned, and then uploaded properly.
•	Update Slugs That Are In/Not In Use
3.5.1 Update Slugs That Are In/Not In Use
1. In the API Fetch Scripts
OPERATIONAL_Catapult__FirstRun_API_Scipt_V3.r
•	Loading Period Slugs (≈ Line 80–82):
The code loads a CSV of period slugs:
###START R CODE###
period_slugs <- read.csv("C:/.../Foundational_CSVs/slugs_periods.csv",
                         header = FALSE, stringsAsFactors = FALSE)
period_slugs <- as.character(period_slugs[[1]])
Note: This CSV must include at least period_id and period_name. If you modify any slug names here, update the CSV content or the extraction logic accordingly.
###END R CODE###
________________________________________
2. In the Data Cleaning Scripts
OPERATIONAL_Catapult_Cleaning_V2.R (and similarly in V3.R)
•	Activities Data Cleaning (≈ Line 40–50):
###START R CODE###
df_activities <- df_activities %>% 
  mutate(
    session_class = ifelse(athlete_count <= (max_session_athletes * 0.25), "Remote",
                           ifelse(athlete_count >= (median_session_athletes * 0.5), "Team", "Small Group/RTP")),
    start_time = as.Date.POSIXct(start_time),
    end_time = as.Date.POSIXct(end_time),
    modified_at = as.Date(modified_at, format = "%Y-%m-%d"),
    start_date = as.Date(start_date, format = "%Y-%m-%d"),
    end_date = as.Date(end_date, format = "%Y-%m-%d"),
    activity_date = as.Date(activity_date, format = "%Y-%m-%d")
  ) %>% 
  filter(athlete_count != 0)
###END R CODE###
Note:
o	session_class, athlete_count: These fields are not listed as prerequisites. If you change the slugs (for example, renaming athlete_count), update them here.
o	start_time, end_time, start_date, end_date, activity_date: These must remain as specified in your prerequisites.
________________________________________
3. In the Aggregation and Game Calendar Script
OPERATIONAL_Catapult_Aggregate_Data_Calculations.R
•	Calendar Joins (≈ Line 35–40):
###START R CODE###
games_only = game_cal %>% 
  select(game_type, game_date) %>% 
  mutate(activity_date = game_date) %>% 
  select(-game_date)

catapult_only = catapult_cal %>% 
  select(activity_date, session_type, in_season_week, quarter, week) %>% 
  mutate(session_class = session_type, activity_date = ymd(activity_date)) %>% 
  select(-session_type)

activity_type_cal = left_join(catapult_only, games_only, by = "activity_date")
###END R CODE###
•	Note: The join key activity_date here is derived from the game calendar and is not explicitly defined in your prerequisites (which list date, start_date, end_date). Update this if you decide to change how dates are handled.
•	Additional Metrics:
Later in the script, metrics such as sprint_distance___average, sprint_efforts, in_season_week, quarter, week, session_type, and participation_type are used. These are non-required slugs. Any changes to these will require corresponding updates in both the aggregation logic and subsequent analyses.

4.	Relational Table Visualization
First Run 
 
Cleaning
 
Data Aggregation and Collection 
 
SQL – BigQuery Upload
