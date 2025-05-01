library(tidyverse)
library(readxl)
library(lubridate)
library(readxl)
library(openxlsx)

##########################################
#Create game calendar
# Read the Excel file
game_cal <- read_excel("C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/Foundational_CSVs/Mlax_Game_Cal.xlsx")

# Data Cleaning and Transformation
game_cal <- game_cal %>% 
  mutate(
    game_date = gsub("\\s*\\(.*\\)", "", Date),  # Remove day of the week
    game_date = mdy(game_date),  # Convert to Date format
    game_type = case_when(
      str_detect(tolower(Tournament), "scrimmage") ~ "Scrimmage", 
      str_detect(tolower(Tournament), "tournament|championship") ~ "Post-season Game",
      TRUE ~ "Game"
    ),
    opponent_rank = str_extract(Opponent, "^#\\d+(?:/\\d+)?"),  # Extract rank (e.g., "#4", "#10/#12")
    Opponent = str_replace(Opponent, "^#\\d+(?:/\\d+)?\\s*", ""),  # Remove rank from Opponent
    Opponent = str_replace(Opponent, "\\s*\\(Scrimmage\\)", "")  # Remove "(Scrimmage)" tag
  ) %>% 
  separate(Result, into = c("result_w_l", "score"), sep = ", ", remove = FALSE, fill = "right") %>% 
  separate(Result, into = c("score_gtown", "score_opponent"), sep = "-", fill = "right") %>% 
  mutate(
    result_w_l = ifelse(result_w_l %in% c("W", "L"), result_w_l, NA), # Keep only W, L, or NA
    score_gtown = suppressWarnings(as.numeric(na_if(score_gtown, ""))),  # Convert to numeric safely
    score_opponent = suppressWarnings(as.numeric(na_if(score_opponent, "")))  # Convert to numeric safely
  ) %>% 
  select(-Date, -TV, -Radio, -Links) %>%  # Remove unnecessary columns
  filter(rowSums(is.na(.)) <= 7) %>%  # Drop rows with more than 5 NA values
  select(where(~ any(!is.na(.))))  # Drop columns where all values are NA

write.csv(game_cal,"C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSVs_Clean/df_game_calandar.csv")

##########################################
#Merge game calander- dates with catapult calander to create cateogry of practice,game,scrimmage

catapult_cal <- read.csv("C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSVs_Clean/df_catapult_calandar.csv")

games_only = game_cal %>% 
  select(game_type,game_date) %>% 
  mutate(
    activity_date = game_date
  ) %>% 
  select(-game_date)
catapult_only = catapult_cal %>% 
  select(activity_date,session_type,in_season_week,quarter,week) %>% 
  mutate(
    session_class = session_type,
    activity_date = ymd(activity_date)
  ) %>% 
  select(-session_type)

activity_type_cal = left_join(catapult_only,games_only,by = "activity_date")

activity_type_cal = activity_type_cal %>% 
  mutate(
    activity_date = format(activity_date, "%m/%d/%Y")
  ) %>% 
  mutate(
    activity_date = mdy(activity_date)
  )

rm(catapult_only,games_only,catapult_cal)

activities = read.csv("C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSVs_Clean/df_activities_clean.csv")

activities = activities %>% 
  mutate(
    participation_type = session_class
  ) %>% 
  select(-session_class) %>% 
  mutate(
    activity_date = ymd(activity_date)
  ) %>% 
  mutate(
    activity_date = format(activity_date, "%m/%d/%Y")
  ) %>% 
  mutate(
    activity_date = mdy(activity_date)
  )

activities = left_join(activities,activity_type_cal,by="activity_date")

activities <- activities %>%
  rename(session_type = game_type) %>%  # Rename column
  mutate(
    session_type = case_when(
      !is.na(session_type) ~ session_type,  # Keep existing values
      is.na(session_type) & participation_type != "remote" ~ "Practice",  # Assign "Practice" if NA and not remote
      TRUE ~ session_type  # Otherwise, keep it as is (NA)
    )
  )

###########################################
#Individual Yearly Practice Average

agg_activities = read.csv("C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSVs_Clean/df_aggregate_activity_stats_clean.csv")

agg_activities = agg_activities%>% 
  mutate(
    activity_date = ymd(activity_date)
  ) %>% 
  mutate(
    activity_date = format(activity_date, "%m/%d/%Y")
  ) %>% 
  mutate(
    activity_date = mdy(activity_date)
  )

merger_session_type_class = activities %>% 
  select(activity_date,participation_type,session_class,session_type,in_season_week,quarter,week) %>%
  distinct(activity_date, .keep_all = TRUE)  # Remove duplicates based on activity_date

agg_activities <- left_join(agg_activities,merger_session_type_class,by = "activity_date")

practice_athlete_week_avg <- agg_activities %>%  
  mutate(year = year(activity_date)) %>% 
  filter(!is.na(session_type) & session_type == "Practice") %>%  # Remove NA values before filtering
  group_by(athlete_id, year, in_season_week) %>% 
  summarise(
    total_distance = mean(total_distance, na.rm = TRUE),
    max_vel = mean(max_vel, na.rm = TRUE),
    max_effort_acceleration = mean(max_effort_acceleration, na.rm = TRUE),
    max_effort_deceleration = mean(max_effort_deceleration, na.rm = TRUE),
    total_acceleration_load = mean(total_acceleration_load, na.rm = TRUE),
    accel_load_density_index = mean(accel_load_density_index, na.rm = TRUE),
    acceleration_density = mean(acceleration_density, na.rm = TRUE),
    high_speed_distance = mean(high_speed_distance, na.rm = TRUE),
    high_speed_efforts = mean(high_speed_efforts, na.rm = TRUE),
    high_intensity_acceleration_efforts = mean(high_intensity_acceleration_efforts, na.rm = TRUE),
    high_intensity_deceleration_efforts = mean(high_intensity_deceleration_efforts, na.rm = TRUE),
    percentage_max_velocity = mean(percentage_max_velocity, na.rm = TRUE),
    sprint_distance___average = mean(sprint_distance___average, na.rm = TRUE),
    sprint_efforts = mean(sprint_efforts, na.rm = TRUE)
  ) %>% 
  ungroup()

# Define important columns
important_columns = c("total_distance","max_vel","max_effort_acceleration","max_effort_deceleration","total_acceleration_load",
                      "accel_load_density_index","acceleration_density","high_speed_distance","high_speed_efforts","high_intensity_acceleration_efforts","high_intensity_deceleration_efforts","percentage_max_velocity")

# Clean the data
practice_athlete_week_avg = practice_athlete_week_avg %>% 
  rowwise() %>% 
  filter(
    sum(is.na(c_across(all_of(important_columns)))) < (length(important_columns) / 2),
    sum(c_across(all_of(important_columns)) == 0) < (length(important_columns) / 4),
    !is.na(in_season_week)
    ) %>% 
  # Remove duplicate combinations of athlete_id, year, and in_season_week
  distinct(athlete_id, year, in_season_week, .keep_all = TRUE) %>% 
  rowwise() %>% 
  ungroup()

#Pivot to longer version
pivoted_practice_averages = practice_athlete_week_avg %>% 
  pivot_longer(
    cols = -c(athlete_id, year, in_season_week),  # Specify the non-metric columns to retain 
    names_to = "metric",  # Name of the new column for metric names
    values_to = "value"   # Name of the new column for metric values
  )

rm(activity_type_cal,game_cal,merger_session_type_class)
###########################################
#Individual Yearly Game Average

game_athlete_week_avg <- agg_activities %>%  
  mutate(year = year(activity_date)) %>% 
  filter(!is.na(session_type) & session_type == "Game") %>%  # Remove NA values before filtering
  group_by(athlete_id, year, in_season_week) %>% 
  summarise(
    total_distance = mean(total_distance, na.rm = TRUE),
    max_vel = mean(max_vel, na.rm = TRUE),
    max_effort_acceleration = mean(max_effort_acceleration, na.rm = TRUE),
    max_effort_deceleration = mean(max_effort_deceleration, na.rm = TRUE),
    total_acceleration_load = mean(total_acceleration_load, na.rm = TRUE),
    accel_load_density_index = mean(accel_load_density_index, na.rm = TRUE),
    acceleration_density = mean(acceleration_density, na.rm = TRUE),
    high_speed_distance = mean(high_speed_distance, na.rm = TRUE),
    high_speed_efforts = mean(high_speed_efforts, na.rm = TRUE),
    high_intensity_acceleration_efforts = mean(high_intensity_acceleration_efforts, na.rm = TRUE),
    high_intensity_deceleration_efforts = mean(high_intensity_deceleration_efforts, na.rm = TRUE),
    percentage_max_velocity = mean(percentage_max_velocity, na.rm = TRUE),
    sprint_distance___average = mean(sprint_distance___average, na.rm = TRUE),
    sprint_efforts = mean(sprint_efforts, na.rm = TRUE)
  ) %>% 
  ungroup()  

# Clean the data
game_athlete_week_avg = game_athlete_week_avg %>% 
  rowwise() %>% 
  filter(
    sum(is.na(c_across(all_of(important_columns)))) < (length(important_columns) / 2),
    sum(c_across(all_of(important_columns)) == 0) < (length(important_columns) / 4),
    !is.na(in_season_week)
  ) %>% 
  # Remove duplicate combinations of athlete_id, year, and in_season_week
  distinct(athlete_id, year, in_season_week, .keep_all = TRUE) %>% 
  rowwise() %>% 
  ungroup()

#Pivot to longer version
pivoted_game_averages = game_athlete_week_avg %>% 
  pivot_longer(
    cols = -c(athlete_id, year, in_season_week),  # Specify the non-metric columns to retain 
    names_to = "metric",  # Name of the new column for metric names
    values_to = "value"   # Name of the new column for metric values
  )


rm(important_columns)
###########################################
#Filter Agg To Main Metrics and Convert to Percentage of Game and Percentage of Practice Averages

#IF WE WANT ANY OTHER METRICS TO COMPARE TO THEY MUST HAVE A PRACTRICE - GAME AVERAGE - ADD TO THE ABOVE

#Filter, compare to game and practice average, clean, pivot longer
useable_agg_activites = agg_activities %>% 
  select(athlete_id,activity_id,activity_date,field_time,total_distance,max_vel,max_effort_acceleration,max_effort_deceleration,total_acceleration_load,accel_load_density_index,
          acceleration_density,high_speed_distance,high_speed_efforts,high_intensity_acceleration_efforts,high_intensity_deceleration_efforts,percentage_max_velocity,
          sprint_distance___average,sprint_efforts) %>% 
  mutate(year = year(activity_date)) %>% 
  pivot_longer(
    cols = -c(athlete_id,activity_id,activity_date,year),  # Specify the non-metric columns to retain 
    names_to = "metric",  # Name of the new column for metric names
    values_to = "value"   # Name of the new column for metric values
  )

#create mergeable average tables
mergeable_game_average = pivoted_game_averages %>% 
  group_by(year,athlete_id,metric) %>% 
  summarise(value = mean(value))

mergeable_practice_average = pivoted_practice_averages %>% 
  group_by(year,athlete_id,metric) %>% 
  summarise(value = mean(value))


# Step 2: Join game and practice averages
useable_agg_activities_with_avgs <- useable_agg_activites %>%
  left_join(
    mergeable_game_average %>% 
      rename(game_avg = value),  # Rename the value column to game_avg
    by = c("athlete_id", "year", "metric")
  ) %>%
  left_join(
    mergeable_practice_average %>% 
      rename(practice_avg = value),  # Rename the value column to practice_avg
    by = c("athlete_id", "year", "metric")
  )

# Step 3: Calculate percentages
useable_agg_activities_with_percentages <- useable_agg_activities_with_avgs %>%
  mutate(
    game_percentage = (value / game_avg) * 100,  # Calculate percentage of game average
    practice_percentage = (value / practice_avg) * 100  # Calculate percentage of practice average
  )

# Step 4: Clean and organize the final table
useable_agg_activites <- useable_agg_activities_with_percentages %>%
  select(
    athlete_id, activity_id, activity_date, year, metric, value, 
    game_percentage, practice_percentage
  )  %>% 
  filter(!is.na(value)) %>% 
  mutate(
    game_percentage = ifelse(value == 0, NA_real_, game_percentage),  # Set to NA if value is 0
    practice_percentage = ifelse(value == 0, NA_real_, practice_percentage)  # Set to NA if value is 0
  ) %>% 
  arrange(athlete_id, year, activity_date, metric)  # Sort the table for readability

rm(useable_agg_activities_with_percentages,useable_agg_activities_with_avgs,mergeable_practice_average,mergeable_game_average)
###########################################
#Week Athlete Total/Running Total

athlete_week_total <- agg_activities %>%  
  mutate(year = year(activity_date)) %>%  # Extract year from activity_date
  filter(!is.na(in_season_week)) %>%  # Remove rows where in_season_week is NA (if needed)
  group_by(athlete_id, year, in_season_week) %>%  # Group by athlete_id, year, and in_season_week
  summarise(
    max_vel = if (all(is.na(max_vel))) NA_real_ else max(max_vel, na.rm = TRUE),  # Handle NA values in max()
    percentage_max_velocity = if (all(is.na(percentage_max_velocity))) NA_real_ else max(percentage_max_velocity, na.rm = TRUE),
    max_effort_acceleration = if (all(is.na(max_effort_acceleration))) NA_real_ else max(max_effort_acceleration, na.rm = TRUE),
    max_effort_deceleration = if (all(is.na(max_effort_deceleration))) NA_real_ else max(max_effort_deceleration, na.rm = TRUE),
    
    total_distance = sum(total_distance, na.rm = TRUE),  # Sum with NA handling
    total_acceleration_load = sum(total_acceleration_load, na.rm = TRUE),
    high_speed_distance = sum(high_speed_distance, na.rm = TRUE),
    high_speed_efforts = sum(high_speed_efforts, na.rm = TRUE),
    high_intensity_acceleration_efforts = sum(high_intensity_acceleration_efforts, na.rm = TRUE),
    high_intensity_deceleration_efforts = sum(high_intensity_deceleration_efforts, na.rm = TRUE),
    sprint_distance___average = sum(sprint_distance___average, na.rm = TRUE),
    sprint_efforts = sum(sprint_efforts, na.rm = TRUE),
    .groups = "drop"  # Drop grouping after summarise
  )

off_season_athlete_week_total <- agg_activities %>%  
  mutate(
    year = year(activity_date),
    week = week(activity_date)
    ) %>%  # Extract year and week from activity_date
  filter(!is.na(week)) %>%  # filter to rows off-season weeks
  group_by(athlete_id, year, week) %>%  # Group by athlete_id, year, and week
  summarise(
    max_vel = if (all(is.na(max_vel))) NA_real_ else max(max_vel, na.rm = TRUE),  # Handle NA values in max()
    percentage_max_velocity = if (all(is.na(percentage_max_velocity))) NA_real_ else max(percentage_max_velocity, na.rm = TRUE),
    max_effort_acceleration = if (all(is.na(max_effort_acceleration))) NA_real_ else max(max_effort_acceleration, na.rm = TRUE),
    max_effort_deceleration = if (all(is.na(max_effort_deceleration))) NA_real_ else max(max_effort_deceleration, na.rm = TRUE),
    
    total_distance = sum(total_distance, na.rm = TRUE),  # Sum with NA handling
    total_acceleration_load = sum(total_acceleration_load, na.rm = TRUE),
    high_speed_distance = sum(high_speed_distance, na.rm = TRUE),
    high_speed_efforts = sum(high_speed_efforts, na.rm = TRUE),
    high_intensity_acceleration_efforts = sum(high_intensity_acceleration_efforts, na.rm = TRUE),
    high_intensity_deceleration_efforts = sum(high_intensity_deceleration_efforts, na.rm = TRUE),
    sprint_distance___average = sum(sprint_distance___average, na.rm = TRUE),
    sprint_efforts = sum(sprint_efforts, na.rm = TRUE),
    .groups = "drop"  # Drop grouping after summarise
  )

###########################################
#Week Team Total/Running Total

team_avg_week_total_inseason = athlete_week_total %>% 
  select(-athlete_id) %>% 
  group_by(in_season_week,year) %>% 
  mutate(
    unique_week = paste(in_season_week,"_",year)
  ) %>% 
  summarise(
    total_distance = mean(total_distance, na.rm = TRUE),
    max_vel = mean(max_vel, na.rm = TRUE),
    max_effort_acceleration = mean(max_effort_acceleration, na.rm = TRUE),
    max_effort_deceleration = mean(max_effort_deceleration, na.rm = TRUE),
    total_acceleration_load = mean(total_acceleration_load, na.rm = TRUE),
    high_speed_distance = mean(high_speed_distance, na.rm = TRUE),
    high_speed_efforts = mean(high_speed_efforts, na.rm = TRUE),
    high_intensity_acceleration_efforts = mean(high_intensity_acceleration_efforts, na.rm = TRUE),
    high_intensity_deceleration_efforts = mean(high_intensity_deceleration_efforts, na.rm = TRUE),
    percentage_max_velocity = mean(percentage_max_velocity, na.rm = TRUE),
    sprint_distance___average = mean(sprint_distance___average, na.rm = TRUE),
    sprint_efforts = mean(sprint_efforts, na.rm = TRUE)
  )
  
team_avg_week_total_offseason = off_season_athlete_week_total %>% 
  select(-athlete_id) %>%
  group_by(week,year) %>% 
  mutate(
    unique_week = paste(week,"_",year)
  ) %>% 
  summarise(
    total_distance = mean(total_distance, na.rm = TRUE),
    max_vel = mean(max_vel, na.rm = TRUE),
    max_effort_acceleration = mean(max_effort_acceleration, na.rm = TRUE),
    max_effort_deceleration = mean(max_effort_deceleration, na.rm = TRUE),
    total_acceleration_load = mean(total_acceleration_load, na.rm = TRUE),
    high_speed_distance = mean(high_speed_distance, na.rm = TRUE),
    high_speed_efforts = mean(high_speed_efforts, na.rm = TRUE),
    high_intensity_acceleration_efforts = mean(high_intensity_acceleration_efforts, na.rm = TRUE),
    high_intensity_deceleration_efforts = mean(high_intensity_deceleration_efforts, na.rm = TRUE),
    percentage_max_velocity = mean(percentage_max_velocity, na.rm = TRUE),
    sprint_distance___average = mean(sprint_distance___average, na.rm = TRUE),
    sprint_efforts = mean(sprint_efforts, na.rm = TRUE)
  )

##############################################################
# Save data to CSV
message("Saving data to CSV...")

# Function to append new data to an existing CSV file
append_to_csv <- function(new_data, filename) {
  file_path <- file.path(output_dir, filename)
  
  if (nrow(new_data) > 0) {
    # Check if the file exists
    if (file.exists(file_path)) {
      # Read existing data
      existing_data <- read_csv(file_path, show_col_types = FALSE)
      
      # Append new data to existing data
      combined_data <- bind_rows(existing_data, new_data)
    } else {
      combined_data <- new_data  # No existing file, use only new data
    }
    
    # Write the combined data back to CSV
    write_csv(combined_data, file_path)
    message(sprintf("Updated %s with new records.", filename))
  } else {
    message(sprintf("No new data available for %s.", filename))
  }
}

# Define output directory
output_dir <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSV_OUTPUTS"

# Track number of successful file writes
files_written <- 0

# Track number of successful file writes
excels_written <- 0
##############################################################

# Save data to CSV files
if (nrow(game_athlete_week_avg) > 0) {
  append_to_csv(game_athlete_week_avg, "game_averages.csv")
  files_written <- files_written + 1
} else {
  print("No Game Averages Data available to write.")
}

if (nrow(practice_athlete_week_avg) > 0) {
  append_to_csv(practice_athlete_week_avg, "practice_averages.csv")
  files_written <- files_written + 1
} else {
  print("No Practice Averages Data available to write.")
}

if (nrow(pivoted_game_averages) > 0) {
  append_to_csv(pivoted_game_averages, "game_pivoted_averages.csv")
  files_written <- files_written + 1
} else {
  print("No Pivoted - Game Averages Data available to write.")
}

if (nrow(pivoted_practice_averages) > 0) {
  append_to_csv(pivoted_practice_averages, "practice_pivoted_averages.csv")
  files_written <- files_written + 1
} else {
  print("No Pivoted - Practice Averages Data available to write.")
}

if (nrow(useable_agg_activites) > 0) {
  append_to_csv(useable_agg_activites, "aggregate_activity_stats_against_averages.csv")
  files_written <- files_written + 1
} else {
  print("No Pivoted Aggregate Data available to write.")
}

if (nrow(athlete_week_total) > 0) {
  append_to_csv(athlete_week_total, "athlete_inseason_totals.csv")
  files_written <- files_written + 1
} else {
  print("No Athlete InSeason Week Total available to write.")
}

if (nrow(off_season_athlete_week_total) > 0) {
  append_to_csv(off_season_athlete_week_total, "athlete_offseason_totals.csv")
  files_written <- files_written + 1
} else {
  print("No Athlete OffSeason Week Total Data available to write.")
}

if (nrow(team_avg_week_total_inseason) > 0) {
  append_to_csv(team_avg_week_total_inseason, "team_inseason_totals.csv")
  files_written <- files_written + 1
} else {
  print("No Team InSeason Week Total Data available to write.")
}

if (nrow(team_avg_week_total_offseason) > 0) {
  append_to_csv(team_avg_week_total_offseason, "team_offseason_totals.csv")
  files_written <- files_written + 1
} else {
  print("No Team OffSeason Week Total Data available to write.")
}
################################################# create one master excel file
input_dir = "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSVs_Clean"

roster = read_excel("C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSV_OUTPUTS/Roster.xlsx")
activites = read.csv(file.path(input_dir,"df_activities_clean.csv"))
periods = read.csv(file.path(input_dir,"df_all_periods_clean.csv"))
game_cal = read.csv(file.path(input_dir,"df_game_calandar.csv"))

temp_inseason = athlete_week_total %>% 
  rename(week = in_season_week) %>% 
  mutate(seasonality = "In-Season")

temp_offseason = off_season_athlete_week_total %>% 
  mutate(seasonality = "Off-Season")

temp_combined = bind_rows(temp_inseason,temp_offseason)
rm(temp_offseason,temp_inseason)

#create one master excel file
# Create a list of data frames
tables <- list(
  roster = roster,
  activites = activites,
  periods = periods,
  games = game_cal,
  game_averages = game_athlete_week_avg,
  practice_averages = practice_athlete_week_avg,
  agg_stats = useable_agg_activites,
  week_totals = temp_combined,
  offseason_totals = off_season_athlete_week_total
)

# Define the output file path
output_file <- file.path(output_dir, "Master_Outputs_Workbook.xlsx")

# Write all tables to the Excel file
write.xlsx(tables, file = output_file)
##############################################################
# Print summary message
if (files_written > 0) {
  print(paste("Data successfully fetched and saved to", files_written, "out of 9 CSV file(s)."))
} else {
  print("No data was available to save.")
}