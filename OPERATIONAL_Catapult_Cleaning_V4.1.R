library(tidyverse)
library(hms)
library(openxlsx)
###############################################################################
#                  TEMP_Catapult_Cleaning_V4.1.R - REVISED                    #
###############################################################################
# This script reads raw Catapult data from CSV files, performs cleaning and 
# reformatting, and exports the cleaned data. 
# Revisions applied per user request:
#  1) File paths & thresholds at top of the script for clarity.
#  2) Each major operation wrapped in well-structured tryCatch with a global 
#     anomaly/error log. A summary of errors is printed at the end.
#  3) Thresholds consolidated near the top for easy access.
#  4) Reformat all date columns to MDY (mm/dd/yyyy) in final output.
#  5) Additional short comments explaining filtering logic.
#  6) Logging row counts for merges.
#  7) Single Excel log for duplicates + final summary.

###############################################################################
#                            SECTION 1: CONFIG                                #
###############################################################################

config <- list(
  input_dir  = "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSVs_Raw",
  output_dir = "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSVs_Clean",
  tag_db_path = "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/Foundational_CSVs/tags_data.csv"
)

# These numeric thresholds are used in outlier filters and data validations.
THRESHOLDS <- list(
  # Activity-level
  min_field_time_activity       = 5,     # min minutes on field for valid data
  max_field_time_activity       = 220,   # max minutes on field for valid data
  min_distance_per_min_activity = 5,     # min yards/min for “active” session
  min_session_distance          = 250,   # min yards for a valid activity
  min_session_duration          = 15,    # min minutes for a valid activity
  
  # Period-level
  max_total_duration_period     = 180,   # max total minutes for a period
  min_distance_per_min_period   = 7.5,   # min yards/min for a valid period
  min_period_distance           = 50,    # min yards for a valid period
  min_period_duration           = 1      # min minutes for a valid period
)

###############################################################################
#                   SECTION 2: LIBRARIES & HELPER FUNCTIONS                   #
###############################################################################
library(tidyverse)
library(hms)
library(openxlsx)
library(lubridate)

# Global anomalies list
anomalies_log <- list()

log_anomaly <- function(step_name, message_text) {
  anomalies_log[[length(anomalies_log) + 1]] <<- data.frame(
    step = step_name,
    message = message_text,
    stringsAsFactors = FALSE
  )
}

# Wrapper that logs any errors, returns NULL on error
safe_run <- function(expr, step_name) {
  tryCatch(
    expr,
    error = function(e) {
      log_anomaly(step_name, e$message)
      NULL
    }
  )
}

# Clean up and unify column names
check_column_names <- function(df) {
  invalid_cols <- names(df)[grepl("[^a-zA-Z0-9_]", names(df))]
  if (length(invalid_cols) > 0) {
    print("Invalid column names detected, renaming...")
    colnames(df) <- gsub("[^a-zA-Z0-9_]", "_", names(df))
  }
  
  col_names <- colnames(df)
  duplicated_names <- duplicated(col_names)
  if (any(duplicated_names)) {
    print("Duplicate column names detected, removing duplicates...")
    df <- df[, !duplicated(col_names)]
  }
  df
}

# Convert any Date or POSIXct columns to "mm/dd/yyyy" character format before saving
convert_dates_to_mdy <- function(df) {
  for (colname in names(df)) {
    # Check if column is Date or POSIXt
    if (inherits(df[[colname]], "Date") || inherits(df[[colname]], "POSIXt")) {
      df[[colname]] <- format(df[[colname]], "%m/%d/%Y")
    }
  }
  df
}

###############################################################################
#                 SECTION 3: LOADING & CLEANING ATHLETES DATA                 #
###############################################################################
df_athletes <- safe_run({
  read.csv(file.path(config$input_dir, "df_all_athletes_raw.csv"))
}, "Load Athletes CSV")

athlete_cleaning <- FALSE

df_athletes <- safe_run({
  df_athletes <- df_athletes %>%
    mutate(
      status = ifelse(suppressWarnings(!is.na(as.numeric(jersey))), "Active", "Archived"),
      gender = "Male",
      full_name = paste(first_name, " ", last_name),
      created_at = as.Date(created_at, format = "%Y-%m-%d"),
      modified_at = as.Date(modified_at, format = "%Y-%m-%d"),
      velocity_max_MPH = velocity_max * 2.236936,
      velocity_max_m_s = velocity_max,
      date_of_birth_date = as.Date(date_of_birth_date)
    )
  
  # Duplicates checks
  athlete_duplicates_fullname <<- df_athletes[
    duplicated(df_athletes$full_name) | duplicated(df_athletes$full_name, fromLast = TRUE), ]
  if (nrow(athlete_duplicates_fullname) > 0) {
    message("Duplicates found in full_name, removing duplicates...")
    df_athletes <- df_athletes[!duplicated(df_athletes$full_name), ]
  }
  
  athlete_duplicates_id <<- df_athletes[
    duplicated(df_athletes$athlete_id) | duplicated(df_athletes$athlete_id, fromLast = TRUE), ]
  if (nrow(athlete_duplicates_id) > 0) {
    message("Duplicates found in athlete_id, removing duplicates...")
    df_athletes <- df_athletes[!duplicated(df_athletes$athlete_id), ]
  }
  
  df_athletes <- check_column_names(df_athletes)
  athlete_cleaning <<- TRUE
  df_athletes
}, "Clean Athletes Data")

###############################################################################
#                 SECTION 4: LOADING & CLEANING ACTIVITIES                    #
###############################################################################
df_activities <- safe_run({
  read.csv(file.path(config$input_dir, "df_activities_raw.csv"))
}, "Load Activities CSV")

activities_cleaning <- FALSE

df_activities <- safe_run({
  median_session_athletes <- median(df_activities$athlete_count)
  sd_session_athletes     <- sd(df_activities$athlete_count)
  max_session_athletes    <- median_session_athletes + sd_session_athletes
  
  df_activities <- df_activities %>%
    mutate(
      session_class = ifelse(
        athlete_count <= (max_session_athletes * 0.25), "Remote",
        ifelse(athlete_count >= (median_session_athletes * 0.5), "Team", "Small Group/RTP")
      ),
      start_time  = as.Date.POSIXct(start_time),
      end_time    = as.Date.POSIXct(end_time),
      modified_at = as.Date(modified_at, format = "%Y-%m-%d"),
      start_date  = as.Date(start_date, format = "%Y-%m-%d"),
      end_date    = as.Date(end_date,   format = "%Y-%m-%d"),
      activity_date = as.Date(activity_date, format = "%Y-%m-%d")
    ) %>%
    filter(athlete_count != 0) %>% 
    # Remove "GPS" from tags
    mutate(
      tags = str_split(tags, ",\\s*"),
      tags = map(tags, ~ .x[.x != "GPS"]),
      tags = map_chr(tags, ~ paste(.x, collapse = ", "))
    ) %>%
    filter(athlete_count > 0 & period_count > 0)
  
  # Duplicates checks
  activity_duplicates_active_id <<- df_activities[
    duplicated(df_activities$activity_id) | duplicated(df_activities$activity_id, fromLast = TRUE), ]
  if (nrow(activity_duplicates_active_id) > 0) {
    message("Duplicates found in activity_id, removing duplicates...")
    df_activities <- df_activities[!duplicated(df_activities$activity_id), ]
  }
  
  activity_duplicates_activity_name <<- df_activities[
    duplicated(df_activities$name) | duplicated(df_activities$name, fromLast = TRUE), ]
  if (nrow(activity_duplicates_activity_name) > 0) {
    message("Duplicates found in activity name. Duplicate names displayed below:")
    print(activity_duplicates_activity_name)
  }
  
  df_unique_activities <<- df_activities %>%
    distinct(activity_id, .keep_all = TRUE) %>%
    select(activity_id, activity_date)
  
  df_activities <- check_column_names(df_activities)
  activities_cleaning <<- TRUE
  df_activities
}, "Clean Activities Data")

###############################################################################
#                   SECTION 5: LOADING & CLEANING PERIODS                     #
###############################################################################
df_periods <- safe_run({
  read.csv(file.path(config$input_dir, "df_periods_raw.csv"))
}, "Load Periods CSV")

period_cleaning <- FALSE

df_periods <- safe_run({
  df_periods <- df_periods %>%
    mutate(
      created_at = as.Date(created_at, format = "%Y-%m-%d"),
      modified_at = as.Date(modified_at, format = "%Y-%m-%d"),
      start_date_time = as.POSIXct(start_date_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      end_date_time   = as.POSIXct(end_date_time,   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      start_date = as.Date(start_date, format = "%Y-%m-%d"),
      start_time = as_hms(start_time),
      end_date   = as.Date(end_date,   format = "%Y-%m-%d"),
      end_time   = as_hms(end_time),
      period_duration = difftime(end_time, start_time, units = "secs"),
      activity_date = start_date
    )
  
  # Load the tags database
  tag_db_local <- read_csv(config$tag_db_path) %>%
    mutate(
      tag_id = id,
      tag_name = name,
      tag_id_clean = tolower(trimws(id))
    ) %>%
    select(tag_id, tag_name, tag_id_clean)
  
  # Clean tags (removing "GPS", that specific ID, etc.)
  df_periods <- df_periods %>%
    mutate(
      tags_vector = str_split(tags, ",\\s*"),
      tags_vector = map(tags_vector, ~ tolower(trimws(.x))),
      tags_vector = map(tags_vector, ~ .x[.x != "gps"]),
      tags_vector = map(tags_vector, ~ .x[.x != "0e81fa0a-e08e-4975-a6c6-f1240aa6337c"]),
      valid_ids   = map(tags_vector, ~ .x[.x %in% tag_db_local$tag_id_clean]),
      valid_names = map(valid_ids, ~ map_chr(.x, function(x) {
        tag_db_local$tag_name[which(tag_db_local$tag_id_clean == x)][1]
      })),
      tag_id  = map_chr(valid_ids, ~ paste(.x, collapse = ", ")),
      tag_name= map_chr(valid_names, ~ paste(.x, collapse = ", "))
    ) %>%
    mutate(tags = tag_name) %>%
    select(-tags_vector, -valid_ids, -valid_names, -tag_name)
  
  # Check duplicates
  periods_duplicates_period_id <<- df_periods[
    duplicated(df_periods$period_id) | duplicated(df_periods$period_id, fromLast = TRUE), ]
  if (nrow(periods_duplicates_period_id) > 0) {
    message("Duplicates found in period_id, removing duplicates...")
    df_periods <- df_periods[!duplicated(df_periods$period_id), ]
  }
  
  # Create unique periods table
  df_unique_periods <<- df_periods %>%
    distinct(name) %>%
    rename(period_name = name)
  
  df_temp_periods <- df_periods
  df_period_stats <- df_temp_periods %>%
    group_by(name) %>%
    summarise(
      avg_duration = mean(period_duration, na.rm = TRUE),
      count_occurrences = n()
    ) %>%
    rename(period_name = name)
  
  df_unique_periods <<- df_unique_periods %>%
    left_join(df_period_stats, by = "period_name")
  
  df_periods <- check_column_names(df_periods)
  period_cleaning <<- TRUE
  df_periods
}, "Clean Periods Data")

###############################################################################
#                          SECTION 6: TAG MAPPINGS                             #
###############################################################################
# Wrap tag usage analysis in safe_run
tag_db <- safe_run({
  # Re-read local tag db for usage counts
  tdb <- read_csv(config$tag_db_path) %>%
    mutate(tag_id = tolower(trimws(id)),
           tag_name = tolower(trimws(name))) %>%
    select(tag_id, tag_name)
  tdb
}, "Load Tag DB")

safe_run({
  # Count usage in periods
  tag_usage_ids <- df_periods %>%
    separate_rows(tag_id, sep = ",\\s*") %>%
    mutate(tag_id = trimws(tag_id)) %>%
    group_by(tag_id) %>%
    summarise(count_id = n(), .groups = "drop")
  
  tag_usage_names <- df_periods %>%
    mutate(tag_name = tags) %>%
    separate_rows(tag_name, sep = ",\\s*") %>%
    mutate(tag_name = trimws(tag_name)) %>%
    group_by(tag_name) %>%
    summarise(count_name = n(), .groups = "drop")
  
  # Merge counts
  tag_db <<- tag_db %>%
    left_join(tag_usage_ids, by = "tag_id") %>%
    mutate(count_id = if_else(is.na(count_id), 0L, count_id)) %>%
    left_join(tag_usage_names, by = c("tag_name")) %>%
    mutate(
      count_name = if_else(is.na(count_name), 0L, count_name),
      count      = pmax(count_id, count_name)
    ) %>%
    select(tag_id, tag_name, count)
  
  # Activity tags pivot
  tags_activities <<- df_activities %>%
    select(activity_id, tags) %>%
    separate_rows(tags, sep = ",") %>%
    mutate(tags = trimws(tags)) %>%
    distinct(activity_id, tags) %>%
    filter(tags != "")
  
  # Period tags pivot
  tags_periods <<- df_periods %>%
    select(period_id, tags) %>%
    separate_rows(tags, sep = ",") %>%
    mutate(tags = trimws(tags)) %>%
    distinct(period_id, tags) %>%
    filter(tags != "")
  
}, "Compute Tag Usage")

###############################################################################
#              SECTION 7: CLEANING AGGREGATE ACTIVITY STATS                   #
###############################################################################
df_stats_activities <- safe_run({
  read.csv(file.path(config$input_dir, "df_aggregate_activity_stats_raw.csv"))
}, "Load df_aggregate_activity_stats_raw")

agg_activity_cleaning_part1 <- FALSE
agg_activity_cleaning_part2 <- FALSE

df_stats_activities <- safe_run({
  df_temp <- df_stats_activities %>%
    mutate(
      start_time     = as_hms(start_time),
      end_time       = as_hms(end_time),
      field_time     = field_time / 60,
      total_duration = total_duration / 60,
      date           = mdy(date),
      mas_duration   = mas_duration_.session_average. / 60
    ) %>%
    rename(activity_date = date)
  
  agg_activity_cleaning_part1 <<- TRUE
  df_temp
}, "Clean Activity Stats (Part 1)")

safe_run({
  # Summaries for speed thresholds
  # Collect max velocities, etc.
  athlete_hi_e <- df_stats_activities %>%
    group_by(athlete_id) %>%
    summarise(
      max_velocity_performed = max(max_vel, na.rm = TRUE),
      max_accel_performed    = max(max_effort_acceleration, na.rm = TRUE),
      max_decel_performed    = min(max_effort_deceleration, na.rm = TRUE),
      avg_velocity_performed = mean(max_vel, na.rm = TRUE),
      avg_accel_performed    = mean(max_effort_acceleration, na.rm = TRUE),
      avg_decel_performed    = mean(max_effort_deceleration, na.rm = TRUE),
      sd_velocity_performed  = sd(max_vel, na.rm = TRUE),
      sd_accel_performed     = sd(max_effort_acceleration, na.rm = TRUE),
      sd_decel_performed     = sd(max_effort_deceleration, na.rm = TRUE)
    ) %>%
    mutate(
      athletes_top_allowable_speed = max_velocity_performed + (sd_velocity_performed * 1.25),
      athletes_top_allowable_accel = max_accel_performed + (sd_accel_performed * 1.5),
      athletes_top_allowable_decel = max_decel_performed - (sd_decel_performed * 1)
    )
  
  # Merge with athlete table to keep only valid athlete IDs 
  athletes_top_speeds <<- df_athletes %>%
    select(athlete_id, full_name) %>%
    left_join(athlete_hi_e, by = "athlete_id") %>%
    drop_na()
  
}, "Compute Athlete Speed Summaries")

temp_athletes_top <- safe_run({
  # Speed threshold table for merges
  athletes_top_speeds %>%
    select(athlete_id, 
           athletes_top_allowable_speed, 
           athletes_top_allowable_accel, 
           athletes_top_allowable_decel)
}, "Extract Speed Thresholds Table")

distance_avg <- safe_run({
  df_stats_activities %>%
    group_by(activity_id) %>%
    summarise(
      avg_dist = mean(total_distance, na.rm = TRUE),
      sd_dist  = sd(total_distance,   na.rm = TRUE),
      avg_time = mean(total_duration, na.rm = TRUE),
      sd_time  = sd(total_duration,   na.rm = TRUE)
    ) %>%
    mutate(
      max_allowable_distance = avg_dist + (sd_dist * 3),
      min_allowable_distance = avg_dist - (sd_dist * 3),
      min_allowable_time     = avg_time - (sd_time * 3)
    ) %>%
    drop_na()
}, "Compute Distance/Duration Averages for Activities")

df_stats_activities <- safe_run({
  df_temp <- df_stats_activities %>%
    left_join(temp_athletes_top, by = "athlete_id") %>%
    left_join(distance_avg,       by = "activity_id") %>%
    filter(
      field_time < THRESHOLDS$max_field_time_activity &
        field_time > THRESHOLDS$min_field_time_activity
    ) %>%
    filter(!(max_vel < 2 | max_vel > 30)) %>%
    filter(!(total_distance > max_allowable_distance | 
               total_distance < min_allowable_distance)) %>%
    filter(!(total_duration < min_allowable_time)) %>%
    mutate(
      max_vel                = if_else(max_vel >= athletes_top_allowable_speed, NA_real_, max_vel),
      max_effort_acceleration= if_else(max_effort_acceleration >= athletes_top_allowable_accel, NA_real_, max_effort_acceleration),
      max_effort_deceleration= if_else(max_effort_deceleration <= athletes_top_allowable_decel, NA_real_, max_effort_deceleration)
    ) %>%
    mutate(distance_per_min = total_distance / total_duration) %>%
    filter(distance_per_min > THRESHOLDS$min_distance_per_min_activity) %>%
    select(-distance_per_min, 
           -athletes_top_allowable_speed, 
           -athletes_top_allowable_accel, 
           -athletes_top_allowable_decel,
           -avg_dist, -sd_dist, -avg_time, -sd_time, 
           -max_allowable_distance, -min_allowable_distance, -min_allowable_time,
           -practice_duration_â.._session_true, -practice_periods, -percentage_max_heart_rate)
  
  agg_activity_cleaning_part2 <<- TRUE
  df_temp
}, "Clean Activity Stats (Part 2)")

safe_run({
  before_merge <- nrow(df_activities)
  temp_activity_stats <- df_stats_activities %>%
    group_by(activity_id) %>%
    summarise(
      session_avg_distance = mean(total_distance, na.rm = TRUE),
      session_avg_duration = mean(total_duration, na.rm = TRUE)
    )
  
  df_activities <<- df_activities %>%
    left_join(temp_activity_stats, by = "activity_id")
  after_merge <- nrow(df_activities)
  message(sprintf("Activities merge row count: %d -> %d", before_merge, after_merge))
}, "Merge Stats into Activities")

safe_run({
  df_activities <<- df_activities %>%
    filter(!is.na(session_avg_distance)) %>%
    filter(!is.na(session_avg_duration)) %>%
    filter(session_avg_distance > THRESHOLDS$min_session_distance) %>%
    filter(session_avg_duration > THRESHOLDS$min_session_duration) %>%
    select(-session_avg_distance, -session_avg_duration)
}, "Filter Invalid Activities with Min Distance & Duration")


###############################################################################
#                SECTION 8: CLEANING AGGREGATE PERIOD STATS                   #
###############################################################################
df_stats_period <- safe_run({
  read.csv(file.path(config$input_dir, "df_aggregate_period_stats_raw.csv"))
}, "Load df_aggregate_period_stats_raw")

agg_period_cleaning_part1 <- FALSE
agg_period_cleaning_part2 <- FALSE

df_stats_period <- safe_run({
  df_temp <- df_stats_period %>%
    mutate(
      start_time     = as_hms(start_time),
      end_time       = as_hms(end_time),
      field_time     = field_time / 60,
      total_duration = total_duration / 60,
      date           = mdy(date),
      mas_duration   = mas_duration_.session_average. / 60
    ) %>%
    rename(activity_date = date) %>%
    mutate(across(where(is.list), ~ map_chr(.x, ~ paste(unlist(.), collapse = ", ")))) %>%
    filter(total_duration < THRESHOLDS$max_total_duration_period)
  
  agg_period_cleaning_part1 <<- TRUE
  df_temp
}, "Clean Period Stats (Part 1)")

safe_run({
  # Remove outliers based on period duration stats
  temp_period_time <- df_stats_period %>%
    group_by(period_id) %>%
    summarise(
      period_median_dur = median(total_duration, na.rm = TRUE),
      period_sd_dur     = sd(total_duration,     na.rm = TRUE)
    ) %>%
    mutate(max_period_dur = period_median_dur + (period_sd_dur * 3))
  
  df_stats_period <<- df_stats_period %>%
    left_join(temp_period_time, by = "period_id") %>%
    filter(total_duration < max_period_dur | total_duration > 0.25) %>%
    mutate(distance_per_min = total_distance / total_duration) %>%
    filter(distance_per_min > THRESHOLDS$min_distance_per_min_period) %>%
    select(-distance_per_min, -max_period_dur)
}, "Filter Period Stats Part 2 - Duration & Distances")

distance_period_avg <- safe_run({
  df_stats_period %>%
    group_by(period_id) %>%
    summarise(
      avg_dist = mean(total_distance, na.rm = TRUE),
      sd_dist  = sd(total_distance,   na.rm = TRUE),
      avg_time = mean(total_duration, na.rm = TRUE),
      sd_time  = sd(total_duration,   na.rm = TRUE)
    ) %>%
    mutate(
      max_allowable_distance = avg_dist + (sd_dist * 3),
      min_allowable_distance = avg_dist - (sd_dist * 3),
      min_allowable_time     = avg_time - (sd_time * 3)
    ) %>%
    drop_na()
}, "Compute Distance Averages for Periods")

df_stats_period <- safe_run({
  df_temp <- df_stats_period %>%
    left_join(temp_athletes_top,    by = "athlete_id") %>%
    left_join(distance_period_avg,  by = "period_id") %>%
    filter(!(max_vel < 2 | max_vel > 30)) %>%
    filter(total_distance <= max_allowable_distance & total_distance >= min_allowable_distance) %>%
    filter(total_duration >= min_allowable_time) %>%
    mutate(
      max_vel                = if_else(max_vel >= athletes_top_allowable_speed, NA_real_, max_vel),
      max_effort_acceleration= if_else(max_effort_acceleration >= athletes_top_allowable_accel, NA_real_, max_effort_acceleration),
      max_effort_deceleration= if_else(max_effort_deceleration <= athletes_top_allowable_decel, NA_real_, max_effort_deceleration)
    ) %>%
    select(-athletes_top_allowable_speed, -athletes_top_allowable_accel, -athletes_top_allowable_decel,
           -avg_dist, -sd_dist, -avg_time, -sd_time, 
           -max_allowable_distance, -min_allowable_distance, -min_allowable_time,
           -practice_duration_â.._session_true, -practice_periods, -percentage_max_heart_rate)
  
  agg_period_cleaning_part2 <<- TRUE
  df_temp
}, "Clean Period Stats (Part 3)")

safe_run({
  before_merge <- nrow(df_periods)
  temp_period_stats <- df_stats_period %>%
    group_by(period_id) %>%
    summarise(
      session_avg_distance = mean(total_distance, na.rm = TRUE),
      session_avg_duration = mean(total_duration, na.rm = TRUE)
    )
  
  df_periods <<- df_periods %>%
    left_join(temp_period_stats, by = "period_id")
  after_merge <- nrow(df_periods)
  message(sprintf("Periods merge row count: %d -> %d", before_merge, after_merge))
}, "Merge Stats into Periods")

safe_run({
  df_periods <<- df_periods %>%
    filter(!is.na(session_avg_distance)) %>%
    filter(!is.na(session_avg_duration)) %>%
    filter(session_avg_distance > THRESHOLDS$min_period_distance) %>%
    filter(session_avg_duration > THRESHOLDS$min_period_duration) %>%
    select(-session_avg_distance, -session_avg_duration)
}, "Filter Invalid Periods with Min Distance & Duration")


###############################################################################
#                   SECTION 9: BUILD CATAPULT CALENDAR                        #
###############################################################################
df_catapult_calendar <- safe_run({
  df_temp <- df_activities %>%
    select(activity_date) %>%
    mutate(
      activity_date = as.Date(activity_date),
      year         = year(activity_date),
      quarter      = quarter(activity_date),
      semester     = semester(activity_date, with_year=FALSE),
      month        = month(activity_date),
      week         = week(activity_date),
      day_of_month = day(activity_date),
      day_of_week  = wday(activity_date)
    )
  
  memorial_day <- function(y) {
    last_monday <- as.Date(paste0(y, "-05-31"))
    last_monday - (wday(last_monday) %% 7)
  }
  
  start_date_2025 <- mdy("02/01/2025") - days(6)
  start_date_2024 <- mdy("02/03/2024") - days(6)
  start_date_2023 <- mdy("02/11/2023") - days(6)
  start_date_2022 <- mdy("02/13/2022") - days(6)
  
  df_temp <- df_temp %>%
    mutate(
      start_date = case_when(
        year == 2025 ~ start_date_2025,
        year == 2024 ~ start_date_2024,
        year == 2023 ~ start_date_2023,
        year == 2022 ~ start_date_2022,
        TRUE         ~ NA_Date_
      ),
      end_date = memorial_day(year),
      jan_1    = as.Date(paste0(year, "-01-01")),
      in_season_week = case_when(
        # Exclude fall (Sep-Dec)
        month %in% c(9, 10, 11, 12) ~ NA_integer_,
        activity_date >= jan_1 & activity_date < start_date ~
          as.integer(difftime(activity_date, start_date, units="weeks")) - 1,
        activity_date >= start_date & activity_date <= end_date ~
          as.integer(difftime(activity_date, start_date, units="weeks")) + 1,
        TRUE ~ NA_integer_
      )
    ) %>%
    mutate(
      session_type = case_when(
        in_season_week < 0 ~ "Pre-Season",
        in_season_week > 0 ~ "In-Season",
        is.na(in_season_week) ~ "Off-Season",
        TRUE ~ "Unknown"
      )
    ) %>%
    select(-jan_1, -start_date, -end_date)
  
  df_temp <- check_column_names(df_temp)
  df_temp
}, "Build Catapult Calendar")


###############################################################################
#                SECTION 10: RELATIONAL TABLE (COLUMN NAMES)                  #
###############################################################################
df_list <- list(
  catapult_calendar    = df_catapult_calendar,
  activities           = df_activities,
  athletes             = df_athletes,
  stats_activities     = df_stats_activities,
  stats_period         = df_stats_period,
  periods              = df_periods
)

convert_colnames_to_tibble <- function(df, df_name) {
  tibble(
    dataframe   = df_name,
    column_name = colnames(df)
  )
}

df_column_names <- safe_run({
  combined <- bind_rows(
    lapply(names(df_list), function(nm) convert_colnames_to_tibble(df_list[[nm]], nm))
  )
  combined
}, "Build Column Names Table")

df_column_names <- safe_run({
  column_counts <- df_column_names %>%
    group_by(column_name) %>%
    summarise(
      occurrences = n(),
      related_dataframes = paste(unique(dataframe), collapse=".")
    ) %>%
    ungroup()
  
  merged_cols <- df_column_names %>%
    left_join(column_counts, by="column_name") %>%
    arrange(column_name, dataframe)
  
  merged_cols <- check_column_names(merged_cols)
  merged_cols
}, "Add Column Occurrence Info")


###############################################################################
#                 SECTION 11: SAVE CLEANED DATA & DUPLICATES                  #
###############################################################################
files_written <- 0
excels_written <- 0

# Utility to finalize data: convert all date columns to MDY, then write CSV
save_csv_mdy <- function(df, path) {
  if (!is.null(df) && nrow(df) > 0) {
    df <- convert_dates_to_mdy(df)
    write_csv(df, path)
    files_written <<- files_written + 1
  } else {
    message("No data or zero rows found. Not writing file -> ", path)
  }
}

safe_run({
  if (athlete_cleaning) {
    save_csv_mdy(df_athletes, file.path(config$output_dir, "df_all_athletes_clean.csv"))
  } else {
    message("Unable to save: athlete data was not cleaned properly")
  }
}, "Save Athletes")

safe_run({
  if (activities_cleaning) {
    save_csv_mdy(df_activities, file.path(config$output_dir, "df_activities_clean.csv"))
  } else {
    message("Unable to save: activities data was not cleaned properly")
  }
}, "Save Activities")

safe_run({
  if (exists("df_unique_activities") && nrow(df_unique_activities) > 0) {
    save_csv_mdy(df_unique_activities, file.path(config$output_dir, "df_unique_activites.csv"))
  } else {
    message("No unique activities to write.")
  }
}, "Save Unique Activities")

safe_run({
  if (period_cleaning) {
    save_csv_mdy(df_periods, file.path(config$output_dir, "df_all_periods_clean.csv"))
  } else {
    message("Unable to save: periods data was not cleaned properly")
  }
}, "Save Periods")

safe_run({
  if (exists("df_unique_periods") && nrow(df_unique_periods) > 0) {
    save_csv_mdy(df_unique_periods, file.path(config$output_dir, "df_unique_periods.csv"))
  } else {
    message("No unique periods to write.")
  }
}, "Save Unique Periods")

safe_run({
  if (agg_activity_cleaning_part1 & agg_activity_cleaning_part2) {
    save_csv_mdy(df_stats_activities, file.path(config$output_dir, "df_aggregate_activity_stats_clean.csv"))
  } else {
    message("Unable to save: Aggregated activity data was not cleaned properly")
  }
}, "Save Aggregated Activity Stats")

safe_run({
  if (agg_period_cleaning_part1 & agg_period_cleaning_part2) {
    save_csv_mdy(df_stats_period, file.path(config$output_dir, "df_aggregate_period_stats_clean.csv"))
  } else {
    message("❌ Unable to save: Aggregated period data was not cleaned properly.")
  }
}, "Save Aggregated Period Stats")

safe_run({
  df_catapult_calendar_mdy <- convert_dates_to_mdy(df_catapult_calendar)
  write.csv(df_catapult_calendar_mdy, file.path(config$output_dir, "df_catapult_calandar.csv"), row.names = FALSE)
  files_written <<- files_written + 1
}, "Save Catapult Calendar")

safe_run({
  df_column_names_mdy <- convert_dates_to_mdy(df_column_names)
  write.csv(df_column_names_mdy, file.path(config$output_dir, "relational_database.csv"), row.names = FALSE)
  files_written <<- files_written + 1
}, "Save Relational Column Names")

safe_run({
  if (exists("tag_db") && nrow(tag_db) > 0) {
    tag_db_mdy <- convert_dates_to_mdy(tag_db)
    write_csv(tag_db_mdy, file.path(config$output_dir, "df_tags_clean.csv"))
    files_written <<- files_written + 1
  } else {
    message("No tags data to write.")
  }
}, "Save Tags DB")

safe_run({
  if (exists("tags_activities") && nrow(tags_activities) > 0) {
    tags_activities_mdy <- convert_dates_to_mdy(tags_activities)
    write_csv(tags_activities_mdy, file.path(config$output_dir, "calc_activities_by_tag.csv"))
    files_written <<- files_written + 1
  } else {
    message("No activity tags data to write.")
  }
}, "Save Activities Tags")

safe_run({
  if (exists("tags_periods") && nrow(tags_periods) > 0) {
    tags_periods_mdy <- convert_dates_to_mdy(tags_periods)
    write_csv(tags_periods_mdy, file.path(config$output_dir, "calc_periods_by_tag.csv"))
    files_written <<- files_written + 1
  } else {
    message("No periods tags data to write.")
  }
}, "Save Periods Tags")

# Duplicates log
safe_run({
  tables <- list(
    activities_ID   = if (exists("activity_duplicates_active_id")) activity_duplicates_active_id else data.frame(),
    activities_name = if (exists("activity_duplicates_activity_name")) activity_duplicates_activity_name else data.frame(),
    athlete_name    = if (exists("athlete_duplicates_fullname")) athlete_duplicates_fullname else data.frame(),
    athlete_ID      = if (exists("athlete_duplicates_id")) athlete_duplicates_id else data.frame(),
    period_ID       = if (exists("periods_duplicates_period_id")) periods_duplicates_period_id else data.frame()
  )
  
  duplicate_log_path <- file.path(config$output_dir, "duplicate_log.xlsx")
  write.xlsx(tables, file = duplicate_log_path)
  excels_written <<- excels_written + 1
}, "Write Duplicate Log Excel")


###############################################################################
#               SECTION 12: FINAL SUMMARY & ANOMALY/ERROR REPORT              #
###############################################################################
message(sprintf("Data saving attempted. CSVs written: %d, Excel files: %d", files_written, excels_written))

if (length(anomalies_log) > 0) {
  message("---------- ANOMALIES / ERRORS DETECTED ----------")
  for (i in seq_along(anomalies_log)) {
    step_name <- anomalies_log[[i]]$step
    msg       <- anomalies_log[[i]]$message
    message(sprintf("STEP: %s | ERROR: %s", step_name, msg))
  }
} else {
  message("No errors encountered during cleaning.")
}

message("----- CLEANING SCRIPT COMPLETED -----")
