# ------------------------- Libraries & Authentication -------------------------

library(httr)
library(jsonlite)
library(tidyverse)
library(progress)
library(catapultR)

# CatapultR authentication
auth_token <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJhdWQiOiI0NjFiMTExMS02ZjdhLTRkYmItOWQyOS0yMzAzOWZlMjI4OGUiLCJqdGkiOiI1YzBhZDVmYjEzYWI2NjFhMjIzMjMxMDNjMjhkN2U3NTJiMWQ1YTZkYWIzZGQ4OGM0YTBiYzcxNzZmNWZhNjhkNzM5YTQ4NjMwOTA0MDMwOSIsImlhdCI6MTcwMjMzMTI2My41NzQ0NDUsIm5iZiI6MTcwMjMzMTI2My41NzQ0NDgsImV4cCI6NDg1NTkzMTI2My41NTY5NzQsInN1YiI6IjFlOWRlYjIxLTFjZWMtNGNhNC05OWUwLTA3MTM3NmY4MDc0MCIsInNjb3BlcyI6WyJjb25uZWN0Il19.fO-yd-_ursSe3VuomSmxyZ4zPsNE3GaCN2G8MwrB1FvWP1I8bMjNdF4VGerO8JGqNqWDKrlCnMG5IuFaUc70cjrK6MhvK8vMG5ULTOhkKietTCNuqo6J0S7BPSBJ1tq5xVBNqPR2qW1mqHGmDcX6zque3EfI_6DJtuZq-ke_qmTVQtFdxW6NuyW9uMrwI7LUT9qDrmI1iPNn1GGxAV5CZXRsW4sqTJz2rRHx-67razDVp6GA8Ps3VBcRYN8YfaYyGK0BdMj_5xTa9cycRXY3tOzecbP2czd3KgN25bphDtmqvW8ZarPXHH0ZrcSrUfO0irR16hTQp8bjJjrmXxSF_DMn9vUwc325DzJa_mOqOF7FUryVky3CweEkon_s_WZaJcQHzllZxk6KIkqvBA54H6sV_z2s6S6CabN6g3fu5PoTVRDdpvp03_NONf0YKyQoIZMHIfTN_SBJws6Kaf7j5AuE3Kx0xavv6Txq8EwuasTjNOZ838YEWOz3fGNTxPOzMSK2-6D6wrSH6fgyVX-PK3XGCBkCfoUXOmcZVdTmWfmKqarEeLXyE-A9ih55SmuYALlxBisGRJmvOQbvbY837HwneCEQUkX2x7k7i1Ihtnobl5jcfEGtiA1vKqm9UIcJzUx0eW-NrHEGFg8sNSkLjxRlmKS6rXQDmqaVRR88_fI"  # Insert real token here
catapultr_token <- ofCloudCreateToken(sToken = auth_token, sRegion = "America")

# Non-CatapultR authentication
headers <- c(
  "Authorization" = paste("Bearer", auth_token),
  "Accept" = "application/json"
)

# ------------------------- Helper Functions -------------------------

# Define function to fetch data from API (unchanged except print->message)
fetch_data <- function(url) {
  response <- VERB("GET", url, add_headers(.headers = headers), content_type("application/octet-stream"))
  
  if (status_code(response) == 200) {
    raw_content <- content(response, "text", encoding = "UTF-8")
    if (nchar(raw_content) > 0) {
      return(fromJSON(raw_content, flatten = TRUE))
    } else {
      message(paste("Warning: Empty API response from:", url))
      return(NULL)
    }
  } else {
    message(paste("Error fetching data from:", url, "Status code:", status_code(response)))
    return(NULL)
  }
}

# Consolidated error log storage
# We will store all errors (activities, periods, etc.) in this one data frame:
df_error_log <- data.frame(
  error_type = character(),
  item_id    = character(),
  error_msg  = character(),
  stringsAsFactors = FALSE
)

append_error_log <- function(err_type, err_id, err_msg) {
  # Helper to unify error logging
  new_row <- data.frame(
    error_type = err_type,
    item_id    = as.character(err_id),
    error_msg  = as.character(err_msg),
    stringsAsFactors = FALSE
  )
  # Use <<- to modify the df_error_log in the parent environment
  df_error_log <<- bind_rows(df_error_log, new_row)
}

# ------------------------- Fetch Base Files -------------------------
message("Starting Base Files: Fetching Athletes...")

# 1) Fetch athletes
athlete_url <- "https://connect-us.catapultsports.com/api/v6/athletes"
athlete_data <- tryCatch({
  fetch_data(athlete_url)
}, error = function(e) { 
  message(paste("Error fetching Athlete Data:", e$message))
  NULL
})
df_all_athletes <- if (!is.null(athlete_data)) as.data.frame(athlete_data) else data.frame()
if ("id" %in% names(df_all_athletes)) {
  colnames(df_all_athletes)[colnames(df_all_athletes) == "id"] <- "athlete_id"
}
rm(athlete_data)

# 2) Fetch activities
message("Fetching activities...")
activities_url <- "https://connect-us.catapultsports.com/api/v6/activities"
activities_data <- tryCatch({
  fetch_data(activities_url)
}, error = function(e) { 
  message(paste("Error fetching Activities Data:", e$message))
  NULL
})
df_activities <- if (!is.null(activities_data)) as.data.frame(activities_data) else data.frame()
if ("id" %in% names(df_activities)) {
  colnames(df_activities)[colnames(df_activities) == "id"] <- "activity_id"
}
if ("activity_athletes" %in% names(df_activities)) {
  df_activities <- df_activities %>%
    mutate(activity_athletes = map_chr(activity_athletes, ~ {
      if (is.list(.x)) {
        paste(map_chr(.x, ~ paste(unlist(.), collapse = ":")), collapse = ", ")
      } else {
        as.character(.x)
      }
    }))
}
df_activities <- df_activities %>%
  mutate(across(where(is.list), ~ map_chr(.x, ~ paste(unlist(.), collapse = ", ")))) %>%
  mutate(
    start_date    = as.Date.POSIXct(start_time),
    end_date      = as.Date.POSIXct(end_time),
    activity_date = start_date
  )
rm(activities_data)

# 3) Fetch periods
message("Fetching Periods...")
periods_url <- "https://connect-us.catapultsports.com/api/v6/periods"
periods_data <- tryCatch({
  fetch_data(periods_url)
}, error = function(e) {
  message(paste("Error fetching Periods Data:", e$message))
  NULL
})
df_periods <- if (!is.null(periods_data)) as.data.frame(periods_data) else data.frame()
if ("id" %in% names(df_periods)) {
  colnames(df_periods)[colnames(df_periods) == "id"] <- "period_id"
}
df_periods <- df_periods %>%
  mutate(across(where(is.list), ~ map_chr(.x, ~ paste(unlist(.), collapse = ", ")))) %>%
  mutate(
    start_date_time = as.POSIXct(start_time, origin = "1970-01-01", tz = "UTC"),
    start_date      = as.Date(start_date_time),
    start_time      = format(start_date_time, "%H:%M:%S"),
    end_date_time   = as.POSIXct(end_time, origin = "1970-01-01", tz = "UTC"),
    end_date        = as.Date(end_date_time),
    end_time        = format(end_date_time, "%H:%M:%S")
  )
rm(periods_data)

message("Completed update of Base Files. Moving to find latest date.")

# ------------------------- Identify New Data Range -------------------------
# If no old activities file is present, you might need a fallback (not removing code, just adding a check)
existing_activities_file <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSVs_Raw/df_activities_raw.csv"
if (!file.exists(existing_activities_file)) {
  message("Warning: Could not find df_activities_raw.csv. It may be your first run. Proceed with caution.")
  # If you want to do something else (like skipping or defaulting to first run) you can add logic here.
}

# Read old activities file
old_activities <- read.csv(existing_activities_file)
old_activities$end_date <- as.Date(old_activities$end_date, format = "%Y-%m-%d")
last_activity_date <- max(old_activities$end_date)

from <- as.integer(as.POSIXct(as.Date(last_activity_date)))
today <- Sys.Date()
to   <- as.integer(as.POSIXct(as.Date(today)))

if (last_activity_date == today) {
  stop("No New Data: The last activity date is today. Stopping the script.")
}

message("Checking for new data. Script is running...")

# ------------------------- Fetch New Activities -------------------------
new_activities_list <- ofCloudGetActivities(catapultr_token, from = from, to = to)
if (!is.null(new_activities_list) && nrow(new_activities_list) > 0) {
  new_activites <- new_activities_list$id
} else {
  # Graceful skip: if no new activities, we skip pulling stats
  message("No new activities found. Skipping subsequent activity-based steps.")
  new_activites <- character(0) 
}

# ------------------------- Fetch New Periods -------------------------
new_periods_df <- data.frame()
if (length(new_activites) > 0) {
  for (id_val in new_activites) {
    temp_periods_new <- ofCloudGetPeriods(catapultr_token, id_val)
    new_periods_df   <- rbind(new_periods_df, temp_periods_new)
  }
}
if (nrow(new_periods_df) > 0) {
  new_periods <- new_periods_df$id
} else {
  message("No new periods were found. Skipping subsequent period-based steps.")
  new_periods <- character(0)
}
rm(new_periods_df)

# ------------------------- Fetch Aggregated Stats for Activities -------------------------
message("Fetching Aggregated Stats Data From CatapultR")

# Load needed slugs
needed_slugs_file <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/Foundational_CSVs/slugs_activities.csv"
needed_slugs_vec  <- read.csv(needed_slugs_file, header = FALSE, stringsAsFactors = FALSE)
needed_slugs_vec  <- as.character(needed_slugs_vec[[1]])

df_activities_stats <- data.frame()

if (length(new_activites) > 0) {
  # Only loop if we actually have new activities
  for (activity_id in new_activites) {
    temp_activities_stats <- tryCatch({
      ofCloudGetStatistics(
        catapultr_token,
        params = needed_slugs_vec,
        filters = list(
          name       = "activity_id",
          comparison = "=",
          values     = activity_id
        )
      )
    }, error = function(e) {
      message(sprintf("Error fetching data for activity_id: %s. Error: %s", activity_id, e$message))
      append_error_log("ActivityStats", activity_id, e$message)
      return(NULL)
    })
    if (!is.null(temp_activities_stats)) {
      df_activities_stats <- bind_rows(df_activities_stats, temp_activities_stats)
    }
  }
} else {
  message("Skipping activity stats fetch due to no new activity IDs.")
}

message("Activity Stat Fetching Completed.")

# ------------------------- Fetch Aggregated Stats for Periods -------------------------
message("Fetching Period Aggregate Data From CatapultR")

period_slugs_file <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/Foundational_CSVs/slugs_periods.csv"
period_slugs <- read.csv(period_slugs_file, header = FALSE, stringsAsFactors = FALSE)
period_slugs <- as.character(period_slugs[[1]])

api_request_count <- 0  # For rate-limiting demonstration
df_period_stats_list <- list()

if (length(new_periods) > 0) {
  # Only loop if we actually have new periods
  for (pid in new_periods) {
    # Provide up to 5 retries for each period
    max_retries  <- 5
    retry_delay  <- 1
    fetch_result <- NULL
    
    for (retry_i in seq_len(max_retries)) {
      res <- tryCatch({
        if (api_request_count > 0) {
          message(sprintf("Pausing for %d second(s) to avoid rate limiting...", retry_delay))
          Sys.sleep(retry_delay)
        }
        api_request_count <- api_request_count + 1
        
        tmp_data <- ofCloudGetStatistics(
          catapultr_token,
          params = period_slugs,
          filters = list(
            name       = "period_id",
            comparison = "=",
            values     = pid
          )
        )
        
        if (is.null(tmp_data) || nrow(tmp_data) == 0 || length(tmp_data) == 0) {
          # We'll treat this as an error event to unify with everything else
          append_error_log("PeriodStats", pid, "No data returned or empty response")
          NULL
        } else {
          tmp_data
        }
      }, error = function(e) {
        if (grepl("status_code = 429", e$message)) {
          # Rate limit, do exponential backoff
          retry_delay <<- retry_delay * 2
          message(sprintf("Rate limit hit. Retrying in %d seconds...", retry_delay))
          return(NULL)
        } else {
          message(sprintf("Error fetching data for period_id: %s. Error: %s", pid, e$message))
          append_error_log("PeriodStats", pid, e$message)
          return(NULL)
        }
      })
      
      if (!is.null(res)) {
        fetch_result <- res
        break
      }
    } # end for (retry)
    
    if (!is.null(fetch_result)) {
      # Convert numeric columns to character, ensure period_name is character
      fetch_result <- fetch_result %>%
        mutate(across(where(is.numeric), as.character),
               period_name = as.character(period_name))
      df_period_stats_list[[length(df_period_stats_list) + 1]] <- fetch_result
    }
  }
} else {
  message("Skipping period stats fetch due to no new period IDs.")
}

df_period_stats <- if (length(df_period_stats_list) > 0) {
  bind_rows(df_period_stats_list)
} else {
  data.frame()
}

message("Period Stat Fetching Completed.")

# ------------------------- Fetch Venues -------------------------
message("Fetching Venues...")
venue_url <- "https://connect-us.catapultsports.com/api/v6/venues"
venue_data <- tryCatch({
  fetch_data(venue_url)
}, error = function(e) {
  message(paste("Error fetching Venue Data:", e$message))
  append_error_log("Venues", "N/A", e$message)
  NULL
})
df_venue <- if (!is.null(venue_data)) as.data.frame(venue_data) else data.frame()

# ------------------------- Save Data to CSV -------------------------
message("Saving data to CSV...")

append_to_csv <- function(new_data, filename) {
  file_path <- file.path(output_dir, filename)
  
  # If no new data rows, just log a message and exit
  if (nrow(new_data) == 0) {
    message(sprintf("No new data available for %s.", filename))
    return(invisible(NULL))
  }
  
  # If the file does not exist yet, write out new_data as is
  if (!file.exists(file_path)) {
    write_csv(new_data, file_path)
    message(sprintf("Created %s with initial records.", filename))
    return(invisible(NULL))
  }
  
  # Otherwise, read existing CSV so we know column names & types
  existing_data <- read_csv(file_path, show_col_types = FALSE)
  
  # 1) Ensure new_data has all columns from existing_data
  for (colname in names(existing_data)) {
    if (!colname %in% names(new_data)) {
      # Add a missing column with NA
      new_data[[colname]] <- NA
    }
  }
  
  # 2) Keep only the columns that exist in existing_data
  #    (this also drops any extra columns in new_data that don't exist in existing_data)
  new_data <- new_data[, names(existing_data), drop = FALSE]
  
  # 3) For each column in existing_data, coerce the new_data column to the same class
  for (colname in names(existing_data)) {
    existing_class <- class(existing_data[[colname]])[1]  # e.g. "character", "numeric", "Date", "POSIXct", etc.
    current_class  <- class(new_data[[colname]])[1]
    
    if (!inherits(new_data[[colname]], existing_class)) {
      # Attempt to coerce
      message(sprintf(
        "Coercing column '%s' from '%s' to '%s'.",
        colname, current_class, existing_class
      ))
      
      # If you have special handling for certain classes, you can do so here. 
      # Otherwise, dynamically use "as.<class>" if it exists:
      convert_fun_name <- paste0("as.", existing_class)
      
      # Some classes won't match a direct "as.<class>" (e.g. factor, POSIXlt).
      # We'll do a generic tryCatch to handle errors gracefully.
      tryCatch({
        convert_fun <- match.fun(convert_fun_name)
        new_data[[colname]] <- convert_fun(new_data[[colname]])
      }, error = function(e) {
        warning(sprintf(
          "Column '%s': failed to coerce from '%s' to '%s'. Retaining original data. Error: %s",
          colname, current_class, existing_class, e$message
        ))
        # If there's a more graceful fallback, apply it here.
      })
    }
  }
  
  # 4) Now that new_data and existing_data share the same columns & classes,
  #    bind them together and write out
  combined_data <- bind_rows(existing_data, new_data)
  write_csv(combined_data, file_path)
  
  message(sprintf("Updated %s with new records.", filename))
}

output_dir <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSVs_Raw"
files_written <- 0

# 1) Athletes
if (nrow(df_all_athletes) > 0) {
  write_csv(df_all_athletes, file.path(output_dir, "df_all_athletes_raw.csv"))
  files_written <- files_written + 1
} else {
  message("No athlete data available to write.")
}

# 2) Activities
if (nrow(df_activities) > 0) {
  write_csv(df_activities, file.path(output_dir, "df_activities_raw.csv"))
  files_written <- files_written + 1
} else {
  message("No activities data available to write.")
}

# 3) Periods
if (nrow(df_periods) > 0) {
  write_csv(df_periods, file.path(output_dir, "df_periods_raw.csv"))
  files_written <- files_written + 1
} else {
  message("No period data available to write.")
}

# 4) Activity Stats (appended)
if (nrow(df_activities_stats) > 0) {
  append_to_csv(df_activities_stats, "df_aggregate_activity_stats_raw.csv")
  files_written <- files_written + 1
} else {
  message("No activity data available to write.")
}

# 5) Period Stats (appended)
if (nrow(df_period_stats) > 0) {
  append_to_csv(df_period_stats, "df_aggregate_period_stats_raw.csv")
  files_written <- files_written + 1
} else {
  message("No period data available to write.")
}

# 6) Venues
if (nrow(df_venue) > 0) {
  write_csv(df_venue, file.path(output_dir, "df_venues.csv"))
  files_written <- files_written + 1
} else {
  message("No venue data available to write.")
}

# ------------------------- Save Consolidated Error Log (if any) -------------------------
if (nrow(df_error_log) > 0) {
  error_dir  <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/Error_Logs"
  if (!dir.exists(error_dir)) {
    dir.create(error_dir, recursive = TRUE)
  }
  error_file <- file.path(error_dir, paste0("rerun_error_log_", Sys.Date(), ".csv"))
  write_csv(df_error_log, error_file)
  message(sprintf("Consolidated error log saved at: %s", error_file))
} else {
  message("No errors logged during this run.")
}

# Final summary message
if (files_written > 0) {
  message(paste("Data successfully fetched and saved to", files_written, "out of 6 CSV file(s)."))
} else {
  message("No data was available to save.")
}

message("ReRun Script Completed.")
