## Catapult API script

#Import libraries # Packages have NOT been installed
library(httr)
library(jsonlite)
library(tidyverse)
library(catapultR)

# Define output directory
output_dir <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSVs_Raw"

# Set API Token
auth_token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJhdWQiOiI0NjFiMTExMS02ZjdhLTRkYmItOWQyOS0yMzAzOWZlMjI4OGUiLCJqdGkiOiI1YzBhZDVmYjEzYWI2NjFhMjIzMjMxMDNjMjhkN2U3NTJiMWQ1YTZkYWIzZGQ4OGM0YTBiYzcxNzZmNWZhNjhkNzM5YTQ4NjMwOTA0MDMwOSIsImlhdCI6MTcwMjMzMTI2My41NzQ0NDUsIm5iZiI6MTcwMjMzMTI2My41NzQ0NDgsImV4cCI6NDg1NTkzMTI2My41NTY5NzQsInN1YiI6IjFlOWRlYjIxLTFjZWMtNGNhNC05OWUwLTA3MTM3NmY4MDc0MCIsInNjb3BlcyI6WyJjb25uZWN0Il19.fO-yd-_ursSe3VuomSmxyZ4zPsNE3GaCN2G8MwrB1FvWP1I8bMjNdF4VGerO8JGqNqWDKrlCnMG5IuFaUc70cjrK6MhvK8vMG5ULTOhkKietTCNuqo6J0S7BPSBJ1tq5xVBNqPR2qW1mqHGmDcX6zque3EfI_6DJtuZq-ke_qmTVQtFdxW6NuyW9uMrwI7LUT9qDrmI1iPNn1GGxAV5CZXRsW4sqTJz2rRHx-67razDVp6GA8Ps3VBcRYN8YfaYyGK0BdMj_5xTa9cycRXY3tOzecbP2czd3KgN25bphDtmqvW8ZarPXHH0ZrcSrUfO0irR16hTQp8bjJjrmXxSF_DMn9vUwc325DzJa_mOqOF7FUryVky3CweEkon_s_WZaJcQHzllZxk6KIkqvBA54H6sV_z2s6S6CabN6g3fu5PoTVRDdpvp03_NONf0YKyQoIZMHIfTN_SBJws6Kaf7j5AuE3Kx0xavv6Txq8EwuasTjNOZ838YEWOz3fGNTxPOzMSK2-6D6wrSH6fgyVX-PK3XGCBkCfoUXOmcZVdTmWfmKqarEeLXyE-A9ih55SmuYALlxBisGRJmvOQbvbY837HwneCEQUkX2x7k7i1Ihtnobl5jcfEGtiA1vKqm9UIcJzUx0eW-NrHEGFg8sNSkLjxRlmKS6rXQDmqaVRR88_fI" #Insert real token here
#API headers
headers<- c(
  "Authorization" = paste("Bearer", auth_token),
  "Accept" = "application/json"
)
#rebuild token specific for catapult r use
catapultr_token <- ofCloudCreateToken(sToken = auth_token,  sRegion = "America")

# Define function to fetch data from API
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
}


### Below are individual sections of data - data fetchers. Each has the same format. Set URL, build api code, set to data frame. prebuilt error message for each section
########################################## athletes
#fetch athletes 
message("Fetching athletes...")
#set URL
athlete_url = "https://connect-us.catapultsports.com/api/v6/athletes"
#build section specific function - trycatch starts error message
athlete_data = tryCatch({
  fetch_data(athlete_url)
},error = function(e) { #Sets error message
  message(paste("Error fetching Athlete Data:",e$message)) #prints specific error message
  NULL
})
#set data frame for section 
df_all_athletes = if (!is.null(athlete_data)) as.data.frame(athlete_data) else data.frame()
# Rename column 'id' to 'athlete_id'
if ("id" %in% names(df_all_athletes)) {
  colnames(df_all_athletes)[colnames(df_all_athletes) == "id"] <- "athlete_id"
}
rm(athlete_data) #clean up redundancy 
########################################## activities
# Fetch activities
message("Fetching activities...")
activities_url <- "https://connect-us.catapultsports.com/api/v6/activities"
activities_data <- tryCatch({
  fetch_data(activities_url)
}, error = function(e) { 
  message(paste("Error fetching Activities Data:", e$message)) 
  NULL
})

# Convert response to DataFrame
df_activities <- if (!is.null(activities_data)) as.data.frame(activities_data) else data.frame()

# Rename 'id' column to 'activity_id'
if ("id" %in% names(df_activities)) {
  colnames(df_activities)[colnames(df_activities) == "id"] <- "activity_id"
}

# Handle 'activity_athletes' explicitly
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

# General conversion of other nested lists to comma-separated strings
df_activities <- df_activities %>%
  mutate(across(where(is.list), ~ map_chr(.x, ~ paste(unlist(.), collapse = ", "))))

# Convert time-related columns
df_activities <- df_activities %>%
  mutate(
    start_date = as.Date.POSIXct(start_time),
    end_date = as.Date.POSIXct(end_time),
    activity_date = start_date
  )

# Clean up
rm(activities_data)
########################################## Periods
# Fetch periods
message("Fetching Periods...")
periods_url <- "https://connect-us.catapultsports.com/api/v6/periods"
periods_data <- tryCatch({
  fetch_data(periods_url)
}, error = function(e) { 
  message(paste("Error fetching Periods Data:", e$message)) 
  NULL
})

# Convert response to DataFrame
df_periods <- if (!is.null(periods_data)) as.data.frame(periods_data) else data.frame()

# Rename 'id' column to 'period_id'
if ("id" %in% names(df_periods)) {
  colnames(df_periods)[colnames(df_periods) == "id"] <- "period_id"
}

# Convert nested lists to comma-separated strings
df_periods <- df_periods %>%
  mutate(across(where(is.list), ~ map_chr(.x, ~ paste(unlist(.), collapse = ", ")))) %>%
  mutate(
    start_date_time = as.POSIXct(start_time, origin = "1970-01-01", tz = "UTC"),
    start_date = as.Date(start_date_time),
    start_time = format(start_date_time, "%H:%M:%S"),
    end_date_time = as.POSIXct(end_time, origin = "1970-01-01", tz = "UTC"),
    end_date = as.Date(end_date_time),
    end_time = format(end_date_time, "%H:%M:%S")
  )

# Clean up
rm(periods_data)
##########################################
message("Fetching Aggregated Stats Data From CatapultR (chunked approach)...")

# 1) Pre-Setup: read your activity slugs + define 'activity_id_list'
#    e.g., from slugs_activities.csv and your df_activities
needed_slugs <- read.csv("C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/Foundational_CSVs/slugs_activities.csv",
                         header = FALSE, stringsAsFactors = FALSE)
needed_slugs <- as.character(needed_slugs[[1]])

# Suppose you have a data frame df_activities with a valid 'activity_id'
activity_id_list <- df_activities %>%
  filter(!is.na(activity_id)) %>%
  pull(activity_id)

# 2) Chunk Setup
chunk_size <- 200  # Adjust as desired
chunks <- split(
  activity_id_list,
  ceiling(seq_along(activity_id_list) / chunk_size)
)

# 3) Prepare partial CSV writes
output_dir <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSVs_Raw"
activity_outfile <- file.path(output_dir, "df_aggregate_activity_stats_raw.csv")

# If you want to overwrite any existing file, remove it:
if (file.exists(activity_outfile)) {
  file.remove(activity_outfile)
}

append_to_csv <- function(new_data, file_path) {
  if (!file.exists(file_path)) {
    # If file doesn't exist yet, write with headers
    write_csv(new_data, file_path)
  } else {
    # Otherwise append without headers
    write_csv(new_data, file_path, append = TRUE, col_names = FALSE)
  }
}

# 4) Initialize counters for missing data + rate limiting
missing_data_log <- list()
api_request_count <- 0

# Variables for exponential backoff reset
consecutive_no_429 <- 0
retry_delay <- 1

# 5) Define the fetch function with the new exponential backoff logic
fetch_activity_stats <- function(activity_id) {
  max_retries <- 5
  
  for (attempt in seq_len(max_retries)) {
    tryCatch({
      if (api_request_count > 0) {
        message(sprintf("Pausing %d second(s) before call...", retry_delay))
        Sys.sleep(retry_delay)
      }
      api_request_count <<- api_request_count + 1
      
      data <- ofCloudGetStatistics(
        catapultr_token, 
        params = needed_slugs, 
        filters = list(
          name = "activity_id",
          comparison = "=",
          values = activity_id
        )
      )
      
      # Check if empty or null
      if (is.null(data) || nrow(data) == 0 || length(data) == 0) {
        missing_data_log[[length(missing_data_log) + 1]] <-
          data.frame(activity_id = activity_id, error = "No data returned or empty response")
        return(NULL)
      }
      
      # If here => success, so increment consecutive_no_429
      consecutive_no_429 <<- consecutive_no_429 + 1
      
      # If we hit 5 consecutive successes, reset backoff
      if (consecutive_no_429 >= 5) {
        message("5 consecutive successful calls, resetting retry_delay to 1.")
        retry_delay <<- 1
        consecutive_no_429 <<- 0
      }
      
      return(data)
      
    }, error = function(e) {
      # Check specifically for "status_code = 429"
      if (grepl("status_code = 429", e$message)) {
        consecutive_no_429 <<- 0
        retry_delay <<- retry_delay * 2
        message(sprintf("Rate limit 429 => doubling retry_delay => %d, will retry...", retry_delay))
        
      } else {
        # Another type of error
        message(sprintf("Error fetching data for activity_id %s => %s", activity_id, e$message))
        missing_data_log[[length(missing_data_log) + 1]] <-
          data.frame(activity_id = activity_id, error = e$message)
        return(NULL)
      }
    })
  }
  
  # If we exhaust all attempts => log it
  message(sprintf("Max retries reached for activity_id: %s.", activity_id))
  missing_data_log[[length(missing_data_log) + 1]] <-
    data.frame(activity_id = activity_id, error = "Max retries reached")
  return(NULL)
}

# 6) Loop over chunks to fetch & parse
for (i in seq_along(chunks)) {
  chunk_ids <- chunks[[i]]
  
  message(sprintf(
    "Processing chunk %d of %d (%d activity IDs)...",
    i, length(chunks), length(chunk_ids)
  ))
  
  # 6a) Fetch stats for this chunk
  df_activities_stats_list <- purrr::map(chunk_ids, fetch_activity_stats)
  
  # 6b) Remove any NULL entries
  df_activities_stats_list <- purrr::compact(df_activities_stats_list)
  
  # 6c) parse_method_b => flatten nested content
  #     Then combine chunk data
  if (length(df_activities_stats_list) > 0) {
    parsed_chunk <- df_activities_stats_list %>%
      purrr::map(parse_method_b) %>%
      dplyr::bind_rows()
  } else {
    parsed_chunk <- data.frame()
  }
  
  # 6d) If data present, write to CSV
  if (nrow(parsed_chunk) > 0) {
    append_to_csv(parsed_chunk, activity_outfile)
    message(sprintf("Wrote %d rows to %s", nrow(parsed_chunk), activity_outfile))
  } else {
    message("No valid activity stats in this chunk.")
  }
  
  rm(df_activities_stats_list, parsed_chunk)
  gc()
}

###############################################################################
# 7) After finishing all chunks, handle the missing data log
###############################################################################
if (length(missing_data_log) > 0) {
  missing_data_df <- dplyr::bind_rows(missing_data_log)
  
  # define output dir for logs
  error_dir <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/Error_Logs/"
  if (!dir.exists(error_dir)) {
    dir.create(error_dir, recursive = TRUE)
  }
  
  log_file <- file.path(error_dir, paste0("activities_error_log_", Sys.Date(), ".csv"))
  
  readr::write_csv(missing_data_df, log_file)
  message(sprintf("Missing data log saved at: %s", log_file))
} else {
  message("No missing data encountered.")
}

message("Activity Stat Fetching Completed with chunked approach and backoff reset.")
##########################################
# Fetch summary GPS data for each athlete in each activity

###subtask - pull parameters first###
# Fetch parameters (metrics)
message("Fetching parameters...")

# Set URL
parameters_url <- "https://connect-us.catapultsports.com/api/v6/parameters"

# API Request for parameters
parameters_data <- tryCatch({
  fetch_data(parameters_url)
}, error = function(e) {
  message(paste("Error fetching Parameters Data:", e$message))
  NULL
})
# Convert to DataFrame
df_parameters <- if (!is.null(parameters_data)) as.data.frame(parameters_data) else data.frame()
#create list of parameters to call - needs slug name - not regular name or parameter ID
## This list is not filterd - we can create table in another file tthat is the parameters we want and only then pull that list for this section
parameter_slugs = df_parameters %>%
  filter(!is.na(slug)) %>%
  pull(slug)
####This parameter section should only be used one time - then it needs to be converted to slugs of used parameters only


#fetch used parameters slugs
# Read CSV (assuming it has one column without a header)
needed_slugs <- read.csv("C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/Foundational_CSVs/slugs_activities.csv",
                         header = FALSE, stringsAsFactors = FALSE)

# Convert to a character vector
needed_slugs <- as.character(needed_slugs[[1]])

#create list of activity IDS
activity_id_list = df_activities %>%
  filter(!is.na(activity_id)) %>%
  pull(activity_id)

message("Fetching Aggregated Stats Data From CatapultR (chunked approach)...")

# 1) Pre-Setup: read your activity slugs + define 'activity_id_list'
#    e.g., from slugs_activities.csv and your df_activities
needed_slugs <- read.csv("C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/Foundational_CSVs/slugs_activities.csv",
                         header = FALSE, stringsAsFactors = FALSE)
needed_slugs <- as.character(needed_slugs[[1]])

# Suppose you have a data frame df_activities with a valid 'activity_id'
activity_id_list <- df_activities %>%
  filter(!is.na(activity_id)) %>%
  pull(activity_id)

# 2) Chunk Setup
chunk_size <- 200  # Adjust as desired
chunks <- split(
  activity_id_list,
  ceiling(seq_along(activity_id_list) / chunk_size)
)

# 3) Prepare partial CSV writes
output_dir <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/CSVs_Raw"
activity_outfile <- file.path(output_dir, "df_aggregate_activity_stats_raw.csv")

# If you want to overwrite any existing file, remove it:
if (file.exists(activity_outfile)) {
  file.remove(activity_outfile)
}

append_to_csv <- function(new_data, file_path) {
  if (!file.exists(file_path)) {
    # If file doesn't exist yet, write with headers
    write_csv(new_data, file_path)
  } else {
    # Otherwise append without headers
    write_csv(new_data, file_path, append = TRUE, col_names = FALSE)
  }
}

# 4) Initialize counters for missing data + rate limiting
missing_data_log <- list()
api_request_count <- 0

# Variables for exponential backoff reset
consecutive_no_429 <- 0
retry_delay <- 1

# 5) Define the fetch function with the new exponential backoff logic
fetch_activity_stats <- function(activity_id) {
  max_retries <- 5
  
  for (attempt in seq_len(max_retries)) {
    tryCatch({
      if (api_request_count > 0) {
        message(sprintf("Pausing %d second(s) before call...", retry_delay))
        Sys.sleep(retry_delay)
      }
      api_request_count <<- api_request_count + 1
      
      data <- ofCloudGetStatistics(
        catapultr_token, 
        params = needed_slugs, 
        filters = list(
          name = "activity_id",
          comparison = "=",
          values = activity_id
        )
      )
      
      # Check if empty or null
      if (is.null(data) || nrow(data) == 0 || length(data) == 0) {
        missing_data_log[[length(missing_data_log) + 1]] <-
          data.frame(activity_id = activity_id, error = "No data returned or empty response")
        return(NULL)
      }
      
      # If here => success, so increment consecutive_no_429
      consecutive_no_429 <<- consecutive_no_429 + 1
      
      # If we hit 5 consecutive successes, reset backoff
      if (consecutive_no_429 >= 5) {
        message("5 consecutive successful calls, resetting retry_delay to 1.")
        retry_delay <<- 1
        consecutive_no_429 <<- 0
      }
      
      return(data)
      
    }, error = function(e) {
      # Check specifically for "status_code = 429"
      if (grepl("status_code = 429", e$message)) {
        consecutive_no_429 <<- 0
        retry_delay <<- retry_delay * 2
        message(sprintf("Rate limit 429 => doubling retry_delay => %d, will retry...", retry_delay))
        
      } else {
        # Another type of error
        message(sprintf("Error fetching data for activity_id %s => %s", activity_id, e$message))
        missing_data_log[[length(missing_data_log) + 1]] <-
          data.frame(activity_id = activity_id, error = e$message)
        return(NULL)
      }
    })
  }
  
  # If we exhaust all attempts => log it
  message(sprintf("Max retries reached for activity_id: %s.", activity_id))
  missing_data_log[[length(missing_data_log) + 1]] <-
    data.frame(activity_id = activity_id, error = "Max retries reached")
  return(NULL)
}

# 6) Loop over chunks to fetch & parse
for (i in seq_along(chunks)) {
  chunk_ids <- chunks[[i]]
  
  message(sprintf(
    "Processing chunk %d of %d (%d activity IDs)...",
    i, length(chunks), length(chunk_ids)
  ))
  
  # 6a) Fetch stats for this chunk
  df_activities_stats_list <- purrr::map(chunk_ids, fetch_activity_stats)
  
  # 6b) Remove any NULL entries
  df_activities_stats_list <- purrr::compact(df_activities_stats_list)
  
  # 6c) parse_method_b => flatten nested content
  #     Then combine chunk data
  if (length(df_activities_stats_list) > 0) {
    parsed_chunk <- df_activities_stats_list %>%
      purrr::map(parse_method_b) %>%
      dplyr::bind_rows()
  } else {
    parsed_chunk <- data.frame()
  }
  
  # 6d) If data present, write to CSV
  if (nrow(parsed_chunk) > 0) {
    append_to_csv(parsed_chunk, activity_outfile)
    message(sprintf("Wrote %d rows to %s", nrow(parsed_chunk), activity_outfile))
  } else {
    message("No valid activity stats in this chunk.")
  }
  
  rm(df_activities_stats_list, parsed_chunk)
  gc()
}

###############################################################################
# 7) After finishing all chunks, handle the missing data log
###############################################################################
if (length(missing_data_log) > 0) {
  missing_data_df <- dplyr::bind_rows(missing_data_log)
  
  # define output dir for logs
  error_dir <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/Error_Logs/"
  if (!dir.exists(error_dir)) {
    dir.create(error_dir, recursive = TRUE)
  }
  
  log_file <- file.path(error_dir, paste0("activities_error_log_", Sys.Date(), ".csv"))
  
  readr::write_csv(missing_data_df, log_file)
  message(sprintf("Missing data log saved at: %s", log_file))
} else {
  message("No missing data encountered.")
}

message("Activity Stat Fetching Completed with chunked approach and backoff reset.")

######################################################################################

#########################################
#Save data to CSV#
message("Saving data to CSV...")

# Track number of successful file writes
files_written <- 0

# Save data to CSV only if data is available
if (nrow(df_all_athletes) > 0) {
  write_csv(df_all_athletes, file.path(output_dir, "df_all_athletes_raw.csv"))
  files_written <- files_written + 1
} else {
  message("No athlete data available to write.")
}

if (nrow(df_activities) > 0) {
  write_csv(df_activities, file.path(output_dir, "df_activities_raw.csv"))
  files_written <- files_written + 1
} else {
  message("No activities data available to write.")
}

if (nrow(df_periods) > 0) {
  write_csv(df_periods, file.path(output_dir, "df_periods_raw.csv"))
  files_written <- files_written + 1
} else {
  message("No period data available to write.")
}

if (nrow(df_activities_stats) > 0) {
  write_csv(df_activities_stats, file.path(output_dir, "df_aggregate_activity_stats_raw.csv"))
  files_written <- files_written + 1
} else {
  message("No activity data available to write.")
}

if (nrow(df_period_stats) > 0) {
  write_csv(df_period_stats, file.path(output_dir, "df_aggregate_period_stats_raw.csv"))
  files_written <- files_written + 1
} else {
  message("No period data available to write.")
}

if (nrow(df_parameters) > 0) {
  write_csv(df_parameters, file.path(output_dir, "df_parameters_raw.csv"))
  files_written <- files_written + 1
} else {
  message("No parameters data available to write.")
}

# Print summary message
if (files_written > 0) {
  message(paste("Data successfully fetched and saved to", files_written, "out of 5 CSV file(s)."))
} else {
  message("No data was available to save.")
}
