# master_script.R

# Load necessary libraries
library(taskscheduleR)

# Define paths to your scripts
script1 <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/OPERATIONAL_Catapult_ReRUN_API_Scipt_V1.R"
script2 <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/OPERATIONAL_Catapult_Cleaning_V1.R"
script3 <- "C:/Users/Torin/Documents/Strength and Conditioning - Jobs/API Test/OPERATIONAL_Catapult_SQL_BIGQUERY_Upload.R"

# Function to run a script and check for errors
run_script <- function(script_path) {
  cat("Running:", script_path, "/n")
  tryCatch({
    source(script_path)  # Run the script
    cat("Completed:", script_path, "/n")
  }, error = function(e) {
    cat("Error in:", script_path, "/n")
    cat("Error message:", e$message, "/n")
    stop("Stopping execution due to error in:", script_path)
  })
}

# Run the scripts in order
run_script(script1)  # Run the first script
run_script(script2)  # Run the second script
run_script(script3)  # Run the third script

cat("All scripts completed successfully./n")