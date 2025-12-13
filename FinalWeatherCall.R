

# ---- Configuration ----
api_key <- 
  "8NSBEWMFSWA52BLEM5ZLQUZPA"
  #           "ZBVNGCL8C6VAQ35Z98M4E4F6F"

library(jsonlite)
library(dplyr)
library(lubridate)
library(httr)

# ==============================================================================
# SECTION 1: Initialize new locations with just 1 day
# ==============================================================================

new_locations <- c(
  "Winnipeg, Manitoba, Canada",
  "Sault Ste. Marie, Ontario, Canada",
  "Toronto, Ontario, Canada",
  "Seattle, Washington, US"
)

# Just fetch yesterday for each new location (1 day = 1 token each)
yesterday <- as.character(today() - days(1))

for (loc in new_locations) {
  cat("\nInitializing", loc, "with 1 day...\n")
  update_weather_data(loc, yesterday, yesterday, api_key)
}

cat("\n✓ All new locations initialized with 1 day each\n")
cat("Total tokens used: 4\n")
cat("Now run smart_update_all() to equalize and backfill gradually\n")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

api_key <- "YOUR_API_KEY_HERE"
daily_api_limit <- 1000

locations <- c(
  "Grand Island, New York, US",
  "Thunder Bay, Ontario, Canada",
  "Winnipeg, Manitoba, Canada",
  "Sault Ste. Marie, Ontario, Canada",
  "Toronto, Ontario, Canada",
  "Minneapolis, Minnesota, US",
  "Chicago, Illinois, US",
  "Seattle, Washington, US"
)

# ==============================================================================
# CORE FUNCTION: Fetch and update weather data
# ==============================================================================

update_weather_data <- function(location, start_date, end_date, api_key, unit_group = "metric") {
  
  clean_location <- gsub("[^[:alnum:]]", "", location)
  data_file <- paste0("VC_", clean_location, ".rds")
  
  cat("\n=== Processing:", location, "===\n")
  cat("File:", data_file, "\n")
  
  start_date_obj <- as.Date(start_date)
  end_date_obj <- as.Date(end_date)
  
  if (file.exists(data_file)) {
    weather_df <- readRDS(data_file)
    existing_dates <- weather_df$date
    
    cat("Existing data:", nrow(weather_df), "records from", 
        as.character(min(existing_dates)), "to", as.character(max(existing_dates)), "\n")
    
    requested_dates <- seq(start_date_obj, end_date_obj, by = "day")
    dates_needed <- requested_dates[!requested_dates %in% existing_dates]
    
    if (length(dates_needed) == 0) {
      cat("All requested dates already exist. No API call needed.\n")
      return(weather_df)
    }
    
    start_date <- as.character(min(dates_needed))
    end_date <- as.character(max(dates_needed))
    
    days_span <- as.numeric(max(dates_needed) - min(dates_needed)) + 1
    if (length(dates_needed) != days_span) {
      cat("WARNING: Requested date range has gaps. Fetching entire range.\n")
    }
    
    cat("Adjusted to fetch only new dates:", start_date, "to", end_date, "\n")
    
  } else {
    weather_df <- tibble()
    cat("No existing data file\n")
    start_date <- as.character(start_date_obj)
    end_date <- as.character(end_date_obj)
  }
  
  base_url <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline"
  query_url <- sprintf(
    "%s/%s/%s/%s?unitGroup=%s&key=%s&include=days",
    base_url, URLencode(location), start_date, end_date, unit_group, api_key
  )
  
  cat("Fetching", start_date, "to", end_date, "...\n")
  resp <- GET(query_url)
  if (resp$status_code != 200) {
    cat("ERROR: API request failed with status", resp$status_code, "\n")
    return(invisible(NULL))
  }
  
  data_json <- content(resp, as = "text", encoding = "UTF-8")
  data_list <- fromJSON(data_json, flatten = TRUE)
  
  new_data <- data_list$days |>
    as_tibble() |>
    mutate(date = ymd(datetime)) |>
    select(date, temp, tempmax, tempmin, precip, humidity, windspeed, cloudcover)
  
  weather_df <- bind_rows(weather_df, new_data) |>
    distinct(date, .keep_all = TRUE) |>
    arrange(date)
  
  saveRDS(weather_df, data_file)
  cat("SAVED:", nrow(weather_df), "total records from", 
      as.character(min(weather_df$date)), "to", as.character(max(weather_df$date)), "\n")
  
  return(weather_df)
}

# ==============================================================================
# MAIN FUNCTION: Analyze, update to current, backfill equally
# ==============================================================================

smart_update_all <- function(locations, daily_api_limit, api_key) {
  
  cat("\n")
  cat("================================================================================\n")
  cat("WEATHER DATA UPDATE - EQUAL DATE RANGES\n")
  cat("================================================================================\n")
  
  calls_used <- 0
  
  # ---- STEP 1: Analyze existing data ----
  cat("\n=== STEP 1: Analyzing existing data ===\n\n")
  
  earliest_dates <- c()
  latest_dates <- c()
  file_exists_flags <- c()
  
  for (loc in locations) {
    clean_loc <- gsub("[^[:alnum:]]", "", loc)
    data_file <- paste0("VC_", clean_loc, ".rds")
    
    if (file.exists(data_file)) {
      existing_df <- readRDS(data_file)
      first_date <- min(existing_df$date, na.rm = TRUE)
      last_date <- max(existing_df$date, na.rm = TRUE)
      
      earliest_dates <- c(earliest_dates, first_date)
      latest_dates <- c(latest_dates, last_date)
      file_exists_flags <- c(file_exists_flags, TRUE)
      
      cat(sprintf("%-40s: %s to %s (%d days)\n", 
                  loc, first_date, last_date, 
                  as.numeric(last_date - first_date) + 1))
    } else {
      earliest_dates <- c(earliest_dates, as.Date(NA))
      latest_dates <- c(latest_dates, as.Date(NA))
      file_exists_flags <- c(file_exists_flags, FALSE)
      
      cat(sprintf("%-40s: NO FILE\n", loc))
    }
  }
  
  earliest_dates <- as.Date(earliest_dates, origin = "1970-01-01")
  latest_dates <- as.Date(latest_dates, origin = "1970-01-01")
  
  if (any(!file_exists_flags)) {
    cat("\n⚠️  ERROR: Some locations don't have data files yet.\n")
    cat("Missing files for:\n")
    for (i in which(!file_exists_flags)) {
      cat("  -", locations[i], "\n")
    }
    cat("\nRun Section 1 first to create initial files, then run this function.\n")
    return(invisible(NULL))
  }
  
  # ---- STEP 2: Update all to today ----
  cat("\n=== STEP 2: Updating all locations to today ===\n")
  
  for (i in seq_along(locations)) {
    loc <- locations[i]
    last_date <- latest_dates[i]
    days_to_update <- as.numeric(today() - last_date)
    
    if (days_to_update > 0) {
      start_date <- as.character(last_date + days(1))
      end_date <- as.character(today())
      
      result <- update_weather_data(loc, start_date, end_date, api_key)
      
      if (!is.null(result)) {
        calls_used <- calls_used + days_to_update
        latest_dates[i] <- today()
      }
    } else {
      cat("\n===", loc, "already up to date ===\n")
    }
  }
  
  cat("\n--- Update Summary ---")
  cat("\nAPI calls used:", calls_used, "/", daily_api_limit, "\n")
  cat("Remaining calls:", daily_api_limit - calls_used, "\n")
  
  # ---- STEP 3: Equalize all locations to achievable start date ----
  cat("\n=== STEP 3: Equalizing all locations to same start date ===\n")
  
  remaining_calls <- daily_api_limit - calls_used
  cat("\nRemaining API calls available:", remaining_calls, "\n")
  
  max_days_affordable <- floor(remaining_calls / length(locations))
  
  cat("Maximum days we can backfill per location:", max_days_affordable, "\n")
  
  if (max_days_affordable == 0) {
    cat("\n⚠️  No API calls remaining for backfill today.\n")
    cat("Try again tomorrow with more API quota.\n")
    return(invisible(NULL))
  }
  
  most_recent_earliest <- max(earliest_dates, na.rm = TRUE)
  target_earliest <- most_recent_earliest - days(max_days_affordable)
  
  cat("Target start date for all locations:", as.character(target_earliest), "\n\n")
  
  total_equalize_calls <- 0
  equalize_plan <- list()
  
  for (i in seq_along(locations)) {
    days_needed <- as.numeric(earliest_dates[i] - target_earliest)
    if (days_needed > 0) {
      equalize_plan[[locations[i]]] <- days_needed
      total_equalize_calls <- total_equalize_calls + days_needed
      cat(sprintf("%-40s: needs %d days\n", locations[i], days_needed))
    } else {
      equalize_plan[[locations[i]]] <- 0
      cat(sprintf("%-40s: already at or past target\n", locations[i]))
    }
  }
  
  cat("\nTotal calls needed:", total_equalize_calls)
  cat("\nRemaining calls:", remaining_calls, "\n")
  
  cat("\n--- Backfilling locations that need equalization ---\n")
  for (loc in locations) {
    if (equalize_plan[[loc]] > 0) {
      clean_loc <- gsub("[^[:alnum:]]", "", loc)
      data_file <- paste0("VC_", clean_loc, ".rds")
      existing_df <- readRDS(data_file)
      earliest_date <- min(existing_df$date)
      
      start_date <- as.character(target_earliest)
      end_date <- as.character(earliest_date - days(1))
      
      result <- update_weather_data(loc, start_date, end_date, api_key)
      
      if (!is.null(result)) {
        calls_used <- calls_used + equalize_plan[[loc]]
        earliest_dates[which(locations == loc)] <- target_earliest
      }
    }
  }
  
  cat("\n✓ All locations now equalized to:", as.character(target_earliest), "\n")
  
  # ---- STEP 4: Backfill ONLY locations with less data ----
  cat("\n=== STEP 4: Additional backfill for locations with less history ===\n")
  
  remaining_calls <- daily_api_limit - calls_used
  
  # Find the oldest start date (location with most history)
  oldest_start <- min(earliest_dates, na.rm = TRUE)
  
  cat("Remaining API calls:", remaining_calls, "\n")
  cat("Oldest start date:", as.character(oldest_start), "\n\n")
  
  # Identify locations that need catching up
  locations_to_backfill <- c()
  for (i in seq_along(locations)) {
    if (earliest_dates[i] > oldest_start) {
      gap_days <- as.numeric(earliest_dates[i] - oldest_start)
      locations_to_backfill <- c(locations_to_backfill, locations[i])
      cat(sprintf("%-40s: %d days behind (starts %s vs %s)\n",
                  locations[i], gap_days, 
                  as.character(earliest_dates[i]), 
                  as.character(oldest_start)))
    }
  }
  
  if (length(locations_to_backfill) == 0) {
    cat("\n✓ All locations already have equal history!\n")
    cat("Using remaining calls to extend ALL locations further back...\n\n")
    
    # All equal, so backfill everyone
    calls_per_location <- floor(remaining_calls / length(locations))
    
    if (calls_per_location > 0) {
      cat("Backfilling each location by", calls_per_location, "additional days...\n")
      
      for (loc in locations) {
        clean_loc <- gsub("[^[:alnum:]]", "", loc)
        data_file <- paste0("VC_", clean_loc, ".rds")
        existing_df <- readRDS(data_file)
        earliest_date <- min(existing_df$date)
        
        start_date <- as.character(earliest_date - days(calls_per_location))
        end_date <- as.character(earliest_date - days(1))
        
        result <- update_weather_data(loc, start_date, end_date, api_key)
        
        if (!is.null(result)) {
          calls_used <- calls_used + calls_per_location
        }
      }
    }
  } else {
    # Only backfill locations that need it
    calls_per_location <- floor(remaining_calls / length(locations_to_backfill))
    
    if (calls_per_location > 0) {
      cat("\nBackfilling only locations with less history by", calls_per_location, "days each...\n")
      
      for (loc in locations_to_backfill) {
        clean_loc <- gsub("[^[:alnum:]]", "", loc)
        data_file <- paste0("VC_", clean_loc, ".rds")
        existing_df <- readRDS(data_file)
        earliest_date <- min(existing_df$date)
        
        # Don't go earlier than the oldest location
        target_start <- max(oldest_start, earliest_date - days(calls_per_location))
        
        start_date <- as.character(target_start)
        end_date <- as.character(earliest_date - days(1))
        
        result <- update_weather_data(loc, start_date, end_date, api_key)
        
        if (!is.null(result)) {
          actual_days <- as.numeric(earliest_date - target_start)
          calls_used <- calls_used + actual_days
        }
      }
    } else {
      cat("\nNo calls remaining for additional backfill.\n")
    }
  }
  
  # ---- FINAL SUMMARY ----
  cat("\n")
  cat("================================================================================\n")
  cat("COMPLETE\n")
  cat("================================================================================\n")
  cat("\nTotal API calls used:", calls_used, "/", daily_api_limit, "\n")
  
  cat("\nFinal date ranges:\n")
  for (loc in locations) {
    clean_loc <- gsub("[^[:alnum:]]", "", loc)
    data_file <- paste0("VC_", clean_loc, ".rds")
    final_df <- readRDS(data_file)
    cat(sprintf("%-40s: %s to %s (%d days)\n",
                loc,
                as.character(min(final_df$date)),
                as.character(max(final_df$date)),
                nrow(final_df)))
  }
  
  cat("\nRun this script again tomorrow to extend history further.\n")
}

# ==============================================================================
# EXECUTION
# ==============================================================================

library(jsonlite)
library(dplyr)
library(lubridate)
library(httr)

smart_update_all(locations, daily_api_limit, api_key)



