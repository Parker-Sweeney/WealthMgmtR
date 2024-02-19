library(httr)
library(jsonlite)

# Function to get the 1-year yield for the current or most recent valid date
get_yield_1y <- function() {
  # Get the current date
  current_date <- Sys.Date()
  
  # Function to find the most recent weekday
  find_most_recent_weekday <- function(date) {
    weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
    while (format(date, "%A") %in% c('Saturday', 'Sunday') || !format(date, "%A") %in% weekdays) {
      date <- date - 1 # Go back one day
    }
    return(date)
  }
  
  # Adjust current date to the most recent weekday if necessary
  adjusted_date <- find_most_recent_weekday(current_date)
  date_string <- format(adjusted_date, "%Y-%m-%d")
  
  attempt_fetch_yield <- function(date_string) {
    api_url <- paste0("https://www.ustreasuryyieldcurve.com/api/v1/yield_curve_snapshot?date=", date_string)
    response <- GET(api_url)
    print(date_string)
    print(api_url)
    print(response)
    if (response$status_code == 200) {
      content <- rawToChar(response$content)
      data <- fromJSON(content)
      
      if (data$yield_curve_date == date_string) {
        return(data$yield_1y)
      }
    }
    return(NULL) # Indicate failure to fetch or match the date
  }
  
  # Attempt to fetch yield with the initial date_string
  yield_1y <- attempt_fetch_yield(date_string)
  
  # If initial fetch fails and the day is Monday, adjust date_string to the previous Friday and retry
  if (is.null(yield_1y) && format(adjusted_date, "%A") == "Monday") {
    adjusted_date <- adjusted_date - 3 # Go back to the previous Friday
    date_string <- format(adjusted_date, "%Y-%m-%d")
    yield_1y <- attempt_fetch_yield(date_string) # Retry with adjusted date
  }
  
  # Final check to handle if yield is still NULL after adjustments
  if (is.null(yield_1y)) {
    warning("Failed to retrieve matching yield curve data after adjustments.")
    return(NA)
  }
  
  return(yield_1y)
}

# Use the function to get the 1-year yield
yield_1y <- get_yield_1y()
print(yield_1y)
