# Works, just need to isolate 1 year value now
library(httr)      # For making HTTP requests
library(jsonlite)  # For parsing JSON
library(lubridate) # For date manipulation

get_yield_1y <- function() {
  query_date <- Sys.Date()
  
  # Adjust for weekend: if Saturday or Sunday, move to Friday
  if (wday(query_date) %in% c(1, 7)) {
    query_date <- query_date - (wday(query_date) - 6)
  }
  
  date_string <- format(query_date, "%Y-%m-%d")
  api_url <- paste0("https://www.ustreasuryyieldcurve.com/api/v1/yield_curve_snapshot?date=", date_string, "&offset=-1")
  
  response <- GET(api_url)
  
  if (response$status_code == 200) {
    content <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content)
    
    # Print the entire data structure for debugging
    print(data)
    
    if (!is.null(data) && length(data) > 0) {
      if (length(data) >= 1 && !is.null(data[[1]])) {
        first_element <- data[[1]]
        
        # Further checks and processing...
        # (Omitting the rest for brevity - focus is on debugging the structure)
      } else {
        warning("The API response does not contain the expected data.")
        return(list(yield_1y = NA, api_url = api_url))
      }
    } else {
      warning("The API response is empty or not as expected.")
      return(list(yield_1y = NA, api_url = api_url))
    }
  } else {
    warning(paste("Failed to retrieve data for date", date_string, ": HTTP status code", response$status_code))
    return(list(yield_1y = NA, api_url = api_url))
  }
}

# Execute the function and catch errors
tryCatch({
  result <- get_yield_1y()
  # Assuming further access and printing of results here
}, error = function(e) {
  print(paste("An error occurred:", e$message))
})
