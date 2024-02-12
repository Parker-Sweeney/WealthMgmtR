######
######
###### This code does not currently work, but adds in a loop to account for weekends
######
######


get_yield_1y <- function() {
  attempt_dates <- 0
  # Loop added due to weekends not having values in the yield curve. In stead of loop, try to have it set date_string to most recent weekday instead of the current method
  while(attempt_dates < 2) {
    # Calculate the date to query based on attempt number
    query_date <- Sys.Date() - attempt_dates
    date_string <- format(query_date, "%Y-%m-%d")
    
    # Construct the API URL with the calculated date
    api_url <- paste0("https://www.ustreasuryyieldcurve.com/api/v1/yield_curve_snapshot?date=", date_string, "&offset=-1")
    
    # Make the GET request to the API endpoint
    response <- GET(api_url)
    
    # Check if the request was successful
    if (response$status_code == 200) {
      # Parse the response
      content <- rawToChar(response$content)
      data <- fromJSON(content)
      
      # Assuming the JSON is an array, access the first element
      first_element <- data[[1]]
      
      if (!is.null(first_element) && first_element$yield_curve_date == date_string) {
        # Extract the 1-year yield value
        yield_1y <- first_element$yield_1y
        
        if (!is.na(yield_1y) && yield_1y != 0) {
          return(list(yield_1y = yield_1y, api_url = api_url))
        } else {
          warning("Yield 1y is NA or 0, which is unexpected.")
        }
      } else {
        warning("The yield curve date does not match the query date or the JSON structure is unexpected.")
      }
    } else {
      warning("Failed to retrieve data for date ", date_string, ": HTTP status code ", response$status_code)
    }
    
    # Increase the attempt count to try the previous day in the next iteration
    attempt_dates <- attempt_dates + 1
  }
  
  # If both attempts fail, return NA and the last attempted API URL
  warning("Failed to retrieve data after 2 attempts.")
  return(list(yield_1y = NA, api_url = api_url))
}

# Use the function to get the 1-year yield
result <- get_yield_1y()
yield_1y <- result$yield_1y
api_url <- result$api_url

print(yield_1y)
print(api_url)
