# Install/load 'httr' package if needed
# install.packages("httr") # Uncomment if 'httr' not installed

library(httr)

# Install/load 'jsonlite' package if needed
# install.packages("jsonlite") # Uncomment if 'httr' not installed
library(jsonlite)

# Function to get the 1-year yield for the current date
get_yield_1y <- function() {
  # Get the current date in 'YYYY-MM-DD' format
  current_date <- Sys.Date()
  date_string <- format(current_date, "%Y-%m-%d")
  
  # Construct the API URL with the current date
  api_url <- paste0("https://www.ustreasuryyieldcurve.com/api/v1/yield_curve_snapshot?date=", date_string, "&offset=-1")
  
  # Make the GET request to the API endpoint
  response <- GET(api_url)
  
  # Check if the request was successful
  if (response$status_code == 200) {
    # Parse the response
    content <- rawToChar(response$content)
    data <- fromJSON(content)
    
    # Check if the 'yield_curve_date' matches the current date
    if (data$yield_curve_date == date_string) {
      # Extract the 1-year yield value
      yield_1y <- data$yield_1y
      return(yield_1y)
    } else {
      warning("The yield curve date does not match the current date.")
      return(NA)
    }
  } else {
    warning("Failed to retrieve data: HTTP status code ", response$status_code)
    return(NA)
  }
}

# Use the function to get the 1-year yield
yield_1y <- get_yield_1y()
print(yield_1y)
