require (rvest)
library (rvest)
#Extracting and exporting 

#getting the root of html node
url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"
#Reading the html node from the url
root_node <- read_html(url)
#Getting the table nodes 
table_node <- html_nodes(root_node,"table")
table_data <- html_table(table_node,fill=TRUE)[[1]] #extracts the first table with [[1]] and fills the empty cells with NA
#Converting the table into dataframe
raw_bike_sharing_systems <- as.data.frame(table_data)
#Summarizing the dataframe
summary(raw_bike_sharing_systems)

#Exporting the dataframe as csv file
write.csv(raw_bike_sharing_systems,"raw_bike_sharing_systems.csv")


#WEATHER API PORTION

library(httr)

#url for current weather api
forecast_url <-'https://api.openweathermap.org/data/2.5/forecast'

#api key
api_key <- "d099422038ccbb6c74572e71cf891bdc"

# Create some empty vectors to hold data temporarily

# City name column
city <- c()
# Weather column, rainy or cloudy, etc
weather <- c()
# Sky visibility column
visibility <- c()
# Current temperature column
temp <- c()
# Max temperature column
temp_min <- c()
# Min temperature column
temp_max <- c()
# Pressure column
pressure <- c()
# Humidity column
humidity <- c()
# Wind speed column
wind_speed <- c()
# Wind direction column
wind_deg <- c()
# Forecast timestamp
forecast_datetime <- c()
# Season column

season <- c()

# Get forecast data for a given city list
get_weather_forecaset_by_cities <- function(city_names) {
  df <- data.frame()
  for (city_name in city_names) {
    # Forecast API URL
    forecast_url <- "https://api.openweathermap.org/data/2.5/forecast"
    # Create query parameters
    forecast_query <- list(q = city_name, appid ={api_key}, units = "metric")
    # Make HTTP GET call for the given city
    forecast_response <- GET(forecast_url, query = forecast_query)
    # Note that the 5-day forecast JSON result is a list of lists. You can print the response to check the results
    forecast_json_list <- content(forecast_response, as = "parsed")
    results <- forecast_json_list$list
    result <- c(1:40)
    
    # Loop the json result
    for (x in result) {
      city <- c(city, city_name)
      # $weather is also a list with one element, its $main element indicates the weather status such as clear or rain
      weather <- c(weather, results[[x]]$weather[[1]]$main)
      # Get Visibility
      visibility <- c(visibility, results[[x]]$visibility)
      # Get current temperature
      temp <- c(temp, results[[x]]$main$temp)
      # Get min temperature
      temp_min <- c(temp_min, results[[x]]$main$temp_min)
      # Get max temperature
      temp_max <- c(temp_max, results[[x]]$main$temp_max)
      # Get pressure
      pressure <- c(pressure, results[[x]]$main$pressure)
      # Get humidity
      humidity <- c(humidity, results[[x]]$main$humidity)
      # Get wind speed
      wind_speed <- c(wind_speed, results[[x]]$wind$speed)
      # Get wind direction
      wind_deg <- c(wind_deg, results[[x]]$wind$deg)
      # Get forcast_datetime
      forecast_datetime <- c(forecast_datetime, results[[x]]$dt_txt)
    }
    
    # Add the R Lists into a data frame
    df <- data.frame(
      city = city, weather = weather,
      visibility = visibility,
      temp = temp,
      temp_min = temp_min,
      temp_max = temp_max,
      pressure = pressure,
      humidity = humidity,
      wind_speed = wind_speed,
      wind_deg = wind_deg,
      forecast_datetime
    )
  }
  
  # Return a data frame
  return(df)
}

cities <- c("Seoul", "Washington, D.C.", "Paris", "Suzhou")
cities_weather_df <- get_weather_forecaset_by_cities(cities)

write.csv(cities_weather_df, "raw_cities_weather_forecast.csv", row.names = FALSE)

                                 
# Download several datasets

# Download some general city information such as name and locations
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_worldcities.csv"
# download the file
download.file(url, destfile = "raw_worldcities.csv")

# Download a specific hourly Seoul bike sharing demand dataset
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"
# download the file
download.file(url, destfile = "raw_seoul_bike_sharing.csv")




