# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
require(lubridate)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions  
source("model_prediction.R")

# Function to generate test weather data
test_weather_data_generation <- function() {
  city_weather_bike_df <- generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df) > 0)
  
  # Generate sequence of dates starting from today
  num_days <- 5
  cities <- unique(city_weather_bike_df$CITY_ASCII)
  city_weather_bike_df <- city_weather_bike_df %>%
    group_by(CITY_ASCII) %>%
    mutate(DATE = rep(seq.Date(Sys.Date(), by = "day", length.out = num_days), length.out = n())) %>%
    ungroup()
  
  city_weather_bike_df$FORECASTDATETIME <- as.POSIXct(city_weather_bike_df$FORECASTDATETIME, format = "%Y-%m-%d %H:%M:%S")
  
  print(city_weather_bike_df) # Debugging: Print the data frame
  return(city_weather_bike_df)
  
  col.names(city_weather_bike_df)
}
# Create a RShiny server
server<-shinyServer(function(input, output) {
  # Define a city list
  city_list <- c('New York', 'London', 'Seoul', 'Suzhou', 'Paris')
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  
  # Test generate_city_weather_bike_data() function
  city_weather_bike_df <- test_weather_data_generation()
  city_weather_bike_df$BIKE_PREDICTION <- as.numeric(city_weather_bike_df$BIKE_PREDICTION)
  
  # Create another data frame called `cities_max_bike` with each row containing city location info and max bike
  cities_max_bike <- city_weather_bike_df %>%
    filter(BIKE_PREDICTION == max(BIKE_PREDICTION))
  
    
  print(city_weather_bike_df)
  
  # Print for debugging purposes
  print(cities_max_bike)
  
  # Observe drop-down event
  observeEvent(input$city_dropdown, {
    selected_city <- input$city_dropdown
    
    if (selected_city == "All") {
      # Render a leaflet map with circle markers for all cities
      output$city_bike_map <- renderLeaflet({
        leaflet(data=city_weather_bike_df) %>%
          addTiles() %>%
          addPopups(data = city_weather_bike_df, 
                           lat = city_weather_bike_df$LAT,
                           lng = city_weather_bike_df$LNG, 
                           popup = ~paste(city_weather_bike_df$LABEL, "<br>",
                                          "Bike Prediction:", city_weather_bike_df$BIKE_PREDICTION))%>%
          addCircleMarkers(data= cities_max_bike,
                           lat= ~LAT,
                           lng= ~LNG,
                           color = color_levels(cities_max_bike$BIKE_PREDICTION_LEVEL))
        
      })
   
      #For all
      output$temp_line <- renderPlot(
        plot.new()
      )
      
      output$bike_line <- renderPlot(
        plot.new()
      )
    } else {
      # Filter data for the selected city
      filtered_data <- city_weather_bike_df %>%
        filter(CITY_ASCII == selected_city)
 
      
      # Render a leaflet map with one marker for the selected city
      output$city_bike_map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addPopups(data = filtered_data, lat = filtered_data$LAT, lng = filtered_data$LNG, 
                    popup = ~paste(filtered_data$LABEL, "<br>",
                                   "Bike Prediction:", filtered_data$BIKE_PREDICTION,"<br>",
                                   "Latitude: ", filtered_data$LAT,"<br>",
                                   "Longitude: ", filtered_data$LNG,"<br>",
                                   "Temperature: ", filtered_data$TEMPERATURE, "<br>",
                                    "Humidity: ", filtered_data$HUMIDITY))
      })
      filtered_data <- arrange(filtered_data, FORECASTDATETIME)
      
      #adding a temp line for selected city
      output$temp_line <- renderPlot({
        ggplot(filtered_data, aes(x = FORECASTDATETIME, y = TEMPERATURE)) +
          geom_line(color="blue") +
          geom_point(color="black") +
          geom_text(aes(label = round(TEMPERATURE, 1)), vjust = -0.5, hjust = 0.5) +
          labs(title = paste("5-Day Temperature Trend in", selected_city),
               x = "Date", y = "Temperature (°C)") +
          theme_minimal() 
      })
      
      #Adding a bike-sharing demand prediction line
      output$bike_line <- renderPlot({
        ggplot(filtered_data, aes(x = FORECASTDATETIME, y = BIKE_PREDICTION)) +
          geom_line(color="blue") +
          geom_point(color="black") +
          geom_text(aes(label = BIKE_PREDICTION), hjust = 0, vjust = -1) +
          labs(title = "Bike Sharing Prediction Trend", x = "Forecast Date & Time", y = "Bike Prediction")+
          theme_minimal()
      })
      
      observeEvent(input$plot_click, {
        
        clicked_point <- input$plot_click
        formatted_date <- format(as.Date(clicked_point$x), format = "%Y-%m-%d %H:%M")
        
        output$bike_date_output <- renderText(
          paste("Bike Prediction: ", clicked_point$y,
                "Forecast Date & Time: ",formatted_date)
        )
      })
      
      output$humidity_pred_chart <- renderPlot(
        ggplot(filtered_data, aes(x= HUMIDITY, y= BIKE_PREDICTION))+
          geom_point()+
          geom_smooth(method="lm", formula=y ~ poly(x, 4))+
          labs(title="Correlation of humidity and bike-sharing demand prediction")
      )
      
      
      
    }
  })
})

