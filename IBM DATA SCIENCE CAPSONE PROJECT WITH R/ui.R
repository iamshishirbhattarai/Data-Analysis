# Load required libraries
require(leaflet)
require(shiny)

# Create a RShiny UI
ui<-shinyUI(
  fluidPage(
    padding = 5,
    titlePanel("Bike-sharing demand prediction app"),
    # Create a side-bar layout
    sidebarLayout(
      # Create a main panel to show cities on a leaflet map
      mainPanel(
        # leaflet output with id = 'city_bike_map', height = 1000
        leafletOutput("city_bike_map", height =700)
      ),
      # Create a side bar to show detailed plots for a city
      sidebarPanel(
        # select drop down list to select city
        selectInput(inputId = "city_dropdown",
                    label = "Choose a city",
                    choices = c("All", "Seoul", "Suzhou", "London", "New York", "Paris"),
                    selected = "All"
        ),
        
        #Adding a temperature line
        plotOutput("temp_line",
                   height= 200,
                   width= "100%"),
        
        #Adding a bike-sharing demand prediction line
        plotOutput("bike_line",
                   height= 200,
                   width="100%",
                   click="plot_click"),
        
        verbatimTextOutput("bike_date_output"),
        
        plotOutput("humidity_pred_chart", height=200,
                   width="100%")
      )
 
     
          
      )
      
    )
  )
