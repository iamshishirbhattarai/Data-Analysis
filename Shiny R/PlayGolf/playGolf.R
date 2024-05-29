library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)

# Read data
weather <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/weather-weka.csv"))

weather$play <- as.factor(weather$play)
weather$outlook <- factor(weather$outlook, levels = c("sunny", "overcast", "rainy"))

# Build model
model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE)

# Save the model to RDS file
saveRDS(model, "model.rds")

# USER INTERFACE
ui <- fluidPage(
  
  headerPanel("Let's Predict, Should you play golf or not?"),
  
  # Input Values
  sidebarPanel(
    h3("Input Parameters"),
    
    selectInput("outLook",
                label = "Outlook",
                choices = c(
                  "Sunny" = "sunny",
                  "Overcast" = "overcast",
                  "Rainy" = "rainy"
                ),
                selected = "sunny"),
    
    sliderInput("temperature",
                label = "Temperature",
                min = 64, max = 86,
                value = 69),
    
    sliderInput("humidity",
                label = "Humidity",
                min = 65, max = 96,
                value = 80),
    
    selectInput("windy",
                label = "Windy",
                choices = c(
                  "yes" = TRUE,
                  "no" = FALSE
                ),
                selected = TRUE),
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    h3("Status/Output "),
    verbatimTextOutput('contents'),
    tableOutput('tabledata')  # Output table
  )
)

# SERVER
server <- function(input, output) {
  
  # Input Data
  datasetInput <- reactive({
    
    # Create data frame with input values
    df <- data.frame(
      outlook = input$outLook,
      temperature = input$temperature,
      humidity = input$humidity,
      windy = as.logical(input$windy),
      stringsAsFactors = FALSE
    )
    
    # Ensure the factors are set correctly
    df$outlook <- factor(df$outlook, levels = c("sunny", "overcast", "rainy"))
    
    # Prediction
    prediction <- predict(model, df)
    prob <- predict(model, df, type = "prob")
    
    # Create a data frame for output
    output <- data.frame(
      Prediction = prediction,
      Yes = prob[1, "yes"],
      No = prob[1, "no"]
    )
    
    return(output)
  })
  
  # Status/Output
  output$contents <- renderPrint({
    if (input$submitbutton > 0) {
      isolate("Calculation complete")
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction result table
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) {
      isolate(datasetInput())
    }
  })
}

shinyApp(ui = ui, server = server)
