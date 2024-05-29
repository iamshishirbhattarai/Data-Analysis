library(shiny)
library(data.table)
library(randomForest)

setwd("/Users/shishir__/Desktop/Data Science/CourseProject/Shiny R/Shiny R/Iris Predictor")
model <- readRDS("model.RDS")

#User Interface

ui <- pageWithSidebar(
  
  #page header
  headerPanel('Iris Predictor'),
  
  #Input Values
  sidebarPanel(
    h3("Input Parameters"),
    numericInput(
      "Sepal.Length",
      label="Sepal Length",
      value=5
    ),
    
    numericInput(
      "Sepal.Width",
      label="Sepal Width",
      value=3.6
    ),
    
    numericInput(
      "Petal.Length",
      label="Petal Length",
      value=1.4
    ),
    
    numericInput(
      "Petal.Width",
      label="Petal Width",
      value=0.2
      
    ),
    
    actionButton("submitbutton",
                 "submit",
                 class="btn btn-primary"
                 )
  ),
  
  mainPanel(
    h3("Status/Output") ,
    verbatimTextOutput('contents'),
    tableOutput('tabledata')
  )
)


server <- function(input, output, session) {
  
  #Input data
  datasetInput <- reactive({
     
    df <- data.frame(
       Sepal.Length= input$Sepal.Length,
       Sepal.Width = input$Sepal.Width,
       Petal.Length= input$Petal.Length,
       Petal.Width= input$Petal.Width
    )
    
    prediction <- predict(model,df)
    prob <- predict(model,df,type="prob")
    
    output <- data.frame(
      Prediction= prediction,
      setosa= prob[,"setosa"],
      versicolor= prob[,"versicolor"],
      virginica= prob[,"virginica"]
    )
    
    return(output)
  })
  
  output$contents <- renderPrint(
    if(input$submitbutton > 0){
      isolate("Calculation Complete !!")
    }
    else{
      return("Server is ready for calculation")
    }
  )
  
  output$tabledata <- renderTable(
    if(input$submitbutton >0){
      isolate(datasetInput())
    }
  )
}

shinyApp(ui=ui, server=server)