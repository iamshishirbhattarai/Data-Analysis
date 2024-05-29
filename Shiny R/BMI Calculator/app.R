library(shiny)
library(shinythemes)
library(markdown)

setwd("/Users/shishir__/Desktop/Data Science/CourseProject/Shiny R/Shiny R/BMI Calculator")

# User Interface
ui <- fluidPage(
  theme = shinytheme("slate"),
  navbarPage(
    "BMI CALCULATOR",
    tabPanel(
      "Home",
      # Input Values
      sidebarPanel(
        h3("Input Parameters"),
        sliderInput(
          "height",
          label = "Height (cm)",
          value = 175,
          min = 40,
          max = 250
        ),
        sliderInput(
          "weight",
          label = "Weight (kg)",
          value = 70,
          min = 20,
          max = 200
        ),
        actionButton(
          "submitbutton",
          "Submit",
          class = "btn btn-primary"
        )
      ),
      mainPanel(
        h3("Output"),
        verbatimTextOutput('contents'),
        tableOutput('tabledata')
      )
    ),
    tabPanel(
      "About",
      titlePanel("About"),
      div(includeMarkdown('about.md'), style = "text-align: justify;")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  datasetInput <- reactive({
    bmi <- input$weight / ((input$height / 100) ^ 2)
    bmi_df <- data.frame(BMI = bmi)
    return(bmi_df)
  })
  
  # Status/Output
  output$contents <- renderPrint({
    if (input$submitbutton > 0) {
      isolate("Calculation Complete !!")
    } else {
      "Server is ready for calculation !!!"
    }
  })
  
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) {
      isolate(datasetInput())
    }
  })
}

shinyApp(ui = ui, server = server)


