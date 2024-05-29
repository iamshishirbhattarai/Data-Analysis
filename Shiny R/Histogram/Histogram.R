library(shiny)
data("airquality")

ui <- fluidPage(
  
  #Add title
  titlePanel("Ozone Level !!!"),
  
  #sibebar layout
  sidebarLayout(
    
    sidebarPanel (
      
      sliderInput(inputId = "bins",
                  label="Number of Bins",
                  min= 0,
                  max=50,
                  value=30,
                  step=2
                  ),
    
      #Mainpanel to display output
    ) ,
    
    mainPanel(
        
         plotOutput(outputId= "distPlot")
      ),
  ),
 ),
  #define server 

server <- function(input, output) { 
    
    output$distPlot <- renderPlot({
      
      x <- airquality$Ozone
      x<- na.omit(x)
      bins<- seq(min(x),max(x), length.out= input$bins+1)
      
      
      hist(x, breaks=bins, col="blue", border="black",
           xlab="Ozone Level",
           main="Histogram of Ozone Level")
    }
      
    )
  }


shinyApp(ui=ui, server=server)