library(shiny)
library(shinythemes)


ui <- fluidPage(
  theme= shinytheme("cerulean"),
  navbarPage(
    "My first app",
    tabPanel(
      "Navbar 1",
      sidebarPanel(
        tagList(tag$h3("Input: ")), 
        textInput("txt1","First Name:","Enter your First Name: "),
        textInput("txt2","Last Name:","Enter your Last Name: "),
      ),
      #sidePanelEnds
      mainPanel(
        h1("Header 1 "),
        
        h4("Output 1"),
        verbatimTextOutput("txtout"),
      ) , #mainPanelEnds
      ),
    tabPanel(
      "Navbar 2", "Empty for now"
    ),
    tabPanel(
      "Navbar 3", "Empty for now"
    )
  )#navbar ends
)#fluidpage ends

#defining server function

server <- function(input, output){
  
  output$txtout <- renderText({
    paste(input$txt1,input$txt2, sep=" ")
  })
} #server ends

shinyapp(ui=ui, server=server)
