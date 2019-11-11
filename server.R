library(shiny)

server <- function(input, output) {
  
  observeEvent(input$close, {
    
    print("Should quit")
    stopApp()
    
  })
  
  output$test <- renderText({toupper(input$test)})
  
}