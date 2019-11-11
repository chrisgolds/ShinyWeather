library(shiny)
library(RCurl)

server <- function(input, output) {
  
  observeEvent(input$getForecast, {
    
    if (input$test == "" || is.null(input$test)) {
      
      #input$test <- HTML()
      
    }
    
    print(input$test)
    print(getURL("api.openweathermap.org/data/2.5/forecast?q=Brentford,uk&APPID=36e53cd3a38129e9abdc5d13b71aa14a"))
    
  })
  
  observeEvent(input$close, {
    
    print("Should quit")
    stopApp()
    
  })
  
}