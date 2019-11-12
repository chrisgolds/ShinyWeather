library(shiny)
library(RCurl)
library(stringr)

server <- function(input, output, session) {
  
  observeEvent(input$getForecast, {
    
    result <- NULL
    
    if (grepl(",", input$test)) {
      
      result <- str_split(input$test, ", ")
      print(getURL(paste("api.openweathermap.org/data/2.5/forecast?q=",result[[1]][1],",",result[[1]][2],"&APPID=36e53cd3a38129e9abdc5d13b71aa14a")))
      
    } else {
      
      result <- input$test
      
    }
    
    print(result)
    
  })
  
  observeEvent(input$close, {
    
    print("Should quit")
    stopApp()
    
  })
  
}