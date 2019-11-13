library(shiny)
library(RCurl)
library(stringr)
library(rjson)

parsedResult <- NULL

server <- function(input, output, session) {
  
  observeEvent(input$getForecast, {
    
    location.formatted <- NULL
    result <- NULL
    
    if (grepl(",", input$test)) {
      
      location.formatted <- str_split(input$test, ", ")
      result <- getURL(gsub(" ", "", paste("api.openweathermap.org/data/2.5/forecast?q=",location.formatted[[1]][1],",",location.formatted[[1]][2],"&APPID=36e53cd3a38129e9abdc5d13b71aa14a"), fixed = TRUE))
      
    } else {
      
      location.formatted <- input$test
      result <- getURL(gsub(" ", "%20", paste("api.openweathermap.org/data/2.5/forecast?q=",input$test,"&APPID=36e53cd3a38129e9abdc5d13b71aa14a"), fixed = TRUE))
      
    }
    
    parsedResult <<- as.data.frame(fromJSON(result))
    
    output$test <- renderText({
      
      paste("Weather in ", parsedResult$city.name, ", ", parsedResult$city.country)
      
    })
    
  })
  
}