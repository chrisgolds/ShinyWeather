library(shiny)
library(RCurl)
library(stringr)
library(rjson)
library(DescTools)

UI.output.flag <- FALSE

parsedResult <- NULL

server <- function(input, output, session) {
  
  observeEvent(input$getForecast, {
    
    location.formatted <- NULL
    result <- NULL
    
    if (grepl(",", input$test)) {
      
      location.formatted <- str_split(input$test, ", ")
      result <- getURL(gsub(" ", "", paste("api.openweathermap.org/data/2.5/forecast?q=",location.formatted[[1]][1],",",location.formatted[[1]][2],"&APPID=36e53cd3a38129e9abdc5d13b71aa14a", sep = ""), fixed = TRUE))
      
    } else {
      
      location.formatted <- input$test
      result <- getURL(gsub(" ", "%20", paste("api.openweathermap.org/data/2.5/forecast?q=",input$test,"&APPID=36e53cd3a38129e9abdc5d13b71aa14a", sep = ""), fixed = TRUE))
      
    }
    
    parsedResult <<- as.data.frame(fromJSON(result))
    print(parsedResult)
    
    output$test <- renderText({
      
      paste("Weather in ", parsedResult$city.name, ", ", parsedResult$city.country, sep = "")
      
    })
    
  })
  
  plotSlider <- eventReactive(input$getForecast, {
    
    if (UI.output.flag == FALSE) {
      
      UI.output.flag <<- TRUE
      date.formatted <- str_split(Sys.time(), " ")
      hour <- RoundTo(as.numeric(format(strptime(date.formatted[[1]][2], "%H:%M:%S"), '%H')), 3)
      
      tags$div(actionButton(inputId = "day", label = "Today!"),
               sliderInput(inputId = "time", "Hour", min = 00, max = 21, value = hour, step = 3)
      )
    
    } else {
      
      tags$div(actionButton(inputId = "day", label = "Today!"),
               sliderInput(inputId = "time", "Hour", min = 00, max = 21, value = input$time, step = 3)
      )
      
    }
  
  })
  
  
  output$timeOut <- renderUI({
    
    plotSlider()
    
  })
  
  
  plotWeatherData <- eventReactive(input$time, {
    
    timePeriod <- NULL
    
    if (input$time == 0) {
      
      timePeriod <- paste(input$time, "0:00:00", sep = "")
      
    } else if (input$time == 24) {
      
      timePeriod <- "00:00:00"
    
    } else {
      
      timePeriod <- paste(input$time, ":00:00", sep = "")
      
    }
    
    print(timePeriod)
    
    
    firstDateAndTime <- str_split(parsedResult[,'list.dt_txt'], " ")
    
    p.time <- format(Sys.Date(), "%Y-%m-%d")
    p.time <- paste(p.time, timePeriod)
    
    if (identical(timePeriod, firstDateAndTime[[1]][2])) {
      
      print('first')
      #First object can be used
      tags$div(tags$img(src = paste("http://openweathermap.org/img/wn/", parsedResult$list.weather.icon,"@2x.png", sep = "")),
               tags$h3(parsedResult$list.weather.main),
               tags$h4(paste(trunc(parsedResult$list.main.temp - 273.15), "\u2103", sep = ""))
      )
      
    } else {
      
      print('second')
      #Another object has to be used
      
    }
    
  })
  
  output$weatherData <- renderUI({
    
    plotWeatherData()
    
  })
  
}