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
    
    output$header <- renderUI({
      
      tags$div(tags$h2(paste(parsedResult$city.name, ", ", parsedResult$city.country, sep = "")),
               tags$h4(paste("Sunrise: ", format(as.POSIXct(parsedResult$city.sunrise + parsedResult$city.timezone, origin="1970-01-01"), "%H:%M"))),
               tags$h4(paste("Sunset: ", format(as.POSIXct(parsedResult$city.sunset + + parsedResult$city.timezone, origin="1970-01-01"), "%H:%M")))
               )
      
    })
    
    output$timeOut <- renderUI({
      
      slidetitle <- paste("UTC Hour - (Current time is ", format(as.POSIXct(Sys.time()), "%H:%M"), ")", sep = "")
      hour <- format(as.POSIXct(parsedResult$list.dt, origin="1970-01-01"), "%H")
      
      if (UI.output.flag == FALSE) {
        
        UI.output.flag <<- TRUE
        tags$div(sliderInput(inputId = "time", slidetitle, min = 00, max = 21, value = hour, step = 3)
                 
      )
        
      } else {
        
        tags$div(sliderInput(inputId = "time", slidetitle, min = 00, max = 21, value = input$time, step = 3))
        
      }
      
    })
    
    plotWeatherData <- eventReactive(input$time, {
      
      timePeriod <- NULL
      
      if (input$time == 0) {
        
        timePeriod <- paste(input$time, "0:00:00", sep = "")
        
      } else if (input$time == 3 || input$time == 6 || input$time == 9) {
        
        timePeriod <- paste("0", input$time, ":00:00", sep = "")
      
      } else if (input$time == 24) {
        
        timePeriod <- "00:00:00"
        
      } else {
        
        timePeriod <- paste(input$time, ":00:00", sep = "")
        
      }
      
      datePeriod <- format(Sys.Date(), "%d/%m/%y") #Change to input$day soon
      print(datePeriod)
      
      JSONTime <- format(as.POSIXct(parsedResult$list.dt, origin="1970-01-01"), "%H:%M:%S")
      JSONDate <- format(as.POSIXct(parsedResult$list.dt, origin="1970-01-01"), "%d/%m/%y")
      
      if (identical(timePeriod, JSONTime) && identical(datePeriod, JSONDate)) {
        
        tags$div(tags$h3(format(as.POSIXct(parsedResult$list.dt + parsedResult$city.timezone, origin="1970-01-01"), "%d/%m/%y %H:%M"),
                         " - Modified for UTC offsets"),
                 tags$img(src = paste("http://openweathermap.org/img/wn/", parsedResult$list.weather.icon,"@2x.png", sep = "")),
                 tags$div(style = "display: inline-block",
                          tags$h3(parsedResult$list.weather.main),
                          tags$h4(paste(trunc(parsedResult$list.main.temp - 273.15), "\u2103", sep = ""))
                 )
        )
        
      } else {
        
        i <- 1
        while (i < parsedResult$cnt) {
          
          JSONTime <- format(as.POSIXct(parsedResult[,paste('list.dt.',i,sep='')], origin="1970-01-01"), "%H:%M:%S")
          JSONDate <- format(as.POSIXct(parsedResult[,paste('list.dt.',i,sep='')], origin="1970-01-01"), "%d/%m/%y")
          
          if (identical(timePeriod, JSONTime) && identical(datePeriod, JSONDate)) {
            
            return (tags$div(tags$h3(format(as.POSIXct(parsedResult[,paste('list.dt.',i,sep='')] + parsedResult$city.timezone, origin="1970-01-01"), "%d/%m/%y %H:%M"),
                                     " - Modified for UTC offsets"),
                             tags$img(src = paste("http://openweathermap.org/img/wn/", parsedResult[,paste('list.weather.icon.',i,sep='')],"@2x.png", sep = "")),
                             tags$div(style = "display: inline-block",
                                      tags$h3(parsedResult[,paste('list.weather.main.',i,sep='')]),
                                      tags$h4(paste(trunc(parsedResult[,paste('list.main.temp.',i,sep='')] - 273.15), "\u2103", sep = ""))
                     )
            ))
            
          }
          
          i <- i + 1
          
        }
        
      }
      
    })
    
    output$weatherData <- renderUI({
      
      plotWeatherData()
      
    })
    
  })
  
}