library(shiny)
library(RCurl)
library(stringr)
library(rjson)
library(DescTools)

parsedResult <- NULL

server <- function(input, output, session) {
  
  observeEvent(input$getForecast, {
    
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')
    
    UTC.time <- Sys.time()
    attr(UTC.time, "tzone") <- "UTC"
    
    location.formatted <- NULL
    result <- NULL
    
    if (grepl(",", input$test)) {
      
      location.formatted <- str_split(input$test, ", ")
      result <- getURL(gsub(" ", "", paste("api.openweathermap.org/data/2.5/forecast?q=",gsub(" ", "%20",location.formatted[[1]][1],fixed = TRUE),",",location.formatted[[1]][2],"&APPID=36e53cd3a38129e9abdc5d13b71aa14a", sep = ""), fixed = TRUE))
      
    } else {
      
      location.formatted <- input$test
      result <- getURL(gsub(" ", "%20", paste("api.openweathermap.org/data/2.5/forecast?q=",input$test,"&APPID=36e53cd3a38129e9abdc5d13b71aa14a", sep = ""), fixed = TRUE))
      
    }
    
    parsedResult <<- as.data.frame(fromJSON(result))
    print(parsedResult)
    
    output$header <- renderUI({
      
      tags$div(tags$h2(paste(parsedResult$city.name, ", ", parsedResult$city.country, sep = "")),
               tags$h4(paste("Sunrise: ", format(as.POSIXct(parsedResult$city.sunrise + parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%H:%M"))),
               tags$h4(paste("Sunset: ", format(as.POSIXct(parsedResult$city.sunset + + parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%H:%M")))
               )
      
    })
    
    output$timeOut <- renderUI({
      
      slidetitle <- paste("UTC Hour - (Current time is ", format(UTC.time, "%H:%M"), ")", sep = "")
      hour <- format(as.POSIXct(parsedResult$list.dt, tz = "UTC", origin="1970-01-01"), "%H")
      
      if (!(isTruthy(input$time))) {
        
        tags$div(sliderInput(inputId = "time", slidetitle, min = 00, max = 21, value = hour, step = 3)
                 
      )
        
      } else {
        
        tags$div(sliderInput(inputId = "time", slidetitle, min = 00, max = 21, value = input$time, step = 3))
        
      }
      
    })
    
    output$dayOut <- renderUI({
      
      formatted.dates <- vector(mode = "character")
      index <- 1
      
      formatted.dates[index] <- format(as.POSIXct(parsedResult$list.dt, tz = "UTC", origin="1970-01-01"), "%d/%m/%y")
      index <- index + 1
      
      i <- 1
      while (i < parsedResult$cnt) {
        
        working.date <- format(as.POSIXct(parsedResult[,paste('list.dt.',i,sep='')], tz = "UTC", origin="1970-01-01"), "%d/%m/%y")
        
        if (!(working.date %in% formatted.dates)) {
          
          formatted.dates[index] <- working.date
          index <- index + 1
          
        }
        
        i <- i + 1
        
      }
      
      selecttitle <- paste("UTC Day - (Current day is ", format(UTC.time, "%d/%m/%y"), ")", sep = "")
      tags$div(selectInput(inputId = "day", selecttitle, choices = formatted.dates))
      
    })
    
    output$weatherData <- renderUI({
      
      print("Function invoked")
      
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
      
      datePeriod <- input$day #Change to input$day soon
      
      JSONTime <- format(as.POSIXct(parsedResult$list.dt, tz = "UTC", origin="1970-01-01"), "%H:%M:%S")
      JSONDate <- format(as.POSIXct(parsedResult$list.dt, tz = "UTC", origin="1970-01-01"), "%d/%m/%y")
      
      if (identical(timePeriod, JSONTime) && identical(datePeriod, JSONDate)) {
        
        tags$div(tags$h3(format(as.POSIXct(parsedResult$list.dt + parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y %H:%M"),
                         " - Modified for UTC offsets"),
                 tags$img(src = paste("http://openweathermap.org/img/wn/", parsedResult$list.weather.icon,"@2x.png", sep = ""), align = "top"),
                 tags$div(style = "display: inline-block",
                          tags$h3(parsedResult$list.weather.main),
                          tags$h4(trunc(parsedResult$list.main.temp - 273.15), "\u2103")),
                 tags$div(style = "display: inline-block; padding: 10px",
                          tags$h4(
                            tags$span(class = "glyphicon glyphicon-arrow-down"),
                            "Min: ", trunc(parsedResult$list.main.temp_min - 273.15), "\u2103"),
                          tags$h4(
                            tags$span(class = "glyphicon glyphicon-arrow-up"),
                            "Max: ", trunc(parsedResult$list.main.temp_max - 273.15), "\u2103")),
                 tags$div(style = "display: inline-block; padding: 10px", 
                          tags$h4(
                            tags$span(class = "glyphicon glyphicon-tint"),
                            "Humidity: ", parsedResult$list.main.humidity, "%"),
                          tags$h4(
                            tags$span(class = "glyphicon glyphicon-cloud"),
                            "Cloud coverage: ", parsedResult$list.all, "%"))
        )
        
      } else {
        
        i <- 1
        while (i < parsedResult$cnt) {
          
          JSONTime <- format(as.POSIXct(parsedResult[,paste('list.dt.',i,sep='')], tz = "UTC", origin="1970-01-01"), "%H:%M:%S")
          JSONDate <- format(as.POSIXct(parsedResult[,paste('list.dt.',i,sep='')], tz = "UTC", origin="1970-01-01"), "%d/%m/%y")
          
          if (identical(timePeriod, JSONTime) && identical(datePeriod, JSONDate)) {
            
            return (tags$div(tags$h3(format(as.POSIXct(parsedResult[,paste('list.dt.',i,sep='')] + parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y %H:%M"),
                                     " - Modified for UTC offsets"),
                             tags$img(src = paste("http://openweathermap.org/img/wn/", parsedResult[,paste('list.weather.icon.',i,sep='')],"@2x.png", sep = ""), align = "top"),
                             tags$div(style = "display: inline-block",
                                      tags$h3(parsedResult[,paste('list.weather.main.',i,sep='')]),
                                      tags$h4(trunc(parsedResult[,paste('list.main.temp.',i,sep='')] - 273.15), "\u2103")),
                             tags$div(style = "display: inline-block; padding: 10px",
                                      tags$h4(
                                        tags$span(class = "glyphicon glyphicon-arrow-down"),
                                        "Min: ", trunc(parsedResult[,paste('list.main.temp_min.',i,sep='')] - 273.15), "\u2103"),
                                      tags$h4(
                                        tags$span(class = "glyphicon glyphicon-arrow-up"),
                                        "Max: ", trunc(parsedResult[,paste('list.main.temp_max.',i,sep='')] - 273.15), "\u2103")),
                             tags$div(style = "display: inline-block; padding: 10px",
                                      tags$h4(
                                        tags$span(class = "glyphicon glyphicon-tint"),
                                        "Humidity: ", parsedResult[,paste('list.main.humidity.',i,sep='')], "%"),
                                      tags$h4(
                                        tags$span(class = "glyphicon glyphicon-cloud"),
                                        "Cloud coverage: ", parsedResult[,paste('list.all.',i,sep='')], "%"))
            )
            )
            
          }
          
          i <- i + 1
          
        }
        
        return(tags$h3("Weather data not avaialble at this time"))
        
      }
      
    })
    
  })
  
}