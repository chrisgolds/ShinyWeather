library(shiny)
library(RCurl)
library(stringr)
library(rjson)
library(countrycode)
library(leaflet)
library(ggplot2)

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
      
      tags$div(tags$h2(paste(parsedResult$city.name, ", ", countrycode(parsedResult$city.country, origin = "iso2c", destination = "country.name"), sep = "")),
               tags$h4(paste("Sunrise: ", format(as.POSIXct(parsedResult$city.sunrise + parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%H:%M"))),
               tags$h4(paste("Sunset: ", format(as.POSIXct(parsedResult$city.sunset + parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%H:%M")))
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
      
      if (!(isTruthy(input$day))) {
        
        tags$div(selectInput(inputId = "day", selecttitle, choices = formatted.dates))
        
      } else {
        
        tags$div(selectInput(inputId = "day", selecttitle, choices = formatted.dates, selected = input$day))
        
      }
      
    })
    
    mapData <- reactiveValues(temp=NULL,main=NULL)
    
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
      
      datePeriod <- input$day
      
      UTC.offset <- 0
      UTC.offset.string <- NULL
      if (parsedResult$city.timezone >= 0) {
        
        UTC.offset <- parsedResult$city.timezone
        UTC.offset.string <- "+"
        
      } else {
        
        UTC.offset <- abs(parsedResult$city.timezone)
        UTC.offset.string <- "-"
        
      }
      
      UTC.offset <- format(as.POSIXct(UTC.offset, tz = "UTC", origin="1970-01-01"), "%H")
      
      JSONTime <- format(as.POSIXct(parsedResult$list.dt, tz = "UTC", origin="1970-01-01"), "%H:%M:%S")
      JSONDate <- format(as.POSIXct(parsedResult$list.dt, tz = "UTC", origin="1970-01-01"), "%d/%m/%y")
      
      if (identical(timePeriod, JSONTime) && identical(datePeriod, JSONDate)) {
        
        mapData$main <- parsedResult$list.weather.main
        mapData$temp <- trunc(parsedResult$list.main.temp - 273.15)
        
        tags$div(tags$h3(format(as.POSIXct(parsedResult$list.dt + parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y %H:%M"),
                         " - UTC", UTC.offset.string, UTC.offset),
                 tags$img(style = "background-color: #3c8dbc; border-radius: 100%", src = paste("http://openweathermap.org/img/wn/", parsedResult$list.weather.icon,"@2x.png", sep = ""), align = "top"),
                 tags$div(style = "display: inline-block",
                          tags$h3(id = "currentMain", parsedResult$list.weather.main),
                          tags$h3(id = "currentTemp", trunc(parsedResult$list.main.temp - 273.15), "\u2103")),
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
            
            mapData$main <- parsedResult[,paste('list.weather.main.',i,sep='')]
            mapData$temp <- trunc(parsedResult[,paste('list.main.temp.',i,sep='')] - 273.15)
            
            return(tags$div(tags$h3(format(as.POSIXct(parsedResult[,paste('list.dt.',i,sep='')] + parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y %H:%M"),
                                    " - UTC", UTC.offset.string, UTC.offset),
                             tags$img(style = "background-color: #3c8dbc; border-radius: 100%", src = paste("http://openweathermap.org/img/wn/", parsedResult[,paste('list.weather.icon.',i,sep='')],"@2x.png", sep = ""), align = "top"),
                             tags$div(style = "display: inline-block",
                                      tags$h3(id = "currentMain", parsedResult[,paste('list.weather.main.',i,sep='')]),
                                      tags$h3(id = "currentTemp", trunc(parsedResult[,paste('list.main.temp.',i,sep='')] - 273.15), "\u2103")),
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
    
    plotMap <- reactive({
      
      
      
    })
    
    output$map <- renderLeaflet({
      
      map <- leaflet() %>%
        addTiles() %>%
        setView(lat = parsedResult$city.coord.lat,
                lng = parsedResult$city.coord.lon,
                zoom = 11) %>%
        addMarkers(lat = parsedResult$city.coord.lat,
                   lng = parsedResult$city.coord.lon,
                   popup = paste(mapData$temp,"\u2103"," -",mapData$main))
      map
      
    })
    
    output$graphHead <- renderUI({
      
      tags$h3(paste("Graphs and charts for ", parsedResult$city.name, ", ", countrycode(parsedResult$city.country, origin = "iso2c", destination = "country.name"), sep = ""))
      
    })
    
    output$graphs <- renderPlot({
      
      tempTodayData <- vector(mode = "numeric")
      tempTodayLabels <- vector(mode = "character")
      JSONDate <- input$day
      i <- 1
      index <- 1
      while (index < parsedResult$cnt) {
        
        if (identical(JSONDate, format(as.POSIXct(parsedResult$list.dt, tz = "UTC", origin="1970-01-01"), "%d/%m/%y")) && length(tempTodayData) == 0 && length(tempTodayLabels) == 0) {
          
          tempTodayData[i] <- trunc(parsedResult$list.main.temp - 273.15)
          tempTodayLabels[i] <- format(as.POSIXct(parsedResult$list.dt + parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%H:%M")
          i <- i + 1
          index <- index - 1
          
        } else if (identical(JSONDate, format(as.POSIXct(parsedResult[,paste('list.dt.',index,sep='')], tz = "UTC", origin="1970-01-01"), "%d/%m/%y"))) {
          
          tempTodayData[i] <- trunc(parsedResult[,paste('list.main.temp.',index,sep='')] - 273.15)
          tempTodayLabels[i] <- format(as.POSIXct(parsedResult[,paste('list.dt.',index,sep='')] + parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%H:%M")
          i <- i + 1
          
        }
        
        index <- index + 1
        
      }
      
      tempTodayDF <- data.frame(time = tempTodayLabels, temp = tempTodayData)
      tempTodayDF$temp <- as.factor(tempTodayDF$temp)
      
      ggplot(data = tempTodayDF, aes(x = time, y = temp, group = "temp")) +
        geom_line(color = '#3c8dbc') + 
        geom_point(color = '#3c8dbc')
      
    })
    
    
  })
  
}