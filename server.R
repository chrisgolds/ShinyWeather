#Import all required libraries here
library(shiny)
library(shinyjs)
library(RCurl)
library(stringr)
library(rjson)
library(countrycode)
library(leaflet)
library(ggplot2)

#JSON_data is a reactive object - the parsedResult property will hold weather information from api.openweathermap.org
#which is read in as JSON string and parsed as a data frame
#UTC.time will hold UNIX timestamp of JSON_data relative to UTC adjustments
JSON_data <- reactiveValues(parsedResult = NULL)
UTC.time <- NULL

#Function implementation for invoking weathermap API calls with appropriate endpoints
#First  parameter is the location of the weatehr data in string format - this can be:
# - City/town name
# - City/town with country code (separated by comma)
# - Coordinates
#Second parameter acts as Boolean flag - if true, will update JSON_data with weather data from location coordinates specified in pinned map point
getData <- function(str_loc, is_update_loc) {
  
  UTC.time <<- Sys.time()
  attr(UTC.time, "tzone") <- "UTC"
  
  location.formatted <- NULL
  result <- NULL
  
  if (is_update_loc) {
    
    location.formatted <- str_split(str_loc, ",")
    result <- getURL(paste("api.openweathermap.org/data/2.5/forecast?lat=",location.formatted[[1]][1],"&lon=",location.formatted[[1]][2],"&APPID=36e53cd3a38129e9abdc5d13b71aa14a", sep = ""))
    
  } else {
    
    if (grepl(",", str_loc)) {
      
      location.formatted <- str_split(str_loc, ", ")
      result <- getURL(paste("api.openweathermap.org/data/2.5/forecast?q=",gsub(" ", "%20",location.formatted[[1]][1],fixed = TRUE),",",location.formatted[[1]][2],"&APPID=36e53cd3a38129e9abdc5d13b71aa14a", sep = ""))
      
    } else {
      
      location.formatted <- str_loc
      result <- getURL(gsub(" ", "%20", paste("api.openweathermap.org/data/2.5/forecast?q=",str_loc,"&APPID=36e53cd3a38129e9abdc5d13b71aa14a", sep = ""), fixed = TRUE))
      
    }
    
  }
  
  #JSON string containing weather data parsed into JSON format first, then into a data frame for ease of use in R
  JSON_data$parsedResult <<- as.data.frame(fromJSON(result))
  
}

#Implementation for server object
server <- function(input, output, session) {
  
  observeEvent(input$getForecast, {
    
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')
    
    getData(input$test, FALSE)
    
    output$header <- renderUI({
      
      maxAndMin <- plotFiveDayAvgTemp()
      print(maxAndMin)
      
      max <- maxAndMin$temp[1]
      maxDate <- maxAndMin$day[1]
      index <- 2
      
      while (index <= nrow(maxAndMin)) {
        
        if (maxAndMin$temp[index] > max) {
          
          max <- maxAndMin$temp[index]
          maxDate <- maxAndMin$day[index]
          
        }
        
        index <- index + 1
        
      }
      
      min <- max
      minDate <- maxDate
      index <- 1
      
      while (index <= nrow(maxAndMin)) {
        
        if (maxAndMin$temp[index] < min) {
          
          min <- maxAndMin$temp[index]
          minDate <- maxAndMin$day[index]
          
        }
        
        index <- index + 1
        
      }
      
      tags$div(tags$h2(paste(JSON_data$parsedResult$city.name, ", ", countrycode(JSON_data$parsedResult$city.country, origin = "iso2c", destination = "country.name"), sep = "")),
               tags$div(class = "row",
                 tags$div(class = "col-sm-6",
                   tags$h4(paste("Sunrise:", format(as.POSIXct(JSON_data$parsedResult$city.sunrise + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%H:%M"))),
                   tags$h4(paste("Sunset:", format(as.POSIXct(JSON_data$parsedResult$city.sunset + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%H:%M")))
                 ),
                 tags$div(class = "col-sm-6",
                   tags$h4(paste("Warmest day:", maxDate, "(", max, "\u2103", ")")),
                   tags$h4(paste("Coldest day:", minDate, "(", min, "\u2103", ")"))
                 )
               )
              )
      
    })
    
    output$timeOut <- renderUI({
      
      slidetitle <- paste("UTC Hour - (Current time is ", format(UTC.time, "%H:%M"), ")", sep = "")
      hour <- format(as.POSIXct(JSON_data$parsedResult$list.dt, tz = "UTC", origin="1970-01-01"), "%H")
      
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
      
      formatted.dates[index] <- format(as.POSIXct(JSON_data$parsedResult$list.dt, tz = "UTC", origin="1970-01-01"), "%d/%m/%y")
      index <- index + 1
      
      i <- 1
      while (i < JSON_data$parsedResult$cnt) {
        
        working.date <- format(as.POSIXct(JSON_data$parsedResult[,paste('list.dt.',i,sep='')], tz = "UTC", origin="1970-01-01"), "%d/%m/%y")
        
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
    
    mapData <- reactiveValues(temp=NULL,main=NULL,icon=NULL)
    
    output$weatherData <- renderUI({
      
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
      if (JSON_data$parsedResult$city.timezone >= 0) {
        
        UTC.offset <- JSON_data$parsedResult$city.timezone
        UTC.offset.string <- "+"
        
      } else {
        
        UTC.offset <- abs(JSON_data$parsedResult$city.timezone)
        UTC.offset.string <- "-"
        
      }
      
      UTC.offset <- format(as.POSIXct(UTC.offset, tz = "UTC", origin="1970-01-01"), "%H")
      
      JSONTime <- format(as.POSIXct(JSON_data$parsedResult$list.dt, tz = "UTC", origin="1970-01-01"), "%H:%M:%S")
      JSONDate <- format(as.POSIXct(JSON_data$parsedResult$list.dt, tz = "UTC", origin="1970-01-01"), "%d/%m/%y")
      
      if (identical(timePeriod, JSONTime) && identical(datePeriod, JSONDate)) {
        
        mapData$main <- JSON_data$parsedResult$list.weather.main
        mapData$temp <- trunc(JSON_data$parsedResult$list.main.temp - 273.15)
        mapData$icon <- paste("http://openweathermap.org/img/wn/", JSON_data$parsedResult$list.weather.icon,"@2x.png", sep = "")
        
        tags$div(tags$h3(format(as.POSIXct(JSON_data$parsedResult$list.dt + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y %H:%M"),
                         " - UTC", UTC.offset.string, UTC.offset),
                 tags$img(style = "background-color: #3c8dbc; border-radius: 100%", src = paste("http://openweathermap.org/img/wn/", JSON_data$parsedResult$list.weather.icon,"@2x.png", sep = ""), align = "top"),
                 tags$div(style = "display: inline-block",
                          tags$h3(id = "currentMain", JSON_data$parsedResult$list.weather.main),
                          tags$h3(id = "currentTemp", trunc(JSON_data$parsedResult$list.main.temp - 273.15), "\u2103")),
                 tags$div(style = "display: inline-block; padding: 10px",
                          tags$h4(
                            tags$span(class = "glyphicon glyphicon-arrow-down"),
                            "Min: ", trunc(JSON_data$parsedResult$list.main.temp_min - 273.15), "\u2103"),
                          tags$h4(
                            tags$span(class = "glyphicon glyphicon-arrow-up"),
                            "Max: ", trunc(JSON_data$parsedResult$list.main.temp_max - 273.15), "\u2103")),
                 tags$div(style = "display: inline-block; padding: 10px", 
                          tags$h4(
                            tags$span(class = "glyphicon glyphicon-tint"),
                            "Humidity: ", JSON_data$parsedResult$list.main.humidity, "%"),
                          tags$h4(
                            tags$span(class = "glyphicon glyphicon-cloud"),
                            "Cloud coverage: ", JSON_data$parsedResult$list.all, "%"))
        )
        
      } else {
        
        i <- 1
        while (i < JSON_data$parsedResult$cnt) {
          
          JSONTime <- format(as.POSIXct(JSON_data$parsedResult[,paste('list.dt.',i,sep='')], tz = "UTC", origin="1970-01-01"), "%H:%M:%S")
          JSONDate <- format(as.POSIXct(JSON_data$parsedResult[,paste('list.dt.',i,sep='')], tz = "UTC", origin="1970-01-01"), "%d/%m/%y")
          
          if (identical(timePeriod, JSONTime) && identical(datePeriod, JSONDate)) {
            
            mapData$main <- JSON_data$parsedResult[,paste('list.weather.main.',i,sep='')]
            mapData$temp <- trunc(JSON_data$parsedResult[,paste('list.main.temp.',i,sep='')] - 273.15)
            mapData$icon <- paste("http://openweathermap.org/img/wn/", JSON_data$parsedResult[,paste('list.weather.icon.',i,sep='')],"@2x.png", sep = "")
            
            return(tags$div(tags$h3(format(as.POSIXct(JSON_data$parsedResult[,paste('list.dt.',i,sep='')] + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y %H:%M"),
                                    " - UTC", UTC.offset.string, UTC.offset),
                             tags$img(style = "background-color: #3c8dbc; border-radius: 100%", src = paste("http://openweathermap.org/img/wn/", JSON_data$parsedResult[,paste('list.weather.icon.',i,sep='')],"@2x.png", sep = ""), align = "top"),
                             tags$div(style = "display: inline-block",
                                      tags$h3(id = "currentMain", JSON_data$parsedResult[,paste('list.weather.main.',i,sep='')]),
                                      tags$h3(id = "currentTemp", trunc(JSON_data$parsedResult[,paste('list.main.temp.',i,sep='')] - 273.15), "\u2103")),
                             tags$div(style = "display: inline-block; padding: 10px",
                                      tags$h4(
                                        tags$span(class = "glyphicon glyphicon-arrow-down"),
                                        "Min: ", trunc(JSON_data$parsedResult[,paste('list.main.temp_min.',i,sep='')] - 273.15), "\u2103"),
                                      tags$h4(
                                        tags$span(class = "glyphicon glyphicon-arrow-up"),
                                        "Max: ", trunc(JSON_data$parsedResult[,paste('list.main.temp_max.',i,sep='')] - 273.15), "\u2103")),
                             tags$div(style = "display: inline-block; padding: 10px",
                                      tags$h4(
                                        tags$span(class = "glyphicon glyphicon-tint"),
                                        "Humidity: ", JSON_data$parsedResult[,paste('list.main.humidity.',i,sep='')], "%"),
                                      tags$h4(
                                        tags$span(class = "glyphicon glyphicon-cloud"),
                                        "Cloud coverage: ", JSON_data$parsedResult[,paste('list.all.',i,sep='')], "%"))
            )
            )
            
          }
          
          i <- i + 1
          
        }
        
        return(tags$h3("Weather data not avaialble at this time"))
        
      }
      
    })
    
    output$map <- renderLeaflet({
      
      mapIcon <- makeIcon(
        iconUrl = mapData$icon,
        iconAnchorX = 50,
        iconAnchorY = 50)
      
      map <- leaflet() %>%
        addTiles() %>%
        setView(lat = JSON_data$parsedResult$city.coord.lat,
                lng = JSON_data$parsedResult$city.coord.lon,
                zoom = 11) %>%
        addMarkers(lat = JSON_data$parsedResult$city.coord.lat,
                   lng = JSON_data$parsedResult$city.coord.lon,
                   popup = paste(mapData$temp,"\u2103"," -",mapData$main),
                   icon = mapIcon)
      
      output$pinnedWeather <- renderUI({
        
        actionButton(inputId = "pinnedWeatherHandle", class = "btn btn-success", label = "Update location", width = "100%", disabled = TRUE)
        
      })
      
      map
      
    })
    
    observeEvent(input$map_click, {
      
      leafletProxy("map") %>%
        removeMarker(layerId = "current") %>%
        addMarkers(lat = input$map_click$lat, lng = input$map_click$lng, layerId = "current")
      
      shinyjs::enable("pinnedWeatherHandle")
      
    })
    
    observeEvent(input$pinnedWeatherHandle, {
      
      progress <- Progress$new(session, min=1, max=15)
      on.exit(progress$close())
      
      progress$set(message = 'Calculation in progress',
                   detail = 'This may take a while...')
      
      getData(paste(input$map_click$lat, input$map_click$lng, sep = ","), TRUE)
      
    })
    
    output$graphHead <- renderUI({
      
      tags$h3(paste("Graphs and charts for ", JSON_data$parsedResult$city.name, ", ", countrycode(JSON_data$parsedResult$city.country, origin = "iso2c", destination = "country.name"), sep = ""))
      
    })
    
    plotDayTemp <- function(JSONDate) {
      
      tempTodayData <- vector(mode = "numeric")
      tempTodayLabels <- vector(mode = "character")
      i <- 1
      index <- 1
      while (index < JSON_data$parsedResult$cnt) {
        
        if (identical(JSONDate, format(as.POSIXct(JSON_data$parsedResult$list.dt + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y")) && length(tempTodayData) == 0 && length(tempTodayLabels) == 0) {
          
          tempTodayData[i] <- trunc(JSON_data$parsedResult$list.main.temp - 273.15)
          tempTodayLabels[i] <- format(as.POSIXct(JSON_data$parsedResult$list.dt + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%H:%M")
          i <- i + 1
          index <- index - 1
          
        } else if (identical(JSONDate, format(as.POSIXct(JSON_data$parsedResult[,paste('list.dt.',index,sep='')] + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y"))) {
          
          tempTodayData[i] <- trunc(JSON_data$parsedResult[,paste('list.main.temp.',index,sep='')] - 273.15)
          tempTodayLabels[i] <- format(as.POSIXct(JSON_data$parsedResult[,paste('list.dt.',index,sep='')] + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%H:%M")
          i <- i + 1
          
        }
        
        index <- index + 1
        
      }
      
      tempTodayDF <- data.frame(time = tempTodayLabels, temp = tempTodayData)
      return (tempTodayDF)
      
    }
    
    plotFiveDayAvgTemp <- function() {
      
      avgs <- vector(mode = "numeric")
      avgsLabels <- vector(mode = "character")
      avgsIndex <- 1
      
      JSONDate <- format(as.POSIXct(JSON_data$parsedResult$list.dt + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y")
      avgsLabels[avgsIndex] <- JSONDate
      workingAvg <- JSON_data$parsedResult$list.main.temp - 273.15
      avgCount <- 1
      i <- 1
      
      while (i < JSON_data$parsedResult$cnt) {
        
        if (identical(JSONDate, format(as.POSIXct(JSON_data$parsedResult[,paste('list.dt.',i,sep='')] + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y"))) {
          
          workingAvg <- workingAvg + as.numeric(trunc(JSON_data$parsedResult[,paste('list.main.temp.',i,sep='')] - 273.15))
          avgCount <- avgCount + 1
          
        } else {
          
          avgs[avgsIndex] <- trunc(workingAvg / avgCount)
          avgsIndex <- avgsIndex + 1
          
          JSONDate <- format(as.POSIXct(JSON_data$parsedResult[,paste('list.dt.',i,sep='')] + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y")
          avgsLabels[avgsIndex] <- JSONDate
          workingAvg <- 0
          avgCount <- 0
          
          i <- i - 1
          
        }
        
        if (i + 1 == JSON_data$parsedResult$cnt) {
          
          avgs[avgsIndex] <- trunc(workingAvg / avgCount)
          
        }
        
        i <- i + 1
        
      }
      
      avgTempFiveDaysDF <- data.frame(day = avgsLabels, temp = avgs)
      return(avgTempFiveDaysDF)
      
    }
    
    plotPercentages <- function(JSONDate) {
      
      humidityOrCloud <- vector(mode = "character")
      percentage <- vector(mode = "numeric")
      percentageTodayLabels <- vector(mode = "character")
      i <- 1
      index <- 1
      while (index < JSON_data$parsedResult$cnt) {
        
        if (identical(JSONDate, format(as.POSIXct(JSON_data$parsedResult$list.dt + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y")) &&
            length(humidityOrCloud) == 0 &&
            length(percentage) == 0 &&
            length(percentageTodayLabels) == 0) {
          
          humidityOrCloud[i] <- "Humidity"
          percentage[i] <- JSON_data$parsedResult$list.main.humidity
          percentageTodayLabels[i] <- format(as.POSIXct(JSON_data$parsedResult$list.dt + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%H:%M")
          i <- i + 1
          
          humidityOrCloud[i] <- "Cloud Coverage"
          percentage[i] <- JSON_data$parsedResult$list.all
          percentageTodayLabels[i] <- format(as.POSIXct(JSON_data$parsedResult$list.dt + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%H:%M")
          i <- i + 1
          
          index <- index - 1
          
        } else if (identical(JSONDate, format(as.POSIXct(JSON_data$parsedResult[,paste('list.dt.',index,sep='')] + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y"))) {
          
          humidityOrCloud[i] <- "Humidity"
          percentage[i] <- JSON_data$parsedResult[,paste('list.main.humidity.',index,sep='')]
          percentageTodayLabels[i] <- format(as.POSIXct(JSON_data$parsedResult[,paste('list.dt.',index,sep='')] + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%H:%M")
          i <- i + 1
          
          humidityOrCloud[i] <- "Cloud Coverage"
          percentage[i] <- JSON_data$parsedResult[,paste('list.all.',index,sep='')]
          percentageTodayLabels[i] <- format(as.POSIXct(JSON_data$parsedResult[,paste('list.dt.',index,sep='')] + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%H:%M")
          i <- i + 1
          
        }
        
        index <- index + 1
        
      }
      
      percentageTodayDF <- data.frame(time = percentageTodayLabels, legend = humidityOrCloud, perc = percentage)
      return(percentageTodayDF)
      
    }
    
    plotAvgPercentages <- function() {
      
      avgType <- vector(mode = "character")
      avgPerc <- vector(mode = "numeric")
      avgsLabels <- vector(mode = "character")
      avgsIndex <- 1
      
      JSONDate <- format(as.POSIXct(JSON_data$parsedResult$list.dt + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y")
      avgsLabels[avgsIndex] <- JSONDate
      workingAvg <- JSON_data$parsedResult$list.main.temp - 273.15
      avgCount <- 1
      i <- 1
      
      while (i < JSON_data$parsedResult$cnt) {
        
        if (identical(JSONDate, format(as.POSIXct(JSON_data$parsedResult[,paste('list.dt.',i,sep='')] + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y"))) {
          
          workingAvg <- workingAvg + as.numeric(trunc(JSON_data$parsedResult[,paste('list.main.temp.',i,sep='')] - 273.15))
          avgCount <- avgCount + 1
          
        } else {
          
          avgs[avgsIndex] <- trunc(workingAvg / avgCount)
          avgsIndex <- avgsIndex + 1
          
          JSONDate <- format(as.POSIXct(JSON_data$parsedResult[,paste('list.dt.',i,sep='')] + JSON_data$parsedResult$city.timezone, tz = "UTC", origin="1970-01-01"), "%d/%m/%y")
          avgsLabels[avgsIndex] <- JSONDate
          workingAvg <- 0
          avgCount <- 0
          
          i <- i - 1
          
        }
        
        if (i + 1 == JSON_data$parsedResult$cnt) {
          
          avgs[avgsIndex] <- trunc(workingAvg / avgCount)
          
        }
        
        i <- i + 1
        
      }
      
      avgTempFiveDaysDF <- data.frame(day = avgsLabels, temp = avgs)
      return(avgTempFiveDaysDF)
      
    }
    
    output$plotDayTemp <- renderPlot({
      
      JSONDate <- input$day
      tempTodayDF <- plotDayTemp(JSONDate)
      
      req(tempTodayDF$time, tempTodayDF$temp)
      
      line_graph <- ggplot(data = tempTodayDF, aes(x = time, y = temp, group = "1")) +
        geom_line(stat = "identity", color = '#3c8dbc', se=FALSE) + 
        geom_point(color = '#3c8dbc', size = 3) +
        labs(title = paste("Temperatures in ",JSON_data$parsedResult$city.name," - ",JSONDate, sep = ""), y = "Temperature \u2103", x = "Time") +
        theme(
          
          panel.background =  element_rect(fill = "white"),
          panel.border = element_rect(fill = "transparent", colour = "black"),
          plot.background = element_rect(fill = "#ecf0f5", colour = "#ecf0f5"),
          text = element_text(family = "Helvetica")
          
        ) +
        scale_x_discrete(limits = tempTodayDF$time)
      
      line_graph
      
    })
    
    output$plotFiveDayAvg <- renderPlot({
      
      avgTempFiveDaysDF <- plotFiveDayAvgTemp()
      
      ggplot(data = avgTempFiveDaysDF, aes(x = day, y = temp, group = "1")) +
        geom_smooth(method=lm, se=FALSE, color = 'red') + 
        geom_point(color = 'red', size = 3) +
        labs(title = paste("Average temperature in ",JSON_data$parsedResult$city.name," (",avgTempFiveDaysDF$day[1]," - ",avgTempFiveDaysDF$day[nrow(avgTempFiveDaysDF)],")", sep = ""), y = "Temperature \u2103", x = "Day") +
        theme(
          
          panel.background =  element_rect(fill = "white"),
          panel.border = element_rect(fill = "transparent", colour = "black"),
          plot.background = element_rect(fill = "#ecf0f5", colour = "#ecf0f5"),
          text = element_text(family = "Helvetica")
          
        ) +
        scale_x_discrete(limits = avgTempFiveDaysDF$day)
      
    })
    
    output$plotPercentages <- renderPlot({
      
      JSONDate <- input$day
      percentageTodayDF <- plotPercentages(JSONDate)
      
      req(percentageTodayDF$time, percentageTodayDF$legend, percentageTodayDF$perc)
      
      bar_chart <- ggplot(data = percentageTodayDF, aes(x = time, y = perc, fill = legend)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        scale_fill_manual(values = c("#3c8dbc", "#222d32")) +
        labs(title = paste("Humidity and Cloud Coverage in ",JSON_data$parsedResult$city.name," - ",JSONDate, sep = ""), y = "Percentage (%)", x = "Time") +
        theme(
          
          panel.background =  element_rect(fill = "white"),
          panel.border = element_rect(fill = "transparent", colour = "black"),
          plot.background = element_rect(fill = "#ecf0f5", colour = "#ecf0f5"),
          text = element_text(family = "Helvetica")
          
        )
      
      bar_chart
      
    })
    
  })
  
}