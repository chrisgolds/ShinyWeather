library(shiny)
library(shinyjs)
library(shinydashboard)
library(leaflet)

ui <- dashboardPage(
  
  dashboardHeader(title = "Shiny Weather!"),
  dashboardSidebar(),
  dashboardBody(
    
    useShinyjs(),
    
    fixedRow(
      
      column(width = 6,
        
        includeScript("app.js"),
        tags$h3("5 day forecast at the push of \"Get data\""),
        tags$br(),
        
        tags$div(class = "form-inline",
                 textInput(inputId = "test", label = "Location:", placeholder = "Enter city, country code"),
                 tags$button(class = "btn btn-primary", onclick = "getLoc()",
                             tags$span(class = "glyphicon glyphicon-refresh"),
                             tags$span(class = "glyphicon glyphicon-map-marker")),
                 actionButton(inputId = "getForecast", class = "btn btn-info", label = "Get data")
        ),
        
        tags$br(),
        tags$br(),
        uiOutput("timeOut"),
        uiOutput("dayOut"),
        uiOutput("weatherData")
        
      ),
      
      column(width = 5,
      
        uiOutput("header"),
        tags$br(),
        leafletOutput("map")
        #uiOutput("pinnedWeather")
             
      )
      
    ),
    
    tags$br(),
    uiOutput("graphHead"),
    
    fixedRow(
      
      
      column(width = 6,
             plotOutput("plotDayTemp")
      ),
      
      column(width = 5,
             
             plotOutput("plotFiveDayAvg"))
      
    ),
    
    fixedRow(
      
      column(width = 12,
             
             plotOutput("plotPercentages")
      )
      
      #column(width = 5,
             
      #       plotOutput("plotAvgPercentages"))
      
    )
    
  )

)