
home_ui <- function(id) {
  
  ns <- NS(id)
  
  tags$div(
    
    shinyjs::useShinyjs(),
    
    fixedRow(
      
      column(width = 6,
             
             tags$h3("5 day forecast at the push of \"Get data\""),
             tags$br(),
             
             tags$div(class = "form-inline",
                      textInput(inputId = ns("test"), label = "Location:", placeholder = "Enter city, country code"),
                      tags$br(),
                      tags$br(),
                      tags$button(class = "btn btn-primary", onclick = "getLoc()",
                                  tags$span(class = "glyphicon glyphicon-refresh"),
                                  tags$span(class = "glyphicon glyphicon-map-marker")),
                      actionButton(inputId = ns("getForecast"), class = "btn btn-info", label = "Get data")
             ),
             
             tags$br(),
             tags$br(),
             uiOutput(ns("timeOut")),
             uiOutput(ns("dayOut")),
             uiOutput(ns("weatherData"))
             
      ),
      
      column(width = 5,
             
             uiOutput(ns("header")),
             tags$br(),
             leafletOutput(ns("map")),
             uiOutput(ns("pinnedWeather"))
             
      )
      
    ),
    
    tags$br(),
    uiOutput(ns("graphHead")),
    
    fixedRow(
      
      
      column(width = 6,
             plotOutput(ns("plotDayTemp"))
      ),
      
      column(width = 5,
             
             plotOutput(ns("plotFiveDayAvg"))
      )
      
    ),
    
    fixedRow(
      
      column(width = 12,
             
             plotOutput(ns("plotPercentages"))
      )
      
    )
    
  )
  
}