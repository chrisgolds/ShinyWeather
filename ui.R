library(shiny)
library(shinyjs)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(title = "Shiny Weather!"),
  dashboardSidebar(),
  dashboardBody(
    
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
    uiOutput("header"),
    uiOutput("weatherData")
    
  )

)