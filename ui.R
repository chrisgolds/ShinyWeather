library(shiny)
library(shinyjs)

ui <- fluidPage(
  
  includeScript("app.js"),

  tags$h1("Shiny Weather!"),
  tags$h4("5 day forecast at the push of \"Get data\""),
  tags$br(),
  
  tags$div(class = "form-inline",
           textInput(inputId = "test", label = "Location:", placeholder = "Enter city, country code"),
           tags$button(class = "btn btn-primary", onclick = "getLoc()",
                       tags$span(class = "glyphicon glyphicon-refresh"),
                       tags$span(class = "glyphicon glyphicon-map-marker")),
           actionButton(inputId = "getForecast", class = "btn btn-success", label = "Get data")
  ),
  
  tags$br(),
  tags$br(),
  uiOutput("timeOut"),
  tags$h2(textOutput("test")),
  uiOutput("weatherData")

)