library(shiny)

ui <- fluidPage(
  
  includeScript("app.js"),

  tags$h1("Shiny Weather!"),
  tags$br(),
  
  tags$div(class = "form-inline",
           textInput(inputId = "test", label = "Location:", placeholder = "Enter city, country code"),
           tags$button(class = "btn btn-primary", onclick = "getLoc()",
                       tags$span(class = "glyphicon glyphicon-refresh"),
                       tags$span(class = "glyphicon glyphicon-map-marker")),
           actionButton(inputId = "getForecast", class = "btn btn-success", label = "Get weather data")
  ),
  
  tags$br(),
  tags$br(),
  actionButton(inputId = "close", label = "End session", class = "btn btn-danger"),
  textOutput("test")

)