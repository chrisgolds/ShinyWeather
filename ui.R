library(shiny)

ui <- fluidPage(
  
  includeScript("app.js"),
  
  tags$h1("Shiny Weather!"),
  tags$br(),
  
  tags$div(class = "form-inline", 
           tags$input(id = "test", type = "text", class = "form-control", size = "60", placeholder = "Enter location"),
           tags$button(class = "btn btn-success", "Get weather data"),
           tags$button(class = "btn btn-primary", onclick = "getLoc()", "Get current location")
  ),
  
  tags$br(),
  tags$br(),
  actionButton(inputId = "close", label = "End session", class = "btn btn-danger"),
  textOutput("test")

)