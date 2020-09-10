
at_a_glance_ui <- function(id) {
  
  ns <- NS(id)
  
  tags$div(
    shinyjs::useShinyjs(),
    actionButton(inputId = ns("add_glance_button"), label = "Add entry"),
    tags$br(),
    tags$div(id = "list_of_glances", class = "row",
      tags$div(class = "col-sm-4", style = "text-align:center;",
        tags$h1("+", id = ns("add_glance_panel"), 'data-value' = 1, class = "add-panel"),
        tags$p("Add a new entry")
      )
    )
  )
  
}