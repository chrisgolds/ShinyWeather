
at_a_glance_server <- function(id) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 try({removeInputHandler("addGlancePanel")})
                 
                 registerInputHandler("addGlancePanel", function(x, shinysession, name) {
                   
                   if (is.null(x)) return(NA)
                   x
                   
                 })
                 
                 observeEvent(c(input$add_glance_button, input$add_glance_panel), {
                   
                   if (input$add_glance_panel > 1) {
                     
                     showModal(
                       modalDialog(
                         title = "Add new glance",
                         footer = modalButton("Close"),
                         easyClose = TRUE
                       )
                     )
                     
                   }
                   
                 })
                 
               }
  )
  
}