
#Implementation for server object
server <- function(input, output, session) {
  
  home_server("home_ns")
  at_a_glance_server("at_a_glance_ns")
  
}