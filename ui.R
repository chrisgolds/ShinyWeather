
ui <- dashboardPage(
  
  dashboardHeader(title = "Shiny Weather"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("At a Glance", tabName = "at_a_glance", icon = icon("eye"))
    )
  ),
  dashboardBody(
    
    singleton(
      tags$script(src = "js/app.js")
    ),
    tabItems(
      tabItem(tabName = "home",
        home_ui("home_ns")
      ),
      tabItem(tabName = "at_a_glance",
        at_a_glance_ui("at_a_glance_ns")
      )
    )
    
  )

)