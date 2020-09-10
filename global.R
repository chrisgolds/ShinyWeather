
# Attach all necessary packages to namepsace. Install any missing packages.

packages <- c("shiny",
              "shinyjs",
              "shinydashboard",
              "leaflet",
              "RCurl",
              "stringr",
              "rjson",
              "countrycode",
              "ggplot2"
            )

for (i in 1:length(packages)) {
  
  if (!requireNamespace(packages[i])) {
    
    install.packages(packages[i])
    
  }
  
  library(packages[i], character.only = T)
  
}

# Read modules

source("modules/home/home_ui.R")
source("modules/home/home_server.R")

source("modules/at_a_glance/at_a_glance_ui.R")
source("modules/at_a_glance/at_a_glance_server.R")

# Utility functions used throughout execution of program

# JSON_data is a reactive object - the parsedResult property will hold weather information from api.openweathermap.org
# which is read in as JSON string and parsed as a data frame
JSON_data <- reactiveValues(parsedResult = NULL)

#Function implementation for invoking weathermap API calls with appropriate endpoints
#First  parameter is the location of the weatehr data in string format - this can be:
# - City/town name
# - City/town with country code (separated by comma)
# - Coordinates
#Second parameter acts as Boolean flag - if true, will update JSON_data with weather data from location coordinates specified in pinned map point
getData <- function(str_loc, is_update_loc) {
  
  JSON_backup <- NULL
  if(!is.null(JSON_data$parsedResult)) {
    
    JSON_backup <- JSON_data$parsedResult #If querying new location data, take copy of latest result in case new query
    #throws an error
    
  }
  
  UTC.time <<- Sys.time()
  attr(UTC.time, "tzone") <- "UTC"
  
  location.formatted <- NULL
  result <- NULL
  
  if (is_update_loc) {
    
    location.formatted <- str_split(str_loc, ",")
    result <- getURL(paste("api.openweathermap.org/data/2.5/forecast?lat=",location.formatted[[1]][1],"&lon=",location.formatted[[1]][2],"&APPID=36e53cd3a38129e9abdc5d13b71aa14a", sep = ""))
    
  } else {
    
    if (grepl(",", str_loc)) {
      
      location.formatted <- str_split(str_loc, ", ")
      result <- getURL(paste("api.openweathermap.org/data/2.5/forecast?q=",gsub(" ", "%20",location.formatted[[1]][1],fixed = TRUE),",",location.formatted[[1]][2],"&APPID=36e53cd3a38129e9abdc5d13b71aa14a", sep = ""))
      
    } else {
      
      location.formatted <- str_loc
      result <- getURL(gsub(" ", "%20", paste("api.openweathermap.org/data/2.5/forecast?q=",str_loc,"&APPID=36e53cd3a38129e9abdc5d13b71aa14a", sep = ""), fixed = TRUE))
      
    }
    
  }
  
  #JSON string containing weather data parsed into JSON format first, then into a data frame for ease of use in R
  JSON_data$parsedResult <<- as.data.frame(fromJSON(result))
  
  if(JSON_data$parsedResult$cod == "404") {
    
    if (!is.na(JSON_backup)) {
      JSON_data$parsedResult <- JSON_backup
    }
    return(1)
    
  } else {
    
    return(0)
    
  }
  
}