#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/  
#

# At the beginning of any R session, record your AWS database password:

# Now, anywhere in your code where the password is needed you can get it using
# getOption("AWSPassword")
# Otherwise it is hidden. So now this code can be shared with anyone 
# without giving them access to your personal AWS database.

source("usePackages.R")
pkgnames <- c("tidyverse","shiny", "shinyjs","DBI","jsonlite","bs4Dash", "shinyauthr", "DT", "sodium", "shinyBS", "plotly", "shinyWidgets", "zeallot", "ggalt", "fresh")
loadPkgs(pkgnames)


#feature Modules
source("router/routerModule.R")

#Helper Functions
source("router/generalDBHelper.R")

ui <- routerModuleUI("router")

# Define server logic required to draw a histogram
server <- function(input, output) {
  routerModuleServer("router")
}

shinyApp(ui = ui, server = server)
