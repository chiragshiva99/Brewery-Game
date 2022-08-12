### Overall distribution
# Chirag: analysis and login folder
# Haohong: leaderboard, userInfo and tutorial folder
# Noel: content of tutorial folder
# Gabriel: game folder and touch up of everyones code


source("usePackages.R")
pkgnames <- c("tidyverse","shiny", "shinyjs","DBI","jsonlite","bs4Dash", "shinyauthr", "sodium", "shinyBS", "plotly", "shinyWidgets", "zeallot", "ggalt", "fresh", "ggdark", "RMySQL")
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
