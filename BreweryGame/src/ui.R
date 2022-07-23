header <- dashboardHeader( title = "Brewery Game", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(collapsed=TRUE, uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))

ui <- dashboardPage(header, sidebar, body, skin = "blue")