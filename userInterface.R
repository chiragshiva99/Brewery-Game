# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("password", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Username: myuser  Password: mypass"),
                     br(),
                     tags$code("Username: myuser1  Password: mypass1")
                   ))
)


## Start game or continue Game
gameChoice <- div(id = "startGame", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   actionButton("startGame", "Start Game"),
                   br(),
                   htmlOutput("continueOption")
                   )
)

## Game interface
gameInterface <- tabItems(
  
  # First tab
  tabItem(tabName ="dashboard", class = "active",
          # Application title
          fluidRow(
            column(2, offset=1,
                   actionButton("reset", "Reset Game"),
                   htmlOutput("gameStatus")
            ),
            column(2, offset=6,
                   actionButton("advance", "Advance: Next Day")
                   
            )),
          fluidRow(
            column(2,h3("Money"),
                   htmlOutput("money"), offset=1),
            column(2, h3("Day"),
                   htmlOutput("day")),
            column(2, h3("Lost Sales"),
                   htmlOutput("lostSales"),
                   htmlOutput("lostSalesPerBeer")
            ),
            column(2,h3("Raw Materials"),
                   htmlOutput("rawMatQty")
            ),
            column(2, h3("Beer Inventory"),
                   htmlOutput("beerQty"))),
          fluidRow(
            column(3, h4("Material Orders"),
                   actionButton("purchase", "Purchase"),
                   fluidRow(
                     htmlOutput("currentOrders")
                   ),offset=1
            ),
            column(3, h4("Brewery Tanks"),
                   actionButton("brew", "Brew"),
                   fluidRow(
                     htmlOutput("tankInfo")
                   )
            ),
            column(4, h4("Customer Demand"),
                   htmlOutput("custDemand"))
          )
  )
)

credentials = data.frame(
  username_id = c("myuser", "myuser1"),
  password   = sapply(c("mypass", "mypass1"),sodium::password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

header <- dashboardHeader( title = "Brewery Game", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(collapsed=TRUE, uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
