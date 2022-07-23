source("materialModule.R")
source("beerModule.R")
source("demandModule.R")

## Start game or continue Game
gameChoice <- div(id = "startGame", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                  wellPanel(
                    actionButton("startGame", "Start Game"),
                    br(),
                    htmlOutput("continueOption")
                  )
)

## Game interface
gameInterface <- tabItem(tabName ="gameTab", class = "active",
                         # Application title
                         fluidRow(
                           box(width=3,
                                  actionButton("reset", "Reset Game"),
                                  htmlOutput("gameStatus")
                           ),
                           box(width=3,
                                  htmlOutput("money")),
                           box(width=3,
                                  htmlOutput("day")),
                           box(width=3,
                                  actionButton("advance", "Advance: Next Day")
                           )
                         ),
                         fluidRow(
                           materialModuleUI("material"),
                           beerModuleUI("beer"),
                           demandModuleUI("demand")
                          )
                      )