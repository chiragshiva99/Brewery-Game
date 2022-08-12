## Start game or continue Game
gameChoice <- function(ns){
  div(id = "startGame", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                  wellPanel(  
                    div(style = "text-align: center;",
                        br(),
                        br(),
                        br(),
                      actionBttn(ns("startGame"), 
                                 "Start Game",
                                 style="jelly",
                                 color="success",
                                 size="lg"
                                 ),
                    ),
                    br(),
                    htmlOutput(ns("continueOption")),
                    h1("Introduction"),
                    tags$p(
                      "Littlefield Brewery opened its first and only Brewery with 3 beer types. You will have to manage this brewery for the next 490 days. From the ordering of raw materials to the service of customers, you will have to do it all. However, you also have the option to automate all these processes! Set reorder quantities and reorder points for your various materials and beers and let the brewery run. You are parachuted in on day 51 with the customer demand data from the past 50 days. You will also have to set automation parameters for the last 100 simulated days where you have no control over the brewery."
                    )
                    )
  )
}