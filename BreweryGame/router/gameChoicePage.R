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
                    htmlOutput(ns("continueOption"))
                    )
  )
}