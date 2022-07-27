## Start game or continue Game
gameChoice <- function(ns){
  div(id = "startGame", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                  wellPanel(  
                    actionButton(ns("startGame"), "Start Game"),
                    br(),
                    htmlOutput(ns("continueOption"))
                    )
  )
}