source("router/leaderboard/leaderboardHelper.R")

leaderboardModuleUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "leaderTab",
    fluidRow(
      h1("Leaderboard"),
    ),
    fluidRow(
      box(
        title=NULL,
        collapsible = F,
        tableOutput(ns("leaderboard"))
      )
    )
  )
}

leaderboardModuleServer <- function(id, USER) {
  moduleServer(
    id,
    function(input, output, session) {
      output$leaderboard <- renderTable({
        click <- USER$finish
        scores <- getLeaderboard()
        
        scores <- scores %>% select(-gameSeed, -userID) %>% rename(User=username, "Cash Balance"=cashBalance)
        
        scores$Ranking <- rownames(scores)
        scores <- scores[,c(3,1,2)]
        scores
      })
    }
  )
}