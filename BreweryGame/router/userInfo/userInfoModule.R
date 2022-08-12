# Done by Haohong
source("router/userInfo/userInfoHelper.R")

userInfoModuleUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "userInfoTab",
    fluidRow(
      htmlOutput(ns("hi")),
    ),
    fluidRow(
      column(width=12,
             box(
               title="User Information",
               collapsible = F,
               htmlOutput(ns("userInfo"))
             ),
             box(
               title="Your Best Performances",
               collapsible = F,
               tableOutput(ns("leaderboard"))
             )
      )
    )
  )
}
# Renders the userInfo page to display some userInfo

userInfoModuleServer <- function(id, USER) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$hi <- renderUI({
        h1(paste0("Hi ", USER$username, "!!!"))
      })
      
      output$userInfo <- renderUI({
        
        paste("Current Game :", USER$gameID)
      })
      
      output$leaderboard <- renderTable({
        click <- USER$finish + USER$gameStart
        scores <- getLeaderboardUser(USER$id)
        
        scores <- scores %>% select(-gameSeed, -userID, -username) %>% rename("Cash Balance"=cashBalance)
        
        scores$Ranking <- rownames(scores)
        scores <- scores[,c(2,1)]
        scores
      })
    }
  )
}