userInfoModuleUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "userInfoTab",
    fluidRow(
      h1("Hi User!!"),
      box(
        title="User Information",
        htmlOutput(ns("userInfo"))
      )
    )
  )
}

userInfoModuleServer <- function(id, USER) {
  moduleServer(
    id,
    function(input, output, session) {
      output$userInfo <- renderUI({
        
        paste("Current Game :", USER$gameID)
      })
    }
  )
}