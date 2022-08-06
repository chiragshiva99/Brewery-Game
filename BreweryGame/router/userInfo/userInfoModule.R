userInfoModuleUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "userInfoTab",
    fluidRow(
      h1("Hi User!!"),
    ),
    fluidRow(
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
      
      observeEvent(input$tab, {
        updateTabsetPanel(session, inputId = "mybox2", input$tab)
      })
      
      output$userInfo <- renderUI({
        
        paste("Current Game :", USER$gameID)
      })
    }
  )
}