menu_tab <- lapply(1:3, function(i) {
  tabPanel(
    sprintf("Menu %s", i),
    sprintf("Hello tab %s", i)
  )
})

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
    ),
    tabBox(
      id = ns("tabcard"),
      title = "A card with tabs",
      selected = "Tab 2",
      status = "primary",
      solidHeader = FALSE,
      type = "tabs",
      tabPanel(
        title = "Tab 1",
        "Content 1"
      ),
      tabPanel(
        title = "Tab 2",
        "Content 2"
      ),
      tabPanel(
        title = "Tab 3",
        "Content 3"
      )
    ),
    tabBox(
      id = ns("mybox2"),
      title = "",
      .list = menu_tab
    ),
    selectInput(
      ns("tab"),
      "Selected a tab",
      choices = paste("Menu", 1:3),
      "Menu 2"
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