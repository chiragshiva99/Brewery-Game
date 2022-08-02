beerTankModuleUI <- function(id) {
  ns <- NS(id)
  div(
    htmlOutput(ns("tank"))
  )
}

beerTankModuleServer <- function(id, beer) {
  moduleServer(
    id,
    function(input, output, session) {
      output$tank <- renderTable({select(beer$tanks, -daysToComplete, -tankSize)})
    }
  )
}