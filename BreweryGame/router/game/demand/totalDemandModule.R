totalDemandModuleUI <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("collatedDemand"))
  )
}

totalDemandModuleServer <- function(id, demand, beerInfo, customerInfo) {
  moduleServer(
    id,
    function(input, output, session) {
      output$collatedDemand <- renderUI({
        
      })
    }
  )
}