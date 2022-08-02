matProgModuleUI <- function(id) {
  ns <- NS(id)
  div(
    htmlOutput(ns("currentOrders"))
  )
}

matProgModuleServer <- function(id, material) {
  moduleServer(
    id,
    function(input, output, session) {
      output$currentOrders <- renderTable({
        select(material$rawMatOrder, -daysToComplete)
      })
    }
  )
}
  
  
  
