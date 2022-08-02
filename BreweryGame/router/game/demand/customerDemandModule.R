customerDemandModuleUI <- function(id) {
  ns <- NS(id)
  div(
    htmlOutput(ns("custDemand"))
  )

}

customerDemandModuleServer <- function(id, demand) {
  moduleServer(
    id,
    function(input, output, session) {
      output$custDemand <- renderTable({
        select(demand$dayDemand, -maxWait, -arrivalDay)
      })
    }
  )
}