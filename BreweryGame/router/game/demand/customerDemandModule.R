customerDemandUI <- function(id) {
  ns <- NS(id)
  column(
    width=4,
    box(width=NULL,
         collapsible=F,
         title="Customer Demand",
         htmlOutput(ns("custDemand"))
        )
  )

}

customerDemandServer <- function(id, demand, disable=F) {
  moduleServer(
    id,
    function(input, output, session) {
      output$custDemand <- renderTable({
        select(demand$dayDemand, -maxWait, -arrivalDay)
      })
    }
  )
}