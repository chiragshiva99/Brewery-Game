library(tidyverse)

demandModuleUI <- function(id) {
  ns <- NS(id)
  column(width=4,
         valueBoxOutput(ns("lostSales"), width=12),
         infoBoxOutput(ns("lostSalesPerBeer"), width=12),
         box(width=NULL,
             title="Customer Demand",
             htmlOutput(ns("custDemand"))
         )
  )
}

demandModuleServer <- function(id, demand, disable=F) {
  moduleServer(
    id,
    function(input, output, session) {
      output$lostSales <- renderValueBox({
        valueBox(
          demand$lostCust,
          "Customers Lost",
          color="danger"
        )
      })
      
      output$lostSalesPerBeer <- renderInfoBox({
        infoBox(
          title="Lost Sales By Beer",
          renderTable({select(demand$lostPerBeer, -stockOut) %>% rename(Beer=name, Quantity=lostQty)})
        )
      })
      
      output$custDemand <- renderTable({
        select(demand$dayDemand, -maxWait, -arrivalDay)
      })
    }
  )
  
}