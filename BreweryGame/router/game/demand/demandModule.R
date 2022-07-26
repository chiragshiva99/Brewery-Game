library(tidyverse)

demandModuleUI <- function(id) {
  ns <- NS(id)
  column(width=4,
         box(width=NULL,
             title="Lost Sales",
             htmlOutput(ns("lostSales")),
             htmlOutput(ns("lostSalesPerBeer"))
         ),
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
      output$lostSales <- renderUI({paste(demand$lostCust, "Customers Lost")})
      
      output$lostSalesPerBeer <- renderTable({
        select(demand$lostPerBeer, -stockOut)
      })
      
      output$custDemand <- renderTable({
        select(demand$dayDemand, -maxWait, -actualDay)
      })
    }
  )
  
}