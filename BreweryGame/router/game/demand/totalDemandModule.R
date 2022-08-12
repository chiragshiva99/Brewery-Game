## Tracks the total demand of all the beers
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
      ns <- session$ns
      
      output$collatedDemand <- renderUI({
        fluidRow(
          column(width=12,
            infoBoxOutput(ns("totalDemand"), width=12),
            lapply(1:nrow(beerInfo), function(i) {
              infoBoxOutput(ns(paste0("beer", beerInfo[i, "beerID"])), width=12)
            })
          )
        )
      })
      
      output$totalDemand <- renderInfoBox({
        infoBox(
          h2(paste(nrow(demand$dayDemand), "Orders")),
          color="lightblue"
        )
      })
      

      lapply(1:nrow(beerInfo), function(i) {
        output[[paste0("beer", beerInfo[i, "beerID"])]] <- renderInfoBox({
          amountNeeded <- 0
          forBeer <- subset(demand$dayDemand, Beer == beerInfo[i, "name"])
          if (nrow(forBeer) > 0) {
            amountNeeded <- sum(forBeer[,3])
          }
          
          infoBox(
            h2(paste(amountNeeded, beerInfo[i, "name"], "needed")),
            color="secondary",
          )
        })
      })
      
    }
  )
}