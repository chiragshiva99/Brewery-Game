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
          nrow(demand$dayDemand),
          subtitle="No. of Orders",
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
            amountNeeded,
            subtitle=paste(beerInfo[i, "name"], "needed"),
            color="secondary",
          )
        })
      })
      
    }
  )
}