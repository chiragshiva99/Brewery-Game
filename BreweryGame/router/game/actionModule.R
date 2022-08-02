actionModuleUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("actionTab"))
}

actionModuleServer <- function(id, general, beer, beerInfo, beerReq, material, costInfo, disabled) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$actionTab <- renderUI({

        actionTabs <- list()

        actionTabs[[1]] <- tabPanel(
          title="Purchase Material",
          matPurchaseModuleUI(ns("material"), unique(costInfo[,"materialName"]))
        )
        
        actionTabs[[2]] <- tabPanel(
          title="Brew Beer",
          beerBrewModuleUI(ns("beer"), beer$tanks[,"Tank"], beer$beerInv[, "name"])
        )
        
        actionTabs[[3]] <- tabPanel(
          title="Store Beer",
          beerStoreModuleUI(ns("beerStore"))
        )

        return(tabBox(title="Actions", id=ns("action"), width=NULL, collapsible=F, .list=actionTabs))
      })
      
      matPurchaseModuleServer("material", general, material, costInfo, disabled)
      beerBrewModuleServer("beer", beer, material, beerInfo, beerReq, disabled)
      beerStoreModuleServer("beerStore", beer, material)
    }
  )
}