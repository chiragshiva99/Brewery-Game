menu_tab <- lapply(1:3, function(i) {
  tabPanel(
    sprintf("Menu %s", i),
    sprintf("Hello tab %s", i)
  )
})


actionModuleUI <- function(id) {
  ns <- NS
  uiOutput(ns("actionTab"))
}

actionModuleServer <- function(id, general, beer, beerInfo, beerReq, material, costInfo, disabled) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$actionTab <- renderUI({
        # supplierInfo <- costInfo %>% subset(materialName==input$matChosen)
        # 
        # actionTabs <- list()
        # 
        # print(actionTabs)
        # print("actions")
        # 
        # # actionTabs[[1]] <- tabPanel(
        # #   title="Purchase Material",
        # #   matPurchaseModuleUI(ns("matPurch"), unique(costInfo[,"materialName"]), supplierInfo[,"supplierName"])
        # # )
        # # 
        # actionTabs[[1]] <- tabPanel(
        #   title="IT WORKS",
        #   div(
        #     h4("WTF")
        #   )
        # )
        # 
        # print(actionTabs)
        return(tabBox(id=ns("actions"), .list=menu_tabs))
      })
      
      # matPurchaseModuleServer("matPurch", general, material, costInfo, disabled)
    }
  )
}