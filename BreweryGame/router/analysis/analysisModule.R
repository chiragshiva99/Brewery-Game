source("router/analysis/analysisHelper.R")

# Split plots into Modules
source("router/analysis/demandPlot/demandPlotModule.R")
source("router/analysis/lostPlot/lostPlotModule.R")
source("router/analysis/beerPlot/beerPlotModule.R")
source("router/analysis/tankPlot/tankPlotModule.R")
source("router/analysis/materialPlot/materialPlotModule.R")
source("router/analysis/moneyPlot/moneyPlotModule.R")




analysisModuleUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "analysisTab",
    fluidRow(
      h1("Analysis!!"),
    ),
    moneyPlotModuleUI(ns("money")),
    tankPlotModuleUI(ns("tank")),
    beerPlotModuleUI(ns("beer")),
    demandPlotModuleUI(ns("demand")),
    materialPlotModuleUI(ns("material")),
    lostPlotModuleUI(ns("lost"))
    # fluidRow(
    #   
    #   box(width=12,
    #       collapsed = T,
    #     title="Tank Status",
    #     tankPlotModuleUI(ns("tank"))
    #   ),
    #   box(
    #     beerPlotModuleUI(ns("beer"))
    #   ),
    #   box(
    #     # call demandPlotModule in UI
    #     demandPlotModuleUI(ns("demand"))
    #   ),
    #   
    #   box(width=12,
    #       collapsed = T,
    #     title="Material inventory Levels",
        # # call materialPlotModule in UI
        
      # ),
      # box(width=12,
      #   collapsed = T,
      #   title="Lost Sales",
      #   
      # )
      
  )
}

analysisModuleServer <- function(id, stateData, input, output) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      materialInfo <- getMaterialInfo()
      beerInfo <- getBeerInfo()
      customerInfo <- getCustomerInfo()
      
      moneyPlotModuleServer("money", stateData)
      # Call demandPlot Module in Server
      demandPlotModuleServer("demand", stateData, beerInfo, customerInfo)
      beerPlotModuleServer("beer", stateData, beerInfo)
      lostPlotModuleServer("lost", stateData, beerInfo, customerInfo)
      materialPlotModuleServer("material", stateData, materialInfo)
      tankPlotModuleServer("tank", stateData, beerInfo)
      
      
      
    }
  )
}
