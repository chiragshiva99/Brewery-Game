source("router/analysis/analysisHelper.R")

# Split plots into Modules
source("router/analysis/demandPlot/demandPlotModule.R")
source("router/analysis/lostPlot/lostPlotModule.R")
source("router/analysis/beerPlot/beerPlotModule.R")
source("router/analysis/tankPlot/tankPlotModule.R")
source("router/analysis/materialPlot/materialPlotModule.R")




analysisModuleUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "analysisTab",
    fluidRow(
      h1("Analysis!!"),
    ),
    fluidRow(
      box(width=12,
          collapsed = T,
        title="Money",
        plotlyOutput(ns("moneyPlot"))
      ),
      box(width=12,
          collapsed = T,
        title="Tank Status",
        tankPlotModuleUI(ns("tank"))
      ),
      box(width=12,
          collapsed = T,
        title="Beer inventory levels",
        beerPlotModuleUI(ns("beer"))
      ),
      box(width=12,
          collapsed = T,
        title="Beer Demand",
        # call demandPlotModule in UI
        demandPlotModuleUI(ns("demand"))
      ),
      box(width=12,
          collapsed = T,
        title="Material inventory Levels",
        # # call materialPlotModule in UI
        materialPlotModuleUI(ns("material"))
      ),
      box(width=12,
        collapsed = T,
        title="Lost Sales",
        lostPlotModuleUI(ns("lost"))
      )
      
    )
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
      
      # Call demandPlot Module in Server
      demandPlotModuleServer("demand", stateData, beerInfo, customerInfo)
      beerPlotModuleServer("beer", stateData, beerInfo)
      lostPlotModuleServer("lost", stateData, beerInfo, customerInfo)
      materialPlotModuleServer("material", stateData, materialInfo)
      tankPlotModuleServer("tank", stateData, beerInfo)
      
      output$moneyPlot <- renderPlotly({
        # print(stateData$cash)
        p <- ggplot(stateData$cash, aes(gameDay, cashBalance)) +
          geom_step(size = 1, color = "green") +
          # geom_hline(mapping=aes(yintercept = 100000), color="grey", size= 0.5, alpha = 0.8) +
          # geom_text(mapping=aes(0, y = 100000,label = "Initial Revenue", vjust = -1, hjust = 0), color = 'white') +
          labs(title="Cash Balance generated everyday", 
               x = "Game Day",
               y = "Cash Balance ($)"
          )+darkTheme 
        
        ggplotly(p)
      })
      
    }
  )
}
