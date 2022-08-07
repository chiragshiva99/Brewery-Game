source("router/analysis/analysisHelper.R")

# Split plots into Modules
source("router/analysis/demandPlot/demandPlotModule.R")

analysisModuleUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "analysisTab",
    fluidRow(
      h1("Analysis!!"),
    ),
    fluidRow(
      box(
        title="Money",
        plotlyOutput(ns("moneyPlot"))
      ),
      box(
        title="Tank Status",
        plotlyOutput(ns("tankPlot"))
      ),
      box(
        title="Beer inventory levels",
        plotlyOutput(ns("beerPlot"))
      ),
      box(
        title="Beer Demand",
        # call demandPlotModule in UI
        demandPlotModuleUI(ns("demand"))
      ),
      box(
        title="Material inventory Levels",
        plotlyOutput(ns("materialPlot"))
      ),
      box(
        title="Lost Sales",
        plotlyOutput(ns("lostPlot"))
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
      demandPlotModuleServer("demand", stateData$demand, beerInfo, customerInfo)
      
      output$moneyPlot <- renderPlotly({
        print(stateData$cash)
        p <- ggplot(stateData$cash, aes(gameDay, cashBalance)) +
          geom_step(size = 1, color = ifelse(stateData$cash$cashBalance>=100000, "green", "red")) +
          # geom_hline(mapping=aes(yintercept = 100000), color="grey", size= 0.5, alpha = 0.8) +
          geom_text(mapping=aes(0, y = 100000,label = "Initial Revenue", vjust = -1, hjust = 0), color = 'white') +
          labs(title="Cash Balance generated everyday", 
               x = "Game Day",
               y = "Cash Balance ($)"
          )+darkTheme 
        
        ggplotly(p)
      })
      
      output$lostPlot <- renderPlotly({
        lostSales <- subset(stateData$demand, serviceDay == -1)
        lostBeer <- select(stateData$beer, gameDay, beerID, lostSale)
        
        p <- ggplot(data=lostBeer, mapping=aes(gameDay, lostSale, fill=as.factor(beerID))) +
          geom_bar(position="stack", stat="identity") + 
          darkTheme
        
        ggplotly(p)
      })
      
      output$beerPlot <- renderPlotly({
        # print(stateData$beer)
        beerData <- stateData$beer
        beerData <- beerData %>% left_join(beerInfo, by=c("beerID")) %>% rename(Beer=name)
        
        p <- ggplot(beerData, aes(gameDay)) + 
          geom_step(aes(y=inventory, color=Beer), size = 1) + 
          # geom_hline(mapping=aes(yintercept = 50), color="grey", size= 2, alpha = 0.8) +
          geom_text(mapping=aes(0, y = 50,label = "Recommended Brewing Point", vjust = -1, hjust = 0), color = 'white') +
          labs(title="Beer Inventory Level", 
               x = "Game Day",
               y = "Inventory"
          )+darkTheme
        
        ggplotly(p)
      })
      
      output$materialPlot <- renderPlotly({
        # print(stateData$mat)
        materialData <- stateData$mat
        materialData <- materialData %>% left_join(materialInfo, by=c("materialID")) %>% rename(Material=name)
        materialData$Material <- ifelse(materialData$materialID==1, "Malt", ifelse(materialData$materialID==2, "Hops", "Yeast"))
        
        p <- ggplot(materialData, aes(gameDay)) + 
          geom_step(aes(y=inventory, color=Material), size= 1) + 
          # geom_line(aes(y=inTransit, color=Material)) +
          # geom_hline(mapping=aes(yintercept = 50), color="grey", size= 1, alpha = 0.8) +
          geom_text(mapping=aes(0, y = 100,label = "Recommended Raw-Material Reorder Point", vjust = -1, hjust = -1), color = 'white') +
          labs(title="Raw Material Inventory Level", 
               x = "Game Day",
               y = "Inventory"
          ) + darkTheme
        
        ggplotly(p)
      })
    }
  )
}
