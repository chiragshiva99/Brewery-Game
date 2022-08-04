source("router/analysis/analysisHelper.R")

analysisModuleUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "analysisTab",
    fluidRow(
      h1("Analysis!!"),
      box(
        title="Money",
        plotlyOutput(ns("moneyPlot"))
      ),
      # not sure
      box(
        title="Tank Status",
        plotlyOutput(ns("tankPlot"))
      ),
      box(
        title="Beer inventory levels",
        plotlyOutput(ns("beerPlot"))
      ),
      # not sure 
      box(
        title="Beer Demand",
        plotlyOutput(ns("demandPlot"))
      ),
      box(
        title="Material inventory Levels",
        plotlyOutput(ns("materialPlot"))
      )
      
    )
  )
}

analysisModuleServer <- function(id, stateData) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      materialInfo <- getMaterialInfo()
      beerInfo <- getBeerInfo()
      customerInfo <- getCustomerInfo()
      
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
      
      # 2 not sure how to plot this 
      output$tankPlot <- renderPlotly({
        print(stateData$tank)
        tankInfo <- stateData$tank
        tankInfo <- tankInfo %>% left_join(beerInfo, by=c("beerID")) %>% rename(Beer=name, Tank=tankID)
        
        p <- ggplot(tankInfo, aes(gameDay, Tank)) + 
          geom_point(aes(color=Beer))
        
        ggplotly(p)
      })
      # 4 not sure how to plot this
      output$demandPlot <- renderPlotly({
        demandData <- stateData$demand
        demandData <- demandData %>% left_join(customerInfo, by=c("customerID")) %>% rename(customerName=name) %>% left_join(beerInfo, by=c("beerID")) %>% rename(beerName=name)
        # print(demandData)
        
        demandBeer1 <- subset(demandData, demandData$beerID==1)
        demandBeer2 <- subset(demandData, demandData$beerID==2)
        demandBeer3 <- subset(demandData, demandData$beerID==3)
        
        # p <- ggplot(demandData, aes(gameDay, quantity)) +
        #   geom_line(aes(color=beerName), size=1) + 
        #   geom_point(aes(shape=customerName, color=beerName))
        
        p <- ggplot(data=demandBeer1, mapping=aes(gameDay, quantity)) +
          geom_bar(mapping = aes(x=gameDay, y=quantity), stat = "identity", fill = "#EC9D00"#,color = "black"
          ) +
          labs(title="Demand for Beer 1", 
               x = "Game Day",
               y = "Beer Quantity"
          )+darkTheme
        
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
