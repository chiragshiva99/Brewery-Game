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
      materialInfo <- getMaterialInfo()
      beerInfo <- getBeerInfo()
      customerInfo <- getCustomerInfo()
      
      output$moneyPlot <- renderPlotly({
        print(stateData$cash)
        p <- ggplot(stateData$cash, aes(gameDay, cashBalance)) + 
              geom_line()
        
        ggplotly(p)
      })
      
      output$tankPlot <- renderPlotly({
        print(stateData$tank)
        tankInfo <- stateData$tank
        tankInfo <- tankInfo %>% left_join(beerInfo, by=c("beerID")) %>% rename(Beer=name, Tank=tankID)
        
        p <- ggplot(tankInfo, aes(gameDay, Tank)) + 
          geom_point(aes(color=Beer))
        
        ggplotly(p)
      })
      
      output$demandPlot <- renderPlotly({
        demandData <- stateData$demand
        demandData <- demandData %>% left_join(customerInfo, by=c("customerID")) %>% rename(customerName=name) %>% left_join(beerInfo, by=c("beerID")) %>% rename(beerName=name)
        
        p <- ggplot(demandData, aes(gameDay, quantity)) +
          geom_line(aes(color=beerName), size=1) + 
          geom_point(aes(shape=customerName, color=beerName))
        
        ggplotly(p)
      })
      
      output$beerPlot <- renderPlotly({
        print(stateData$beer)
        beerData <- stateData$beer
        beerData <- beerData %>% left_join(beerInfo, by=c("beerID")) %>% rename(Beer=name)
        
        p <- ggplot(beerData, aes(gameDay)) + 
          geom_line(aes(y=inventory, color=Beer)) + 
          geom_line(aes(y=inTank, color=Beer)) + 
          geom_line(aes(y=lostSale, color=Beer))
        
        ggplotly(p)
      })
      
      output$materialPlot <- renderPlotly({
        print(stateData$mat)
        materialData <- stateData$mat
        materialData <- materialData %>% left_join(materialInfo, by=c("materialID")) %>% rename(Material=name)
        
        p <- ggplot(materialData, aes(gameDay)) + 
          geom_line(aes(y=inventory, color=Material)) + 
          geom_line(aes(y=inTransit, color=Material))
        
        ggplotly(p)
      })
    }
  )
}