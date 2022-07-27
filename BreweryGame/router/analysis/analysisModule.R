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
        title="Lost Customers",
        plotlyOutput(ns("customerPlot"))
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
      output$moneyPlot <- renderPlotly({
        print(stateData$cash)
        p <- ggplot(stateData$cash, aes(gameDay, cashBalance)) + 
              geom_line()
        
        ggplotly(p)
      })
    }
  )
}