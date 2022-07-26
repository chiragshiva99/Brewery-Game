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
        "Includes all lost customers and by beer also"
      ),
      box(
        title="Beer inventory levels",
        "Tracks all beer inventory, in tank and outside"
      ),
      box(
        title="Beer Demand",
        "Shows beer Demand over entire game"
      ),
      box(
        title="Material inventory Levels",
        "All material inventory, in delivery and on hand"
      )
      
    )
  )
}

analysisModuleServer <- function(id, stateData) {
  moduleServer(
    id,
    function(input, output, session) {
      output$moneyPlot <- renderPlotly({
        print(stateData$state)
        p <- ggplot(stateData$state, aes(day, cash)) + 
              geom_line()
        
        ggplotly(p)
      })
    }
  )
}