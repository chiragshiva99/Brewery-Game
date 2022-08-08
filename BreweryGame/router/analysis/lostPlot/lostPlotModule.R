lostPlotModuleUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    dropdownButton(
      
      tags$h3("List of Inputs"),
      htmlOutput(ns("lostInput")),
      
      circle = F, status = "primary",
      icon = icon("gear"), width = "300px",
      tooltip = tooltipOptions(title = "Click to see inputs !")
    ),
    plotlyOutput(ns("lostPlot"))
  )
}

lostPlotModuleServer <- function(id, stateData, beerInfo){
  moduleServer(
    id, 
    function(input, output, session){
      ns <- session$ns
      
      output$lostInput <- renderUI({
        ## Get options to put in checkboxGroup
        beerOptions <- beerInfo[, "name"]
        ## Add the select All option
        beerOptions <- c("All", beerOptions)
        
        div(
          awesomeCheckboxGroup(
            inputId = ns("beerSelect"),
            label = "Choose by Beer", 
            choices = beerOptions,
            selected = "All",
            inline = TRUE, 
            status = "primary"
          )
        )
      })
      
      output$lostPlot <- renderPlotly({
        lostSales <- subset(stateData$demand, serviceDay == -1)
        lostBeer <- select(stateData$beer, gameDay, beerID, lostSale)
        
        p <- ggplot(data=lostBeer, mapping=aes(gameDay, lostSale, fill=as.factor(beerID))) +
          geom_bar(position="stack", stat="identity") +
          darkTheme
        
        ggplotly(p)
      })
      
    }
  )
}