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

lostPlotModuleServer <- function(id, lost, beerInfo){
  moduleServer(
    id, 
    function(input, output, session){
      ns <- session$ns
      
      beerInfo <- getBeerInfo()
      
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
      
      output$lostPlot <- renderPlotly({})
      
    }
  )
}