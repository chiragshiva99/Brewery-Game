beerPlotModuleUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    dropdownButton(
      
      tags$h3("List of Inputs"),
      htmlOutput(ns("beerInput")),
      
      circle = F, status = "primary",
      icon = icon("gear"), width = "300px",
      tooltip = tooltipOptions(title = "Click to see inputs !")
    ),
    plotlyOutput(ns("beerPlot"))
  )
}

beerPlotModuleServer <- function(id, beer, beerInfo, statusInfo){
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      beerInfo <- getBeerInfo()
      # statuslInfo <-
      
      output$beerInput <- renderUI({
        ## Get options to put in checkboxGroup
        beerOptions <- beerInfo[, "name"]
        ## Add the select All option
        beerOptions <- c("All", beerOptions)
        
        # Same but for status
        ## ---------- Function of Brewing or Completed
        # statusOptions <-
        
        div(
          awesomeCheckboxGroup(
            inputId = ns("beerSelect"),
            label = "Choose by Beer", 
            choices = beerOptions,
            selected = "All",
            inline = TRUE, 
            status = "primary"
          ),
          awesomeCheckboxGroup(
            inputId = ns("statusSelect"),
            label = "Choose by Status", 
            choices = c("Brewing", "Completed"),
            selected = "All",
            inline = TRUE, 
            status = "primary"
          )
        )
      })
      
      output$beerPlot <- renderPlotly({})
      
    }
  )
}