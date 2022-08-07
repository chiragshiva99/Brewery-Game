materialPlotModuleUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    dropdownButton(
      
      tags$h3("List of Inputs"),
      htmlOutput(ns("materialInput")),
      
      circle = F, status = "primary",
      icon = icon("gear"), width = "300px",
      tooltip = tooltipOptions(title = "Click to see inputs !")
    ),
    plotlyOutput(ns("materialPlot"))
  )
}

materialPlotModuleServer <- function(id, material, materialInfo, statusInfo){
  moduleServer(
    id, 
    function(input, output, session){
      ns <- session$ns
      
      materialInfo <- getMaterialInfo()
      # statuslInfo <-
      
      output$materialInput <- renderUI({
        ## Get options to put in checkboxGroup
        materialOptions <- materialInfo[, "name"]
        ## Add the select All option
        materialOptions <- c("All", materialOptions)
        
        # Same but for status
        ## ---------- Function of Brewing or Completed
        # statusOptions <-
        
        div(
          awesomeCheckboxGroup(
            inputId = ns("materialSelect"),
            label = "Choose by Material", 
            choices = materialOptions,
            selected = "All",
            inline = TRUE, 
            status = "primary"
          ),
          awesomeCheckboxGroup(
            inputId = ns("materialSelect"),
            label = "Choose by Status", 
            choices =  c("Yet to arrive", "Arrived"),  #check
            selected = "All",
            inline = TRUE, 
            status = "primary"
          )
        )
      })
      
      output$materialPlot <- renderPlotly({})
      
    }
  )
}


