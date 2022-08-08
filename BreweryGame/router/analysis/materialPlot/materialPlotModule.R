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

materialPlotModuleServer <- function(id, stateData, materialInfo){
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      
      output$materialInput <- renderUI({
        ## Get options to put in checkboxGroup
        materialOptions <- materialInfo[, "name"]
        ## Add the select All option
        materialOptions <- c("All", materialOptions)
        
        div(
          checkboxGroupButtons(
            inputId = ns("statusSelect"),
            label = "Choose by Status",
            choiceNames = c("Both", "Inventory", "Brewing"),
            choiceValues = c("total", "inventory", "inTransit"),
            selected = "total",
            justified = TRUE,
            checkIcon = list(
              yes = icon("ok",
                         lib = "glyphicon"))
          ),
          awesomeCheckboxGroup(
            inputId = ns("materialSelect"),
            label = "Choose by Material", 
            choices = materialOptions,
            selected = "All",
            inline = TRUE, 
            status = "primary"
          )
        )
      })
      
      output$materialPlot <- renderPlotly({
        material <- stateData$mat
        materialData <- material %>% left_join(materialInfo, by=c("materialID")) %>% rename(Material=name)
        materialData$total <- materialData$inTransit + materialData$inventory
        
        selected <- input$statusSelect
        
        materialSelected <- input$materialSelect
        
        if(is.null(selected)) {
          plotData <- materialData[,c("gameDay", "Material", "total")]
        } else {
          plotData <- materialData[,c("gameDay", "Material", selected)]
        }
        
        if(!is.null(materialSelected)) {
          if(materialSelected[1] != "All") {
            plotData <- subset(plotData, Material %in% materialSelected)
          }
        }
        
        
        p <- ggplot(plotData, aes(gameDay))
        if ("inventory" %in% selected) {
          p <- p + geom_step(aes(y=inventory, color=Material), size=1)
        }
        
        if ("inTransit" %in% selected) {
          p <- p + geom_step(aes(y=inTransit, color=Material), size=1)
        }
        
        if ("total" %in% selected | is.null(selected)) {
          p <- p + geom_step(aes(y=total, color=Material), size=1)
        }
        # geom_hline(mapping=aes(yintercept = 50), color="grey", size= 2, alpha = 0.8) +
        p <- p + geom_text(mapping=aes(0, y = 50,label = "Recommended Reorder Point", vjust = -1, hjust = 0), color = 'white') +
          labs(title="Material Inventory Level", 
               x = "Game Day",
               y = "Inventory"
          )+darkTheme
        
        ggplotly(p)
      })
      
    }
  )
}