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
    function(input, output, session){
      ns <- session$ns
      
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
            choices =  c("All" ,"Inventory", "Orders"),  #check
            selected = "All",
            inline = TRUE, 
            status = "primary"
          )
        )
      })
      
      output$materialPlot <- renderPlotly({
        # print(stateData$mat)
        material <- stateData$mat
        materialData <- material %>% left_join(materialInfo, by=c("materialID")) %>% rename(Material=name)
        
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


