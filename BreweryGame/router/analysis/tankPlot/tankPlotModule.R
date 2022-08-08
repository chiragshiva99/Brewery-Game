tankPlotModuleUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    dropdownButton(
      
      tags$h3("List of Inputs"),
      htmlOutput(ns("tankInput")),
      
      circle = F, status = "primary",
      icon = icon("gear"), width = "300px",
      tooltip = tooltipOptions(title = "Click to see inputs !")
    ),
    plotOutput(ns("tankPlot"))
  )
}

tankPlotModuleServer <- function(id, stateData, beerInfo) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$tankInput <- renderUI({
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
      
      output$tankPlot <- renderPlot({
        tank <- stateData$tank
        print(tank)
        visTank <- createVisTank(tank)
        print(visTank)
        
        beerSelected <- input$beerSelect
        
        visTank <- visTank %>% rename(beerID=event) %>% left_join(beerInfo, by=c("beerID")) %>% rename(Beer=name)
        print(visTank)
        plotData <- visTank
        if(!is.null(beerSelected)) {
          if(beerSelected[1] != "All") {
            plotData <- subset(visTank, Beer %in% beerSelected)
          }
        }
        
        p <- ggplot(data = plotData, mapping = aes(x= start, xend= end, y= group, color=Beer)) +
          geom_dumbbell(size=5,
                        colour_x = "red", colour_xend = "grey",
                        dot_guide=TRUE, dot_guide_size=0.5, alpha= 0.7) +
          labs(title="Usage of Beer Tank", 
               x = "Game Day",
               y = "Tank No."
          ) +
          darkTheme
        
        p
      })
    }
  )
}