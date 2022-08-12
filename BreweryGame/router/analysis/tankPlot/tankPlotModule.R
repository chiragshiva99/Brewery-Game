## Tank Plot Module
## Done by Chirag

tankPlotModuleUI <- function(id) {
  ns <- NS(id)
  box(width=12,
      collapsed = T,
      title="Tank Status",
    fluidRow(
      column(width=1,
             dropdownButton(
               
               tags$h3("List of Inputs"),
               htmlOutput(ns("tankInput")),
               
               circle = F, status = "primary",
               icon = icon("gear"), width = "300px",
               tooltip = tooltipOptions(title = "Click to see inputs !")
             ),
      ),
      column(width=6,
             htmlOutput(ns("downloadOption"))
      ),
    ),
    htmlOutput(ns("plotArea"))
  )
}

tankPlotModuleServer <- function(id, stateData, beerInfo) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      ## Download button related
      output$downloadOption <- renderUI({
        if(nrow(stateData$tank) > 0 ) {
          downloadBttn(ns('downloadData'), 'Download', style="bordered", size="sm")
        }
      })
      
      output$downloadData <- downloadHandler(
        filename=function() {
          paste0('tankData-Day-',max(stateData$cash$gameDay))
        },
        content=function(con) {
          tank <- stateData$tank
          visTank <- createVisTank(tank)
          
          visTank <- visTank %>% rename(beerID=event) %>% left_join(beerInfo, by=c("beerID")) %>% rename(Beer=name, Tank=group) %>% select(Tank, Beer, start, end)
          
          write.csv(visTank, con)
        }
      )
      
      ## Filter related features
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
      
      # Plots data if it is available
      output$plotArea <- renderUI({
        tank <- stateData$tank
        if(nrow(tank) == 0) {
          return(h2("No Data at the moment"))
        } else {
          plotOutput(ns("tankPlot"))
        }
      })
      
      # Renders plot
      output$tankPlot <- renderPlot({
        tank <- stateData$tank
        visTank <- createVisTank(tank)
        
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
        print(p)
        p
      })
    }
  )
}