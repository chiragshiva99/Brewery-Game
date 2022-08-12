beerPlotModuleUI <- function(id) {
  ns <- NS(id)
  box(
    width=12,
    collapsed = T,
    title="Beer inventory levels",
    fluidRow(
      column(width=1,
             dropdownButton(
               tags$h3("List of Inputs"),
               htmlOutput(ns("beerInput")),
               
               circle = F, status = "primary",
               icon = icon("gear"), width = "300px",
               tooltip = tooltipOptions(title = "Click to see inputs !")
             ),
             ),
      column(width=6,
             htmlOutput(ns("downloadOption"))
             )
      
      
    ),
    htmlOutput(ns("plotArea"))
  )
}

beerPlotModuleServer <- function(id, stateData, beerInfo){
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      output$downloadOption <- renderUI({
        if(nrow(stateData$beer) > 0 ) {
          downloadBttn(ns('downloadData'), 'Download', style="bordered", size="sm")
        }
      })
      
      output$downloadData <- downloadHandler(
        filename=function() {
          paste0('beerData-Day-',max(stateData$cash$gameDay))
        },
        content=function(con) {
          beerData <- beer %>% left_join(beerInfo, by=c("beerID")) %>% rename(Beer=name) %>% select(gameDay, Beer, inventory, inTank)
          
          write.csv(beerData, con)
        }
      )
      
      output$plotArea <- renderUI({
        beer <- stateData$beer
        if(nrow(beer) == 0) {
          return(h2("No Data at the moment"))
        } else {
          plotlyOutput(ns("beerPlot"))
        }
      })
      
      output$beerInput <- renderUI({
        ## Get options to put in checkboxGroup
        beerOptions <- beerInfo[, "name"]
        ## Add the select All option
        beerOptions <- c("All", beerOptions)
        
        div(
          checkboxGroupButtons(
            inputId = ns("statusSelect"),
            label = "Choose by Status",
            choiceNames = c("Both", "Inventory", "Brewing"),
            choiceValues = c("total", "inventory", "inTank"),
            selected = "total",
            justified = TRUE,
            checkIcon = list(
              yes = icon("ok",
                         lib = "glyphicon"))
          ),
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
      
      output$beerPlot <- renderPlotly({
        beer <- stateData$beer
        beerData <- beer %>% left_join(beerInfo, by=c("beerID")) %>% rename(Beer=name)
        
        beerData$total <- beerData$inTank + beerData$inventory
        
        selected <- input$statusSelect
        
        beerSelected <- input$beerSelect
        
        if(is.null(selected)) {
          plotData <- beerData[,c("gameDay", "Beer", "total")]
        } else {
          plotData <- beerData[,c("gameDay", "Beer", selected)]
        }
        
        if(!is.null(beerSelected)) {
          if(beerSelected[1] != "All") {
            plotData <- subset(plotData, Beer %in% beerSelected)
          }
        }

        
        p <- ggplot(plotData, aes(gameDay))
        if ("inventory" %in% selected) {
          p <- p + geom_step(aes(y=inventory, color=Beer), size=1)
        }
        
        if ("inTank" %in% selected) {
          p <- p + geom_step(aes(y=inTank, color=Beer), size=1)
        }
        
        if ("total" %in% selected | is.null(selected)) {
          p <- p + geom_step(aes(y=total, color=Beer), size=1)
        }
          # geom_hline(mapping=aes(yintercept = 50), color="grey", size= 2, alpha = 0.8) +
        p <- p + labs(title="Beer Inventory Level", 
               x = "Game Day",
               y = "Inventory"
          )+darkTheme
        
        ggplotly(p)
      })
      
    }
  )
}