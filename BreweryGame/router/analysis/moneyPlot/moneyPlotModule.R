moneyPlotModuleUI <- function(id) {
  ns <- NS(id)
  box(width=12,
          collapsed = T,
          title="Money",
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
            ),
          ),
          plotlyOutput(ns("moneyPlot"))
        )
}

moneyPlotModuleServer <- function(id, stateData) {
  moduleServer(
    id,
    function(input, output, session) {
      output$downloadOption <- renderUI({
        if(nrow(stateData$cash) > 0 ) {
          downloadBttn(ns('downloadData'), 'Download', style="bordered", size="sm")
        }
      })
      
      output$downloadData <- downloadHandler(
        filename=function() {
          paste0('cashData-Day-',max(stateData$cash$gameDay))
        },
        content=function(con) {
          cashData <- stateData$cash
          
          write.csv(beerData, con)
        }
      )
      
      output$moneyPlot <- renderPlotly({
        # print(stateData$cash)
        p <- ggplot(stateData$cash, aes(gameDay, cashBalance)) +
          geom_step(size = 1, color = "green") +
          # geom_hline(mapping=aes(yintercept = 100000), color="grey", size= 0.5, alpha = 0.8) +
          # geom_text(mapping=aes(0, y = 100000,label = "Initial Revenue", vjust = -1, hjust = 0), color = 'white') +
          labs(title="Cash Balance generated everyday", 
               x = "Game Day",
               y = "Cash Balance ($)"
          )+darkTheme 
        
        ggplotly(p)
      })
    }
  )
}