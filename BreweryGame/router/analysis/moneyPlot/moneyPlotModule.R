moneyPlotModuleUI <- function(id) {
  ns <- NS(id)
  box(width=12,
          collapsed = T,
          title="Money",
          fluidRow(
            column(width=1,
                   dropdownButton(
                     
                     tags$h3("List of Inputs"),
                     htmlOutput(ns("cashInput")),
                     
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

moneyPlotModuleServer <- function(id, stateData) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
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
          
          write.csv(cashData, con)
        }
      )
      
      output$cashInput <- renderUI({
        radioGroupButtons(
          inputId=ns("viewType"),
          label=NULL,
          choiceNames=c("Cash Balance", "Revenue", "Lost Revenue", "Holding Cost"),
          choiceValues=c("cashBalance", "revenue", "lostRev", "holdingCost"),
          selected = "cashBalance",
          justified = TRUE,
          checkIcon = list(
            yes = icon("ok",
                       lib = "glyphicon"))
          
        )
      })
      
      output$plotArea <- renderUI({
        cash <- stateData$cash
        if(nrow(cash) == 0) {
          return(h2("No Data at the moment"))
        } else {
          plotlyOutput(ns("moneyPlot"))
        }
      })

      output$moneyPlot <- renderPlotly({
        type <- input$viewType
        # print(stateData$cash)
        if(is.null(type)) {
          type <- "cashBalance"
        }
        
        if(is.na(type)) {
          type <- "cashBalance"
        }
        
        if(type == "cashBalance") {
          p <- ggplot(stateData$cash, aes(gameDay, cashBalance)) +
            geom_step(size = 1, color = "green") +
            labs(title="Cash Balance", 
                 x = "Game Day",
                 y = "Cash Balance ($)"
            )+darkTheme 
        } else if (type == "revenue") {
          p <- ggplot(stateData$cash, aes(gameDay, revenue)) +
            geom_bar(position="stack", stat="identity", fill = "green") +
            labs(title="Revenue by day", 
                 x = "Game Day",
                 y = "Revenue ($)"
            )+darkTheme 
        } else if (type == "lostRev") {
          p <- ggplot(stateData$cash, aes(gameDay, lostRev)) +
            geom_bar(position="stack", stat="identity", fill = "red") +
            labs(title="Lost Revenue by day", 
                 x = "Game Day",
                 y = "Lost Revenue ($)"
            )+darkTheme 
        } else if (type == "holdingCost") {
          p <- ggplot(stateData$cash, aes(gameDay, holdingCost)) +
            geom_bar(position="stack", stat="identity", fill = "red") +
            labs(title="Holding Cost by day", 
                 x = "Game Day",
                 y = "Holding Cost ($)"
            )+darkTheme 
        }
        
        ggplotly(p)
      })
    }
  )
}