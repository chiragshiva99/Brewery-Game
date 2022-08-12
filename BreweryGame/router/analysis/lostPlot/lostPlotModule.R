lostPlotModuleUI <- function(id) {
  ns <- NS(id)
  box(width=12,
      collapsed = T,
      title="Lost Customers/Orders",
    fluidRow(
      column(width=1,
             dropdownButton(
               
               tags$h3("List of Inputs"),
               htmlOutput(ns("lostInput")),
               
               circle = F, status = "primary",
               icon = icon("gear"), width = "300px",
               tooltip = tooltipOptions(title = "Click to see inputs !")
             ),
      ),
      column(width=3,
             htmlOutput(ns("downloadCustOption"))
      ),
      column(width=3,
             htmlOutput(ns("downloadOrderOption"))
      ),
    ),
    htmlOutput(ns("plotArea"))
  )
}

lostPlotModuleServer <- function(id, stateData, beerInfo, customerInfo){
  moduleServer(
    id, 
    function(input, output, session){
      ns <- session$ns
      
      output$downloadCustOption <- renderUI({
        lostSales <- subset(stateData$demand, serviceDay == -1)
        
        if(nrow(lostSales) > 0 ) {
          downloadBttn(ns('lostcustData'), 'Customers', style="bordered", size="sm")
        }
      })
      
      output$downloadOrderOption <- renderUI({
        lostBeer <- select(stateData$beer, gameDay, beerID, lostSale)
        
        if(nrow(lostBeer) > 0 ) {
          downloadBttn(ns('lostorderData'), 'Orders', style="bordered", size="sm")
        }
      })
      
      output$lostorderData <- downloadHandler(
        filename=function() {
          paste0('lostorderData-Day-',max(stateData$cash$gameDay))
        },
        content=function(con) {
          lostBeer <- stateData$beer %>% left_join(beerInfo, by=c("beerID")) %>% select(gameDay, name, lostSale)
          
          write.csv(lostBeer, con)
        }
      )
      
      output$lostcustData <- downloadHandler(
        filename=function() {
          paste0('lostcustData-Day-',max(stateData$cash$gameDay))
        },
        content=function(con) {
          lostSales <- subset(stateData$demand, serviceDay == -1) %>% left_join(customerInfo, by=c("customerID")) %>% count(gameDay, name) %>% rename(lostSale=n) %>% select(gameDay, lostSale)
          
          write.csv(lostSales, con)
        }
      )
      
      output$lostInput <- renderUI({
        
        div(
          radioGroupButtons(
            inputId = ns("statusSelect"),
            label = "Choose by Status",
            choices = c("Beer", "Customer"),
            selected = "Beer",
            justified = TRUE,
            checkIcon = list(
              yes = icon("ok",
                         lib = "glyphicon"))
          ),
          htmlOutput(ns("furtherDetail"))
        )
      })
      
      output$furtherDetail <- renderUI({

        
        if(input$statusSelect == "Beer") {
          ## Get options to put in checkboxGroup
          options <- beerInfo[, "name"]
          ## Add the select All option
          options <- c("All", options)
          label <- "Choose By Beer"
          
          } else {
            ## Get options to put in checkboxGroup
            options <- customerInfo[, "name"]
            ## Add the select All option
            options <- c("All", options)
            label <- "Choose by Customer"
          }
        
        return(
          awesomeCheckboxGroup(
            inputId = ns("typeSelect"),
            label = label, 
            choices = options,
            selected = "All",
            inline = TRUE, 
            status = "primary"
          )
        )
      })
      
      output$plotArea <- renderUI({
        beer <- stateData$beer
        demand <- stateData$demand
        if(nrow(beer) == 0 & nrow(demand) == 0) {
          return(h2("No Data at the moment"))
        } else {
          plotlyOutput(ns("lostPlot"))
        }
      })
      
      output$lostPlot <- renderPlotly({
        lostSales <- subset(stateData$demand, serviceDay == -1)
        lostBeer <- select(stateData$beer, gameDay, beerID, lostSale)
        
        selected <- input$statusSelect
        furtherSelect <- input$typeSelect
        
        if(is.null(selected)) {
          selected <- "Beer"
        }
        
        if(selected == "Beer") {
          graphData <- lostBeer %>% left_join(beerInfo, by=c("beerID"))
          label = "Beer"
        } else {
          graphData <- lostSales %>% left_join(customerInfo, by=c("customerID")) %>% count(gameDay, name) %>% rename(lostSale=n)
          label = "Customer"
        }
        graphData <- graphData %>% rename(variable=name)
        print(graphData)
        plotData <- graphData
        print(furtherSelect)
        
        if(!is.null(furtherSelect)) {
          if(furtherSelect[1] != "All") {
            plotData <- subset(graphData, variable %in% furtherSelect)
          }
        }
        print(plotData)
        p <- ggplot(data=plotData, mapping=aes(gameDay, lostSale, fill=variable)) +
          geom_bar(position="stack", stat="identity") +
          labs(title="Lost Orders", 
               x = "Game Day",
               y = "Quantity",
               fill = label
          ) + 
          darkTheme
        
        ggplotly(p)
      })
      
    }
  )
}