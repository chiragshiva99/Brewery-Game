demandPlotModuleUI <- function(id) {
  ns <- NS(id)
  box(width=12,
      collapsed = T,
      title="Beer Demand",
      fluidRow(
        column(width=1,
               dropdownButton(
                 
                 tags$h3("List of Inputs"),
                 htmlOutput(ns("demandInput")),
                 
                 circle = F, status = "primary",
                 icon = icon("gear"), width = "300px",
                 tooltip = tooltipOptions(title = "Click to see inputs !")
        ),
        column(width=6,
               htmlOutput(ns("downloadOption"))
            ),
          ),
      ),
      plotlyOutput(ns("demandPlot"))
  )
}

demandPlotModuleServer <- function(id, stateData, beerInfo, customerInfo) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$downloadOption <- renderUI({
        if(nrow(stateData$demand) > 0 ) {
          downloadBttn(ns('downloadData'), 'Download', style="bordered", size="sm")
        }
      })
      
      output$downloadData <- downloadHandler(
        filename=function() {
          paste0('demandData-Day-',max(stateData$cash$gameDay))
        },
        content=function(con) {
          demandData <- demand %>% left_join(customerInfo, by=c("customerID")) %>% rename(customerName=name) %>% left_join(beerInfo, by=c("beerID")) %>% rename(beerName=name)
          
          write.csv(demandData, con)
        }
      )
      
      output$demandInput <- renderUI({
        ## Get options to put in checkboxGroup
        beerOptions <- beerInfo[, "name"]
        ## Add the select All option
        beerOptions <- c("All", beerOptions)
        
        # Same but for customers
        customerOptions <- customerInfo[,"name"]
        customerOptions <- c("All", customerOptions)
        
        div(
          radioGroupButtons(
            inputId = ns("group"),
            label = "Color Variable: ",
            choices = c("Beer", "Customer"),
            selected="Beer",
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
          ),
          multiInput(
            inputId = ns("customerSelect"),
            label = "Choose by Customer:", 
            choices = customerOptions,
          )
        )
      })
      
      output$demandPlot <- renderPlotly({
        ## selected is a vector of all the options selected by the user
        group <- input$group
        # Depending on the group selected
        demand <- stateData$demand
        originalData <- demand %>% left_join(customerInfo, by=c("customerID")) %>% rename(customerName=name) %>% left_join(beerInfo, by=c("beerID")) %>% rename(beerName=name)
        
        # Initializes the variables based on the group
        if(is.null(group)) {
          legendTitle <- "Beer"
          selected <- input$beerSelect
          otherSelect <- input$customerSelect
          demandData <- originalData %>% rename(variable1=beerName, variable2=customerName)
        } else if(group == "Beer") {
          legendTitle <- "Beer"
          selected <- input$beerSelect
          otherSelect <- input$customerSelect
          demandData <- originalData %>% rename(variable1=beerName, variable2=customerName)
        } else {
          legendTitle <- "Customer"
          selected <- input$customerSelect
          otherSelect <- input$beerSelect
          demandData <- originalData %>% rename(variable1=customerName, variable2=beerName)
        }
          
        ## Initializes a dataframe of the same type as demandData
        graphData <- demandData[0,]
        
        # Initialize the starting string for the title
        titleText <- "Demand for Beer"
        
        ## This is gonna be very inefficient but I will subset and build graphData up from the ground based on the selected variables
        if(is.null(selected)) {
          # Use the full dataframe
          graphData <- demandData

        } else {
          if(selected[1] == "All") {
            graphData <- demandData
            
          } else {
            graphData <- subset(demandData, variable1 %in% selected)
            
            titleText <- substr(titleText, 1, nchar(titleText) - 4)
            titleText <- paste0(titleText, paste0(selected, collapse=", "), ".")
          }
        
        }
        
        # Final Filter
        # Effectively filter with the other variable
        finalData <- graphData[0,]
        
        if(is.null(otherSelect)) {
          finalData <- graphData
        } else {
          if(otherSelect[1] == "All") {
            finalData <- graphData
          } else {
            finalData <- subset(graphData, variable2 %in% otherSelect)
          }
        }
        
        #generate the plot as desired
        p <- ggplot(data=finalData, mapping=aes(gameDay, quantity, fill=variable1)) +
          geom_bar(stat = "identity", #fill = "#EC9D00"#,color = "black"
          ) +
          labs(title=titleText, 
               x = "Game Day",
               y = "Beer Quantity"
          ) + 
          guides(fill=guide_legend(title=legendTitle)) + darkTheme
        
        ggplotly(p)
      })
    }
  )
}