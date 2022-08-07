demandPlotModuleUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    dropdownButton(
      
      tags$h3("List of Inputs"),
      htmlOutput(ns("demandInput")),
      
      circle = F, status = "primary",
      icon = icon("gear"), width = "300px",
      tooltip = tooltipOptions(title = "Click to see inputs !")
    ),
    plotlyOutput(ns("demandPlot"))
  )
}

demandPlotModuleServer <- function(id, demand, beerInfo, customerInfo) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
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
        titleText <- "Demand for "
        
        ## This is gonna be very inefficient but I will subset and build graphData up from the ground based on the selected variables
        if(is.null(selected)) {
          # Use the full dataframe
          graphData <- demandData
          # Exit the loop
          ## Set the title name to be default
          titleText <- "Demand for Beer"
        } else {
          for(i in selected) {
            # if all is selected, break the loop and just plot
            if(i == "All") {
              # Use the full dataframe
              graphData <- demandData
              # Exit the loop
              ## Set the title name to be default
              titleText <- "Demand for Beer"
              break
            }
            # Subset demandData according to the selected beer and add to graphData
            subsetData <- subset(demandData, variable1 == i)
            
            # If subsetData contains data then rbind
            if(nrow(subsetData) > 0) {
              graphData <- rbind(graphData, subsetData)
            }
            # Add the beername to the title
            titleText <- paste0(titleText, " ", i, ", ")
          }
          # If "All" is not selected, remove comma
          if(selected[1] != "All") {
            # Remove the comma at the end
            titleText <- substr(titleText, 1, nchar(titleText)-2)
          }
        }
        
        # Final Filter
        # Effectively filter with the other variable
        finalData <- graphData[0,]
        
        if(is.null(otherSelect)) {
          finalData <- graphData
        } else {
          for(i in otherSelect) { 
            if( i == "All") {
              finalData <- graphData
            }
            subsetData <- subset(graphData, variable2 == i)
            if(nrow(subsetData) > 0) {
              finalData <- rbind(finalData, subsetData)
            }
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