matPurchaseModuleUI <- function(id, materialOptions) {
  ns <- NS(id)
  div(
    h3("Purchase Material"),
    selectInput(ns("matChosen"), "Choose a Material", choices=materialOptions),
    htmlOutput(ns("supplierCompare")),
    uiOutput(ns("supplierInput")),
    htmlOutput(ns("quantityInput")),
    htmlOutput(ns("costOfPurchase")),
    br(),
    htmlOutput(ns("purchaseButton"))
  )
}

matPurchaseModuleServer <- function(id, general, material, costInfo, disabled, selected) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      setQty <- reactive(0)
      
      output$purchaseButton <- renderUI({
        amt <- calculateCost(costInfo, input$matChosen, input$supplierChosen, input$purchQty)
        
        color <- "success"
        if(is.null(input$purchQty)) {
          color <- "danger"
        } else if (is.na(input$purchQty)){
          color <- "danger"
        } else if((input$purchQty < 1) | (input$purchQty != as.integer(input$purchQty))) {
          color <- "danger"
        }

        if(!is.null(input$purchQty)) {
          if(amt > general$money) {
            color <- "danger"
          }
        }   
        return(
          actionBttn(ns("purchaseok"), "Confirm Purchase", style="jelly", color=color)
        )
      })
      
      output$quantityInput <- renderUI({
        return(
          numericInput(ns("purchQty"), "Enter a Quantity to Purchase", value=setQty(), min=1, step=1)
        )
      })
      
      output$supplierCompare <- renderTable({
        supplierInfo <- costInfo %>% subset(materialName==input$matChosen) %>% select(-materialName) %>% rename(Supplier=supplierName, "Order Cost"=fixedCost, "Unit Cost"=variableCost, "Lead Time"=daysToComplete)
        supplierInfo
      })
      
      output$supplierInput <- renderUI({
        supplierInfo <- costInfo %>% subset(materialName==input$matChosen)
        
        selectInput(ns("supplierChosen"), "Choose a Supplier", choices=supplierInfo[,"supplierName"])
      })
      
      output$costOfPurchase <- renderUI({
        # shinyjs::disable("purchaseok")
        amt <- calculateCost(costInfo, input$matChosen, input$supplierChosen, input$purchQty)
        
        if (is.null(input$purchQty)) {
          text <- "Please input a value"
        } else if (input$purchQty != as.integer(input$purchQty)){
          text <- "Please enter an Integer value"
        } else if (input$purchQty < 1) {
          text <- "Please enter a valid value"
        } else if (amt <= general$money) {
          text <-  paste("Cost:", amt)
          shinyjs::enable("purchaseok")
        } else {
          text <- "Not Enough Money to purchase!"
        }
        h4(text) 
      })
      
      observeEvent(input$purchaseok, {
        
        amt <- calculateCost(costInfo, input$matChosen, input$supplierChosen, input$purchQty)
        if((is.null(input$purchQty)) | (is.na(input$purchQty))) {
          return(
            sendSweetAlert(
              session=session,
              title="Input Invalid!",
              text=NULL,
              type="error"
            )
          )
        }
        
        if((input$purchQty < 1) | (input$purchQty != as.integer(input$purchQty))) {
          return(
            sendSweetAlert(
              session=session,
              title="Quantity Input Invalid!",
              text=NULL,
              type="error"
            )
          )
        }
        
        if(!is.null(input$purchQty)) {
          if(amt > general$money) {
            return(
              sendSweetAlert(
                session=session,
                title="Not Enough Money!",
                text=NULL,
                type="warning"
              )
            )
          }
        }
        
        c(general, material) %<-% orderMaterial(general, material, costInfo, input$matChosen, input$purchQty, input$supplierChosen)
        setQty <- 0
        
        return(
          sendSweetAlert(
            session=session,
            title="Purchased",
            text=paste(input$purchQty, input$matChosen, "bought from", input$supplierChosen),
            type="success"
          )
        )
      })
      
      observeEvent(disabled(), {
        if(disabled()) {
          shinyjs::disable("purchaseok")
        } else {
          shinyjs::enable("purchaseok")
        }
      })
    }
  )
}