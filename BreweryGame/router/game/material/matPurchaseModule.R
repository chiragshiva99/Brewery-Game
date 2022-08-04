matPurchaseModuleUI <- function(id, materialOptions) {
  ns <- NS(id)
  div(
    selectInput(ns("matChosen"), "Choose a Material", choices=materialOptions),
    htmlOutput(ns("supplierCompare")),
    uiOutput(ns("supplierInput")),
    numericInput(ns("purchQty"), "Enter a Quantity to Purchase", value=0, min=1, step=1),
    htmlOutput(ns("costOfPurchase")),
    actionButton(ns("purchaseok"), "Confirm Purchase")
  )
}

matPurchaseModuleServer <- function(id, general, material, costInfo, disabled) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$supplierCompare <- renderTable({
        supplierInfo <- costInfo %>% subset(materialName==input$matChosen) %>% select(-materialName) %>% rename(Supplier=supplierName, "Order Cost"=fixedCost, "Unit Cost"=variableCost, "Lead Time"=daysToComplete)
        supplierInfo
      })
      
      output$supplierInput <- renderUI({
        supplierInfo <- costInfo %>% subset(materialName==input$matChosen)
        
        selectInput(ns("supplierChosen"), "Choose a Supplier", choices=supplierInfo[,"supplierName"])
      })
      
      output$costOfPurchase <- renderUI({
        shinyjs::disable("purchaseok")
        amt <- calculateCost(costInfo, input$matChosen, input$supplierChosen, input$purchQty)
        print(amt)
        if (is.na(amt)) {
          text <- "Please input a value"
        } else if (input$purchQty != as.integer(input$purchQty)){
          text <- "Please enter an Integer value"
        } else if (amt <= general$money) {
          text <-  paste("Amount:", amt)
          shinyjs::enable("purchaseok")
        } else {
          text <- "Not Enough Money to purchase!"
        }
        text 
      })
      
      observeEvent(input$purchaseok, {
        c(general, material) %<-% orderMaterial(general, material, costInfo, input$matChosen, input$purchQty, input$supplierChosen)
        removeModal()
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