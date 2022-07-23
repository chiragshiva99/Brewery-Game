source("materialHelper.R")

purchaseModal <- function(session, materialOptions) {
  ns <- session$ns
  modalDialog(
    title="Purchase of Raw Materials",
    selectInput(ns("matChosen"), "Choose a Material", choices=materialOptions),
    footer=tagList(
      modalButton("Cancel"),
      actionButton(ns("selectSupplier"), "Next: Select Supplier"))
  )
}

supplierModal <- function(session, supplierOptions, matChosen) {
  ns <- session$ns
  modalDialog(
    title=paste0("Select a Supplier to buy ", matChosen, " from"),
    htmlOutput(ns("supplierCompare")),
    selectInput(ns("supplierChosen"), "Choose a Supplier", choices=supplierOptions),
    numericInput(ns("purchQty"), "Enter a Quantity to Purchase", value=0, min=1, step=1),
    htmlOutput(ns("costOfPurchase")),
    footer=tagList(
      modalButton("Cancel"),
      actionButton(ns("selectMat"), "Previous: Select Material"),
      actionButton(ns("purchaseok"), "Confirm Purchase")
    )
  )
}

materialModuleUI <- function(id) {
  ns <- NS(id)
  column(width=4,
         box(
           width=NULL,
           title="Raw Materials", 
           htmlOutput(ns("rawMatQty"))
         ),
         box(
           width=NULL,
           title="Material Orders",
           actionButton(ns("purchase"), "Purchase"),
           htmlOutput(ns("currentOrders"))
         )
  )
  
}

materialModuleServer <- function(id, material, general, costInfo) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$purchase, {
        removeModal()
        showModal(purchaseModal(session, unique(costInfo[,"materialName"])))
      })
      
      observeEvent(input$selectMat, {
        removeModal()
        showModal(purchaseModal(session, unique(costInfo[,"materialName"])))
      })
      
      
      output$supplierCompare <- renderTable({
        supplierInfo <- costInfo %>% subset(materialName==input$matChosen) %>% select(-materialName)
        supplierInfo
      })
      
      observeEvent(input$selectSupplier, {
        removeModal()
        supplierInfo <- costInfo %>% subset(materialName==input$matChosen)
        showModal(supplierModal(session, supplierInfo[,"supplierName"], input$matChosen))
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
      
      output$rawMatQty <- renderTable({
        material$rawMatQty
      })
      
      observeEvent(input$purchaseok, {
        newEntry <- data.frame(Material=input$matChosen, Quantity=input$purchQty, Days=0, Supplier=input$supplierChosen, daysToComplete=costInfo[which(costInfo$materialName == input$matChosen), "daysToComplete"])
        material$rawMatOrder <- rbind(material$rawMatOrder, newEntry)
        general$money <- general$money - calculateCost(costInfo, input$matChosen, input$supplierChosen, input$purchQty)
        removeModal()
      })
      
      output$currentOrders <- renderTable({
        click <- input$purchaseok + input$advance
        select(material$rawMatOrder, -daysToComplete)
        
      })
    }
  )
}