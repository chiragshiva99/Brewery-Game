automateModuleUI <- function(id) {
  ns <- NS(id)
  div(
    fluidRow(
      column(width=4,
             uiOutput(ns("autoSwitch"))
      )
    ),
    fluidRow(
      column(width=5,
             uiOutput(ns("autoMaterial")),
             uiOutput(ns("autoBeer"))
      ),
      column(width=6, offset=1,
             uiOutput(ns("autoServe")),
             uiOutput(ns("autoStore"))
      )
    ),
    uiOutput(ns("materialInput")),
    uiOutput(ns("beerInput"))
    
  )
}

automateModuleServer <- function(id, AUTO, materialInfo, beerInfo, costInfo, selected) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      updateValMat <- reactiveValues()
      updateValBeer <- reactiveValues()
      
      for (i in 1:nrow(materialInfo)) {
        updateValMat[[paste0("material", materialInfo[i, "name"])]] <- T
      }
      
      for (i in 1:nrow(beerInfo)) {
        updateValBeer[[paste0("beer", beerInfo[i, "name"])]] <- T
      }
      
      observeEvent(input$allAuto, {
        if(!is.null(input$allAuto)) {
          AUTO$all <- input$allAuto
          AUTO$material <- input$allAuto
          AUTO$beer <- input$allAuto
          AUTO$beerStore <- input$allAuto
          AUTO$serveCust <- input$allAuto
          
          if(AUTO$all) {
            shinyjs::disable("materialAuto")
            shinyjs::disable("beerAuto")
          } else {
            shinyjs::enable("materialAuto")
            shinyjs::enable("beerAuto")
          }
        }
        selected$tab <- "Automate"
      })
      
      output$autoSwitch <- renderUI({
        materialSwitch(
          inputId = ns("allAuto"),
          label = "Automate All", 
          value = AUTO$all,
          status = "success",
          right=T
        )
      })
      
      observeEvent(input$materialAuto, {
        if(!is.null(input$materialAuto)) {
          AUTO$material <- input$materialAuto
        }
        selected$tab <- "Automate"
      })
      
      observeEvent(input$beerAuto, {
        if(!is.null(input$beerAuto)) {
          AUTO$beer <- input$beerAuto
        }
        selected$tab <- "Automate"
      })
      
      observeEvent(input$serveAuto, {
        if(!is.null(input$serveAuto)) {
          AUTO$serveCust <- input$serveAuto
        }
        selected$tab <- "Automate"
      })
      
      observeEvent(input$storeAuto, {
        if(!is.null(input$storeAuto)) {
          AUTO$beerStore <- input$storeAuto
        }
        selected$tab <- "Automate"
      })
      
      output$autoMaterial <- renderUI({
        materialSwitch(
          inputId = ns("materialAuto"),
          label = "Auto Material Order", 
          value = (AUTO$material | AUTO$all),
          status = "success",
          right=T
        )
      })
      
      output$autoBeer <- renderUI({
        materialSwitch(
          inputId = ns("beerAuto"),
          label = "Auto Brewing", 
          value = (AUTO$beer | AUTO$all),
          status = "success",
          right=T
        )
      })
      
      output$autoServe <- renderUI({
        materialSwitch(
          inputId = ns("serveAuto"),
          label = "Auto Serve Customers", 
          value = (AUTO$serveCust | AUTO$all),
          status = "success",
          right=T
        )
      })
      
      output$autoStore <- renderUI({
        materialSwitch(
          inputId = ns("storeAuto"),
          label = "Auto Store Beer", 
          value = (AUTO$beerStore | AUTO$all),
          status = "success",
          right=T
        )
      })
      
      output$materialInput <- renderUI({
        div(
          tags$table(class="table table-striped table-sm",
                     tags$thead(
                       tags$tr(
                         tags$th(style="width: 20%",
                                 strong("Material")
                         ),
                         tags$th(style="width: 30%",
                                 strong("Reorder Quantity")
                         ),
                         tags$th(style="width: 30%",
                                 strong("Reorder Point")
                         )
                       )
                     ),
                     tags$tbody(
                       lapply(1:nrow(materialInfo), function(i) {
                         matName <- materialInfo[i, "name"]
                         
                         div(
                           tags$tr(
                             tags$td(style="width: 20%", rowspan=2,
                                     tags$em(materialInfo[i, "name"])
                             ),
                             tags$td(style="width: 60%", colspan=2,
                                     htmlOutput(ns(paste0("supplier", matName)))
                             ),
                             tags$td(style="width: 20%", rowspan=2,
                                     htmlOutput(ns(paste0("action", matName)))
                                     
                             )
                           ),
                           tags$tr(
                             tags$td(style="width: 30%",
                                     htmlOutput(ns(paste0("reQty", matName)))
                             ),
                             tags$td(style="width: 30%",
                                     htmlOutput(ns(paste0("rePt", matName)))
                             )
                           )
                         )
                       })
                     )
          )
        )
      })
      
      lapply(1:nrow(materialInfo), function(i) {
        output[[paste0("supplier", materialInfo[i, "name"])]] <- renderUI({
          matIdx <- which(AUTO$materialAuto$name == materialInfo[i, "name"])
          
          if(updateValMat[[paste0("material", materialInfo[i, "name"])]]) {
            supplierInfo <- costInfo %>% subset(materialName==materialInfo[i, "name"])
            names <- c("Choose a Supplier" = "", supplierInfo[, "supplierName"])
            
            supplierChosen <- ifelse(!is.na(AUTO$materialAuto[matIdx, "supplier"]), AUTO$materialAuto[matIdx, "supplier"], "")
            print(supplierChosen)
            return(
              selectInput(
                ns(
                paste0("supplier", materialInfo[i, "name"], "Input")), 
                label=NULL, 
                choices=names,
                selected=supplierChosen
                )
            )
          } else {
            return(AUTO$materialAuto[matIdx, "supplier"])
          }
        })
      })
      
      lapply(1:nrow(materialInfo), function(i) {
        output[[paste0("reQty", materialInfo[i, "name"])]] <- renderUI({
          matIdx <- which(AUTO$materialAuto$name == materialInfo[i, "name"])
          
          if(updateValMat[[paste0("material", materialInfo[i, "name"])]]) {
            return(numericInput(
              ns(paste0("reQty", materialInfo[i, "name"], "Input")),
              label=NULL,
              value=AUTO$materialAuto[matIdx, "reorderQuantity"], min=1, step=1
            )
            )
          } else {
            return(AUTO$materialAuto[matIdx, "reorderQuantity"])
          }
        })
      })
      
      lapply(1:nrow(materialInfo), function(i) {
        output[[paste0("rePt", materialInfo[i, "name"])]] <- renderUI({
          matIdx <- which(AUTO$materialAuto$name == materialInfo[i, "name"])
          
          if(updateValMat[[paste0("material", materialInfo[i, "name"])]]) {
            return(numericInput(
              ns(paste0("rePt", materialInfo[i, "name"], "Input")),
              label=NULL,
              value=AUTO$materialAuto[matIdx, "reorderPoint"], min=1, step=1
            )
            )
          } else {
            return(AUTO$materialAuto[matIdx, "reorderPoint"])
          }
        })
      })
      
      lapply(1:nrow(materialInfo), function(i) {
        output[[paste0("action", materialInfo[i, "name"])]] <- renderUI({
          if(updateValMat[[paste0("material", materialInfo[i, "name"])]]) {
            return(
              actionBttn(
                inputId=ns(paste0("submitVal", materialInfo[i, "name"])),
                label="Submit",
                style="fill",
                color="primary",
                size="sm",
                block=T
              )
            )
          } else {
            return(
              actionBttn(
                inputId=ns(paste0("updateVal", materialInfo[i, "name"])),
                label="Update",
                style="fill",
                color="success",
                size="xs",
                block=T
              )
              
            )
          }
        })
      })
      
      lapply(1:nrow(materialInfo), function(i) {
        matName <- materialInfo[i, "name"]
        observeEvent(input[[paste0("submitVal", matName)]], {
          selected$tab <- "Automate"
          if(input[[paste0("supplier", matName, "Input")]] == "") {
            return(
            sendSweetAlert(
              session = session,
              title = "No Supplier Chosen !!!",
              text = NULL,
              type = "warning"
            ))
          }
          
          updateValMat[[paste0("material", matName)]] <- F
          
          matIdx <- which(AUTO$materialAuto$name == matName)
          
          AUTO$materialAuto[matIdx, "supplier"] <- input[[paste0("supplier", matName, "Input")]]
          
          AUTO$materialAuto[matIdx, "reorderQuantity"] <- input[[paste0("reQty", matName, "Input")]]
          AUTO$materialAuto[matIdx, "reorderPoint"] <- input[[paste0("rePt", matName, "Input")]]
          
        })
      })
      
      lapply(1:nrow(materialInfo), function(i) {
        observeEvent(input[[paste0("updateVal", materialInfo[i, "name"])]], {
          selected$tab <- "Automate"
          updateValMat[[paste0("material", materialInfo[i, "name"])]] <- T
        })
      })
      
      output$beerInput <- renderUI({
        div(
          tags$table(class="table table-striped table-sm",
                     tags$thead(
                       tags$tr(
                         tags$th(style="width: 20%",
                                 strong("Beer")
                         ),
                         tags$th(style="width: 30%",
                                 strong("Rebrew Point")
                         )
                       )
                     ),
                     tags$tbody(
                       lapply(1:nrow(beerInfo), function(i) {
                         tags$tr(
                           tags$td(style="width: 20%",
                                   tags$em(beerInfo[i, "name"])
                           ),
                           tags$td(style="width: 30%",
                                   htmlOutput(ns(paste0("rePt", beerInfo[i, "name"])))
                           ),
                           tags$td(style="width: 20%",
                                   htmlOutput(ns(paste0("action", beerInfo[i, "name"])))
                                   
                           )
                         )
                       })
                     )
          )
        )
      })
      
      lapply(1:nrow(beerInfo), function(i) {
        output[[paste0("rePt", beerInfo[i, "name"])]] <- renderUI({
          beerIdx <- which(AUTO$beerAuto$name == beerInfo[i, "name"])
          
          if(updateValBeer[[paste0("beer", beerInfo[i, "name"])]]) {
            return(numericInput(
              ns(paste0("rePt", beerInfo[i, "name"], "Input")),
              label=NULL,
              value=AUTO$beerAuto[beerIdx, "reorderPoint"], min=1, step=1
            )
            )
          } else {
            return(AUTO$beerAuto[beerIdx, "reorderPoint"])
          }
        })
      })
      
      lapply(1:nrow(beerInfo), function(i) {
        output[[paste0("action", beerInfo[i, "name"])]] <- renderUI({
          if(updateValBeer[[paste0("beer", beerInfo[i, "name"])]]) {
            return(
              actionBttn(
                inputId=ns(paste0("submitVal", beerInfo[i, "name"])),
                label="Submit",
                style="fill",
                color="primary",
                size="sm",
                block=T
              )
            )
          } else {
            return(
              actionBttn(
                inputId=ns(paste0("updateVal", beerInfo[i, "name"])),
                label="Update",
                style="fill",
                color="success",
                size="xs",
                block=T
              )
              
            )
          }
        })
      })
      
      lapply(1:nrow(beerInfo), function(i) {
        observeEvent(input[[paste0("submitVal", beerInfo[i, "name"])]], {
          selected$tab <- "Automate"
          updateValBeer[[paste0("beer", beerInfo[i, "name"])]] <- F
          
          beerIdx <- which(AUTO$beerAuto$name == beerInfo[i, "name"])

          AUTO$beerAuto[beerIdx, "reorderPoint"] <- input[[paste0("rePt", beerInfo[i, "name"], "Input")]]
          
        })
      })
      
      lapply(1:nrow(beerInfo), function(i) {
        observeEvent(input[[paste0("updateVal", beerInfo[i, "name"])]], {
          selected$tab <- "Automate"
          updateValBeer[[paste0("beer", beerInfo[i, "name"])]] <- T
        })
      })
      return(AUTO)
    }
  )
}