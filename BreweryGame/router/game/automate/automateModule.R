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
      
      tempMatQ <- reactiveValues()
      tempMatR <- reactiveValues()
      tempMatS <- reactiveValues()
      
      tempBeerR <- reactiveValues()
      
      for (i in 1:nrow(materialInfo)) {
        updateValMat[[paste0("material", materialInfo[i, "name"])]] <- T
        tempMatQ[[paste0("material", materialInfo[i, "name"])]] <- 0
        tempMatR[[paste0("material", materialInfo[i, "name"])]] <- 0
        tempMatS[[paste0("material", materialInfo[i, "name"])]] <- NA
      }
      
      for (i in 1:nrow(beerInfo)) {
        updateValBeer[[paste0("beer", beerInfo[i, "name"])]] <- T
        tempBeerR[[paste0("beer", beerInfo[i, "name"])]] <- 0
      }
      
      observeEvent(input$allAuto, {
        if(!is.null(input$allAuto)) {
          if(input$allAuto) {
            matLogical <- createLogicalList(updateValMat, materialInfo, "material")
            beerLogical <- createLogicalList(updateValBeer, beerInfo, "beer")
            if(any(matLogical == T) | any(beerLogical == T)) {
              AUTO$all <- F
              return(
                sendSweetAlert(
                  session = session,
                  title = "Parameters not set for Automation!",
                  type = "warning"
                )
              )
            }
          }
          
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
        click <- input$allAuto
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
          if(input$materialAuto) {
            matLogical <- createLogicalList(updateValMat, materialInfo, "material")
            if(any(matLogical == T)) {
              AUTO$material <- F
              return(
                sendSweetAlert(
                  session = session,
                  title = "Parameters not set for Automation!",
                  type = "error"
                )
              )
            }
          }
          AUTO$material <- input$materialAuto
        }
      })
      
      observeEvent(input$beerAuto, {
        if(!is.null(input$beerAuto)) {
          if(input$beerAuto) {
            beerLogical <- createLogicalList(updateValBeer, beerInfo, "beer")
            if(any(beerLogical == T)) {
              AUTO$beer <- F
              return(
                sendSweetAlert(
                  session = session,
                  title = "Parameters not set for Automation!",
                  type = "error"
                )
              )
            }
          }
          
          AUTO$beer <- input$beerAuto
        }
      })
      
      observeEvent(input$serveAuto, {
        if(!is.null(input$serveAuto)) {
          AUTO$serveCust <- input$serveAuto
        }
      })
      
      observeEvent(input$storeAuto, {
        if(!is.null(input$storeAuto)) {
          AUTO$beerStore <- input$storeAuto
        }
      })
      
      output$autoMaterial <- renderUI({
        click <- input$materialAuto
        materialSwitch(
          inputId = ns("materialAuto"),
          label = "Auto Material Order", 
          value = (AUTO$material | AUTO$all),
          status = "success",
          right=T
        )
      })
      
      output$autoBeer <- renderUI({
        click <- input$beerAuto
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

            if(!is.na(AUTO$materialAuto[matIdx, "supplier"])) {
              supplierChosen <- AUTO$materialAuto[matIdx, "supplier"]
            } else if (!is.na(tempMatS[[paste0("material", materialInfo[i, "name"])]])) {
              supplierChosen <- tempMatS[[paste0("material", materialInfo[i, "name"])]]
            } else {
              supplierChosen <- ""
            }
            
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
          
          if(AUTO$materialAuto[matIdx, "reorderQuantity"] != 0) {
            curValue <- AUTO$materialAuto[matIdx, "reorderQuantity"]
          } else if (tempMatQ[[paste0("material", materialInfo[i, "name"])]] != 0) {
            curValue <- tempMatQ[[paste0("material", materialInfo[i, "name"])]]
          } else {
            curValue <- 0
          }
          
          if(updateValMat[[paste0("material", materialInfo[i, "name"])]]) {
            return(numericInput(
              ns(paste0("reQty", materialInfo[i, "name"], "Input")),
              label=NULL,
              value=curValue, min=1, step=1
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
            
            if(AUTO$materialAuto[matIdx, "reorderPoint"] != 0) {
              curValue <- AUTO$materialAuto[matIdx, "reorderPoint"]
            } else if (tempMatR[[paste0("material", materialInfo[i, "name"])]] != 0) {
              curValue <- tempMatR[[paste0("material", materialInfo[i, "name"])]]
            } else {
              curValue <- 0
            }
            
            return(numericInput(
              ns(paste0("rePt", materialInfo[i, "name"], "Input")),
              label=NULL,
              value=curValue, min=1, step=1
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
          c(tempMatQ, tempMatR, tempMatS, tempBeerR) %<-% updateTemp(input, materialInfo, beerInfo, tempMatQ, tempMatR, tempMatS, tempBeerR)
          
          supChosen <- input[[paste0("supplier", matName, "Input")]]
          reQty <- input[[paste0("reQty", matName, "Input")]]
          rePt <- input[[paste0("rePt", matName, "Input")]]
          
          if(supChosen == "") {
            return(
            sendSweetAlert(
              session = session,
              title = "No Supplier Chosen !!!",
              text = NULL,
              type = "warning"
            ))
          }
          
          if(as.integer(reQty) != reQty) {
            return(
              sendSweetAlert(
                session = session,
                title = "Set an Integer Value for Reorder Quantity",
                text = NULL,
                type = "warning"
              ))
          }
          
          if(as.integer(rePt) != rePt) {
            return(
              sendSweetAlert(
                session = session,
                title = "Set an Integer Value for Reorder Point",
                text = NULL,
                type = "warning"
              ))
          }
          
          if(rePt < 0 | reQty < 0) {
            return(
              sendSweetAlert(
                session = session,
                title = "Can't Set a Negative Value",
                text = NULL,
                type = "warning"
              ))
          }
          
          updateValMat[[paste0("material", matName)]] <- F
          
          matIdx <- which(AUTO$materialAuto$name == matName)
          
          AUTO$materialAuto[matIdx, "supplier"] <- supChosen
          
          AUTO$materialAuto[matIdx, "reorderQuantity"] <- reQty
          AUTO$materialAuto[matIdx, "reorderPoint"] <- rePt
          
          if(reQty == 0) {
            sendSweetAlert(
              session = session,
              title = "Material will not be reordered",
              text = "Reorder Quantity set to 0",
              type = "info"
            )
          }
          
        })
      })
      
      lapply(1:nrow(materialInfo), function(i) {
        observeEvent(input[[paste0("updateVal", materialInfo[i, "name"])]], {
          c(tempMatQ, tempMatR, tempMatS, tempBeerR) %<-% updateTemp(input, materialInfo, beerInfo, tempMatQ, tempMatR, tempMatS, tempBeerR)
          
          updateValMat[[paste0("material", materialInfo[i, "name"])]] <- T
        })
      })
      
      output$beerInput <- renderUI({
        div(
          tags$table(class="table table-sm",
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
            
            if(AUTO$beerAuto[beerIdx, "reorderPoint"] != 0) {
              curValue <- AUTO$beerAuto[beerIdx, "reorderPoint"]
            } else if (tempBeerR[[paste0("beer", beerInfo[i, "name"])]] != 0) {
              curValue <- tempBeerR[[paste0("beer", beerInfo[i, "name"])]]
            } else {
              curValue <- 0
            }
            
            return(numericInput(
              ns(paste0("rePt", beerInfo[i, "name"], "Input")),
              label=NULL,
              value=curValue, min=1, step=1
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
          c(tempMatQ, tempMatR, tempMatS, tempBeerR) %<-% updateTemp(input, materialInfo, beerInfo, tempMatQ, tempMatR, tempMatS, tempBeerR)
          
          rePt <- input[[paste0("rePt", beerInfo[i, "name"], "Input")]]
          
          if(as.integer(rePt) != rePt) {
            return(
              sendSweetAlert(
                session = session,
                title = "Set an Integer Value for Reorder Point",
                text = NULL,
                type = "warning"
              ))
          }
          
          if(rePt < 0) {
            return(
              sendSweetAlert(
                session = session,
                title = "Can't Set a Negative Value",
                text = NULL,
                type = "warning"
              ))
          }
          
          updateValBeer[[paste0("beer", beerInfo[i, "name"])]] <- F
          
          beerIdx <- which(AUTO$beerAuto$name == beerInfo[i, "name"])

          AUTO$beerAuto[beerIdx, "reorderPoint"] <- rePt
          
          
        })
      })
      
      lapply(1:nrow(beerInfo), function(i) {
        observeEvent(input[[paste0("updateVal", beerInfo[i, "name"])]], {
          c(tempMatQ, tempMatR, tempMatS, tempBeerR) %<-% updateTemp(input, materialInfo, beerInfo, tempMatQ, tempMatR, tempMatS, tempBeerR)
          
          updateValBeer[[paste0("beer", beerInfo[i, "name"])]] <- T
        })
      })
      return(AUTO)
    }
  )
}