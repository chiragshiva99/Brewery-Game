notEnoughModal <- function(ns) {
  modalDialog(
    title="Not Enough Beer",
    easyClose=T
  )
}

customerDemandModuleUI <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("autoSwitch")),
    uiOutput(ns("custDemand"))
  )

}

customerDemandModuleServer <- function(id, demand, general, beer, beerInfo, customerInfo, customerDemand, AUTO, selected) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$serveCustAuto, {
        if(!is.null(input$serveCustAuto)) {
          AUTO$serveCust <- input$serveCustAuto
        }
      })
      
      output$autoSwitch <- renderUI({
        materialSwitch(
          inputId = ns("serveCustAuto"),
          label = "Auto Serve", 
          value = (AUTO$serveCust | AUTO$all),
          status = "success",
          right=T
        )
      })
      
      output$custDemand <- renderUI({
        if(nrow(demand$dayDemand) == 0) {
          custStuff <- h3("No Customers Waiting")
        } else {
          custStuff <- div(
            tags$table(class="table table-striped table-sm",
              tags$thead(
                tags$tr(
                  tags$th(style="width: 30%",
                          strong("Customer")
                  ),
                  tags$th(style="width: 20%",
                          strong("Beer")
                  ),
                  tags$th(style="width: 10%",
                          strong("Qty")
                  ),
                  tags$th(style="width: 30%",
                          strong("Wait Time")
                  )
                )
              ),
              tags$tbody(
                lapply(1:nrow(demand$dayDemand), function(i) {
                  div(
                  tags$tr(
                    tags$td(style="width: 30%",
                            demand$dayDemand[i, 1]
                    ),
                    tags$td(style="width: 20%",
                           demand$dayDemand[i, 2]
                    ),
                    tags$td(style="width: 10%",
                           demand$dayDemand[i, 3]
                    ),
                    tags$td(style="width: 20%",
                           demand$dayDemand[i, 4]
                    ),
                    tags$td(style="width: 20%",
                            htmlOutput(ns(paste0("button", rownames(demand$dayDemand)[i])))
                    )
                  ),
                  tags$tr(
                    tags$td(colspan="4", style="width: 20%",
                        progressBar(id = ns(paste0("pb", rownames(demand$dayDemand)[i])), value = 100*(as.integer(demand$dayDemand[i, 4])/as.integer(demand$dayDemand[i, "maxWait"])), status = "success", size = "xs")
                    )
                  )
                  )
                })
              )
            )
          )
        }
        return(custStuff)
      })
      
      observe({
        if(nrow(demand$dayDemand) > 0) {
          lapply(1:nrow(demand$dayDemand), function(i) {
            output[[paste0("button", rownames(demand$dayDemand)[i])]] <- renderUI({
              beerType <- demand$dayDemand[i, "Beer"]
              qty <- demand$dayDemand[i, "Quantity"]
              beerIdx <- which(beer$beerInv$name == beerType)
              curBeerInv <- beer$beerInv[beerIdx,"qty"]
              
              if(is.na(qty)) {
                return()
              }
              
              if(identical(numeric(0), curBeerInv)) {
                return()
              }
              if(qty <= curBeerInv) {
                return(
                  actionBttn(
                    inputId=ns(paste0("custServe",rownames(demand$dayDemand)[i])),
                    label="Serve",
                    style="fill",
                    color="warning",
                    size="xs",
                    block=T
                  )
                )
              } else {
                return()
              }
            })
          })
        }
      })
      
      observe({
        if (nrow(demand$dayDemand)> 0) {
          lapply(1:nrow(demand$dayDemand), function(i) {
            observeEvent(input[[paste0("custServe",rownames(demand$dayDemand)[i])]], {
              selected$tab <- "Serve Customers"
              
              # Satisfied?
              demandData <- list()
              c(addToDB, demandData, general, demand, beer) %<-% serveCustomer(i, general, demand, beer, beerInfo, customerInfo, customerDemand)
              
              # Remove
              
              
              ## Add to dayDemandDF
              if(addToDB) {
                demand$dayDemand <- demand$dayDemand[-c(i),]
                demand$dayDemandDF <- rbind(demand$dayDemandDF, demandData)
              } else {
                return(
                  sendSweetAlert(
                    session = session,
                    title = "Not Enough Beer",
                    type = "error"
                  )
                )
              }
            })
          })
        } 
      })
      
      return(AUTO)
    }
  )
}