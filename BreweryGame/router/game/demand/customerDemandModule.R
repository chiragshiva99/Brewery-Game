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

customerDemandModuleServer <- function(id, demand, general, beer, beerInfo, customerInfo, customerDemand, AUTO) {
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
          value = AUTO$serveCust,
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
                           actionBttn(
                             inputId=ns(paste0("custServe",rownames(demand$dayDemand)[i])),
                             label="Serve",
                             style="fill",
                             color="warning",
                             size="xs",
                             block=T
                           )
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
        if (nrow(demand$dayDemand)> 0) {
          res <- lapply(1:nrow(demand$dayDemand), function(i) {
            input[[paste0("custServe",rownames(demand$dayDemand)[i])]]
          })
        } else {
          res <- list()
        }
        served <- c()
        if(length(res) > 0) {
          for(i in 1:length(res)) {
            if(!is.null(res[[i]])) {
              if(res[[i]] > 0) {
                served <- c(served, i)
              }
            } 
          }
        }

        if(length(served) > 0) {
          for(i in served) {
            # Satisfied?
            demandData <- list()
            addToDB <- F

            beerType <- demand$dayDemand[i, "Beer"]
            beerID <- beerInfo[which(beerInfo$name == beerType), "beerID"]
            customerName <- demand$dayDemand[i, "Customer"]
            qty <- demand$dayDemand[i, "Quantity"]
            beerIdx <- which(beer$beerInv$name == beerType)
            if (beer$beerInv[beerIdx,"qty"] >= qty) {
              addToDB <- T
              beer$beerInv[beerIdx, "qty"] <- beer$beerInv[beerIdx, "qty"] - qty
              beerRevenue <- qty*(beerInfo[which(beerInfo$name == beerType), "revenue"] + customerDemand[which((customerDemand$beerName == beerType) & (customerDemand$customerName == customerName)), "revenueExtra"])

              general$money <- general$money + beerRevenue

              general$dayRevenue <- general$dayRevenue + beerRevenue

              demandData$serviceDay <- general$day
              print(demand$dayDemand)
              print(i)
              demand$dayDemand <- demand$dayDemand[-c(i),]
            }

            ## Add to dayDemandDF
            if(addToDB) {
              demandData$gameDay <- general$day
              demandData$beerID <- beerID
              demandData$customerID <- customerInfo[which(customerInfo$name == customerName), "customerID"]
              demandData$quantity <- qty
              demandData$arrivalDay <- demand$dayDemand[i, "arrivalDay"]

              demand$dayDemandDF <- rbind(demand$dayDemandDF, demandData)
            } else {
              showModal(notEnoughModal(ns))
            }
            print(demand$dayDemand)
          }
        }
      })
      
      return(AUTO)
    }
  )
}