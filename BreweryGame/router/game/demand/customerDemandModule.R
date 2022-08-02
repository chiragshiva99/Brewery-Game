notEnoughModal <- function(ns) {
  modalDialog(
    title="Not Enough Beer",
    easyClose=T
  )
}

customerDemandModuleUI <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("custDemand"))
  )

}

customerDemandModuleServer <- function(id, demand, general, beer, beerInfo, customerInfo, customerDemand) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$custDemand <- renderUI({
        if(nrow(demand$dayDemand) == 0) {
          custStuff <- h3("No Customers Waiting")
        } else {
          custStuff <- div(
            fluidRow(
              column(width=3,
                     strong("Customer")
                     ),
              column(width=2,
                     strong("Beer")
                     ),
              column(width=2,
                     strong("Qty")
                     ),
              column(width=3,
                     strong("Wait Time")
                     )
            ),
            lapply(1:nrow(demand$dayDemand), function(i) {
              fluidRow(
                column(width=3,
                      demand$dayDemand[i, 1]
                       ),
                column(width=2,
                       demand$dayDemand[i, 2]
                       ),
                column(width=2,
                       demand$dayDemand[i, 3]
                       ),
                column(width=2,
                       demand$dayDemand[i, 4]
                       ),
                column(width=3,
                       actionBttn(
                         inputId=ns(paste0("custServe",rownames(demand$dayDemand)[i])),
                         label="Serve",
                         style="stretch",
                         color="warning"
                        )
                       )
              )
            })
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
    }
  )
}