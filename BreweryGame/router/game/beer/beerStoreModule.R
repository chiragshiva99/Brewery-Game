beerStoreModuleUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("tankStore"))
}

beerStoreModuleServer <- function(id, beer, material) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$tankStore <- renderUI({
        finishTanks <- subset(beer$tanks, (Beer!="Empty") & (DaysInTank >= daysToComplete))
        if(nrow(finishTanks) > 0) {
          buttonStuff <- div(
            lapply(1:nrow(finishTanks), function(i) {
              actionBttn(inputId=ns(paste0("storeTank", finishTanks[i,"Tank"])),
                         label=paste0("Store ", finishTanks[i, "Beer"], " from Tank ", finishTanks[i,"Tank"]),
                         style="jelly"
                         
              )
            }) 
          )
        } else {
          buttonStuff <- div(
            h3("Nothing to Store")
          )
        }
        
        return(buttonStuff)
      })
      
      observe({
        finishTanks <- subset(beer$tanks, Beer!="Empty")
        
        if(nrow(finishTanks) > 0) {
          res <- lapply(1:nrow(finishTanks), function(i) {
            input[[paste0("storeTank", finishTanks[i,"Tank"])]]
          })
        } else {
          res <- list()
        }
        print(paste("res", res))
        print(finishTanks)
        if ((nrow(finishTanks) > 0) & (length(res) > 0)) {
          for (i in 1:nrow(finishTanks)) {
            tank <- finishTanks[i, "Tank"]
            print(paste("i:"))
            print(res[[i]])
            if(!is.null(res[[i]])) {
              if(res[[i]] > 0) {
                print("store beer!!!")
                beerIdx <- which(beer$beerInv["name"] == beer$tanks[tank, "Beer"])
                beer$beerInv[beerIdx, "qty"] <- beer$beerInv[beerIdx, "qty"] + beer$tanks[tank, "tankSize"]
                
                beer$tanks[tank, "Beer"] <- "Empty"
                beer$tanks[tank, "DaysInTank"] <- NA
                beer$tanks[tank, "daysToComplete"] <- NA
              }
            } 
          }
        }
        print(length(res))
        print("response from buttons")
        print(res)

      })
    }
  )
}