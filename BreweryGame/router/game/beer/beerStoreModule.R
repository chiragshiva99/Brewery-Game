beerStoreModuleUI <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("autoSwitch")),
    uiOutput(ns("tankStore"))
  )

}

beerStoreModuleServer <- function(id, beer, material, AUTO) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      storeAuto <- reactive({FALSE})
      
      observeEvent(input$beerStoreAuto, {
        if(!is.null(input$beerStoreAuto)) {
          AUTO$beerStore <- input$beerStoreAuto
        }
      })
      
      output$autoSwitch <- renderUI({
        materialSwitch(
          inputId = ns("beerStoreAuto"),
          label = "Auto Storage", 
          value = AUTO$beerStore,
          status = "success",
          right=T
        )
      })
      
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

        if ((nrow(finishTanks) > 0) & (length(res) > 0)) {
          for (i in 1:nrow(finishTanks)) {
            tank <- finishTanks[i, "Tank"]
            print(paste("i:"))
            print(res[[i]])
            if(!is.null(res[[i]])) {
              if(res[[i]] > 0) {
                beerIdx <- which(beer$beerInv["name"] == beer$tanks[tank, "Beer"])
                beer$beerInv[beerIdx, "qty"] <- beer$beerInv[beerIdx, "qty"] + beer$tanks[tank, "tankSize"]
                
                beer$tanks[tank, "Beer"] <- "Empty"
                beer$tanks[tank, "DaysInTank"] <- NA
                beer$tanks[tank, "daysToComplete"] <- NA
              }
            } 
          }
        }
      })
      
      return(AUTO)
    }
  )
}