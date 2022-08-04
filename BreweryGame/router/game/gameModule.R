## Customer Modules
source("router/game/demand/customerLostModule.R")

## Inventory Module
source("router/game/invModule.R")
### Inv SubModules
source("router/game/beer/beerInvModule.R")
source("router/game/material/matInvModule.R")

## Action Module
source("router/game/actionModule.R")
### Action SubModules
source("router/game/material/matPurchaseModule.R")
source("router/game/beer/beerBrewModule.R")
source("router/game/demand/customerDemandModule.R")
source("router/game/automate/automateModule.R")

## Progress Module
source("router/game/progressModule.R")
### Progress SubModules
source("router/game/material/matProgModule.R")
source("router/game/beer/beerTankModule.R")
source("router/game/demand/totalDemandModule.R")

source("router/game/material/materialHelper.R")

source("router/game/beer/beerHelper.R")
source("router/game/gameHelper/gameDBHelper.R")
source("router/game/gameHelper/helper.R")
source("router/game/gameHelper/demandHelper.R")
source("router/game/gameHelper/stateHelper.R")
source("router/game/gameHelper/advanceHelper.R")

resetDialog <- function(session) {
  ns <- session$ns
  modalDialog(
    title="Reset?",
    div("Are you sure you want to reset?"),
    footer=tagList(
      modalButton("Cancel"),
      actionButton(ns("resetok"), "Reset")
    )
  )
}

endGameModal <- function(session) {
  ns <- session$ns
  modalDialog(
    title="End of Game",
    div("The game has ended!"),
    footer=tagList(
      actionButton(ns("resetok"), "Play Again!"),
      actionButton(ns("gotoAnalysis"), "Analyse Performance")
    )
  )
}


gameModuleUI <- function(id, disabled=F) {
  ns <- NS(id)
  tabItem(tabName ="gameTab", class = "active",
          # Application title
          fluidRow(
            # column(width=3,
            #     actionBttn(
            #       inputId=ns("reset"), 
            #       label="Reset Game",
            #       style="minimal",
            #       color="default"),
            #     htmlOutput(ns("gameStatus"))
            # ),
            bs4ValueBoxOutput(ns("day"), width=2),
            bs4ValueBoxOutput(ns("money"), width=2),
            column(width=5,
                   customerLostUI(ns("customerLost"))
                   ),
            column(width=3,
                actionBttn(
                  inputId=ns("advance"), 
                  label="Advance: Next Day",
                  style="jelly",
                  color="danger")
            )
          ),
          fluidRow(
            column(
              width=3,
              invModuleUI(ns("inventory"))
            ),
            column(
              width=5,
              actionModuleUI(ns("action"))
            ),
            column(
              width=4,
              progressModuleUI(ns("progress"))
            )
          )
  )
}

gameModuleServer <- function(id, USER) {
  moduleServer(
    id,
    function(input, output, session) {
      ## Initializing stuff
      condition <- 1
      rawMat <- c("Malt", "Hops", "Yeast")
      
      tankSize <- 100
      tanks <-  getTankDF(6, tankSize)
      
      rawMatOrder <- data.frame(matrix(nrow=0, ncol=5))
      colnames(rawMatOrder) <- c("Material", "Quantity", "Days", "Supplier", "daysToComplete")
      
      beerInv <-  getStartQty(1, "beerParameters")
      
      beerInfo <- getBeerInfo()
      
      materialInfo <- getMaterialInfo()
      
      customerInfo <- getCustomerInfo()
      
      rawMatQty <- getStartQty(1, "materialNames")
      
      beerReq <- getBeerReq()
      
      costInfo <- getMaterialCost()
      
      startingMoney <- 100000
      
      totalDays <- 20
      
      endDays <- 20
      
      initDay <- 1
      
      ## Demand Generation
      customerDemand <- getCustomerDemandData()
      
      seed <- sample(1:2^15, 1)
      set.seed(seed)
      
      totalDemand <- generateTotalDemand(customerDemand, totalDays)
      dayDemand <- subset(totalDemand, arrivalDay==1)
      
      ## Lost Sales Tracking
      lostPerBeer <- getLostSalesDF(beerInfo)
      
      ## State Tracking
      cashState <- createCashStateDF()
      beerState <- createBeerStateDF()
      matState <- createMatStateDF()
      demandState <- createDemandStateDF()
      tankState <- createTankStateDF()
      
      ## Automate State Storing
      materialAuto <- createMaterialAuto(materialInfo)
      beerAuto <- createBeerAuto(beerInfo)
      
      # Initialize whole gameStateData
      gameStateData <- reactiveValues(beer=beerState, cash=cashState, mat=matState, demand=demandState, tank=tankState)
      
      # Reactive Values
      general <- reactiveValues(money=startingMoney, day=initDay, dayRevenue=0)
      beer <- reactiveValues(tanks=tanks, beerInv=beerInv)
      material <- reactiveValues(rawMatOrder=rawMatOrder, rawMatQty=rawMatQty)
      demand <- reactiveValues(dayDemand=dayDemand, lostCust=0, lostPerBeer=lostPerBeer,   dayDemandDF=demandState)
      AUTO <- reactiveValues(all=F, beerStore=F, serveCust=F, beer=F, material=F, materialAuto=materialAuto, beerAuto=beerAuto)
      
      # General
      ## Reset Game 
      observeEvent(input$reset, {
        print("resetting")
        showModal(resetDialog(session))
      })
      
      observeEvent(input$resetok, {
        removeModal()
        
        USER$finish <- F
        
        shinyjs::enable("brew")
        shinyjs::enable("purchase")
        shinyjs::enable("advance")
        
        seed <- sample(1:2^15, 1)
        set.seed(seed)
        
        totalDemand <- generateTotalDemand(customerDemand, totalDays)
        dayDemand <- subset(totalDemand, arrivalDay==1)
        
        general$money <- startingMoney
        general$day <- 1
        demand$dayDemand <- dayDemand
        demand$lostCust <- 0
        demand$lostPerBeer <- lostPerBeer
        beer$tanks <- tanks
        beer$beerInv <- beerInv
        material$rawMatOrder <- rawMatOrder
        material$rawMatQty <- rawMatQty
        
        result <- createGame(USER$id)
        if(is.null(result)) {
          print("ERROR in creation")
        } else {
          USER$gameID <- result
        }
        
        ### Assign as current GameID in database
        result <- updateGameID(USER$id, USER$gameID)
      })
      
      observeEvent(input$gotoAnalysis,{
        removeModal()
        USER$selectedTab <- "analysisTab"
      })
      ## Info params
      output$money <- renderbs4ValueBox({
        bs4ValueBox(
          paste("$", as.character(general$money)), 
          "Cash Balance",
          icon=icon("dollar-sign"),
          color="success",
          gradient=T
        )
      })
      
      output$day <- renderbs4ValueBox({
        bs4ValueBox(
          general$day, 
          "Day",
          icon=icon("calendar"),
          color="info",
          gradient=T
        )
      })
      
      ## Advance Button
      observeEvent(input$advance, {
        ## Store seed if game started
        if(general$day == 1) {
          print(seed)
          updateSeed(USER$id, USER$gameID, seed)
        }
        
        ## If Auto Serve Customers
        if (AUTO$serveCust | AUTO$all) {
          c(general, demand, beer) %<-% satisfyDemandAuto(general, demand, beer, beerInfo, customerInfo, customerDemand)
        } 
        print("SATISFIED")
        print(demand$dayDemand)
        demand$dayDemand$Day <- demand$dayDemand$Day + 1
        ## Check for unsatisfied customers and remove them
        c(general, demand, beer, lostRev, lostBeerOrders) %<-% checkUnsatisfiedCust(general, demand, beer, beerInfo, customerInfo)
        print("DEMAND DONE")
        ## Store all relevant data
        ## Add demand Data to DB if necessary
        if(nrow(demand$dayDemandDF) > 0) {
          demand$dayDemandDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(demand$dayDemandDF)),demand$dayDemandDF)
          addToTable("demandTrack", demand$dayDemandDF)
        }
        
        day <- general$day
        
        ## UPDATE STATES TO DATABASE
        dayCashDF <- createCashStateDF()
        
        # Store Cash Statuses
        data <- list()
        data$gameDay <- day
        data$cashBalance <- general$money
        data$revenue <- general$dayRevenue
        data$lostRev <- lostRev
        
        dayCashDF <- rbind(dayCashDF, data)
        dayCashDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(dayCashDF)), dayCashDF)
        
        addToTable("cashTrack", dayCashDF)
        
        # Store Tank Levels
        
        dayTankDF <- generateTankDataToStore(day, beer, beerInfo)
        
        if(nrow(dayTankDF) > 0) {
          dayTankDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(dayTankDF)), dayTankDF)
          print(dayTankDF)
          addToTable("tankTrack", dayTankDF)
        }
        
        #Store inventory levels of Beer
        
        dayBeerDF <- generateBeerDataToStore(day, beer, beerInfo, lostBeerOrders)
        
        dayBeerDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(dayBeerDF)), dayBeerDF)
        addToTable("beerTrack", dayBeerDF)
        
        # Store Material Data
        dayMatDF <- generateMaterialDataToStore(day, material, materialInfo)
        
        dayMatDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(dayMatDF)), dayMatDF)
        addToTable("materialTrack", dayMatDF)
        
        ## Add all to gameStateData
        gameStateData$beer <- addToGameState(gameStateData$beer, dayBeerDF)
        gameStateData$mat <- addToGameState(gameStateData$mat, dayMatDF)
        gameStateData$demand <- addToGameState(gameStateData$demand, demand$dayDemandDF)
        gameStateData$cash <- addToGameState(gameStateData$cash, dayCashDF)
        gameStateData$tank <- addToGameState(gameStateData$tank, dayTankDF)
        
        if(general$day == endDays) {
          updateCashBalance(USER$id, USER$gameID, general$money)
          showModal(endGameModal(session))
        }
        
        ## Resetting reactive Values
        demand$dayDemandDF <- createDemandStateDF()
        general$dayRevenue <- 0
        
        
        #### START OF DAY n+1 ####
        
        ## Advance game as required
        
        general$day <- general$day + 1
        
        ## Increase the number of days for tanks and Order
        beer$tanks$DaysInTank <- beer$tanks$DaysInTank + 1
        material$rawMatOrder$Days <- material$rawMatOrder$Days + 1
        
        # Add New Demand
        ## Update the demand of system
        if(nrow(demand$dayDemand) > 0) {
          print(paste("updating demand for day", general$day))
        }
        
        newDemand <- subset(totalDemand, arrivalDay==general$day)
        if (nrow(newDemand) > 0) {
          demand$dayDemand <- rbind(demand$dayDemand, newDemand)
        }
        
        ## Complete Tank Orders if automated
        if (AUTO$beerStore | AUTO$all) {
          c(beer, AUTO) %<-% completeBeerInTank(beer, AUTO)
        }
        
        ## IF auto brew
        ## BREW BEER WHERE IT IS BELOW REORDER QUANTITY
        if (AUTO$beer | AUTO$all) {
          # Check empty tanks
          emptyTanks <- subset(beer$tanks, Beer == "Empty")
          
          # If none Go to next day
          if(nrow(emptyTanks) > 0) {
            # If tanks are empty
            beerOrder <- sample(beerInfo[, "name"])
            tankOrder <- sample(emptyTanks[,"Tank"])
            # Check if any beers are below reorder quantity
            tankChoice <- 1
            for (i in 1:length(beerOrder)) {
              beerName <- beerOrder[i]
              beerIdx <- which(beer$beerInv$name == beerName)
              beerAutoIdx <- which(AUTO$beerAuto$name == beerName)
              beerCurInv <- beer$beerInv[beerIdx, "qty"]
              beerReorderPoint <- AUTO$beerAuto[beerAutoIdx, "reorderPoint"]
              
              if ((beerCurInv <= beerReorderPoint) & (!AUTO$beerAuto[beerAutoIdx, "rebrew"])) {
                tankChosen <- tankOrder[tankChoice]
                
                # rebrew if possible
                c(beer$tanks, material$rawMatQty) %<-% brewBeer(beer$tanks, tankChosen, beerName, beerInfo, beerReq, material$rawMatQty)
                
                # Prevent further rebrew 
                AUTO$beerAuto[beerAutoIdx, "rebrew"] <- T
                
                tankChoice <- tankChoice + 1
              }
            }
          }
        }
        
        
        ## Add completed Raw Material Orders
        c(material, AUTO) %<-% completeMaterialOrder(material, AUTO)
        
        ## IF auto order
        ## ORDER MATERIALS IF BELOW REORDER QUANTITY
        print("Check on AUTO")
        print(AUTO$material)
        print(AUTO$all)
        if (AUTO$material | AUTO$all) {
          for (i in 1:nrow(material$rawMatQty)) {
            matName <- material$rawMatQty[i, "name"]
            matCurInv <- material$rawMatQty[i, "qty"]
            matAutoIdx <- which(AUTO$materialAuto$name == matName)
            matReorderPoint <- AUTO$materialAuto[matAutoIdx, "reorderPoint"]
            print(matName)
            print(matCurInv)
            print(matReorderPoint)
            if ((matCurInv <= matReorderPoint) & (!AUTO$materialAuto[matAutoIdx, "reorder"])) {
              matReorderQuantity <- AUTO$materialAuto[which(AUTO$materialAuto$name == matName), "reorderQuantity"]
              matReorderSupplier <- AUTO$materialAuto[which(AUTO$materialAuto$name == matName), "supplier"]
              print(matReorderSupplier)
              # Function does not order if not enough money
              c(general, material) %<-% orderMaterial(general, material, costInfo, matName, matReorderQuantity, matReorderSupplier)
              
              # Prevent Reorder (notes that reorder is in progress)
              AUTO$materialAuto[matAutoIdx, "reorder"] <- T
            }
          }
        }
        
        # End game if User is finished
        if (general$day > endDays) {
          USER$finish <- T
          USER$gameID <- -1
          ### update end of game
          result <- updateGameID(USER$id, USER$gameID)
        }
        
      })
      
      observe({
        if(USER$finish) {
          shinyjs::disable("advance")
        } else {
          shinyjs::enable("advance")
        }
      })
      
      output$gameStatus <- renderUI({
        if (general$day > endDays) {
          text <- "Game has Ended!"
        } else {
          text <- ""
        }
        text
      })
      
      ### Inventory
      invModuleServer("inventory", beer, material)
      
      
      ## Tanks and Beers
      disabled <- reactive(USER$finish)
      observeEvent(USER$finish, {
        disabled <- USER$finish
      })
      
      AUTO <- actionModuleServer("action", general, beer, beerInfo, beerReq, material, costInfo, disabled, AUTO, demand, customerInfo, customerDemand, materialInfo)
      
      AUTO <- progressModuleServer("progress", material, beer, demand, general, beerInfo, customerInfo, customerDemand, tanks, AUTO)
      
      customerLostServer("customerLost", demand)
      
      return(list(USER, gameStateData))
    }
  )
}