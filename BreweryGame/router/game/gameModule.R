source("router/game/material/materialModule.R")
source("router/game/beer/beerModule.R")
source("router/game/demand/demandModule.R")

source("router/game/dbHelper.R")
source("router/game/helper.R")
source("router/game/demandHelper.R")
source("router/game/stateHelper.R")

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


gameModuleUI <- function(id, disabled=F) {
  ns <- NS(id)
  tabItem(tabName ="gameTab", class = "active",
          # Application title
          fluidRow(
            box(width=3,
                actionButton(ns("reset"), "Reset Game"),
                htmlOutput(ns("gameStatus"))
            ),
            box(width=3,
                htmlOutput(ns("money"))),
            box(width=3,
                htmlOutput(ns("day"))),
            box(width=3,
                actionButton(ns("advance"), "Advance: Next Day")
            )
          ),
          fluidRow(
            materialModuleUI(ns("material")),
            beerModuleUI(ns("beer")),
            demandModuleUI(ns("demand"))
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
      tanks <-  getTankDF(4, tankSize)
      
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
      customers <- getCustomerData()
      
      totalDemand <- generateTotalDemand(customers, totalDays)
      dayDemand <- subset(totalDemand, arrivalDay==1)

      ## Lost Sales Tracking
      lostPerBeer <- getLostSalesDF(beerInfo)
      
      ## State Tracking
      cashState <- createCashStateDF()
      beerState <- createBeerStateDF()
      matState <- createMatStateDF()
      demandState <- createDemandStateDF()
      tankState <- createTankStateDF()
      
      # Initialize whole gameStateData
      gameStateData <- reactiveValues(beer=beerState, cash=cashState, mat=matState, demand=demandState, tank=tankState)
      
      # Reactive Values
      general <- reactiveValues(money=startingMoney, day=initDay)
      beer <- reactiveValues(tanks=tanks, beerInv=beerInv)
      material <- reactiveValues(rawMatOrder=rawMatOrder, rawMatQty=rawMatQty)
      demand <- reactiveValues(dayDemand=dayDemand, lostCust=0, lostPerBeer=lostPerBeer)
      
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
        
        totalDemand <- generateTotalDemand(customers, totalDays)
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
      })
      
      ## Info params
      output$money <- renderUI({h4(paste("Cash Balance: $", general$money))})
      
      output$day <- renderUI({
        h4(paste("Days:", general$day))
      })
      
      ## Advance Button
      observeEvent(input$advance, {

        lostBeerOrders <- getLostBeerList(beerInfo)
        
        dayDemandDF <- createDemandStateDF()
        
        ## Satisfy and unsatisfied Demand
        removeDemand <- c()
        unsatisDemand <- c()
        revenue <- 0
        lostRev <- 0
        if (nrow(demand$dayDemand) > 0) {
          for (row in 1:nrow(demand$dayDemand)) {
            # Satisfied?
            demandData <- list()
            addToDB <- F
            
            beerType <- demand$dayDemand[row, "Beer"]
            beerID <- beerInfo[which(beerInfo$name == beerType), "beerID"]
            customerName <- demand$dayDemand[row, "Customer"]
            qty <- demand$dayDemand[row, "Quantity"]
            beerIdx <- which(beer$beerInv$name == beerType)
            if (beer$beerInv[beerIdx,"qty"] >= qty) {
              addToDB <- T
              beer$beerInv[beerIdx, "qty"] <- beer$beerInv[beerIdx, "qty"] - qty
              beerRevenue <- qty*(beerInfo[which(beerInfo$name == beerType), "revenue"] + customers[which((customers$beerName == beerType) & (customers$customerName == customerName)), "revenueExtra"])
              
              general$money <- general$money + beerRevenue
              
              revenue <- revenue + beerRevenue
              
              removeDemand <- c(removeDemand, row)
              
              demandData$serviceDay <- general$day
            }
            
            # Unsatisfied?
            dayWait <- demand$dayDemand[row, "Day"]
            maxWait <- demand$dayDemand[row, "maxWait"]
            lostIdx <- which(demand$lostPerBeer$name == beerType)

            if (dayWait > maxWait) {
              addToDB <- T
              unsatisDemand <- c(unsatisDemand, row)
              demand$lostPerBeer[lostIdx, "lostQty"] <- demand$lostPerBeer[lostIdx, "lostQty"] + qty
              beerLostRev <- qty*demand$lostPerBeer[lostIdx, "stockOut"]
              general$money <- general$money - beerLostRev
              lostRev <- lostRev + beerLostRev
              
              lostBeerOrders[[beerID]] <- lostBeerOrders[[beerID]] + qty
              
              removeDemand <- c(removeDemand, row)
              demandData$serviceDay <- -1
            }
            
            ## Add to dayDemandDF
            if(addToDB) {
              demandData$gameDay <- general$day
              demandData$beerID <- beerID
              demandData$customerID <- customerInfo[which(customerInfo$name == customerName), "customerID"]
              demandData$quantity <- qty
              demandData$arrivalDay <- demand$dayDemand[row, "arrivalDay"]
              
              dayDemandDF <- rbind(dayDemandDF, demandData)
            }
          }
        }
        
        
        ## Add demand Data to DB if necessary
        if(nrow(dayDemandDF) > 0) {
          dayDemandDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(dayDemandDF)),dayDemandDF)
          addToTable("demandTrack", dayDemandDF)
        }
        
        if (!vector.is.empty(removeDemand)){
          demand$dayDemand <- demand$dayDemand[-c(removeDemand),]
        }
        
        if (length(unsatisDemand) > 0) {
          demand$lostCust <- demand$lostCust + length(unsatisDemand)
        }
        
        
        #### END OF DAY n ####
        # if(general$day <= endDays) {
        #   updateStatesToDB()
        # }
        day <- general$day
        
        ## UPDATE STATES TO DATABASE
        
        dayCashDF <- createCashStateDF()
        dayBeerDF <- createBeerStateDF()
        dayTankDF <- createTankStateDF()
        dayMatDF <- createMatStateDF()
        
        # Store Cash Statuses
        data <- list()
        data$gameDay <- day
        data$cashBalance <- general$money
        data$revenue <- revenue
        data$lostRev <- lostRev
        
        dayCashDF <- rbind(dayCashDF, data)
        dayCashDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(dayCashDF)), dayCashDF)
        
        addToTable("cashTrack", dayCashDF)
        
        # Store Tank Levels
        
        for (tank in beer$tanks[, "Tank"]) {
          data <- list()
          data$gameDay <- day
          
          data$tankID <- tank
          data$beerID <- beerInfo[which(beerInfo$name == beer$tanks[tank, "Beer"]), "beerID"]
          data$tankSize <- beer$tanks[tank, "tankSize"]
          data$completed <- (beer$tanks[tank, "DaysInTank"] > beer$tanks[tank, "daysToComplete"])
          
          if (!identical(data$beerID,integer(0))) {
            dayTankDF <- rbind(dayTankDF, data)
          }
        }
        print(dayTankDF)
        if(nrow(dayTankDF) > 0) {
          dayTankDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(dayTankDF)), dayTankDF)
          print(dayTankDF)
          addToTable("tankTrack", dayTankDF)
        }

        
        #Store inventory levels of Beer
        
        for (drink in beerInv[, "name"]) {
          data <- list()
          data$gameDay <- day
          
          beerID <- beerInfo[which(beerInfo$name == drink), "beerID"]
          
          data$beerID <- beerID
          
          data$inventory <- beer$beerInv[which(beer$beerInv$name == drink), "qty"]
          
          data$inTank <- sum(beer$tanks[which(beer$tanks$Beer == drink), "tankSize"])
          
          data$lostSale <- lostBeerOrders[[beerID]]
          
          dayBeerDF <- rbind(dayBeerDF, data)
          
        }
        dayBeerDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(dayBeerDF)), dayBeerDF)
        addToTable("beerTrack", dayBeerDF)
        
        # Store Material Data
        for (mat in rawMatQty[,"name"]) {
          data <- list()
          
          data$gameDay <- day
          
          materialID <- materialInfo[which(materialInfo$name == mat), "materialID"]
          data$materialID <- materialID
          
          data$inventory <- material$rawMatQty[which(material$rawMatQty$name == mat), "qty"]
          
          data$inTransit <- sum(material$rawMatOrder[which(material$rawMatOrder$Material==mat), "Quantity"])
          
          dayMatDF <- rbind(dayMatDF, data)
        }
        dayMatDF <- dayMatDF[,colnames(createMatStateDF())]
        dayMatDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(dayMatDF)), dayMatDF)
        addToTable("materialTrack", dayMatDF)
        
        ## Add all to gameStateData
        gameStateData$beer <- addToGameState(gameStateData$beer, dayBeerDF)
        gameStateData$mat <- addToGameState(gameStateData$mat, dayMatDF)
        gameStateData$demand <- addToGameState(gameStateData$demand, dayDemandDF)
        gameStateData$cash <- addToGameState(gameStateData$cash, dayCashDF)
        gameStateData$tank <- addToGameState(gameStateData$tank, dayTankDF)
        

        #### START OF DAY n+1 ####
        
        
        ## Advance game as required
        
        general$day <- general$day + 1
        
        ## Increase the number of days for tanks and Order
        beer$tanks$DaysInTank <- beer$tanks$DaysInTank + 1
        material$rawMatOrder$Days <- material$rawMatOrder$Days + 1
        demand$dayDemand$Day <- demand$dayDemand$Day + 1
        
        # Add New Demand
        newDemand <- subset(totalDemand, arrivalDay==general$day)
        if (nrow(newDemand) > 0) {
          demand$dayDemand <- rbind(demand$dayDemand, newDemand)
        }
        
        
        ## Add completed Beers
        completeTanks <- which(beer$tanks$DaysInTank >= beer$tanks$daysToComplete)
        for (tank in completeTanks) {
          beerIdx <- which(beer$beerInv["name"] == beer$tanks[tank, "Beer"])
          beer$beerInv[beerIdx, "qty"] <- beer$beerInv[beerIdx, "qty"] + beer$tanks[tank, "tankSize"]
          
          beer$tanks[tank, "Beer"] <- "Empty"
          beer$tanks[tank, "DaysInTank"] <- NA
          beer$tanks[tank, "daysToComplete"] <- NA
        }
        
        ## Add completed Raw Material Orders
        completeOrders <- which(material$rawMatOrder$Days >= material$rawMatOrder$daysToComplete)
        for (order in completeOrders) {
          matIdx <- which(material$rawMatQty["name"] == material$rawMatOrder[order, "Material"])
          material$rawMatQty[matIdx, "qty"] <- material$rawMatQty[matIdx, "qty"] + material$rawMatOrder[order, "Quantity"]
        }
        if (! vector.is.empty(completeOrders) ){
          material$rawMatOrder <- material$rawMatOrder[-c(completeOrders),]
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
      
      ## Tanks and Beers
      disabled <- reactive(USER$finish)
      observeEvent(USER$finish, {
        print("USER outer scope")
        print(USER$finish)
        disabled <- USER$finish
      })
      beerModuleServer("beer", beer, material, beerInfo, beerReq, disabled)

      ## Raw Material
      materialModuleServer("material", material, general, costInfo, disabled)
      
      ## Demand
      demandModuleServer("demand", demand, disabled)
      
      return(gameStateData)
    }
  )
}