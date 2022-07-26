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
      tanks <-  as.data.frame(matrix(nrow=4, ncol=5))
      colnames(tanks) <- c("Tank", "Beer", "DaysInTank", "daysToComplete", "tankSize")
      for (i in 1:nrow(tanks)){
        tanks[i, "Tank"] <- i
        tanks$Beer <- "Empty"
        tanks$tankSize <- tankSize
      }
      
      rawMatOrder <-  data.frame(matrix(nrow=0, ncol=5))
      colnames(rawMatOrder) <- c("Material", "Quantity", "Days", "Supplier", "daysToComplete")
      
      beerInv <-  getStartQty(1, "beerParameters")
      
      beerInfo <- getBeerInfo()
      
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
      dayDemand <- subset(totalDemand, Day==1)
      if (nrow(dayDemand) > 0){
        dayDemand$Day <- 0
      }
      
      ## Lost Sales Tracking
      lostPerBeer <- data.frame(matrix(nrow=nrow(beerInfo), ncol=2))
      colnames(lostPerBeer) <- c("name", "lostQty")
      lostPerBeer$stockOut <- beerInfo$stockOut
      lostPerBeer$name <- beerInfo$name
      lostPerBeer$lostQty <- 0
      
      ## State Tracking
      emptyStateData <- createStateDataFrame(beerInfo, rawMatQty, tanks)
      
      # Initialize whole gameStateData
      gameStateData <- reactiveValues(state=emptyStateData)
      
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
        dayDemand <- subset(totalDemand, Day==1)
        if (nrow(dayDemand) > 0){
          dayDemand$day <- 0
        }
        
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
        print(gameStateData$state)
        # Initialize dayStateDataFrame
        dayStateData <- list()
        dayStateData$day <- general$day
        for (tank in beer$tanks[, "Tank"]) {
          dayStateData[paste0("tank",as.character(tank))] <- beer$tanks[tank, "Beer"]
        }
        
        #Store inventory levels of Beer
        for (drink in beerInv[, "name"]) {
          #Track the amount in each Tank
          dayStateData[paste0("tank", drink)] <- sum(beer$tanks[which(beer$tanks$Beer == drink), "tankSize"])
          dayStateData[paste0("lost", drink)] <- 0
        }
        
        for (mat in rawMatQty[,"name"]) {
          dayStateData[paste0("inv", mat)] <- material$rawMatQty[which(material$rawMatQty$name == mat), "qty"]
          dayStateData[paste0("order", mat)] <- sum(material$rawMatOrder[which(material$rawMatOrder$Material == mat), "Quantity"])
        }
        ## Advance game as required
        
        general$day <- general$day + 1
        
        ## Increase the number of days for tanks and Order
        beer$tanks$DaysInTank <- beer$tanks$DaysInTank + 1
        material$rawMatOrder$Days <- material$rawMatOrder$Days + 1
        demand$dayDemand$Day <- demand$dayDemand$Day + 1
        
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
        
        ## Satisfy and unsatisfied Demand
        removeDemand <- c()
        unsatisDemand <- c()
        revenue <- 0
        lostRev <- 0
        if (nrow(demand$dayDemand) > 0) {
          for (row in 1:nrow(demand$dayDemand)) {
            # Satisfied?
            beerType <- demand$dayDemand[row, "Beer"]
            customerName <- demand$dayDemand[row, "Customer"]
            qty <- demand$dayDemand[row, "Quantity"]
            beerIdx <- which(beer$beerInv$name == beerType)
            if (beer$beerInv[beerIdx,"qty"] >= qty) {
              beer$beerInv[beerIdx, "qty"] <- beer$beerInv[beerIdx, "qty"] - qty
              beerRevenue <- qty*(beerInfo[which(beerInfo$name == beerType), "revenue"] + customers[which((customers$beerName == beerType) & (customers$customerName == customerName)), "revenueExtra"])
              
              general$money <- general$money + beerRevenue
              
              revenue <- revenue + beerRevenue
              
              removeDemand <- c(removeDemand, row)
            }
            
            # Unsatisfied?
            dayWait <- demand$dayDemand[row, "Day"]
            maxWait <- demand$dayDemand[row, "maxWait"]
            lostIdx <- which(demand$lostPerBeer$name == beerType)

            if (dayWait >= maxWait) {
              unsatisDemand <- c(unsatisDemand, row)
              demand$lostPerBeer[lostIdx, "lostQty"] <- demand$lostPerBeer[lostIdx, "lostQty"] + qty
              beerLostRev <- qty*demand$lostPerBeer[lostIdx, "stockOut"]
              general$money <- general$money - beerLostRev
              lostRev <- lostRev + beerLostRev
              
              dayStateData[paste0("lost",beerType)] <- dayStateData[paste0("lost",beerType)] + qty
              
              removeDemand <- c(removeDemand, row)
            }
          }
        }
        
        if (!vector.is.empty(removeDemand)){
          demand$dayDemand <- demand$dayDemand[-c(removeDemand),]
        }
        
        if (length(unsatisDemand) > 0) {
          demand$lostCust <- demand$lostCust + length(unsatisDemand)
        }
        
        # Add New Demand
        newDemand <- subset(totalDemand, Day==general$day)
        if (nrow(newDemand) > 0) {
          newDemand$Day <-  0
          demand$dayDemand <- rbind(demand$dayDemand, newDemand)
        }
        
        # End game if User is finished
        if (general$day > endDays) {
          USER$finish <- T
        }
        
        #Record User State and Store in DataFrame
        if (general$day <= endDays + 1) {
          dayStateData$cash <- general$money
          dayStateData$revenue <- revenue
          dayStateData$lostRev <- lostRev
          
          for (beer in beerInv[, "name"]) {
            # Track the amount in inventory
            dayStateData[paste0("inv", beer)] <- beerInv[which(beerInv$name == beer), "qty"]
          }
          dayStateData <- rbind(emptyStateData, dayStateData)
          dayStateData <- dayStateData[,colnames(emptyStateData)]
          gameStateData$state <- rbind(gameStateData$state, dayStateData)
          
          
          ## Update Game State to DB
          # updateDayState(dayStateData, USER$gameID, USER$id)
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