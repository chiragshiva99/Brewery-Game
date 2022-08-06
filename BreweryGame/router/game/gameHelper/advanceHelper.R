
#### DEMAND RELATED ####
serveCustomer <- function(i, general, demand, beer, beerInfo, customerInfo, customerDemand) {
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
  }
  
  demandData$gameDay <- general$day
  demandData$beerID <- beerID
  demandData$customerID <- customerInfo[which(customerInfo$name == customerName), "customerID"]
  demandData$quantity <- qty
  demandData$arrivalDay <- demand$dayDemand[i, "arrivalDay"]
  demandData$serviceDay <- general$day
  
  return(
    list(
      addToDB,
      demandData,
      general,
      demand,
      beer
    )
  )
}

satisfyDemandAuto <- function(general, demand, beer, beerInfo, customerInfo, customerDemand) {
  ## Satisfy and unsatisfied Demand
  removeDemand <- c()

  if (nrow(demand$dayDemand) > 0) {
    for (row in 1:nrow(demand$dayDemand)) {
      # Satisfied?
      c(addToDB, demandData, general, demand, beer) %<-% serveCustomer(row, general, demand, beer, beerInfo, customerInfo, customerDemand)

      ## Add to dayDemandDF
      if(addToDB) {
        removeDemand <- c(removeDemand, row)
        demand$dayDemandDF <- rbind(demand$dayDemandDF, demandData)
      }
    }
  }
  
  if (!vector.is.empty(removeDemand)){
    demand$dayDemand <- demand$dayDemand[-c(removeDemand),]
  }
  
  return(list(
    general,
    demand,
    beer
  ))
}

## Check unsatisfied Customers
checkUnsatisfiedCust <- function(general, demand, beer, beerInfo, customerInfo) {
  lostBeerOrders <- getLostBeerList(beerInfo)
  lostRev <- 0
  
  unsatisDemand <- which(demand$dayDemand$Day > demand$dayDemand$maxWait) 
  
  if(length(unsatisDemand) == 0) {
    return(list(
      general,
      demand,
      beer,
      lostRev,
      lostBeerOrders
    ))
  }
  
  for (i in 1:length(unsatisDemand)) {
    demandData <- list()
    uIdx <- unsatisDemand[i]
    beerType <- demand$dayDemand[uIdx, "Beer"]
    beerID <- beerInfo[which(beerInfo$name == beerType), "beerID"]
    customerName <- demand$dayDemand[uIdx, "Customer"]
    qty <- demand$dayDemand[uIdx, "Quantity"]
    beerIdx <- which(beer$beerInv$name == beerType)
    
    lostIdx <- which(demand$lostPerBeer$name == beerType)
    
    demand$lostPerBeer[lostIdx, "lostQty"] <- demand$lostPerBeer[lostIdx, "lostQty"] + qty
    beerLostRev <- qty*demand$lostPerBeer[lostIdx, "stockOut"]
    general$money <- general$money - beerLostRev
    
    # Tracking Stats
    lostRev <- lostRev + beerLostRev
    lostBeerOrders[[beerID]] <- lostBeerOrders[[beerID]] + qty
    
    ## Add to Database
    demandData$gameDay <- general$day
    demandData$beerID <- beerID
    demandData$customerID <- customerInfo[which(customerInfo$name == customerName), "customerID"]
    demandData$quantity <- qty
    demandData$arrivalDay <- demand$dayDemand[i, "arrivalDay"]
    demandData$serviceDay <- -1
    demand$dayDemandDF <- rbind(demand$dayDemandDF, demandData)
  }
  
  if (length(unsatisDemand) > 0){
    demand$dayDemand <- demand$dayDemand[-unsatisDemand,]
    demand$lostCust <- demand$lostCust + length(unsatisDemand)
  }
  
  return(list(
    general,
    demand,
    beer,
    lostRev,
    lostBeerOrders
  ))
}

#### DATABASE RELATED #### 
generateTankDataToStore <- function(day, beer, beerInfo) {
  dayTankDF <- createTankStateDF()
  for (tank in beer$tanks[, "Tank"]) {
    data <- list()
    data$gameDay <- day
    
    data$tankID <- tank
    data$beerID <- beerInfo[which(beerInfo$name == beer$tanks[tank, "Beer"]), "beerID"]
    data$tankSize <- beer$tanks[tank, "tankSize"]
    data$completed <- as.integer(beer$tanks[tank, "DaysInTank"] >= beer$tanks[tank, "daysToComplete"])
    
    if (!identical(data$beerID,integer(0))) {
      dayTankDF <- rbind(dayTankDF, data)
    }
  }
  
  return(dayTankDF)
}

generateBeerDataToStore <- function(day, beer, beerInfo, lostBeerOrders) {
  dayBeerDF <- createBeerStateDF()
  for (drink in beer$beerInv[, "name"]) {
    data <- list()
    data$gameDay <- day
    
    beerID <- beerInfo[which(beerInfo$name == drink), "beerID"]
    
    data$beerID <- beerID
    
    data$inventory <- beer$beerInv[which(beer$beerInv$name == drink), "qty"]
    
    data$inTank <- sum(beer$tanks[which(beer$tanks$Beer == drink), "tankSize"])
    
    data$lostSale <- lostBeerOrders[[beerID]]
    
    dayBeerDF <- rbind(dayBeerDF, data)
    
  }
  return(dayBeerDF)
}

generateMaterialDataToStore <- function(day, material, materialInfo) {
  dayMatDF <- createMatStateDF()
  for (mat in material$rawMatQty[,"name"]) {
    data <- list()
    
    data$gameDay <- day
    
    materialID <- materialInfo[which(materialInfo$name == mat), "materialID"]
    data$materialID <- materialID
    
    data$inventory <- material$rawMatQty[which(material$rawMatQty$name == mat), "qty"]
    
    data$inTransit <- sum(material$rawMatOrder[which(material$rawMatOrder$Material==mat), "Quantity"])
    
    dayMatDF <- rbind(dayMatDF, data)
  }
  
  return(dayMatDF)
}

completeBeerInTank <- function(beer, AUTO, general) {
  
  completeTanks <- which(beer$tanks$DaysInTank >= beer$tanks$daysToComplete)
  for (tank in completeTanks) {
    if(general$action >= general$maxAction) {
      break
    }
    
    beerIdx <- which(beer$beerInv["name"] == beer$tanks[tank, "Beer"])
    autoIdx <- which(AUTO$beerAuto["name"] == beer$tanks[tank, "Beer"])
    
    beer$beerInv[beerIdx, "qty"] <- beer$beerInv[beerIdx, "qty"] + beer$tanks[tank, "tankSize"]
    
    beer$tanks[tank, "Beer"] <- "Empty"
    beer$tanks[tank, "DaysInTank"] <- NA
    beer$tanks[tank, "daysToComplete"] <- NA
    
    ## Reset if it was brewed automatically
    if(AUTO$beerAuto[autoIdx, "rebrew"]) {
      AUTO$beerAuto[autoIdx, "rebrew"] <- F
    }
    
    ## Add to turn counter
    general$action <- general$action + 1
  }
  return(
    list(
      beer,
      AUTO,
      general
    )
  )
}

completeMaterialOrder <- function(material, AUTO) {
  completeOrders <- which(material$rawMatOrder$Days >= material$rawMatOrder$daysToComplete)
  for (order in completeOrders) {
    matIdx <- which(material$rawMatQty["name"] == material$rawMatOrder[order, "Material"])
    autoIdx <- which(AUTO$materialAuto["name"] == material$rawMatOrder[order, "Material"])
    
    material$rawMatQty[matIdx, "qty"] <- material$rawMatQty[matIdx, "qty"] + material$rawMatOrder[order, "Quantity"]
    
    ## Reset if it was ordered automatically
    if(AUTO$materialAuto[autoIdx, "reorder"]) {
      AUTO$materialAuto[autoIdx, "reorder"] <- F
    }
  }
  if (! vector.is.empty(completeOrders) ){
    material$rawMatOrder <- material$rawMatOrder[-c(completeOrders),]
  }
  
  return(
    list(
      material,
      AUTO
    )
  )
}

advanceDay <- function(USER, AUTO, gameStateData, general, beer, material, demand, INIT) {
  ## Store seed if game started
  print(paste("Advancing for day", general$day))
  if(general$day == INIT$initDay) {
    print(paste("SEED:", INIT$seed))
    updateSeed(USER$id, USER$gameID, INIT$seed)
  }
  
  ## If Auto Serve Customers
  if (AUTO$serveCust | AUTO$all) {
    c(general, demand, beer) %<-% satisfyDemandAuto(general, demand, beer, INIT$beerInfo, INIT$customerInfo, INIT$customerDemand)
  } 
  
  # Progress day for demand to identify unsatisfied demand
  demand$dayDemand$Day <- demand$dayDemand$Day + 1
  
  ## Check for unsatisfied customers and remove them
  c(general, demand, beer, lostRev, lostBeerOrders) %<-% checkUnsatisfiedCust(general, demand, beer, INIT$beerInfo, INIT$customerInfo)
  
  ## Store all relevant data
  ## Add demand Data to DB if necessary
  if(nrow(demand$dayDemandDF) > 0) {
    demand$dayDemandDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(demand$dayDemandDF)),demand$dayDemandDF)
    if(INIT$dbStore) {
      addToTable("demandTrack", demand$dayDemandDF)
    }
  }
  
  day <- general$day
  
  ## UPDATE STATES TO DATABASE
  dayCashDF <- INIT$cashState
  
  ## Get interest
  general$money <- general$money * (1 + (INIT$interestRate/365))
  
  # Store Cash Statuses
  data <- list()
  data$gameDay <- day
  data$cashBalance <- general$money
  data$revenue <- general$dayRevenue
  data$lostRev <- lostRev
  
  dayCashDF <- rbind(dayCashDF, data)
  dayCashDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(dayCashDF)), dayCashDF)
  
  if(INIT$dbStore) {
    addToTable("cashTrack", dayCashDF)
  }

  
  # Store Tank Levels
  
  dayTankDF <- generateTankDataToStore(day, beer, INIT$beerInfo)
  
  if(nrow(dayTankDF) > 0) {
    dayTankDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(dayTankDF)), dayTankDF)
    
    if(INIT$dbStore) {
      addToTable("tankTrack", dayTankDF)
    }
  }
  
  #Store inventory levels of Beer
  
  dayBeerDF <- generateBeerDataToStore(day, beer, INIT$beerInfo, lostBeerOrders)
  
  dayBeerDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(dayBeerDF)), dayBeerDF)

  
  # Store Material Data
  dayMatDF <- generateMaterialDataToStore(day, material, INIT$materialInfo)
  
  dayMatDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(dayMatDF)), dayMatDF)
  
  if(INIT$dbStore) {
    addToTable("beerTrack", dayBeerDF)
    addToTable("materialTrack", dayMatDF)
  }

  
  ## Add all to gameStateData
  gameStateData$beer <- addToGameState(gameStateData$beer, dayBeerDF)
  gameStateData$mat <- addToGameState(gameStateData$mat, dayMatDF)
  gameStateData$demand <- addToGameState(gameStateData$demand, demand$dayDemandDF)
  gameStateData$cash <- addToGameState(gameStateData$cash, dayCashDF)
  gameStateData$tank <- addToGameState(gameStateData$tank, dayTankDF)
  
  if(general$day == INIT$totalDays) {
    updateCashBalance(USER$id, USER$gameID, general$money)
  }
  
  ## Resetting reactive Values
  demand$dayDemandDF <- INIT$demandState
  general$dayRevenue <- 0
  
  #### START OF DAY n+1 ####
  
  ## Advance game as required
  
  ## Reset Action counter
  general$action <- 0
  
  general$day <- general$day + 1
  
  ## Increase the number of days for tanks and Order
  beer$tanks$DaysInTank <- beer$tanks$DaysInTank + 1
  material$rawMatOrder$Days <- material$rawMatOrder$Days + 1
  
  # Add New Demand
  ## Update the demand of system
  
  newDemand <- subset(INIT$totalDemand, arrivalDay==general$day)
  if (nrow(newDemand) > 0) {
    demand$dayDemand <- rbind(demand$dayDemand, newDemand)
  }
  
  ## Complete Tank Orders if automated
  if (AUTO$beerStore | AUTO$all) {
    c(beer, AUTO, general) %<-% completeBeerInTank(beer, AUTO, general)
  }
  
  ## IF auto brew
  ## BREW BEER WHERE IT IS BELOW REORDER QUANTITY
  if (AUTO$beer | AUTO$all) {
    # Check empty tanks
    emptyTanks <- subset(beer$tanks, Beer == "Empty")
    
    # If none Go to next day
    if(nrow(emptyTanks) > 0) {
      # If tanks are empty
      beerOrder <- sample(INIT$beerInfo[, "name"])
      tankOrder <- sample(emptyTanks[,"Tank"])
      # Check if any beers are below reorder quantity
      tankChoice <- 1
      for (i in 1:length(beerOrder)) {
        if(general$action >= general$maxAction) {
          break
        }
        
        beerName <- beerOrder[i]
        beerIdx <- which(beer$beerInv$name == beerName)
        beerAutoIdx <- which(AUTO$beerAuto$name == beerName)
        beerCurInv <- beer$beerInv[beerIdx, "qty"]
        beerReorderPoint <- AUTO$beerAuto[beerAutoIdx, "reorderPoint"]
        
        if ((beerCurInv <= beerReorderPoint) & (!AUTO$beerAuto[beerAutoIdx, "rebrew"])) {
          tankChosen <- tankOrder[tankChoice]
          
          # rebrew if possible
          c(beer$tanks, material$rawMatQty, general) %<-% brewBeer(beer$tanks, tankChosen, beerName, INIT$beerInfo, INIT$beerReq, material$rawMatQty, general)
          
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
  if (AUTO$material | AUTO$all) {
    for (i in 1:nrow(material$rawMatQty)) {
      matName <- material$rawMatQty[i, "name"]
      matCurInv <- material$rawMatQty[i, "qty"]
      matAutoIdx <- which(AUTO$materialAuto$name == matName)
      matReorderPoint <- AUTO$materialAuto[matAutoIdx, "reorderPoint"]
      matReorderQuantity <- AUTO$materialAuto[which(AUTO$materialAuto$name == matName), "reorderQuantity"]
      
      if(matReorderQuantity == 0) {
        next
      }
      
      if ((matCurInv <= matReorderPoint) & (!AUTO$materialAuto[matAutoIdx, "reorder"])) {
        matReorderSupplier <- AUTO$materialAuto[which(AUTO$materialAuto$name == matName), "supplier"]
        # Function does not order if not enough money
        c(general, material) %<-% orderMaterial(general, material, INIT$costInfo, matName, matReorderQuantity, matReorderSupplier)
        
        # Prevent Reorder (notes that reorder is in progress)
        AUTO$materialAuto[matAutoIdx, "reorder"] <- T
      }
    }
  }
  
  # End game if User is finished
  if (general$day > INIT$totalDays) {
    USER$finish <- T
    USER$gameID <- -1
    ### update end of game
    result <- updateGameID(USER$id, USER$gameID)
  }

  
  return(
    list(
      USER, 
      AUTO,
      gameStateData,
      general, 
      beer, 
      material, 
      demand
    )
  )
}