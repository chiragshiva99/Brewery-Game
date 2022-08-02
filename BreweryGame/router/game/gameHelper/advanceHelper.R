satisfyDemandAuto <- function(beerInfo, demand, beer, general, customerInfo, customerDemand) {
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
        beerRevenue <- qty*(beerInfo[which(beerInfo$name == beerType), "revenue"] + customerDemand[which((customerDemand$beerName == beerType) & (customerDemand$customerName == customerName)), "revenueExtra"])
        
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
  
  return(list(
    demand,
    dayDemandDF,
    beer,
    revenue,
    lostRev,
    lostBeerOrders,
    removeDemand,
    unsatisDemand
  ))
}

generateTankDataToStore <- function(day, beer, beerInfo) {
  dayTankDF <- createTankStateDF()
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

completeBeerInTank <- function(beer) {
  completeTanks <- which(beer$tanks$DaysInTank >= beer$tanks$daysToComplete)
  for (tank in completeTanks) {
    beerIdx <- which(beer$beerInv["name"] == beer$tanks[tank, "Beer"])
    beer$beerInv[beerIdx, "qty"] <- beer$beerInv[beerIdx, "qty"] + beer$tanks[tank, "tankSize"]
    
    beer$tanks[tank, "Beer"] <- "Empty"
    beer$tanks[tank, "DaysInTank"] <- NA
    beer$tanks[tank, "daysToComplete"] <- NA
  }
  return(beer)
}

completeMaterialOrder <- function(material) {
  completeOrders <- which(material$rawMatOrder$Days >= material$rawMatOrder$daysToComplete)
  for (order in completeOrders) {
    matIdx <- which(material$rawMatQty["name"] == material$rawMatOrder[order, "Material"])
    material$rawMatQty[matIdx, "qty"] <- material$rawMatQty[matIdx, "qty"] + material$rawMatOrder[order, "Quantity"]
  }
  if (! vector.is.empty(completeOrders) ){
    material$rawMatOrder <- material$rawMatOrder[-c(completeOrders),]
  }
  
  return(material)
}