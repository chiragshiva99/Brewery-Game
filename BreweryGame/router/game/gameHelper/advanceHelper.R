
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
  print(unsatisDemand)
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
  
  print("UnsatisWorks")
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

completeBeerInTank <- function(beer, AUTO) {
  
  completeTanks <- which(beer$tanks$DaysInTank >= beer$tanks$daysToComplete)
  for (tank in completeTanks) {
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
    
  }
  return(
    list(
      beer,
      AUTO
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