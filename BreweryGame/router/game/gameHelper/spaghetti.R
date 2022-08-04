if(general$day == 1) {
  print(seed)
  updateSeed(USER$id, USER$gameID, seed)
}

if (AUTO$serveCust) {
  c(demand, beer, general, lostRev, lostBeerOrders, removeDemand, unsatisDemand) %<-% satisfyDemandAuto(beerInfo, demand, beer, general, customerInfo, customerDemand)
} else {
  lostRev <- 0
  lostBeerOrders <- getLostBeerList(beerInfo)
  removeDemand <- c()
  unsatisDemand <- c()
}


## Add demand Data to DB if necessary
if(nrow(demand$dayDemandDF) > 0) {
  demand$dayDemandDF <- cbind(getBaseData(USER$gameID, USER$id, nrow(demand$dayDemandDF)),demand$dayDemandDF)
  addToTable("demandTrack", demand$dayDemandDF)
}



## Update the demand of system
if(nrow(demand$dayDemand) > 0) {
  print(paste("updating demand for day", general$day))
}

if (!vector.is.empty(removeDemand)){
  demand$dayDemand <- demand$dayDemand[-c(removeDemand),]
}

if (length(unsatisDemand) > 0) {
  demand$lostCust <- demand$lostCust + length(unsatisDemand)
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
demand$dayDemand$Day <- demand$dayDemand$Day + 1

# Add New Demand
newDemand <- subset(totalDemand, arrivalDay==general$day)
if (nrow(newDemand) > 0) {
  demand$dayDemand <- rbind(demand$dayDemand, newDemand)
}

## Complete Tank Orders if automated
print("Tank Store Auto Status")
print(AUTO$beerStore)
print(AUTO$beerStore)
if (AUTO$beerStore) {
  beer <- completeBeerInTank(beer)
}

## Add completed Raw Material Orders
material <- completeMaterialOrder(material)

# End game if User is finished
if (general$day > endDays) {
  USER$finish <- T
  USER$gameID <- -1
  ### update end of game
  result <- updateGameID(USER$id, USER$gameID)
}