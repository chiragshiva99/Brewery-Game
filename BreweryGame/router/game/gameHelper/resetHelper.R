resetGameArgs <- function(INIT, gameStateData, AUTO, general, beer, material, demand) {

  INIT$seed <- sample(1:2^15, 1)
  set.seed(INIT$seed)
  
  totalDemand <- generateTotalDemand(INIT$customerDemand, INIT$totalDays)
  INIT$totalDemand <- totalDemand
  dayDemand <- subset(INIT$totalDemand, arrivalDay==INIT$initDay)

  # Add relevant demand Info
  initialDemand <- createDemandStateDF()
  for(day in 1:(INIT$initDay - 1)) {
    
    addDayDemand <- subset(INIT$totalDemand, arrivalDay==day)
    
    formattedData <- createDemandStateDF()
    if(nrow(addDayDemand) == 0) {
      next
    }
    
    for(i in 1:nrow(addDayDemand)) {
      demandData <- list()
      beerType <- addDayDemand[i, "Beer"]
      beerID <- INIT$beerInfo[which(INIT$beerInfo$name == beerType), "beerID"]
      customerName <- addDayDemand[i, "Customer"]
      qty <- addDayDemand[i, "Quantity"]
      customerID <- INIT$customerInfo[which(INIT$customerInfo$name == customerName), "customerID"]
      arrivalDay <- addDayDemand[i, "arrivalDay"]
      
      demandData$gameDay <- day
      demandData$beerID <- beerID
      demandData$customerID <- customerID
      demandData$quantity <- qty
      demandData$arrivalDay <- arrivalDay
      demandData$serviceDay <- 0
      formattedData <- rbind(formattedData, demandData)
    }
    
    
    initialDemand <- addToGameState(initialDemand, formattedData) 
  }
  
  # Initialize whole gameStateData
  gameStateData$beer <- INIT$beerState
  gameStateData$cash <- INIT$cashState
  gameStateData$mat <- INIT$matState
  gameStateData$demand <- initialDemand
  gameStateData$tank <- INIT$tankState
  
  
  # Reactive Values
  # General
  general$money <- INIT$startingMoney
  general$day <- INIT$initDay
  general$dayRevenue <- 0
  general$action <- 0
  general$holdingCost <- 0
  general$lostRev <- 0
  
  #beer
  beer$tanks <- INIT$tanks
  beer$beerInv <- INIT$beerInv
  
  #material
  material$rawMatOrder <- INIT$rawMatOrder
  material$rawMatQty <- INIT$rawMatQty
  
  demand$dayDemand <- dayDemand
  demand$lostCust <- 0
  demand$lostPerBeer <- INIT$lostPerBeer
  demand$dayDemandDF <- INIT$demandState
  
  states <- c("all", "beerStore", "serveCust", "beer", "material")
  for (i in states) {
    AUTO[[i]] <- F
  }
  AUTO$materialAuto <- INIT$materialAuto
  AUTO$beerAuto <- INIT$beerAuto
  
  return(list(
    INIT, 
    gameStateData, 
    AUTO, 
    general, 
    beer, 
    material, 
    demand
  ))
}