initGameArgs <- function(INIT) {
  
  INIT$tanks <-  getTankDF(INIT$tankOptions$tankNo, INIT$tankOptions$tankSize)
  
  INIT$rawMatOrder <- initRawMatOrder()
  
  INIT$beerInv <-  getStartQty(INIT$condition, "beerParameters")
  
  INIT$beerInfo <- getBeerInfo()
  
  INIT$materialInfo <- getMaterialInfo()
  
  INIT$customerInfo <- getCustomerInfo()
  
  INIT$rawMatQty <- getStartQty(INIT$condition, "materialNames")
  
  INIT$beerReq <- getBeerReq()
  
  INIT$costInfo <- getMaterialCost()
  
  ## Demand Generation
  INIT$customerDemand <- getCustomerDemandData()
  
  INIT$seed <- sample(1:2^15, 1)
  set.seed(INIT$seed)
  
  totalDemand <- generateTotalDemand(INIT$customerDemand, INIT$totalDays)
  INIT$totalDemand <- totalDemand
  dayDemand <- subset(INIT$totalDemand, arrivalDay==INIT$initDay)

  ## Lost Sales Tracking
  INIT$lostPerBeer <- getLostSalesDF(INIT$beerInfo)
  
  ## State Tracking
  INIT$cashState <- createCashStateDF()
  INIT$beerState <- createBeerStateDF()
  INIT$matState <- createMatStateDF()
  INIT$demandState <- createDemandStateDF()
  INIT$tankState <- createTankStateDF()
  
  ## Automate State Storing
  INIT$materialAuto <- createMaterialAuto(INIT$materialInfo)
  INIT$beerAuto <- createBeerAuto(INIT$beerInfo)
  
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
      demandData$arrivalDay <- day
      demandData$serviceDay <- day
      print(formattedData)
      print(demandData)
      formattedData <- rbind(formattedData, demandData)
    }
    
    
    initialDemand <- addToGameState(initialDemand, formattedData) 
  }
  
  # Initialize whole gameStateData
  gameStateData <- reactiveValues(
    beer=INIT$beerState, 
    cash=INIT$cashState, 
    mat=INIT$matState, 
    demand=initialDemand, 
    tank=INIT$tankState)
  
  
  # Reactive Values
  general <- reactiveValues(money=INIT$startingMoney, day=INIT$initDay, dayRevenue=0, action=0, maxAction=3)
  beer <- reactiveValues(tanks=INIT$tanks, beerInv=INIT$beerInv)
  material <- reactiveValues(rawMatOrder=INIT$rawMatOrder, rawMatQty=INIT$rawMatQty)
  demand <- reactiveValues(dayDemand=dayDemand, lostCust=0, lostPerBeer=INIT$lostPerBeer,   dayDemandDF=INIT$demandState)
  AUTO <- reactiveValues(all=F, beerStore=F, serveCust=F, beer=F, material=F, materialAuto=INIT$materialAuto, beerAuto=INIT$beerAuto)
  
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