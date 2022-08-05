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
  
  INIT$totalDemand <- generateTotalDemand(INIT$customerDemand, INIT$totalDays)
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
  
  # Initialize whole gameStateData
  gameStateData <- reactiveValues(
                          beer=INIT$beerState, 
                          cash=INIT$cashState, 
                          mat=INIT$matState, 
                          demand=INIT$demandState, 
                          tank=INIT$tankState)
  
  # Reactive Values
  general <- reactiveValues(money=INIT$startingMoney, day=INIT$initDay, dayRevenue=0)
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