## Creates dataframes that help in the storing of state

getBaseData <- function(gameID, id, rowCount) {
  df <- data.frame(matrix(nrow=rowCount, ncol=2))
  colnames(df) <- c("gameID", "userID")
  df$gameID <- gameID
  df$userID <- id
  
  df
}

createEmptyDF <- function(colNames) {
  df <- data.frame(matrix(nrow=0, ncol=length(colNames)))
  colnames(df) <- colNames
  
  df
}

initRawMatOrder <- function() {
  return(createEmptyDF(c("Material", "Quantity", "Days", "Supplier", "daysToComplete")))
}

createCashStateDF <- function() {
  return(createEmptyDF( c("gameDay", "cashBalance", "revenue", "lostRev", "holdingCost")))
}

createBeerStateDF <- function() {
  return(createEmptyDF(c("gameDay", "beerID", "inventory", "inTank", "lostSale")))
}

createTankStateDF <- function() {
  return(createEmptyDF(c("gameDay", "tankID", "beerID", "tankSize", "completed")))
}

createMatStateDF <- function() {
  return(createEmptyDF(c("gameDay", "materialID", "inventory", "inTransit")))
}

createDemandStateDF <- function() {
  return(createEmptyDF(c("gameDay", "customerID", "beerID", "quantity", "arrivalDay", "serviceDay")))
}

addToGameState <- function(gameState, input) {
  if(nrow(input) > 0) {
    return(rbind(gameState, input[,colnames(gameState)]))
  } else {
    return(gameState)
  }
}

createStateDataFrame <- function(beerInfo, rawMatQty, tanks) {
  stateCol <- c(1, 1, 1, 1, 3*nrow(beerInfo), 2*nrow(rawMatQty), nrow(tanks))
  stateData <- data.frame(matrix(nrow=0, ncol=sum(stateCol)))
  # get col names
  stateDataColNames <- c("day", "cash", "revenue", "lostRev")
  lostBeer <- c("lost")
  invState <- c("inv", "tank")
  for (string in c(lostBeer, invState)) {
    for (beer in beerInfo[,"name"]) {
      stateDataColNames <- c(stateDataColNames, paste0(string, beer))
    }
  }
  invState[2] <- "order"
  for (string in invState) {
    for (mat in rawMatQty[, "name"]) {
      stateDataColNames <- c(stateDataColNames, paste0(string, mat))
    }
  }
  
  for (tank in tanks[, "Tank"]) {
    stateDataColNames <- c(stateDataColNames, paste0("tank", tank))
  }
  
  colnames(stateData) <- stateDataColNames
  
  return(stateData)
}

createMaterialAuto <- function(materialInfo) {
  materialInfo$reorderQuantity <- 0
  materialInfo$reorderPoint <- 0
  materialInfo$supplier <- NA
  materialInfo$reorder <- F
  
  return(materialInfo)
}

createBeerAuto <- function(beerInfo) {
  beerInfo <- beerInfo[, c("beerID", "name")]
  beerInfo$reorderPoint <- 0
  beerInfo$rebrew <- F
  
  return(beerInfo)
}