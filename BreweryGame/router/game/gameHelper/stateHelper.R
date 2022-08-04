getBaseData <- function(gameID, id, rowCount) {
  df <- data.frame(matrix(nrow=rowCount, ncol=2))
  colnames(df) <- c("gameID", "userID")
  df$gameID <- gameID
  df$userID <- id
  
  df
}

createCashStateDF <- function() {
  stateData <- data.frame(matrix(nrow=0, ncol=4))
  colnames(stateData) <- c("gameDay", "cashBalance", "revenue", "lostRev")
  
  stateData
}

createBeerStateDF <- function() {
  stateData <- data.frame(matrix(nrow=0, ncol=5))
  colnames(stateData) <- c("gameDay", "beerID", "inventory", "inTank", "lostSale")
  
  stateData
}

createTankStateDF <- function() {
  stateData <- data.frame(matrix(nrow=0, ncol=5))
  colnames(stateData) <- c("gameDay", "tankID", "beerID", "tankSize", "completed")
  
  stateData
}

createMatStateDF <- function() {
  stateData <- data.frame(matrix(nrow=0, ncol=4))
  colnames(stateData) <- c("gameDay", "materialID", "inventory", "inTransit")
  
  stateData
}

createDemandStateDF <- function() {
  stateData <- data.frame(matrix(nrow=0, ncol=6))
  colnames(stateData) <- c("gameDay", "customerID", "beerID", "quantity", "arrivalDay", "serviceDay")
  
  stateData
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