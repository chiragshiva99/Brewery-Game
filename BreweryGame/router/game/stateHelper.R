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