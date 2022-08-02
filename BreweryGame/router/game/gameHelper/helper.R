## Both
addNewEntry <- function(infoDF, row, value) {
  # Check if na
  added <- F
  if (is.na(infoDF[row, 1])){
    infoDF[row, 1] <- value
    infoDF[row, 2] <- 0
    added <- T
  }

  list(infoDF, added)
}

vector.is.empty <- function(x) {
  return(length(x) ==0 )
}

incrementDays <- function(frameInfo, col) {
  frameInfo[col] <- frameInfo[col] + 1
  frameInfo
}

### Init helper functions ###

getTankDF <- function(tankNo, tankSize) {
  tanks <- as.data.frame(matrix(nrow=tankNo, ncol=5))
  colnames(tanks) <- c("Tank", "Beer", "DaysInTank", "daysToComplete", "tankSize")
  for (i in 1:nrow(tanks)){
    tanks[i, "Tank"] <- i
    tanks$Beer <- "Empty"
    tanks$tankSize <- tankSize
  }
  tanks
}

getLostSalesDF <- function(beerInfo) {
  lostPerBeer <- data.frame(matrix(nrow=nrow(beerInfo), ncol=2))
  colnames(lostPerBeer) <- c("name", "lostQty")
  lostPerBeer$stockOut <- beerInfo$stockOut
  lostPerBeer$name <- beerInfo$name
  lostPerBeer$lostQty <- 0
  
  lostPerBeer
}

getLostBeerList <- function(beerInfo) {
  lostBeer <- list()
  for (drink in beerInfo[, "beerID"]) {
    lostBeer[[drink]] <- 0
  }
  
  lostBeer
}

storeAllDemand <- function(totalDemand) {
  addToTable("", totalDemand)
  
}
