updateTemp <- function(input, materialInfo, beerInfo, tempMatQ, tempMatR, tempMatS, tempBeerR) {
  
  for (i in 1:nrow(materialInfo)) {
    matName <- materialInfo[i, "name"]
    tempMatS[[paste0("material", matName)]] <- input[[paste0("supplier", matName, "Input")]]
    tempMatQ[[paste0("material", matName)]] <- input[[paste0("reQty", matName, "Input")]]
    tempMatR[[paste0("material", matName)]] <- input[[paste0("rePt", matName, "Input")]]
  }
  
  for(i in 1:nrow(beerInfo)) {
    tempBeerR[[paste0("beer", beerInfo[i, "name"])]] <- input[[paste0("rePt", beerInfo[i, "name"], "Input")]]
  }
  
  
  return(list(
    tempMatQ,
    tempMatR,
    tempMatS,
    tempBeerR
  ))
}

createLogicalList <- function(reVals, info, type) {
  logicals <- c()
  for(i in 1:length(reVals)) {
    logicals <- c(logicals, reVals[[paste0(type, info[i, "name"])]])
  }
  
  return(logicals)
}