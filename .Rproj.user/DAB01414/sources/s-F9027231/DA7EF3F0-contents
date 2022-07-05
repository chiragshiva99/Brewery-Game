## Both
addNewEntry <- function(infoDF, row, value) {
  # Check if na
  added <- F
  if (is.na(infoDF[row, 1])){
    infoDF[row, 1] <- value
    infoDF[row, 2] <- 0
    added <- T
  }

  c(infoDF, added)
}

vector.is.empty <- function(x) return(length(x) ==0 )

## Beer Tanks
tankStatus <- function(tankInfo, tankNo) {
  if (is.na(tankInfo[tankNo,1])) {
    text <- "Empty"
  } else {
    text <- paste(tankInfo[tankNo,1], tankInfo[tankNo, 2])
  }
  text
}

makeBeerModal <- function(tankNo){
  modalDialog(
    title = paste("Choose a Beer to Make for Tank", tankNo),
    radioButtons("beerChosen", "Choose a Style", choices=c("Lager", "IPA", "Stout")),
    footer=tagList(
      modalButton("Cancel"),
      actionButton("makeBeer", "Make Beer"))
  )
}

curBeerModal <- function(tankInfo, tankNo) {
  modalDialog(
    title = paste0("Tank" , tankNo, "Info"),
    div("Beer: ", tankInfo[tankNo, 1]),
    div("Days in Tank:", tankInfo[tankNo, 2]),
    footer=tagList(modalButton("Cancel"))
  )
}

tankModal <- function(tankInfo, tankNo) {
  if(is.na(tankInfo[tankNo,1])){
    showModal(makeBeerModal(tankNo))
  } else {
    showModal(curBeerModal(tankInfo, tankNo))
  }
}

updateRawMatQty <- function(beerReq, rawMatInfo, beerType) {
  rawMat <- c("Malt", "Hops", "Yeast")
  for (mat in rawMat) {
    rawMatInfo[mat] <- rawMatInfo[mat] - beerReq[beerType, mat]
  }
  rawMatInfo
}

## Raw Material Ordering
purchaseModal <- function() {
  modalDialog(
    title="Purchase of Raw Materials",
    radioButtons("matChosen", "Choose an ingredient", choices=c("Malt", "Hops", "Yeast")),
    numericInput("quantity", "Enter purchase quantity:", 0, min=0),
    htmlOutput("costOfPurchase"),
    footer=tagList(
      modalButton("Cancel"),
      actionButton("purchaseok", "Make Purchase"))
  )
}

calculateCost <- function(costInfo, mat, qty){
  cost <- qty * costInfo[mat, "Variable"]
  cost <- cost + costInfo[mat, "Fixed"]
  
  cost
}

## Advance Button
incrementDays <- function(frameInfo) {
  frameInfo["Days"] <- frameInfo["Days"] + 1
  frameInfo
}