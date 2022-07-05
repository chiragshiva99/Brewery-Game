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

addBeerToTank <- function(tankInfo, tankNo, beerType) {
  tankInfo[tankNo, 1] <- beerType
  tankInfo[tankNo, 2] <- 0
  
  tankInfo
}

## Raw Material Ordering



## Advance Button
incrementDays <- function(frameInfo) {
  frameInfo["Days"] <- frameInfo["Days"] + 1
  frameInfo
}