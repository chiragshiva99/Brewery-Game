## Gabriel

# Gets required amount for beer
getReqAmt <- function(beer, reqTable, matQty, pID=1) {
  beerReqTable <- reqTable %>% subset(beerName == beer) %>% subset(processID == pID) %>% subset(select=c("materialName", "qty"))
  
  text <- "Required Amounts: "
  for (i in 1:nrow(beerReqTable)) {
    text <- paste0(text, beerReqTable[i, "qty"], " ", beerReqTable[i, "materialName"], ", ")
  }
  text <- substr(text, 1, nchar(text)-2)
  text <- paste0(text,".")
  
  # print(beerReqTable)
  # print(matQty)
  beerReqTable <- beerReqTable %>% rename(reqQty = qty) %>% rename(name=materialName)
  checkAmtTable <- merge(beerReqTable, matQty, by=c("name"), all.x=T)
  shinyjs::enable("brewBeer")
  
  if(any(checkAmtTable[, "reqQty"] > checkAmtTable[, "qty"])) {
    shinyjs::disable("brewBeer")
    text <- "Need"
    for (i in 1:nrow(checkAmtTable)) {
      
      if(checkAmtTable[i, "reqQty"] > checkAmtTable[i, "qty"]) {
        diff <- checkAmtTable[i, "reqQty"] - checkAmtTable[i, "qty"]
        mat <- checkAmtTable[i, "name"]
        text <- paste0(text, " ",diff, " more ", mat, ", ")
      }
    }
    text <- substr(text, 1, nchar(text)-2)
    text <- paste0(text,".")
  }
  
  text
}

# Checks whether there is enough material
checkMaterialAmount <- function(beer, reqTable, matQty, pID=1) {
  enough <- T
  beerReqTable <- reqTable %>% subset(beerName == beer) %>% subset(processID == pID) %>% subset(select=c("materialName", "qty"))
  
  beerReqTable <- beerReqTable %>% rename(reqQty = qty) %>% rename(name=materialName)
  checkAmtTable <- merge(beerReqTable, matQty, by=c("name"), all.x=T)
  
  if(any(checkAmtTable[, "reqQty"] > checkAmtTable[, "qty"])) {
    enough <- F

  }
  
  enough
}

# adds beer to tank
addBeerToTank <- function(tanks, select, beer, beerInfo) {
  completeDays <- subset(beerInfo, name==beer)[1,"daysToComplete"]
  tanks[select, "daysToComplete"] <- completeDays
  tanks[select, "Beer"] <- beer
  tanks[select, "DaysInTank"] <- 0
  tanks
}

updateRawMatQty <- function(beerReq, rawMatInfo, beer) {
  beerReqTable <- beerReq %>% subset(beerName == beer) %>% subset(processID == 1) %>% subset(select=c("materialName", "qty")) %>% rename(reqQty = qty) %>% rename(name=materialName)
  checkAmtTable <- merge(beerReqTable, rawMatInfo, by=c("name"), all.x=T)
  
  
  checkAmtTable$qty <- checkAmtTable$qty - checkAmtTable$reqQty
  
  if(any(checkAmtTable$qty < 0)) {
    return(rawMatInfo)
  } else {
    checkAmtTable <- subset(checkAmtTable, select=c("name", "qty"))
    
    return(checkAmtTable)
  }
}

brewBeer <- function(tanks, tankSelect, beerChosen, beerInfo, beerReq, rawMatQty, general) {
  tanks <- addBeerToTank(tanks, tankSelect, beerChosen, beerInfo)
  rawMatQty <- updateRawMatQty(beerReq, rawMatQty, beerChosen)
  general$action <- general$action + 1
  return(
    list(
      tanks,
      rawMatQty,
      general
    )
  )
}