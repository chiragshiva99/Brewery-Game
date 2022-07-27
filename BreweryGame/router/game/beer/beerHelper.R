getReqAmt <- function(beer, reqTable, matQty, pID=1) {
  beerReqTable <- reqTable %>% subset(beerName == beer) %>% subset(processID == pID) %>% subset(select=c("materialName", "qty"))
  
  text <- "Required Amounts: "
  for (i in 1:nrow(beerReqTable)) {
    text <- paste0(text, beerReqTable[i, "qty"], " ", beerReqTable[i, "materialName"], ", ")
  }
  text <- substr(text, 1, nchar(text)-2)
  text <- paste0(text,".")
  
  print(beerReqTable)
  print(matQty)
  beerReqTable <- beerReqTable %>% rename(reqQty = qty) %>% rename(name=materialName)
  checkAmtTable <- merge(beerReqTable, matQty, by=c("name"), all.x=T)
  shinyjs::enable("brewBeer")
  for (i in 1:nrow(checkAmtTable)){
    if(checkAmtTable[i, "reqQty"] > checkAmtTable[i, "qty"]) {
      text <- paste0("Not enough material for ",beer)
      shinyjs::disable("brewBeer")
      break
    }
  }
  text
}



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
  
  checkAmtTable <- subset(checkAmtTable, select=c("name", "qty"))
  checkAmtTable
}