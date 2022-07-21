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

## Beer Tanks
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

brewModal <- function(tankOptions, beerOptions) {
  modalDialog(
    title = "Brew a Beer",
    selectInput("tankSelect", "Choose a Tank", choices=tankOptions),
    selectInput("beerChosen", "Choose a Beer", choices=beerOptions),
    htmlOutput("beerReq"),
    footer=tagList(
      modalButton("Cancel"),
      actionButton("brewBeer", "Brew Beer")
    )
    
  )
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

## Raw Material Ordering
purchaseModal <- function(materialOptions) {
  modalDialog(
    title="Purchase of Raw Materials",
    selectInput("matChosen", "Choose a Material", choices=materialOptions),
    footer=tagList(
      modalButton("Cancel"),
      actionButton("selectSupplier", "Next: Select Supplier"))
  )
}

supplierModal <- function(supplierOptions, matChosen) {
  modalDialog(
    title=paste0("Select a Supplier to buy ", matChosen, " from"),
    htmlOutput("supplierCompare"),
    selectInput("supplierChosen", "Choose a Supplier", choices=supplierOptions),
    numericInput("quantity", "Enter a Quantity to Purchase", value=0, min=1, step=1),
    htmlOutput("costOfPurchase"),
    footer=tagList(
      modalButton("Cancel"),
      actionButton("selectMat", "Previous: Select Material"),
      actionButton("purchaseok", "Confirm Purchase")
    )
  )
  
}

calculateCost <- function(costInfo, mat, supplier, qty){
  costStuff <- costInfo %>% subset(materialName==mat) %>% subset(supplierName==supplier)
  cost <- qty*costStuff[1, "variableCost"] + costStuff[1, "fixedCost"]
  as.double(cost)
}

## Advance Button
incrementDays <- function(frameInfo, col) {
  frameInfo[col] <- frameInfo[col] + 1
  frameInfo
}

## Demand
generateDemand <- function(customer, iia, normParam, days, beer, maxWait) {
  demand <- data.frame(matrix(nrow=0, ncol=6))
  colnames(demand) <- c("Customer", "Beer", "Quantity", "Day", "actualDay", "maxWait")
  cumDay <- 0
  while(cumDay < days) {
    qty <- ceiling(rnorm(1, mean=normParam[1], sd=normParam[2]))
    if (qty <= 0) {
      next
    }
    dayToNextOrder <- rexp(1, rate=(1/iia))
    cumDay <- cumDay + dayToNextOrder
    order <- data.frame(Customer=customer, Beer=beer, Quantity=qty, Day=ceiling(cumDay), actualDay=cumDay, maxWait=maxWait)
    demand <- rbind(demand, order)
  }
  demand <- demand[-c(nrow(demand)),]
  demand
}

generateTotalDemand <- function(table,days=100) {
  totalDemand <-   demand <- data.frame(matrix(nrow=0, ncol=6))
  colnames(demand) <- c("Customer","Beer", "Quantity", "Day", "actualDay", "maxWait")
  for (i in 1:nrow(table)){
    indivDemand <- generateDemand(table[i, "customerName"], table[i,"meanArrivalTime"], c(table[i,"mean"], table[i, "sd"]),days, table[i, "beerName"], table[i, "waitTime"])
    print(indivDemand)
    totalDemand <- rbind(totalDemand, indivDemand)
  }
  print(totalDemand)
  totalDemand
}

## Reset
resetDialog <- function() {
  modalDialog(
    title="Reset?",
    div("Are you sure you want to reset?"),
    footer=tagList(
      modalButton("Cancel"),
      actionButton("resetok", "Reset")
    )
  )
}