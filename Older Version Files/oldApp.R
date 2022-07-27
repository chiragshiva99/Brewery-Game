#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# At the beginning of any R session, record your AWS database password:

# Now, anywhere in your code where the password is needed you can get it using
# getOption("AWSPassword")
# Otherwise it is hidden. So now this code can be shared with anyone 
# without giving them access to your personal AWS database.

source("usePackages.R")
pkgnames <- c("tidyverse","shiny", "shinyjs","DBI","jsonlite","shinydashboard", "shiny.router")
loadPkgs(pkgnames)


source("helper.R")
source("dbHelper.R")
source("userInterface.R")
source("urlRouter.R")
source("credentialsHelper.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("The Brewery Game"),
  actionButton("register", "Register"),
  actionButton("login", "Login"),
  gameInterface
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # reactiveValues object for storing items like the user password
  vals <- reactiveValues(password=NULL,userid=NULL,username=NULL,curGameid=-1)
  
  #when the user clicks the Register button
  observeEvent(input$register, {
    showModal(signupModal(failed1=FALSE, failed2 = FALSE))
  })
  #When user click the signupok button
  observeEvent(input$signupok, {
    #check if password1 exits and == password2
    if (str_length(input$password1)>0 && (input$password1 == input$password2)){
      #store the username and password and close the dialog
      vals$username <- input$username
      vals$password <- input$password1
      print(vals$username)
      #print(vals$password)
      vals$username = registerUser(vals$username, vals$password)
      
      if (!is.null(vals$username)){
        vals$userid <- getUserID(vals$username,vals$password)
      }
      print(vals$userid) # for debugging
      removeModal()
      
    }
    else {
      #-----------ADD SOME CODE IF USERNAME ALREADY EXISTS-----------------
      #if username already exists {
      #showModal(signupModal(failed1= TRUE, failed2 = FALSE))
      #}
      
      #if passwords dont match 
      if (input$password1 != input$password2){
        showModal(signupModal(failed1= FALSE, failed2 = TRUE))
      }
      else{
        showModal(signupModal(failed1= TRUE, failed2 = TRUE))
      }
    }
  })
  
  #when the user clicks the Login button
  observeEvent(input$login, {
    showModal(loginModal(failed=FALSE))
  })
  #when user clicks the login button
  observeEvent(input$loginok, {
    # Check that password1 exists and it matches password2
    userid <- getUserID(input$username,input$password3)
    if (userid>0) {
      #store the playerid and playername and close the dialog
      vals$userid <- userid
      #print(vals$userid) # for debugging
      vals$username <- input$username
      #print(vals$username) # for debugging
      removeModal()
    } else {
      showModal(loginModal(failed = TRUE))
    }
  })
  
  #when user clicks the changepw button  
  observeEvent(input$changepw, {
    showModal(changepwModal(failed1 = FALSE, failed2 = FALSE))
  })
  #when the user clicks the changepwok button
  observeEvent(input$changepwok, {
    # Get the playerID and check if it is valid
    userid <- getUserID(input$username,input$password3)
    userpassword <- getUserPassword(userid)
    print(userpassword)
    if ((userid>0) && (input$password3==userpassword) && str_length(input$password3) >0 && (input$password4 == input$password5) && (str_length(input$password4)>0)){
      #store the playerid and playername and close the dialog
      result <- updatePassword(input$username,input$password4)
      removeModal()
    } 
    else {
      #if failed 1 
      #if user id doesn't exist{
      #  showModal(changepwModal(failed1 = TRUE, failed2 = FALSE))
      #}
      #if failed 2
      if ((input$password4 != input$password5) && (str_length(input$password4)>0)){
        showModal(changepwModal(failed1 = FALSE, failed2 = TRUE))
      }
      else{
        showModal(changepwModal(failed1 = TRUE, failed2 = TRUE))
      }
    }
  })
  
  
  ## Initializing stuff
  condition <- 1
  rawMat <- c("Malt", "Hops", "Yeast")
  
  tankSize <- 100
  tanks <-  as.data.frame(matrix(nrow=4, ncol=5))
  colnames(tanks) <- c("Tank","Beer", "DaysInTank", "daysToComplete", "tankSize")
  for (i in 1:nrow(tanks)){
    tanks[i, "Tank"] <- i
    tanks$Beer <- "Empty"
    tanks$tankSize <- tankSize
  }
  
  rawMatOrder <-  data.frame(matrix(nrow=0, ncol=5))
  colnames(rawMatOrder) <- c("Material","Quantity", "Days", "Supplier", "daysToComplete")
  
  beerInv <-  getStartQty(1, "beerParameters")
  
  beerInfo <- getBeerInfo()
  # stockOut <- c(Lager=3, IPA=3, Stout=3)
  
  rawMatQty <- getStartQty(1, "materialNames")
  rawMatQty[1,"qty"] <- 550
  
  beerReq <- getBeerReq()
  
  costInfo <- getMaterialCost()
  
  startingMoney <- 100000
  
  
  totalDays <- 10
  
  endDays <- 10
  
  ## Demand Generation
  customers <- getCustomerData()
  # totalDemand <- generateDemand(meanArrivalTimes, normDistParams, totalDays)
  totalDemand <- generateTotalDemand(customers, totalDays)
  demand <- subset(totalDemand, Day==1)
  if (nrow(demand) > 0){
    demand$Day <- 0
  }
  
  ## Lost Sales Tracking
  lostPerBeer <- data.frame(matrix(nrow=nrow(beerInfo), ncol=2))
  colnames(lostPerBeer) <- c("name", "lostQty")
  lostPerBeer$stockOut <- beerInfo$stockOut
  lostPerBeer$name <- beerInfo$name
  lostPerBeer$lostQty <- 0
  #Reactive Values
  vals <- reactiveValues(money=startingMoney, day=1, demand=demand, lostCust=0, lostPerBeer=lostPerBeer, tanks=tanks, beerInv=beerInv, rawMatOrder=rawMatOrder, rawMatQty=rawMatQty, tankSelect=NULL, beerChosen=NULL, purchQty=NULL, matChosen=NULL, supplierChosen=NULL)
  
  ## Reset Game 
  observeEvent(input$reset, {
    print("resetting")
    showModal(resetDialog())
  })
  
  observeEvent(input$resetok, {
    removeModal()
    
    USER$finish <- F
    
    shinyjs::enable("brew")
    shinyjs::enable("purchase")
    shinyjs::enable("advance")
    totalDemand <- generateTotalDemand(customers, totalDays)
    demand <- subset(totalDemand, Day==1)
    if (nrow(demand) > 0){
      demand$Day <- 0
    }
    
    vals$money <- startingMoney
    vals$day <- 1
    vals$demand <- demand
    vals$lostCust <- 0
    vals$lostPerBeer <- lostPerBeer
    vals$tanks <- tanks
    vals$beerInv <- beerInv
    vals$rawMatOrder <- rawMatOrder
    vals$rawMatQty <- rawMatQty
  })
  
  ## Info params
  output$money <- renderUI({paste("$", vals$money)})
  output$day <- renderUI({vals$day})
  output$lostSales <- renderUI({paste(vals$lostCust, "Customers Lost")})
  
  output$lostSalesPerBeer <- renderTable({
    select(vals$lostPerBeer, -stockOut)
  })
  ## Advance Button
  
  observeEvent(input$advance, {
    vals$day <- vals$day + 1
    
    ## Increase the number of days for tanks and Order
    vals$tanks$DaysInTank <- vals$tanks$DaysInTank + 1
    vals$rawMatOrder$Days <- vals$rawMatOrder$Days + 1
    vals$demand$Day <- vals$demand$Day + 1
    
    ## Add completed Beers
    completeTanks <- which(vals$tanks$DaysInTank >= vals$tanks$daysToComplete)
    for (tank in completeTanks) {
      beerIdx <- which(vals$beerInv["name"] == vals$tanks[tank, "Beer"])
      vals$beerInv[beerIdx, "qty"] <- vals$beerInv[beerIdx, "qty"] + vals$tanks[tank, "tankSize"]
      
      vals$tanks[tank, "Beer"] <- "Empty"
      vals$tanks[tank, "DaysInTank"] <- NA
      vals$tanks[tank, "daysToComplete"] <- NA
    }
    
    ## Add completed Raw Material Orders
    completeOrders <- which(vals$rawMatOrder$Days >= vals$rawMatOrder$daysToComplete)
    for (order in completeOrders) {
      matIdx <- which(vals$rawMatQty["name"] == vals$rawMatOrder[order, "Material"])
      vals$rawMatQty[matIdx, "qty"] <- vals$rawMatQty[matIdx, "qty"] + vals$rawMatOrder[order, "Quantity"]
    }
    if (! vector.is.empty(completeOrders) ){
      vals$rawMatOrder <- vals$rawMatOrder[-c(completeOrders),]
    }
    
    ## Satisfy and unsatisfied Demand
    removeDemand <- c()
    unsatisDemand <- c()
    if (nrow(vals$demand) > 0) {
      for (row in 1:nrow(vals$demand)) {
        # Satisfied?
        beerType <- vals$demand[row, "Beer"]
        customerName <- vals$demand[row, "Customer"]
        qty <- vals$demand[row, "Quantity"]
        beerIdx <- which(vals$beerInv$name == beerType)
        if (vals$beerInv[beerIdx,"qty"] >= qty) {
          vals$beerInv[beerIdx, "qty"] <- vals$beerInv[beerIdx, "qty"] - qty
          vals$money <- vals$money + qty*(beerInfo[which(beerInfo$name == beerType), "revenue"] + customers[which((customers$beerName == beerType) & (customers$customerName == customerName)), "revenueExtra"])
          removeDemand <- c(removeDemand, row)
        }
        
        # Unsatisfied?
        dayWait <- vals$demand[row, "Day"]
        maxWait <- vals$demand[row, "maxWait"]
        lostIdx <- which(vals$lostPerBeer$name == beerType)
        if (dayWait >= maxWait) {
          unsatisDemand <- c(unsatisDemand, row)
          print("still working")
          vals$lostPerBeer[lostIdx, "lostQty"] <- vals$lostPerBeer[lostIdx, "lostQty"] + qty
          print("working")
          vals$money <- vals$money - qty*vals$lostPerBeer[lostIdx, "stockOut"]
          print("not working")
          removeDemand <- c(removeDemand, row)
        }
      }
    }
    
    if (!vector.is.empty(removeDemand)){
      vals$demand <- vals$demand[-c(removeDemand),]
    }
    
    if (length(unsatisDemand) > 0) {
      vals$lostCust <- vals$lostCust + length(unsatisDemand)
    }
    
    # Add New Demand
    newDemand <- subset(totalDemand, Day==vals$day)
    if (nrow(newDemand) > 0) {
      newDemand$Day <-  0
      vals$demand <- rbind(vals$demand, newDemand)
    }
    
    if (vals$day > endDays) {
      # USER$gameStart <- F
      USER$finish <- T
      shinyjs::disable("brew")
      shinyjs::disable("purchase")
      shinyjs::disable("advance")
      
    }
    
  })
  
  output$gameStatus <- renderUI({
    click <- input$advance
    if (vals$day > endDays) {
      text <- "Game has Ended!"
    } else {
      text <- ""
    }
    text
  })
  
  
  ## Tanks
  output$tankInfo <- renderTable({select(vals$tanks, -daysToComplete, -tankSize)})
  observeEvent(input$brew, {    
    shinyjs::disable("brewBeer")
    showModal(brewModal(vals$tanks[,"Tank"], vals$beerInv[, "name"]))})
  
  output$beerReq <- renderUI({
    click <- input$brew
    shinyjs::disable("brewBeer")
    if (vals$tanks[vals$tankSelect,"Beer"] == "Empty") {
      text <- getReqAmt(vals$beerChosen, beerReq, vals$rawMatQty)
    } else {
      text <- paste("Tank", vals$tankSelect, "is occupied.")
      shinyjs::disable("brewBeer")
    }
    text
  })
  
  observeEvent(input$brewBeer, {
    vals$tanks <- addBeerToTank(vals$tanks, vals$tankSelect, vals$beerChosen, beerInfo)
    vals$rawMatQty <- updateRawMatQty(beerReq, vals$rawMatQty, vals$beerChosen)
    removeModal()
  })
  
  observeEvent(input$beerChosen, {
    vals$beerChosen <- input$beerChosen
  })
  
  observeEvent(input$tankSelect, {
    vals$tankSelect <- input$tankSelect
  })
  
  ## Raw Material
  
  observeEvent(input$purchase, {
    removeModal()
    showModal(purchaseModal(unique(costInfo[,"materialName"])))
  })
  
  observeEvent(input$selectMat, {
    removeModal()
    showModal(purchaseModal(unique(costInfo[,"materialName"])))
  })
  observeEvent(input$matChosen, {vals$matChosen <- input$matChosen})
  
  
  output$supplierCompare <- renderTable({
    supplierInfo <- costInfo %>% subset(materialName==vals$matChosen) %>% select(-materialName)
    supplierInfo
  })
  
  observeEvent(input$selectSupplier, {
    removeModal()
    supplierInfo <- costInfo %>% subset(materialName==vals$matChosen)
    showModal(supplierModal(supplierInfo[,"supplierName"], vals$matChosen))
  })
  
  observeEvent(input$supplierChosen, {vals$supplierChosen <- input$supplierChosen})
  
  observeEvent(input$quantity, {vals$purchQty <- input$quantity})
  
  output$costOfPurchase <- renderUI({
    click <- input$quantity
    
    others <- input$selectSupplier
    shinyjs::disable("purchaseok")
    amt <- calculateCost(costInfo, vals$matChosen, vals$supplierChosen, vals$purchQty)
    print(amt)
    if (is.na(amt)) {
      text <- "Please input a value"
    } else if (vals$purchQty != as.integer(vals$purchQty)){
      text <- "Please enter an Integer value"
    } else if (amt <= vals$money) {
      text <-  paste("Amount:", amt)
      shinyjs::enable("purchaseok")
    } else {
      text <- "Not Enough Money to purchase!"
    }
    text 
  })
  
  output$rawMatQty <- renderTable({
    click <- input$advance + input$makeBeer
    vals$rawMatQty
  })
  
  observeEvent(input$purchaseok, {
    newEntry <- data.frame(Material=vals$matChosen, Quantity=vals$purchQty, Days=0, Supplier=vals$supplierChosen, daysToComplete=costInfo[which(costInfo$materialName == vals$matChosen), "daysToComplete"])
    vals$rawMatOrder <- rbind(vals$rawMatOrder, newEntry)
    vals$money <- vals$money - calculateCost(costInfo, vals$matChosen, vals$supplierChosen, vals$purchQty)
    removeModal()
  })
  
  output$currentOrders <- renderTable({
    click <- input$purchaseok + input$advance
    select(vals$rawMatOrder, -daysToComplete)
    
  })
  
  ## Beer Inventory
  
  output$beerQty <- renderTable({vals$beerInv})
  
  ## Demand
  output$custDemand <- renderTable({
    click <- input$advance
    select(vals$demand, -maxWait)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)