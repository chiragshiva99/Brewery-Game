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
pkgnames <- c("tidyverse","shiny", "shinyjs","DBI","jsonlite","shinydashboard", "shinyauthr", "DT", "sodium", "shinyBS")
loadPkgs(pkgnames)

#UIs
source("analysisPage.R")
source("loginPage.R")
source("gamePage.R")

#feature Modules
source("materialModule.R")
source("beerModule.R")
source("demandModule.R")

#Helper Functions
source("helper.R")
source("dbHelper.R")
source("demandHelper.R")


header <- dashboardHeader( title = "Brewery Game", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(collapsed=TRUE, uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### LOGIN STUFF ###   From: https://www.listendata.com/2019/06/how-to-add-login-page-in-shiny-r.html
  
  loginInit <- F
  gameStartInit <- F
  finishInit <- F
  hasGameInit <- F
  prevGameInit <- -1
  signupInit <- F
  USER <- reactiveValues(login = loginInit, gameStart = gameStartInit, finish=finishInit, hasGame=hasGameInit, prevGame=prevGameInit, signup=signupInit)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$password)
          credentials <- getCredentials(Username)
          if(length(which(credentials$username==Username))==1) { 
            pasmatch  <- credentials["password"][which(credentials$username==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
              hasGame <- credentials["curGameID"][which(credentials$username==Username),]
              prevGame <- credentials["prevGameID"][which(credentials$username==Username),]
              if(hasGame != -1) {
                USER$hasGame <- T
                USER$prevGame <- prevGame
              }
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  observeEvent(input$startGame, {
    if(USER$gameStart == F) {
      USER$gameStart <-  T
    }
  })
  
  output$continueOption <- renderUI({
    if(USER$hasGame == T) {
      actionButton("continueGame", "Continue Game")
    }
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE & USER$gameStart == T & USER$finish == F){ 
      sidebarMenu(
        menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard"))
        
      )
    } else if (USER$login == T & USER$gameStart == T & USER$finish == T) {
      sidebarMenu(
        menuItem("Main Page", tabName = "gameTab", icon = icon("dashboard")),
        menuItem("Analysis Page", tabName = "analysisTab", icon = icon("dashboard"))
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == T & USER$gameStart == T) {
      if (USER$finish == F) {
        tabItems(
          gameInterface
        )
      } else {
        tabItems(
          gameInterface,
          analysisInterface
        )
      }
    }
    else if (USER$login == T & USER$gameStart == F & USER$finish == F) {
      gameChoice
    }
    else if (USER$signup == T) {
      signuppage
    } 
    else {
      loginpage
    }
    
  })
  
  #### GAME STUFF ####
  
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
  
  
  totalDays <- 20
  
  endDays <- 20
  
  ## Demand Generation
  customers <- getCustomerData()
  # totalDemand <- generateDemand(meanArrivalTimes, normDistParams, totalDays)
  totalDemand <- generateTotalDemand(customers, totalDays)
  dayDemand <- subset(totalDemand, Day==1)
  if (nrow(dayDemand) > 0){
    dayDemand$Day <- 0
  }
  
  ## Lost Sales Tracking
  lostPerBeer <- data.frame(matrix(nrow=nrow(beerInfo), ncol=2))
  colnames(lostPerBeer) <- c("name", "lostQty")
  lostPerBeer$stockOut <- beerInfo$stockOut
  lostPerBeer$name <- beerInfo$name
  lostPerBeer$lostQty <- 0
  #Reactive Values
  general <- reactiveValues(money=startingMoney, day=1)
  beer <- reactiveValues(tanks=tanks, beerInv=beerInv)
  material <- reactiveValues(rawMatOrder=rawMatOrder, rawMatQty=rawMatQty)
  demand <- reactiveValues(dayDemand=dayDemand, lostCust=0, lostPerBeer=lostPerBeer)
  
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
    
    general$money <- startingMoney
    general$day <- 1
    demand$dayDemand <- dayDemand
    demand$lostCust <- 0
    demand$lostPerBeer <- lostPerBeer
    beer$tanks <- tanks
    beer$beerInv <- beerInv
    material$rawMatOrder <- rawMatOrder
    material$rawMatQty <- rawMatQty
  })
  
  ## Info params
  output$money <- renderUI({h4(paste("Cash Balance: $", general$money))})
  output$day <- renderUI({
    h4(paste("Days:", general$day))
  })
  ## Advance Button
  
  observeEvent(input$advance, {
    general$day <- general$day + 1
    
    ## Increase the number of days for tanks and Order
    beer$tanks$DaysInTank <- beer$tanks$DaysInTank + 1
    material$rawMatOrder$Days <- material$rawMatOrder$Days + 1
    demand$dayDemand$Day <- demand$dayDemand$Day + 1
    
    ## Add completed Beers
    completeTanks <- which(beer$tanks$DaysInTank >= beer$tanks$daysToComplete)
    for (tank in completeTanks) {
      beerIdx <- which(beer$beerInv["name"] == beer$tanks[tank, "Beer"])
      beer$beerInv[beerIdx, "qty"] <- beer$beerInv[beerIdx, "qty"] + beer$tanks[tank, "tankSize"]
      
      beer$tanks[tank, "Beer"] <- "Empty"
      beer$tanks[tank, "DaysInTank"] <- NA
      beer$tanks[tank, "daysToComplete"] <- NA
    }
    
    ## Add completed Raw Material Orders
    completeOrders <- which(material$rawMatOrder$Days >= material$rawMatOrder$daysToComplete)
    for (order in completeOrders) {
      matIdx <- which(material$rawMatQty["name"] == material$rawMatOrder[order, "Material"])
      material$rawMatQty[matIdx, "qty"] <- material$rawMatQty[matIdx, "qty"] + material$rawMatOrder[order, "Quantity"]
    }
    if (! vector.is.empty(completeOrders) ){
      material$rawMatOrder <- material$rawMatOrder[-c(completeOrders),]
    }
    
    ## Satisfy and unsatisfied Demand
    removeDemand <- c()
    unsatisDemand <- c()
    if (nrow(demand$dayDemand) > 0) {
      for (row in 1:nrow(demand$dayDemand)) {
        # Satisfied?
        beerType <- demand$dayDemand[row, "Beer"]
        customerName <- demand$dayDemand[row, "Customer"]
        qty <- demand$dayDemand[row, "Quantity"]
        beerIdx <- which(beer$beerInv$name == beerType)
        if (beer$beerInv[beerIdx,"qty"] >= qty) {
          beer$beerInv[beerIdx, "qty"] <- beer$beerInv[beerIdx, "qty"] - qty
          general$money <- general$money + qty*(beerInfo[which(beerInfo$name == beerType), "revenue"] + customers[which((customers$beerName == beerType) & (customers$customerName == customerName)), "revenueExtra"])
          removeDemand <- c(removeDemand, row)
        }
        
        # Unsatisfied?
        dayWait <- demand$dayDemand[row, "Day"]
        maxWait <- demand$dayDemand[row, "maxWait"]
        lostIdx <- which(demand$lostPerBeer$name == beerType)
        if (dayWait >= maxWait) {
          unsatisDemand <- c(unsatisDemand, row)
          print("still working")
          demand$lostPerBeer[lostIdx, "lostQty"] <- demand$lostPerBeer[lostIdx, "lostQty"] + qty
          print("working")
          general$money <- general$money - qty*demand$lostPerBeer[lostIdx, "stockOut"]
          print("not working")
          removeDemand <- c(removeDemand, row)
        }
      }
    }
    
    if (!vector.is.empty(removeDemand)){
      demand$dayDemand <- demand$dayDemand[-c(removeDemand),]
    }
    
    if (length(unsatisDemand) > 0) {
      demand$lostCust <- demand$lostCust + length(unsatisDemand)
    }
    
    # Add New Demand
    newDemand <- subset(totalDemand, Day==general$day)
    if (nrow(newDemand) > 0) {
      newDemand$Day <-  0
      demand$dayDemand <- rbind(demand$dayDemand, newDemand)
    }
    
    if (general$day > endDays) {
      # USER$gameStart <- F
      USER$finish <- T
      shinyjs::disable("brew")
      shinyjs::disable("purchase")
      shinyjs::disable("advance")
      
    }
    
  })
  
  output$gameStatus <- renderUI({
    if (general$day > endDays) {
      text <- "Game has Ended!"
    } else {
      text <- ""
    }
    text
  })
  
  ## Tanks and Beers
  beerModuleServer("beer", beer, material)
  
  ## Raw Material
  
  materialModuleServer("material", material, general, costInfo)
  
  ## Demand
  demandModuleServer("demand", demand)
}

shinyApp(ui = ui, server = server)
