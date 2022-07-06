#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# At the beginning of any R session, record your AWS database password:
source("dbHelper.R")

# Now, anywhere in your code where the password is needed you can get it using
# getOption("AWSPassword")
# Otherwise it is hidden. So now this code can be shared with anyone 
# without giving them access to your personal AWS database.

source("usePackages.R")
pkgnames <- c("tidyverse","shiny", "shinyjs","DBI","jsonlite","shinydashboard")
loadPkgs(pkgnames)

source("helper.R")
source("dbHelper.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The Brewery Game"),
    fluidRow(
      column(2,h3("Money"),
               htmlOutput("money"), offset=1),
      column(2, h3("Day"),
                htmlOutput("day")),
      column(2, h3("Lost Sales"),
             htmlOutput("lostSales"),
             htmlOutput("lostSalesPerBeer")
             ),
      column(2,
             actionButton("reset", "Reset Game"))),
    fluidRow(
      column(3, h3("Raw Materials"),
               htmlOutput("maltQty"),
               htmlOutput("hopsQty"),
               htmlOutput("yeastQty"),
             fluidRow(
               actionButton("purchase", "Purchase"),
               htmlOutput("currentOrders")
             )
             ),
      column(3, h3("Brewery Tanks"),
             column(6,
               htmlOutput("tank1status"),
               actionButton("tank1", "Tank 1"),
               htmlOutput("tank2status"),
               actionButton("tank2", "Tank 2")
             ),
             column(6,
               htmlOutput("tank3status"),
               actionButton("tank3", "Tank 3"),
               htmlOutput("tank4status"),
               actionButton("tank4", "Tank 4")
             )
             ),
      column(3, h3("Beer Inventory"),
             htmlOutput("lagerQty"),
             htmlOutput("ipaQty"),
             htmlOutput("stoutQty")),
      column(3, h3("Customer Demand"),
             htmlOutput("custDemand"))
    ),
    fluidRow(
      column(2, offset=10,
             actionButton("advance", "Advance: Next Day"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ## Initializing stuff
  rawMat <- c("Malt", "Hops", "Yeast")
  tanks <-  as.data.frame(matrix(nrow=4, ncol=2))
  colnames(tanks) <- c("Beers", "Days")
  
  rawMatOrder <-  data.frame(matrix(nrow=0, ncol=3))
  colnames(rawMatOrder) <- c("Material","Quantity", "Days")
  
  beerInv <-  c(Lager=10, IPA=10, Stout=10)
  beerRev <- c(Lager=10, IPA=10, Stout=10)
  stockOut <- c(Lager=3, IPA=3, Stout=3)
  
  rawMatQty <- c(Malt=30, Hops=30, Yeast=30)
  
  beerReq <- as.data.frame(matrix(rep(3:1,3), nrow=3, ncol=3))
  colnames(beerReq) <- c("Malt", "Hops", "Yeast")
  rownames(beerReq) <- c("Lager", "IPA", "Stout")
  
  costInfo <- as.data.frame(matrix(rep(3, 6), nrow=3, ncol=2))
  colnames(costInfo) <- c("Fixed", "Variable")
  rownames(costInfo) <- c("Malt", "Hops", "Yeast")
  
  startingMoney <- 10000
  tankSize <- 10
  fermentDays <- 3
  orderComplete <- 2
  totalDays <- 10
  meanArrivalTimes <- 1
  maxWait <- 3
  normDistParams <- c(5,3)
  
  ## Demand Generation
  totalDemand <- generateDemand(meanArrivalTimes, normDistParams, totalDays)
  demand <- subset(totalDemand, Day==1)
  if (nrow(demand) > 0){
    demand$Day <- 0
  }
  
  ## Lost Sales Tracking
  lostPerBeer <- c(Lager=0, IPA=0, Stout=0)
  #Reactive Values
  vals <- reactiveValues(money=startingMoney, day=1, demand=demand, lostCust=0, lostPerBeer=lostPerBeer, tanks=tanks, beerInv=beerInv, rawMatOrder=rawMatOrder, rawMatQty=rawMatQty, tankSelect=NULL, beerChosen=NULL, purchQty=NULL, matChosen=NULL)
  
  ## Reset Game 
  observeEvent(input$reset, {
    print("resetting")
    showModal(resetDialog())
  })
  
  observeEvent(input$resetok, {
    removeModal()
    totalDemand <- generateDemand(meanArrivalTimes, normDistParams, totalDays)
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
    click <- vals$advance + vals$lostPerBeer
    lostTable <- data.frame(matrix(vals$lostPerBeer, nrow=1, ncol=3))
    colnames(lostTable) <- names(vals$lostPerBeer)
    lostTable
    })
  ## Advance Button
  
  observeEvent(input$advance, {
    vals$day <- vals$day + 1
    ## Increase the number of days for tanks and Order
    vals$tanks <- incrementDays(vals$tanks, "Days")
    vals$rawMatOrder <- incrementDays(vals$rawMatOrder, "Days")
    vals$demand <- incrementDays(vals$demand, "Day")
    
    ## Add completed Beers
    completeTanks <- which(vals$tanks["Days"] >= fermentDays)
    completeBeers <- c()
    for (tank in completeTanks) {
      completeBeers <- c(completeBeers, vals$tanks[tank, "Beers"])
      vals$tanks[tank, ] <- NA
    }
    for (beer in completeBeers) { # Can be refactored to use factor instead of list of completeBeers
      vals$beerInv[beer] <- vals$beerInv[beer] + tankSize
    }
    
    ## Add completed raw Material Orders
    collate <- c(c(which((vals$rawMatOrder["Days"] >= orderComplete) & (vals$rawMatOrder["Material"] == "Malt"))), c(which((vals$rawMatOrder["Days"] >= orderComplete) & (vals$rawMatOrder["Material"] == "Hops"))),c(which((vals$rawMatOrder["Days"] >= orderComplete) & (vals$rawMatOrder["Material"] == "Yeast"))))
    
    orderArrived <- c()
    for (idx in collate) {
      if (!identical(idx, integer(0))) {
        orderArrived <- c(orderArrived, idx)
      }
    }
    for (order in orderArrived) {
      mat <- vals$rawMatOrder[order, "Material"]
      qty <- vals$rawMatOrder[order, "Quantity"]
      vals$rawMatQty[mat] <- vals$rawMatQty[mat] + qty
    }
    if (! vector.is.empty(orderArrived) ){
      vals$rawMatOrder <- vals$rawMatOrder[-c(orderArrived),]
    }

    ## Satisfy and unsatisfied Demand
    removeDemand <- c()
    unsatisDemand <- c()
    if (nrow(vals$demand) > 0) {
      for (row in 1:nrow(vals$demand)) {
        # Satisfied?
        beerType <- vals$demand[row, "Beer"]
        qty <- vals$demand[row, "Quantity"]

        if (unname(vals$beerInv[beerType]) >= qty) {
          vals$beerInv[beerType] <- vals$beerInv[beerType] - qty
          vals$money <- vals$money + qty*beerRev[beerType]
          removeDemand <- c(removeDemand, row)
        }
        
        # Unsatisfied?
        dayWait <- vals$demand[row, "Day"]
        if (dayWait >= maxWait) {
          unsatisDemand <- c(unsatisDemand, row)
          vals$lostPerBeer[beerType] <- vals$lostPerBeer[beerType] + qty
          vals$money <- vals$money - qty*stockOut[beerType]
          removeDemand <- c(removeDemand, row)
        }
      }
    }
    
    if (! vector.is.empty(removeDemand)){
      vals$demand <- vals$demand[-c(removeDemand),]
    }
    
    if (length(unsatisDemand) > 0) {
      vals$lostCust <- vals$lostCust + length(unsatisDemand)
    }
    
    ## Add New Demand
    newDemand <- subset(totalDemand, Day==vals$day)
    if (nrow(newDemand) > 0) {
      newDemand$Day <-  0
      vals$demand <- rbind(vals$demand, newDemand)
    }
  
  })
  
  
  ## Tanks
  
  output$tank1status <- renderUI({tankStatus(vals$tanks, 1)})
  output$tank2status <- renderUI({tankStatus(vals$tanks, 2)})
  output$tank3status <- renderUI({tankStatus(vals$tanks, 3)})
  output$tank4status <- renderUI({tankStatus(vals$tanks, 4)})
  
  observeEvent(input$tank1, {
    vals$tankSelect <- 1
    tankModal(vals$tanks, 1)})
  observeEvent(input$tank2, {
    vals$tankSelect <- 2
    tankModal(vals$tanks, 2)})
  observeEvent(input$tank3, {
    vals$tankSelect <- 3
    tankModal(vals$tanks, 3)})
  observeEvent(input$tank4, {
    vals$tankSelect <- 4
    tankModal(vals$tanks, 4)})
  
  observeEvent(input$beerChosen, {
    vals$beerChosen <- input$beerChosen
  })
  
  observeEvent(input$makeBeer, {
    entryResult <- addNewEntry(vals$tanks, vals$tankSelect, vals$beerChosen)
    vals$tanks <- entryResult[[1]]
    removeModal()
    if (entryResult[[2]]) {
      vals$rawMatQty <- updateRawMatQty(beerReq, vals$rawMatQty, vals$beerChosen)
    } else {
      showModal(modalDialog(
        title="Tank is already full", easyClose=T
      ))
    }
  })
  
  ## Raw Material
  
  observeEvent(input$purchase, {
    showModal(purchaseModal())
  })

  output$maltQty <- renderUI({paste0("Malt: ", vals$rawMatQty["Malt"])})
  output$hopsQty <- renderUI({paste0("Hops: ", vals$rawMatQty["Hops"])})
  output$yeastQty <- renderUI({paste0("Yeast: ", vals$rawMatQty["Yeast"])})
  
  ### Purchase of Raw Mat
  observeEvent(input$quantity, {vals$purchQty <- input$quantity})
  observeEvent(input$matChosen, {vals$matChosen <- input$matChosen})
  
  output$costOfPurchase <- renderUI({paste("Amount:", calculateCost(costInfo, vals$matChosen, vals$purchQty))})
  
  observeEvent(input$purchaseok, {
    newEntry <- data.frame(Material=vals$matChosen, Quantity=vals$purchQty, Days=0)
    vals$rawMatOrder <- rbind(vals$rawMatOrder, newEntry)
    vals$money <- vals$money - calculateCost(costInfo, vals$matChosen, vals$purchQty)
    removeModal()
  })
  
  output$currentOrders <- renderTable({
    click <- input$purchaseok + input$advance
    vals$rawMatOrder
    
  })
  
  ## Beer Inventory
  
  output$lagerQty <- renderUI({paste0("Lager: ", vals$beerInv["Lager"])})
  output$ipaQty <- renderUI({paste0("IPA: ", vals$beerInv["IPA"])})
  output$stoutQty <- renderUI({paste0("Stout: ", vals$beerInv["Stout"])})
  
  ## Demand
  output$custDemand <- renderTable({
    click <- input$advance
    vals$demand
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
