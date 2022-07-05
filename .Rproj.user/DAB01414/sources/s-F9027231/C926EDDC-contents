#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("usePackages.R")
pkgnames <- c("tidyverse","shiny", "shinyjs","DBI","jsonlite","shinydashboard")
loadPkgs(pkgnames)

source("helper.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The Brewery Game"),
    fluidRow(column(2,h3("Money"),
             htmlOutput("money"), offset=1)),
    fluidRow(
      column(3, h3("Raw Materials"),
               htmlOutput("maltQty"),
               htmlOutput("hopsQty"),
               htmlOutput("yeastQty"),
             fluidRow(
               actionButton("purchase", "Purchase")
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
      column(3, h3("Customer Demand"))
    ),
    fluidRow(
      column(2, offset=10,
             actionButton("advance", "Advance: Next Day"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  tanks <-  as.data.frame(matrix(nrow=4, ncol=2))
  colnames(tanks) <- c("Beers", "Days")
  
  rawMatOrder <-  as.data.frame(matrix(nrow=3, ncol=2))
  colnames(rawMatOrder) <- c("Quantity", "Days")
  rownames(rawMatOrder) <- c("Malt", "Hops", "Yeast")
  
  beerInv <-  c(Lager=10, IPA=20, Stout=30)
  
  rawMatQty <- c(Malt=30, Hops=30, Yeast=30)
  
  beerReq <- as.data.frame(matrix(rep(3:1,3), nrow=3, ncol=3))
  colnames(beerReq) <- c("Malt", "Hops", "Yeast")
  rownames(beerReq) <- c("Lager", "IPA", "Stout")
  
  costInfo <- as.data.frame(matrix(rep(3, 6), nrow=3, ncol=2))
  colnames(costInfo) <- c("Fixed", "Variable")
  rownames(costInfo) <- c("Malt", "Hops", "Yeast")
  
  tankSize <- 10
  fermentDays <- 3
  orderComplete <- 2
  
  #Reactive Values
  vals <- reactiveValues(money=10000, tanks=tanks, beerInv=beerInv, rawMatOrder=rawMatOrder, rawMatQty=rawMatQty, tankSelect=NULL, beerChosen=NULL, purchQty=NULL, matChosen=NULL)
  
  ## Money
  output$money <- renderUI({paste("$", vals$money)})
  
  ## Advance Button
  
  observeEvent(input$advance, {
    print("Advance")
    
    ## Increase the number of days for tanks and Order
    vals$tanks <- incrementDays(vals$tanks)
    vals$rawMatOrder <- incrementDays(vals$rawMatOrder)
    completeTanks <- which(vals$tanks["Days"] >= fermentDays)
    completeBeers <- c()
    for (tank in completeTanks) {
      completeBeers <- c(completeBeers, vals$tanks[tank, "Beers"])
      vals$tanks[tank, ] <- NA
    }
    print(completeBeers)
    for (beer in completeBeers) {
      vals$beerInv[beer] <- vals$beerInv[beer] + tankSize
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
    vals$tanks <- addNewEntry(vals$tanks, vals$tankSelect, vals$beerChosen)
    vals$rawMatQty <- updateRawMatQty(beerReq, vals$rawMatQty, vals$beerChosen)
    removeModal()
  })
  
  ## Raw Material
  
  observeEvent(input$purchase, {
    showModal(purchaseModal())
    print("Purchase")
  })

  output$maltQty <- renderUI({paste0("Malts: ", vals$rawMatQty[1])})
  output$hopsQty <- renderUI({paste0("Hops: ", vals$rawMatQty[2])})
  output$yeastQty <- renderUI({paste0("Yeast: ", vals$rawMatQty[3])})
  
  ### Purchase of Raw Mat
  observeEvent(input$quantity, {vals$purchQty <- input$quantity})
  observeEvent(input$matChosen, {vals$matChosen <- input$matChosen})
  
  output$costOfPurchase <- renderUI({paste("Amount:", calculateCost(costInfo, vals$matChosen, vals$purchQty))})
  
  observeEvent(input$purchaseok, {
    vals$rawMatOrder <- addNewEntry(vals$rawMatOrder, vals$matChosen, vals$purchQty)
    vals$money <- vals$money - calculateCost(costInfo, vals$matChosen, vals$purchQty)
    removeModal()
  })
  
  ## Beer Inventory
  
  output$lagerQty <- renderUI({paste0("Lager: ", vals$beerInv[1])})
  output$ipaQty <- renderUI({paste0("IPA: ", vals$beerInv[2])})
  output$stoutQty <- renderUI({paste0("Stout: ", vals$beerInv[3])})
  
  ## Demand
}

# Run the application 
shinyApp(ui = ui, server = server)
