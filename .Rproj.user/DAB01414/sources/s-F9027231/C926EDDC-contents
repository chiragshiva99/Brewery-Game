#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(shiny)
library(shinyjs) 

source("helper.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The Brewery Game"),

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
  
  rawMatOrders <-  as.data.frame(matrix(nrow=3, ncol=2))
  colnames(rawMatOrders) <- c("Quantity", "Days")
  rownames(rawMatOrders) <- c("Malt", "Hops", "Yeast")
  
  beerInv <-  c(10, 20, 30)
  
  rawMatQty <- c(30, 30, 30)
  #Reactive Values
  vals <- reactiveValues(tanks=tanks, beerInv=beerInv, rawMatOrders=rawMatOrders, rawMatQty=rawMatQty, tankSelect=NULL, beerChosen=NULL)
  
  ## Advance Button
  
  observeEvent(input$advance, {
    print("Advance")
    vals$tanks <- incrementDays(vals$tanks)
    vals$rawMatOrder <- incrementDays(vals$rawMatOrder)
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
    vals$tanks <- addBeerToTank(vals$tanks, vals$tankSelect, vals$beerChosen)
    removeModal()
  })
  
  ## Raw Material
  
  observeEvent(input$purchase, {
    purchaseModal()
    print("Purchase")
  })
  
  observeEvent(input$purchaseok, {
    vals$rawMatOrders <- addOrder(vals$rawMatOrders)
  })

  output$maltQty <- renderUI({paste0("Malts: ", vals$rawMatQty[1])})
  output$hopsQty <- renderUI({paste0("Hops: ", vals$rawMatQty[2])})
  output$yeastQty <- renderUI({paste0("Yeast: ", vals$rawMatQty[3])})
  
  ## Beer Inventory
  
  output$lagerQty <- renderUI({paste0("Lager: ", vals$beerInv[1])})
  output$ipaQty <- renderUI({paste0("IPA: ", vals$beerInv[2])})
  output$stoutQty <- renderUI({paste0("Stout: ", vals$beerInv[3])})
  
  ## Demand
}

# Run the application 
shinyApp(ui = ui, server = server)
