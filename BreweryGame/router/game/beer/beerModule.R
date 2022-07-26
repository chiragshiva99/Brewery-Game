source("router/game/beer/beerHelper.R")

brewModal <- function(session, tankOptions, beerOptions) {
  ns <- session$ns
  modalDialog(
    title = "Brew a Beer",
    selectInput(ns("tankSelect"), "Choose a Tank", choices=tankOptions),
    selectInput(ns("beerChosen"), "Choose a Beer", choices=beerOptions),
    htmlOutput(ns("beerReq")),
    footer=tagList(
      modalButton("Cancel"),
      actionButton(ns("brewBeer"), "Brew Beer")
    )
    
  )
}

beerModuleUI <- function(id) {
  ns <- NS(id)
  column(width=4,
         box(width=NULL,
             title="Beer Inventory",
             htmlOutput(ns("beerQty"))),
         box(width=NULL,
             title="Brewery Tanks",
             actionButton(ns("brew"), "Brew"),
             htmlOutput(ns("tankInfo"))
         )
  )
}

beerModuleServer <- function(id, beer, material, beerInfo, beerReq, disabled) {
  moduleServer(
    id,
    function(input, output, session) {
      output$beerQty <- renderTable({beer$beerInv})
      
      output$tankInfo <- renderTable({select(beer$tanks, -daysToComplete, -tankSize)})
      
      observeEvent(input$brew, {    
        shinyjs::disable("brewBeer")
        showModal(brewModal(session, beer$tanks[,"Tank"], beer$beerInv[, "name"]))})
      
      output$beerReq <- renderUI({
        click <- input$brew
        shinyjs::disable("brewBeer")
        if (beer$tanks[input$tankSelect,"Beer"] == "Empty") {
          text <- getReqAmt(input$beerChosen, beerReq, material$rawMatQty)
        } else {
          text <- paste("Tank", input$tankSelect, "is occupied.")
          shinyjs::disable("brewBeer")
        }
        text
      })
      
      observeEvent(input$brewBeer, {
        beer$tanks <- addBeerToTank(beer$tanks, input$tankSelect, input$beerChosen, beerInfo)
        material$rawMatQty <- updateRawMatQty(beerReq, material$rawMatQty, input$beerChosen)
        removeModal()
      })
      
      observeEvent(disabled(), {
        print("USER inner scope")
        print(disabled)
        if(disabled()) {
          shinyjs::disable("brew")
        } else {
          shinyjs::enable("brew")
        }
      })
      
    }
   
  )
}