beerBrewModuleUI <- function(id, tankOptions, beerOptions) {
  ns <- NS(id)
  div(
    h4("Brew a Beer"),
    selectInput(ns("tankSelect"), "Choose a Tank", choices=tankOptions),
    selectInput(ns("beerChosen"), "Choose a Beer", choices=beerOptions),
    htmlOutput(ns("beerReq")),
    actionButton(ns("brewBeer"), "Brew Beer")
  )
}

beerBrewModuleServer <- function(id, beer, material, beerInfo, beerReq, disabled, general) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      
      output$beerReq <- renderUI({
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
        if(general$action >= general$maxAction) {
          return(
            sendSweetAlert(
              session=session,
              title="Max Actions Taken Today!",
              text=NULL,
              type="warning"
            )
          )
        }
        
        
        c(tanks, rawMatQty, general) %<-% brewBeer(beer$tanks, input$tankSelect, input$beerChosen, beerInfo, beerReq, material$rawMatQty, general)
        beer$tanks <- tanks
        material$rawMatQty <- rawMatQty
      })
      
      observeEvent(disabled(), {
        if(disabled()) {
          shinyjs::disable("brew")
        } else {
          shinyjs::enable("brew")
        }
      })
    }
  )
}