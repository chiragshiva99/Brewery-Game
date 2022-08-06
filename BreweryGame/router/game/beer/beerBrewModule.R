beerBrewModuleUI <- function(id, tankOptions, beerOptions) {
  ns <- NS(id)
  div(
    h4("Brew a Beer"),
    selectInput(ns("tankSelect"), "Choose a Tank", choices=tankOptions),
    selectInput(ns("beerChosen"), "Choose a Beer", choices=beerOptions),
    htmlOutput(ns("beerReq")),
    br(),
    htmlOutput(ns("brewButton"))
  )
}

beerBrewModuleServer <- function(id, beer, material, beerInfo, beerReq, disabled, general, selected) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$brewButton <- renderUI({
        
        color <- "success"
        
        if(general$action >= general$maxAction) {
          color <- "danger"
        }
        
        if(beer$tanks[input$tankSelect,"Beer"] != "Empty") {
          color <- "danger"
        }
        
        if(!checkMaterialAmount(input$beerChosen, beerReq, material$rawMatQty)) {
          color <- "danger"
        }
        
        return(
          actionBttn(ns("brewBeer"), "Brew Beer", style="jelly", color=color)
        )
      })
      
      output$beerReq <- renderUI({
        shinyjs::disable("brewBeer")
        if (beer$tanks[input$tankSelect,"Beer"] == "Empty") {
          text <- getReqAmt(input$beerChosen, beerReq, material$rawMatQty)
        } else {
          text <- paste("Tank", input$tankSelect, "is occupied.")
        }
        h4(text)
      })
      
      observeEvent(input$brewBeer, {
        selected$tab <- "Brew Beer"
        
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
        
        if(beer$tanks[input$tankSelect,"Beer"] != "Empty") {
          return(
            sendSweetAlert(
              session=session,
              title=paste("Tank", input$tankSelect, "is occupied!"),
              text=NULL,
              type="error"
            )
          )
        }
        
        if(!checkMaterialAmount(input$beerChosen, beerReq, material$rawMatQty)) {
          return(
            sendSweetAlert(
              session=session,
              title="Not Enough Material!",
              text=NULL,
              type="error"
            )
          )
        }
        
        c(tanks, rawMatQty, general) %<-% brewBeer(beer$tanks, input$tankSelect, input$beerChosen, beerInfo, beerReq, material$rawMatQty, general)
        beer$tanks <- tanks
        material$rawMatQty <- rawMatQty
        
        return(
          sendSweetAlert(
            session=session,
            title="Brewed",
            text=paste(input$beerChosen, "brewed in Tank", input$tankSelect),
            type="success"
          )
        )
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