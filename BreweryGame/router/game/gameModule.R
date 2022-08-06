## Customer Modules
source("router/game/demand/customerLostModule.R")

## Inventory Module
source("router/game/invModule.R")
### Inv SubModules
source("router/game/beer/beerInvModule.R")
source("router/game/material/matInvModule.R")

## Action Module
source("router/game/actionModule.R")
### Action SubModules
source("router/game/material/matPurchaseModule.R")
source("router/game/beer/beerBrewModule.R")
source("router/game/demand/customerDemandModule.R")
source("router/game/automate/automateModule.R")

## Progress Module
source("router/game/progressModule.R")
### Progress SubModules
source("router/game/material/matProgModule.R")
source("router/game/beer/beerTankModule.R")
source("router/game/demand/totalDemandModule.R")

source("router/game/material/materialHelper.R")

source("router/game/beer/beerHelper.R")
source("router/game/gameHelper/gameDBHelper.R")
source("router/game/gameHelper/helper.R")
source("router/game/gameHelper/demandHelper.R")
source("router/game/gameHelper/stateHelper.R")
source("router/game/gameHelper/advanceHelper.R")
source("router/game/gameHelper/initHelper.R")

resetDialog <- function(session) {
  ns <- session$ns
  modalDialog(
    title="Reset?",
    div("Are you sure you want to reset?"),
    footer=tagList(
      modalButton("Cancel"),
      actionButton(ns("resetok"), "Reset")
    )
  )
}

endGameModal <- function(session) {
  ns <- session$ns
  modalDialog(
    title="End of Game",
    div("The game has ended!"),
    footer=tagList(
      actionButton(ns("resetok"), "Play Again!"),
      actionButton(ns("gotoAnalysis"), "Analyse Performance")
    )
  )
}


gameModuleUI <- function(id, disabled=F) {
  ns <- NS(id)
  tabItem(tabName ="gameTab", class = "active",
          # Application title
          fluidRow(
            column(width=2,
                   bs4ValueBoxOutput(ns("day"), width=12),
                   br()
                   ),
            bs4ValueBoxOutput(ns("money"), width=2),
            column(width=3,
                   customerLostUI(ns("customerLost"))
                   ),
            bs4ValueBoxOutput(ns("actionCounter"), width=2),
            column(width=3,
                fluidRow(
                  actionBttn(
                    inputId=ns("advance"), 
                    label="Advance: Next Day",
                    style="jelly",
                    color="danger")
                ),
                br(),
                fluidRow(
                  column(width=5,
                         htmlOutput(ns("advanceNInput"))
                         ),
                  column(width=7,
                         br(),
                         htmlOutput(ns("advanceNButton"))
                         )
                )
            )
          ),
          fluidRow(
            column(
              width=3,
              invModuleUI(ns("inventory"))
            ),
            column(
              width=5,
              actionModuleUI(ns("action"))
            ),
            column(
              width=4,
              progressModuleUI(ns("progress"))
            )
          )
  )
}

gameModuleServer <- function(id, USER) {
  moduleServer(
    id,
    function(input, output, session) {
      ## Initializing stuff
      ns <- session$ns
      
      INIT = list(
        condition=1,
        tankOptions=list(tankNo=6, tankSize=100),
        startingMoney=100000,
        totalDays=200,
        endDays=100,
        initDay=51,
        interestRate=0.1
      )
      
      c(INIT, gameStateData, AUTO, general, beer, material, demand) %<-% initGameArgs(INIT)
      
      # General
      ## Reset Game 
      observeEvent(input$reset, {
        print("resetting")
        showModal(resetDialog(session))
      })
      
      observeEvent(input$resetok, {
        removeModal()
        
        USER$finish <- F
        
        shinyjs::enable("brew")
        shinyjs::enable("purchase")
        shinyjs::enable("advance")
        
        INIT$seed <- sample(1:2^15, 1)
        set.seed(INIT$seed)
        
        INIT$totalDemand <- generateTotalDemand(INIT$customerDemand, totalDays)
        dayDemand <- subset(INIT$totalDemand, arrivalDay==1)
        
        general$money <- INIT$tartingMoney
        general$day <- INIT$initDay
        demand$dayDemand <- dayDemand
        demand$lostCust <- 0
        demand$lostPerBeer <- INIT$lostPerBeer
        beer$tanks <- INIT$tanks
        beer$beerInv <- INIT$beerInv
        material$rawMatOrder <- INIT$rawMatOrder
        material$rawMatQty <- INIT$rawMatQty
        
        result <- createGame(USER$id)
        if(is.null(result)) {
          print("ERROR in creation")
        } else {
          USER$gameID <- result
        }
        
        ### Assign as current GameID in database
        result <- updateGameID(USER$id, USER$gameID)
      })
      
      observeEvent(input$gotoAnalysis,{
        removeModal()
        USER$selectedTab <- "analysisTab"
      })
      ## Info params
      output$money <- renderbs4ValueBox({
        bs4ValueBox(
          h1(paste("$", as.character(general$money))), 
          "Cash Balance",
          icon=icon("dollar-sign"),
          color="success",
          gradient=T
        )
      })
      
      output$day <- renderbs4ValueBox({
        bs4ValueBox(
          h1(general$day), 
          "Day",
          icon=icon("calendar"),
          color="info",
          gradient=T
        )
      })
      
      output$actionCounter <- renderbs4ValueBox({
        bs4ValueBox(
          h1(paste0(general$action, "/", general$maxAction)),
          "Beer Actions Taken",
          color="danger"
        )
      })
      
      ## Advance Button
      
      output$advanceNInput <- renderUI({
        numericInput(
          ns("advDays"),
          label="Advance N days",
          value=10,
          min=1,
          max=INIT$endDays - general$day + 1
        )
      })
      
      output$advanceNButton <- renderUI({
        actionBttn(
          inputId=ns("advanceN"), 
          label=paste("Advance:", input$advDays, "Days"),
          style="jelly",
          color="danger",
          size="sm"
          )
      })
      
      observeEvent(input$advance, {
        c(USER, AUTO, gameStateData, general, beer, material, demand) %<-% advanceDay(USER, AUTO, gameStateData, general, beer, material, demand, INIT)
      })
      
      observeEvent(input$advanceN, {
        if(as.integer(input$advDays) != input$advDays) {
          return(
            sendSweetAlert(
              session = session,
              title = "Key in an Integer !!!",
              text = NULL,
              type = "error"
            )
          )
        } 
        
        if(input$advDays < 1) {
          return(
            sendSweetAlert(
              session = session,
              title = "Key in a value larger than 1 !!!",
              text = NULL,
              type = "error"
            )
          )
        } 
        
        if(input$advDays > (INIT$endDays - general$day)) {
          return(
            sendSweetAlert(
              session = session,
              title = "Can't Advance beyond Day 390 !!!",
              text = paste("Key in a value less than", INIT$endDays - general$day),
              type = "error"
            )
          )
        }
        
        sendSweetAlert(
          session = session,
          title = "Please wait while the game is advancing",
          text = NULL,
          type = "info"
        )
        
        for(i in 1:input$advDays) {
          c(USER, AUTO, gameStateData, general, beer, material, demand) %<-% advanceDay(USER, AUTO, gameStateData, general, beer, material, demand, INIT)
        }
      })
      
      observe({
        if(USER$finish) {
          shinyjs::disable("advance")
        } else {
          shinyjs::enable("advance")
        }
      })
      
      output$gameStatus <- renderUI({
        if (general$day > INIT$endDays) {
          text <- "Game has Ended!"
        } else {
          text <- ""
        }
        text
      })
      
      observeEvent(general$day, {
        if(general$day == (INIT$endDay + 1)) {
          sendSweetAlert(
            session = session,
            title = "The Game will now simulate for the remaining 100 Days",
            text = NULL,
            type = "info"
          )
          
          for(i in 1:100) {
            c(USER, AUTO, gameStateData, general, beer, material, demand) %<-% advanceDay(USER, AUTO, gameStateData, general, beer, material, demand, INIT)
          }
          return()
        }
        
        if(general$day == (INIT$totalDays)) {
          return(
            endGameModal(session)
          )
        }
      })
      
      ### Inventory
      invModuleServer("inventory", beer, material)
      
      
      ## Tanks and Beers
      disabled <- reactive(USER$finish)
      observeEvent(USER$finish, {
        disabled <- USER$finish
      })
      
      AUTO <- actionModuleServer("action", general, beer, INIT$beerInfo, INIT$beerReq, material, INIT$costInfo, disabled, AUTO, demand, INIT$customerInfo, INIT$customerDemand, INIT$materialInfo)
      
      AUTO <- progressModuleServer("progress", material, beer, demand, general, INIT$beerInfo, INIT$customerInfo, INIT$customerDemand, INIT$tanks, AUTO)
      
      customerLostServer("customerLost", demand)
      
      return(list(USER, gameStateData))
    }
  )
}