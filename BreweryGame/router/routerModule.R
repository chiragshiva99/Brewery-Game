source("router/game/gameModule.R")
source("router/analysis/analysisModule.R")

source("router/analysisPage.R")
source("router/loginPage.R")
source("router/gameChoicePage.R")

# source("game/helper.R")
# source("game/dbHelper.R")

routerModuleUI <- function(id) {
  ns <- NS(id)
  dashboardPage(
    dashboardHeader( title = "Brewery Game", uiOutput(ns("logoutbtn"))), 
    dashboardSidebar(collapsed=TRUE, uiOutput(ns("sidebarpanel"))), 
    dashboardBody(shinyjs::useShinyjs(), uiOutput(ns("body"))), 
    skin = "blue")
}

routerModuleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ### LOGIN STUFF ###   From: https://www.listendata.com/2019/06/how-to-add-login-page-in-shiny-r.html
      ns <- session$ns 
      loginInit <- T
      gameStartInit <- T
      finishInit <- F
      hasGameInit <- F
      prevGameInit <- -1
      signupInit <- F
      id=1
      gameID=1
      USER <- reactiveValues(id=id, gameID=gameID, login = loginInit, gameStart = gameStartInit, finish=finishInit, hasGame=hasGameInit, prevGame=prevGameInit, signup=signupInit)
      
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
          print(USER$gameStart)
          USER$gameStart <-  T
        }
      })
      
      output$continueOption <- renderUI({
        if(USER$hasGame == T) {
          actionButton(ns("continueGame"), "Continue Game")
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
        sidebarMenuItems <- list()
        
        if(USER$login == T) {
          sidebarMenuItems <- list(sidebarMenuItems, menuItem("User Info", tabName = "userInfo", icon=icon("dashboard")))
        }
        
        if (USER$gameStart == T) {
          sidebarMenuItems <- list(sidebarMenuItems, menuItem("Main Page", tabName = "gameTab", icon = icon("dashboard")))
        }
        
        if (USER$finish == T | USER$finish == F) {
          sidebarMenuItems <- list(sidebarMenuItems, menuItem("Analysis Page", tabName = "analysisTab", icon = icon("dashboard")))
        }
        
        print(sidebarMenuItems)
        return(sidebarMenu(.list=sidebarMenuItems))
      })
      
      output$body <- renderUI({
        if (USER$login == T & USER$gameStart == T) {
          tabItems(
            gameModuleUI(ns("game")),
            analysisModuleUI(ns("analysis"))
          )
        }
        else if (USER$login == T & USER$gameStart == F & USER$finish == F) {
          gameChoice(ns)
        }
        else if (USER$signup == T) {
          signuppage(ns)
        }
        else {
          loginpage(ns)
        }
      })
        
        
      
      #### GAME STUFF ####
      stateData <- gameModuleServer("game", USER)
      
      #### ANALYSIS ####
      analysisModuleServer("analysis", stateData)
    }
  )
}