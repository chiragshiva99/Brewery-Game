source("router/game/gameModule.R")
source("router/analysis/analysisModule.R")
source("router/login/loginModule.R")
source("router/userInfo/userInfoModule.R")

source("router/gameChoicePage.R")
source("router/routerDBHelper.R")

routerModuleUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    dashboardPage(
      title = "The Brewery Game Dashboard",
      fullscreen = TRUE,
      
      header = dashboardHeader(
        title = dashboardBrand(
          title = "The Brewery Game",
          color = "primary",
          href = "www",
          image = "BeerBoys.jpg",
        ),
        skin = "light",
        status = "white",
        border = TRUE,
        sidebarIcon = icon("bars"),
        controlbarIcon = icon("th"),
        fixed = FALSE,
        leftUi = tagList(
          dropdownMenu(
            badgeStatus = "info",
            type = "notifications",
            notificationItem(
              inputId = "triggerAction2",
              text = "Error!",
              status = "danger"
            )
          )
        ),
        uiOutput(ns("logoutbtn"))
      ),
      sidebar = dashboardSidebar(
        collapsed=TRUE, 
        skin = "light",
        status = "primary",
        elevation = 3,
        sidebarUserPanel(
          name = "Save Water, Drink Beer!"
        ),
        uiOutput(ns("sidebarpanel"))
      ),
      #controlbar = dashboardControlbar(),
      footer =  dashboardFooter(),
      body = dashboardBody(shinyjs::useShinyjs(), uiOutput(ns("body")))
    )
    )
}

routerModuleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ### LOGIN STUFF ###   From: https://www.listendata.com/2019/06/how-to-add-login-page-in-shiny-r.html
      ns <- session$ns 
      
      USER <- loginModuleServer("login")
      
      observeEvent(input$startGame, {
          
          ## Delete previous Game activity if it is in progress
          if(USER$gameID != -1) {
            result <- deleteGame(USER$id, USER$gameID)
          }
          
          USER$gameStart <-  T
          ### Assign User a gameID
          result <- createGame(USER$id)
          if(is.null(result)) {
            print("ERROR in creation")
          } else {
            USER$gameID <- result
          }
          
          ### Assign as current GameID in database
          result <- updateGameID(USER$id, USER$gameID)
      })
      
      #### Temporarily scrapped since letting user restore previous game is kind of troublesome to implement
      # output$continueOption <- renderUI({
      #   # print(paste("previous gameID", USER$gameID))
      #   # if(USER$gameID != -1) {
      #   #   actionButton(ns("continueGame"), "Continue Game")
      #   # }
      # })
      
      # observeEvent(input$continueGame, {
      #   ### Load Old Game State Data
      #   prevStateData <- loadPrevState(USER$id, USER$gameID)
      #   ### pass it to gameModule
      #   
      #   ### set user start game
      #   USER$gameStart <- T
      # })
      
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
        counter <- 0
        if(USER$login == T) {
          counter <- counter + 1
          sidebarMenuItems[[counter]] <- menuItem("User Info", tabName = "userInfoTab", icon=icon("dashboard"))
        }
        
        if (USER$gameStart == T) {
          counter <- counter + 1
          sidebarMenuItems[[counter]] <- menuItem("Main Page", tabName = "gameTab", icon = icon("dashboard"))
        }
        
        if ((USER$finish == T | USER$finish == F) & USER$gameStart == T) {
          counter <- counter + 1
          sidebarMenuItems[[counter]] <- menuItem("Analysis Page", tabName = "analysisTab", icon = icon("dashboard"))
        }
        
        print(sidebarMenuItems)
        return(sidebarMenu(.list=sidebarMenuItems))
      })
      
      output$body <- renderUI({
        if (USER$login == T & USER$gameStart == T) {
          tabItems(
            userInfoModuleUI(ns("user")),
            gameModuleUI(ns("game")),
            analysisModuleUI(ns("analysis"))
          )
        }
        else if (USER$login == T & USER$gameStart == F & USER$finish == F) {
          gameChoice(ns)
        }
        else {
          loginModuleUI(ns("login"))
        }
      })
        
      #### UserInfo Page ####
      userInfoModuleServer("user", USER)
      
      #### GAME STUFF ####
      stateData <- gameModuleServer("game", USER)
      
      #### ANALYSIS ####
      analysisModuleServer("analysis", stateData)
    }
  )
}