source("router/game/gameModule.R")
source("router/analysis/analysisModule.R")
source("router/login/loginModule.R")
source("router/userInfo/userInfoModule.R")
source("router/leaderboard/leaderboardModule.R")
source("router/tutorial/tutorialModule.R")

source("router/gameChoicePage.R")
source("router/routerDBHelper.R")


## File done by Gabriel
mytheme <- create_theme(
  bs4dash_font(
    size_base="0.8rem",
    weight_bold=900
  )
)

routerModuleUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    dashboardPage(
      title = "The Brewery Game Dashboard",
      fullscreen = TRUE,
      dark=TRUE,

      
      header = dashboardHeader(
        h3("Littlefield Brewery", style="color:#ffffff"),
        title = dashboardBrand(
          title = "The Brewery Game",
          color = "primary",
          href = "www",
          image = "BeerBoys.jpg",
        ),
        skin = "light",
        status = "dark",
        border = TRUE,
        sidebarIcon = icon("bars"),
        # controlbarIcon = icon("th"),
        fixed = FALSE,
        rightUi = userOutput(ns("userDropdown")),
        .list = 
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
      footer =  dashboardFooter(),
      body = dashboardBody(
        use_theme(mytheme),
        shinyjs::useShinyjs(), 
        uiOutput(ns("body"))
      )
                           
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
      
      # Starts game for user
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
      
      # Renders user dropdown
      output$userDropdown <- renderUser({
        req(USER$login)
        dashboardUser(
          name = USER$username,
          image = 'BeerBoys.jpg',
          title= USER$username,
          dashboardUserItem(
            width=18,
            uiOutput(ns("logoutbtn"))
          )
        )
      })
      
      # Logs out the user
      ## Code from https://www.listendata.com/2019/06/how-to-add-login-page-in-shiny-r.html
      output$logoutbtn <- renderUI({
        req(USER$login)
        tags$li(a(icon("fa fa-sign-out"), "Logout", 
                  href="javascript:window.location.reload(true)"),
                class = "dropdown", 
                style = "!important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
      })
      
      # Renders sidebar panel depending on conditions
      output$sidebarpanel <- renderUI({
        sidebarMenuItems <- list()
        counter <- 0
        if(USER$login == T & USER$gameStart == T) {
          sidebarMenuItems[[1]] <- menuItem("User Info", tabName = "userInfoTab", icon=icon("user"))
          sidebarMenuItems[[2]] <- menuItem("Main Page", tabName = "game", icon = icon("gamepad"))
          sidebarMenuItems[[3]] <- menuItem("Analysis Page", tabName = "analysisTab", icon = icon("dashboard"))
          sidebarMenuItems[[4]] <- menuItem("Leaderboard", tabName = "leaderTab", icon = icon("star"))
          sidebarMenuItems[[5]] <- menuItem("Tutorial", tabName = "tutorialTab", icon = icon("person-chalkboard"))
          
        }
        return(sidebarMenu(id=ns("tabs"), .list=sidebarMenuItems))
      })
      
      # Renders the body depending on conditions
      output$body <- renderUI({
        if (USER$login == T & USER$gameStart == T) {
          tabItems(
            userInfoModuleUI(ns("user")),
            gameModuleUI(ns("game")),
            analysisModuleUI(ns("analysis")),
            leaderboardModuleUI(ns("leader")),
            tutorialModuleUI(ns("tutorial"))
          )
        }
        else if (USER$login == T & USER$gameStart == F & USER$finish == F) {
          gameChoice(ns)
        }
        else {
          loginModuleUI(ns("login"))
        }
      })
      
      # Updates users selected tab
      observe({
        updateTabItems(inputId="tabs", selected=USER$selectedTab)
      })
        
      #### UserInfo Page ####
      userInfoModuleServer("user", USER)
      
      #### GAME STUFF ####
      c(USER, stateData) %<-%  gameModuleServer("game", USER)
      
      #### ANALYSIS ####
      analysisModuleServer("analysis", stateData)
      
      #### Leaderboard ####
      leaderboardModuleServer("leader", USER)
      
      #### Tutorial ####
      tutorialModuleServer("tutorial")
    }
  )
}