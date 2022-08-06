source("router/login/loginDBHelper.R")

loginModuleUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("page"))
}

# Main login screen
loginpage <- function(ns) {
  div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      wellPanel(
        tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
        textInput(ns("username"), placeholder="Username", label = tagList(icon("user"), "Username")),
        passwordInput(ns("password"), placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
        br(),
        div(
          style = "text-align: center;",
          actionButton(ns("login"), "LOG IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
          br(),
          actionButton(ns("signup"), "Sign Up"),
          shinyjs::hidden(
            div(id = ns("nomatch"),
                tags$p("Oops! Incorrect username or password!",
                       style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                       class = "text-center"))),
          br(),
          br(),
          tags$code("Username: G  Password: mypass"),
          br(),
          tags$code("Username: myuser1  Password: mypass1")
        ))
  )
}
# Main login screen
signuppage <- function(ns) {
  div(id = "signuppage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      wellPanel(
        tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
        textInput(ns("usernameSignup"), placeholder="Username", label = tagList(icon("user"), "Username")),
        passwordInput(ns("passwordSignup"), placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
        br(),
        div(
          style = "text-align: center;",
          actionButton(ns("signupok"), "SIGN UP", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
          br(),
          actionButton(ns("loginReturn"), "Log In instead"),
          shinyjs::hidden(
            div(id = ns("userNotAvail"),
                tags$p("Oops! Username Not Available!",
                       style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                       class = "text-center"))),
          shinyjs::hidden(
            div(id = ns("errorRegister"),
                tags$p("Oops! Error Registering!",
                       style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                       class = "text-center"))),
        ))
  )
}

loginModuleServer <- function(id, USER) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      loginInit <- F
      gameStartInit <- F
      finishInit <- F
      signupInit <- F
      id=1
      gameID=-1
      USER <- reactiveValues(id=id, gameID=gameID, login = loginInit, gameStart = gameStartInit, finish=finishInit, signup=signupInit, selectedTab = NULL)
      
      observe({ 
        if (USER$login == FALSE) {
          if (!is.null(input$login)) {
            if (input$login > 0) {
              Username <- isolate(input$username)
              Password <- isolate(input$password)
              credentials <- getCredentials(Username)
              if(length(which(credentials$username==Username))==1) { 
                pasmatch  <- credentials["password"][which(credentials$username==Username),]
                pasverify <- password_verify(pasmatch, Password)
                print(str(credentials))
                if(pasverify) {
                  USER$login <- TRUE
                  USER$gameID <- credentials["curGameID"][which(credentials$username==Username),]
                  USER$id <- credentials["userID"][which(credentials$username==Username),]
                
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
      
      output$page <- renderUI({
        if(USER$signup == T) {
          signuppage(ns)
        } else {
          loginpage(ns)
        }
      })
      
      observeEvent(input$signup, {
        USER$signup <- T
      })
      
      observeEvent(input$loginReturn, {
        USER$signup <- F
      })
      
      observeEvent(input$signupok, {
        username <- input$usernameSignup
        password <- input$passwordSignup
        passwordHash <- password_store(password)
        print(username)
        ### Check for any SQL insertion attacks or any profanities in username
        # if(invalid) {
        #   shinyjs::toggle(id="userNotAvail")
        # }
        ### Check if Username is in use
        if(as.logical(nrow(getUserID(username)))) {
          print("USERNAME NOT AVAILABLE")
          shinyjs::toggle(id="userNotAvail")
        } else {
          ### Register User
          userID <- registerUser(username, passwordHash)
          if(is.null(userID)) {
            shinyjs::toggle(id="errorRegister")
          } else {
            ### Login User
            USER$login <- T
            USER$signup <- F
            USER$id <- userID
          }
        }
      })
      
      return(USER)
    }
  )
}