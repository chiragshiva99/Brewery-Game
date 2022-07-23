# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("password", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "LOG IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     br(),
                     actionButton("signup", "Sign Up"),
                     shinyjs::hidden(
                       div(id = "nomatch",
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
# Main login screen
signuppage <- div(id = "signuppage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                  wellPanel(
                    tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                    textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                    passwordInput("password", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                    br(),
                    div(
                      style = "text-align: center;",
                      actionButton("signupok", "SIGN UP", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                      br(),
                      actionButton("loginReturn", "Log In instead")
                    ))
)


 