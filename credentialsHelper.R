signupModal <- function(failed1 = FALSE, failed2 = FALSE) {
  modalDialog(
    title = "Create a new account",
    textInput("username", "Enter your Username", ""),
    passwordInput("password1", "Enter your password:"),
    passwordInput("password2", "Confirm your password:"),
    "If successful, you will be assigned a Username to go with this password.",
    
    if (failed1)
      div(tags$b("The Username already exists. Try again.", style = "color: red;")),
    if (failed2)
      div(tags$b("The passwords do not match. Try again.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("signupok", "OK")
    )
  )
}


loginModal <- function(failed = FALSE) {
  modalDialog(
    title = "Login to your account",
    textInput("username2", "Enter your assigned Username", ""),
    passwordInput("password3", "Enter your password:"),
    if (failed)
      div(tags$b("There is no registered player with that name and password. Try again or re-register.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("loginok", "OK")
    )
  )
}


changepwModal <- function(failed1 = FALSE, failed2 = FALSE) {
  modalDialog(
    title = "Change Password",
    textInput("username3", "Enter your assigned Username", ""),
    passwordInput("password3", "Enter your current password:"),
    passwordInput("password4", "Enter your new password"),
    passwordInput("password5", "Confirm your new password"),
    
    if (failed1)
      div(tags$b("The Username does not exist. Try again.", style = "color: red;")),
    if (failed2)
      div(tags$b("The passwords do not match. Try again.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("changepwok", "OK")
    )
  )
}