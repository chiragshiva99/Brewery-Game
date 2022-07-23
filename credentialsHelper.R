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
    textInput("username", "Enter your assigned Username", ""),
    passwordInput("password3", "Enter your password:"),
    if (failed)
      div(tags$b("There is no registered user with that name and password. Try again or re-register.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("loginok", "OK"),
      actionButton("changepw", "Change Password")
    )
  )
}


changepwModal <- function(failed1 = FALSE, failed2 = FALSE) {
  modalDialog(
    title = "Change Password",
    textInput("username", "Enter your assigned Username", ""),
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


getUserID <- function(username, password) {
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for username and password
  querytemplate <- "SELECT * FROM userInfo WHERE username=?id1 AND password=?id2;"
  query<- sqlInterpolate(conn, querytemplate,id1=username,id2=password)
  print(query) #for debug
  result <- dbGetQuery(conn,query)
  
  # If the query is successful, result should be a dataframe with one row
  if (nrow(result)==1){
    userid <- result$userID[1]
  } else {
    print(result) #for debugging
    userid <- 0
  }
  #print(result)
  #print(userid)
  dbDisconnect(conn) #Close the connection
  userid #return the userid
}

UserPasswordQuery <- function(conn, userid) {
  querytemplate <- "SELECT password FROM userInfo where userID = ?id1;"
  query <- sqlInterpolate(conn, querytemplate, id1=userid)
}

getUserPassword <- function(userid){
  conn <- getAWSConnection()
  query <- UserPasswordQuery(conn, userid)
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}

#CHECK 
createNewUserQuery <- function(conn,username,password){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  querytemplate <- "INSERT INTO userInfo (username,password, curGameID) VALUES (?id1,?id2, -1);" #HELP ME CHECK IF THIS LOOKS CORRECT
  query <- sqlInterpolate(conn, querytemplate,id1=username,id2=password)
}  

updatepwquery <- function(conn,username,password){
  querytemplate <- "UPDATE userInfo set password = ?id1 where username = ?id2;"
  query <- sqlInterpolate(conn, querytemplate, id1=password, id2=username)
}

updatePassword <- function(username,password){
  conn <- getAWSConnection()
  query <- updatepwquery(conn,username,password)
  result <- dbExecute(conn,query)
  dbDisconnect(conn)
}

registerUser <- function(username, password){
  #open the connection
  conn <- getAWSConnection()
  query <- createNewUserQuery(conn, username, password)
  print(query) #for debug
  
  #CHECK SUCCESS WHILE LOOP
  
  
  
  #REGISTERING USER 
  if (!success) username = NULL
  #Close the connection
  dbDisconnect(conn)
  username
}




















