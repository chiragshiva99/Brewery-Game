
### CHIRAG

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

getUserUsername <- function(conn, userid) {
  #Given a connection, call the table 'userInfo' and return the resulting name
  querytemplate <- "SELECT username FROM userInfo where userID = ?id1;"
  result <- dbGetQuery(conn,querytemplate, id1=userid)
  username <- result$username[1]
  playername
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
  querytemplate <- "INSERT INTO userInfo (username,password) VALUES (?id1,?id2);" #HELP ME CHECK IF THIS LOOKS CORRECT
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
  #username <- getUserUsername(conn, userid)
  query <- createNewUserQuery(conn, username, password)
  print(query) #for debug
  
  #CHECK SUCCESS WHILE LOOP
  
  # This query could fail to run properly so we wrap it in a loop with tryCatch()
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        print(result)
        success <- TRUE
      }, error=function(cond){print("registerPlayer: ERROR")
        print(cond)
        # The query failed, likely because of a duplicate playername
        #playername <- getRandomPlayerName(conn)
        query <- createNewUserQuery(conn,username,password) }, 
      warning=function(cond){print("registerUser: WARNING")
        print(cond)},
      finally = {print(paste0("Iteration ",iter," done."))
      }
    )
  } # end while loop
  
  
  #REGISTERING USER 
  if (!success) username = NULL
  #Close the connection
  dbDisconnect(conn)
  username
}








