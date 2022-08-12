getCredentials <- function(username) {
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  querytemplate <- "SELECT * FROM userInfo WHERE username=?id1;"
  query<- sqlInterpolate(conn, querytemplate,id1=username)
  print(query) #for debug
  result <- dbGetQuery(conn,query)
  print(result)
  # If the query is successful, result should be a dataframe with one row else return NA
  if (nrow(result)!=1){
    result <- data.frame(userID=0, username=NA, password=NA, curGameID=NA)
  } 
  print(result)
  
  #Close the connection
  dbDisconnect(conn)
  result
  
}

getUserID <- function(username, connect=NULL) {
  if(is.null(connect)) {
    conn <- getAWSConnection()
  } else {
    conn <- connect
  }
  
  queryTemplate <- "SELECT userID FROM userInfo WHERE username=?id1"
  query <- sqlInterpolate(conn, queryTemplate, id1=username)
  result <- dbGetQuery(conn, query)
  # print(result)

  # prevent SQL insertion attacks
  # if (nrow(result)==1){
  #   return(result)
  # } else {
  #   print(result) #for debugging
  #   userid <- 0
  # }
  if (is.null(connect)) {
    dbDisconnect(conn)
  }
  return(result)
}

createNewUserQuery <- function(conn,username,password){
  querytemplate <- "INSERT INTO userInfo (username,password) VALUES (?id1,?id2);" #HELP ME CHECK IF THIS LOOKS CORRECT
  query <- sqlInterpolate(conn, querytemplate,id1=username,id2=password)
}  

registerUser <- function(username, password){
  #open the connection
  conn <- getAWSConnection()
  

  query <- createNewUserQuery(conn, username, password)
  # print(query) #for debug
  success <- F
  #CHECK SUCCESS WHILE LOOP
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print(result)
      print("result")
      success <- TRUE
    }, 
    error=function(cond){print("registerUser: ERROR")
      print(cond)}, 
    warning=function(cond){print("registerUser: WARNING")
      print(cond)}
  )
  
  
  #REGISTERING USER 
  if (!success) {
    userID = NULL
  } else {
    result <- getUserID(username, conn)
    userID <- result[1, "userID"]
  }
  #Close the connection
  dbDisconnect(conn)
  
  return(userID)
}
