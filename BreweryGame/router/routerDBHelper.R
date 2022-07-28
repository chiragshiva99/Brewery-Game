getGameID <- function(userID, connect=NULL) {
  if(is.null(connect)) {
    conn <- getAWSConnection()
  } else {
    conn <- connect
  }
  
  queryTemplate <- "SELECT max(gameID) as gameID FROM gameTrack WHERE userID=?id1;"
  query <- sqlInterpolate(conn, queryTemplate, id1=userID)
  result <- dbGetQuery(conn, query)
  
  if (is.null(connect)) {
    dbDisconnect(conn)
  }
  return(result)
}

createGame <- function(userID, conditionID=1) {
  conn <- getAWSConnection()
  
  queryTemplate <- "INSERT INTO gameTrack (userID, conditionID) VALUES (?id1, ?id2)"
  query <- sqlInterpolate(conn, queryTemplate, id1=userID, id2=conditionID)
  success <- F
  
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print(result)
      success <- TRUE
    }, 
    error=function(cond){print("registerGame: ERROR")
      print(cond)}, 
    warning=function(cond){print("registerGame: WARNING")
      print(cond)}
  )
  
  
  #REGISTERING USER 
  if (!success) {
    gameID = NULL
  } else {
    result <- getGameID(userID, conn)
    print("result")
    print(result)
    gameID <- result[1, "gameID"]
  }
  #Close the connection
  dbDisconnect(conn)
  print(success)
  print(paste("gameID", gameID))
  return(gameID)
}  

updateGameID <- function(userID, gameID) {
  conn <- getAWSConnection()
  
  queryTemplate <- "UPDATE userInfo SET curGameID=?id1 WHERE userID=?id2"
  query <- sqlInterpolate(conn, queryTemplate, id1=gameID, id2=userID)
  result <- dbExecute(conn, query)
  
  result
}

deleteGame <- function(userID, gameID, range=c(1:length(tables))) {
  tables <- c("beerTrack", "materialTrack", "cashTrack", "demandTrack", "tankTrack", "gameTrack")
  results <- c()
  for (table in tables[range]) {
    res <- deleteFromTable(table, list(userID=userID, gameID=gameID))
    results <- c(results, res)
  }
  
  if(all(results==T)) {
    print("all deleted")
    return(T)
  } else {
    return(F)
  }
}
