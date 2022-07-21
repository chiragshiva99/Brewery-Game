pwd <- rstudioapi::askForPassword("AWS database password")
options(AWSPassword=pwd)
rm(pwd)

getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "project016",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    port = 3306,
    username = "project016",
    password = getOption("AWSPassword"))
  conn
}

##Add a dataframe of values to DB
addToTable <- function(table,dfin){
  conn <- getAWSConnection()
  columns <-  paste0(colnames(dfin), collapse=", ")
  dfin[sapply(dfin, is.character)] <- lapply(dfin[sapply(dfin, is.character)], function(x) paste0("\"",x,"\""))
  print(dfin)
  for (i in 1:nrow(dfin)){
    values <- paste(dfin[i,], collapse=", ")
    query <- paste("INSERT INTO", table, "(", columns, ") VALUES (", values, ")")
    #print(query) #for debug
    success <- FALSE
    tryCatch(
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        print("Value added")
        print(i)
        success <- TRUE
      }, error=function(cond){print("update: ERROR")
        print(cond)
        break
      }, 
      warning=function(cond){print("update: WARNING")
        print(cond)
        break
      },
      finally = {}
    )
  }
  dbDisconnect(conn)
}

#Get starting quantity of beer and RawMat Inv
getStartQty <- function(condition, table){
  conn <- getAWSConnection()
  if(table == "beerParameters"){
    id <- "beerID"
  } else if (table == "materialNames") {
    id <- "materialID"
  }
  query <- paste0("SELECT b.name, f.qty FROM (SELECT t.name as tableName, c.anyID, c.qty FROM conditionExtra c INNER JOIN tableNames t ON (t.tableID=c.tableID) WHERE t.name =\"",table,"\" AND c.conditionID=",condition ,") f INNER JOIN ",table," b ON ", "b.",id,"=f.anyID")
  result <- dbGetQuery(conn,query)
  #result should return a single row
  print(result)
  dbDisconnect(conn)
  result
}

getBeerReq <- function(){
  conn <- getAWSConnection()
  query <- "SELECT f.beerName, m.name as materialName, f.qty, f.processID FROM (SELECT b.name as beerName, r.materialID, r.qty, r.processID FROM beerReqParameters r INNER JOIN beerParameters b ON b.beerID=r.beerID) f INNER JOIN materialNames m ON m.materialID=f.materialID"
  result <- dbGetQuery(conn,query)
  #result should return a single row
  print(result)
  dbDisconnect(conn)
  result
}

getMaterialCost <- function(){
  conn <- getAWSConnection()
  query <- "SELECT f.supplierName, m.name as materialName, f.fixedCost, f.variableCost, f.daysToComplete FROM (SELECT s.name as supplierName, p.materialID, p.fixedCost, p.variableCost, p.daysToComplete FROM supplierNames s INNER JOIN supplierParameters p ON s.supplierID=p.supplierID) f INNER JOIN materialNames m ON f.materialID=m.materialID"
  result <- dbGetQuery(conn,query)
  #result should return a single row
  print(result)
  dbDisconnect(conn)
  result
}

getBeerInfo <- function(){
  conn <- getAWSConnection()
  query <- "SELECT name, revenue, daysToComplete, stockOut FROM beerParameters"
  result <- dbGetQuery(conn,query)
  #result should return a single row
  print(result)
  dbDisconnect(conn)
  result
}

getCustomerData <- function(){
  conn <- getAWSConnection()
  query <- "SELECT f.customerName, b.name as beerName, f.waitTime, f.meanArrivalTime, f.revenueExtra, f.mean, f.sd FROM (SELECT c.name as customerName, d.waitTime, d.meanArrivalTime, d.beerID, d.revenueExtra, d.mean, d.sd FROM customerNames c INNER JOIN demandParameters d ON c.customerID=d.customerID) f INNER JOIN beerParameters b ON f.beerID=b.beerID"
  result <- dbGetQuery(conn,query)
  #result should return a single row
  print(result)
  dbDisconnect(conn)
  result
}

### Stuff from LeaderBox, don't know if it'll be useful

getPlayerID <- function(playername,password){
  
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  querytemplate <- "SELECT * FROM LeaderPlayer WHERE playername=?id1 AND password=?id2;"
  query<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
  print(query) #for debug
  result <- dbGetQuery(conn,query)
  # If the query is successful, result should be a dataframe with one row
  if (nrow(result)==1){
    playerid <- result$playerid[1]
  } else {
    print(result) #for debugging
    playerid <- 0
  }
  #print(result)
  #print(playerid)
  #Close the connection
  dbDisconnect(conn)
  # return the playerid
  playerid
}

getRandomPlayerName <- function(conn){
  #Given a connection, call the View 'LeaderRandomName' and return the resulting name
  result <- dbGetQuery(conn,"SELECT * FROM LeaderRandomName")
  # result should be a dataframe with a single row and a column named 'playername'
  playername <- result$playername[1]
  # To test what happens when there is a duplicate entry, we can override the random result
  #playername <- "SophisticatedImaginaryZoo" # This matches an existing player in my database
  playername
}

createNewPlayerQuery <- function(conn,playername,password){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  querytemplate <- "INSERT INTO LeaderPlayer (playername,password) VALUES (?id1,?id2);"
  query<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
}

updateGame <- function(playerid,gamevariantid,turnstate,gamestate){
  # gamestate is a list of some sort so we convert it to a JSON string
  jsongamestate <-  paste0("'",toJSON(gamestate),"'")
  #open the connection
  conn <- getAWSConnection()
  #We could encounter an error or warning when we interact with the AWS database so enclose with a TryCatch()
  tryCatch({
    # A gamestate record should exist for this player and gamevariant.
    # So an UPDATE query should be safe.
    query <- paste0("UPDATE LeaderGameState SET turnstate=",turnstate,", jsongamestate=",jsongamestate)
    
    query <- paste0(query," WHERE playerid=",playerid," AND gamevariantid=",gamevariantid)
    #print(query)
    result <- dbExecute(conn,query)
    print(result) # for debugging: this will print the number of records affected. It should be 1 if successful.
    
  },
  error=function(cond){print(paste0("updateGame ERROR: ",cond))},
  warning=function(cond){print(paste0("updateGame WARNING: ",cond))},
  finally={}
  )
  #Close the connection
  dbDisconnect(conn)
}

registerPlayer <- function(password){
  #open the connection
  conn <- getAWSConnection()
  playername <- getRandomPlayerName(conn)
  query <- createNewPlayerQuery(conn,playername,password)
  print(query) #for debug
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
        playername <- getRandomPlayerName(conn)
        query <- createNewPlayerQuery(conn,playername,password) }, 
      warning=function(cond){print("registerPlayer: WARNING")
        print(cond)},
      finally = {print(paste0("Iteration ",iter," done."))
      }
    )
  } # end while loop
  # This may not have been successful
  if (!success) playername = NULL
  #Close the connection
  dbDisconnect(conn)
  playername
}

getGameVariantList <- function(){
  #open the connection
  conn <- getAWSConnection()
  gamevariants <- dbGetQuery(conn,"SELECT * FROM LeaderGameVariant")
  variantids <- gamevariants$gamevariantid # a vector
  variantnames <- gamevariants$variantname # a vector
  names(variantids) <- variantnames
  #Close the connection
  dbDisconnect(conn)
  variantids
}

publishScore <- function(playerid,gamevariantid,score){
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO LeaderScore (playerid,gamevariantid,asoftime,score) VALUES (?id1,?id2,NOW(),?id3)"
  query <- sqlInterpolate(conn, querytemplate,id1=playerid,id2=gamevariantid,id3=score)
  #print(query) #for debug
  success <- FALSE
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print("Score published")
      success <- TRUE
    }, error=function(cond){print("publishScore: ERROR")
      print(cond)
    }, 
    warning=function(cond){print("publishScore: WARNING")
      print(cond)},
    finally = {}
  )
  dbDisconnect(conn)
}

getLeaderBoard <- function(gamevariantid){
  conn <- getAWSConnection()
  #First, we need to know whether highscorewins for this game variant
  query <- paste0("SELECT highscorewins FROM LeaderGameVariant WHERE gamevariantid=",gamevariantid)
  result <- dbGetQuery(conn,query)
  #result should return a single row
  highscorewins <- result$highscorewins[1]
  #Assemble the query for this gamevariantid
  query <- "SELECT lp.playername,ls.score,ls.asoftime  FROM LeaderScore as ls INNER JOIN LeaderPlayer as lp"
  query <- paste0(query," ON (ls.playerid=lp.playerid) WHERE ls.gamevariantid =")
  query <- paste0(query,gamevariantid)
  if (highscorewins)
    query <- paste0(query, " ORDER BY ls.score DESC,ls.asoftime ASC")
  else
    query <- paste0(query, " ORDER BY ls.score ASC,ls.asoftime ASC")
  print(query) # for debugging
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}


startNewGame <- function(playerid,gamevariantid,playercolor,gamestate){
  # gamestate is a list of some sort so we convert it to a JSON string
  jsongamestate <-  paste0("'",toJSON(gamestate),"'")
  #open the connection
  conn <- getAWSConnection()
  #We could encounter an error or warning when we interact with the AWS database so enclose with a TryCatch()
  tryCatch({
    # A gamestate record may or may not exist for this player and gamevariant.
    # An UPDATE query will fail if no matching record exists.
    # An INSERT query may fail (because of duplicates) if a matching record exists.
    # So the safest action to DELETE all matching records and then INSERT a new one.
    query <-  paste0("DELETE FROM LeaderGameState WHERE playerid=",playerid," AND gamevariantid=",gamevariantid)
    print(query) # for debugging
    result <- dbExecute(conn,query)
    print(result) # for debugging: this will print the number of records affected. It will likely be 0 or 1.
    # Now we can insert a new record for this game.
    query <- paste0("INSERT INTO LeaderGameState (playerid,gamevariantid,turnstate,jsongamestate) VALUES (")
    query <- paste0(query,playerid,",",gamevariantid,",",playercolor,",",jsongamestate,")")
    print(query)
    result <- dbExecute(conn,query)
    print(result) # for debugging: this will print the number of records affected. It should be 1 if successful.
    
  },
  error=function(cond){print(paste0("startNewGame ERROR: ",cond))},
  warning=function(cond){print(paste0("startNewGame WARNING: ",cond))},
  finally={}
  )
  #Close the connection
  dbDisconnect(conn)
}

getGameTurnAndState <- function(playerid,gamevariantid){
  #open the connection
  conn <- getAWSConnection()
  query <- paste0("SELECT turnstate,jsongamestate FROM LeaderGameState WHERE playerid=",playerid," AND gamevariantid=",gamevariantid)
  print(query) # for debugging
  result <- dbGetQuery(conn,query)
  turnAndState <- list(turnstate=-1,gamestate=list())
  if (nrow(result)>0){
    turnAndState$turnstate <- result$turnstate[1]
    turnAndState$gamestate <- fromJSON(result$jsongamestate[1])
  }
  print(turnAndState) # for debugging  
  #Close the connection
  dbDisconnect(conn)
  turnAndState
}