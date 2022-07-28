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
  query <- "SELECT beerID, name, revenue, daysToComplete, stockOut FROM beerParameters"
  result <- dbGetQuery(conn,query)
  #result should return a single row
  print(result)
  dbDisconnect(conn)
  result
}

getMaterialInfo <- function() {
  conn <- getAWSConnection()
  query <- "SELECT materialID, name FROM materialNames"
  result <- dbGetQuery(conn, query)
  dbDisconnect(conn)
  
  result
}

getCustomerInfo <- function() {
  conn <- getAWSConnection()
  query <- "SELECT customerID, name FROM customerNames"
  result <- dbGetQuery(conn, query)
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

createNewPlayerQuery <- function(conn,playername,password){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  querytemplate <- "INSERT INTO LeaderPlayer (playername,password) VALUES (?id1,?id2);"
  query<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
}

updateDayState <- function(dayStateData, gameID, userID) {
  day <- dayStateData[1,"day"]
  jsonString <- toJSON(dayStateData)
  
  #open the connection
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO stateTrack (gameID, userID, gameDay, state) VALUES (?id1, ?id2, ?id3, ?id4);"
  query <- sqlInterpolate(conn, querytemplate,id1=gameID,id2=userID, id3=day, id4=jsonString)
  print(query) #for debug
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print("State Saved")
      success <- TRUE
    }, error=function(cond){print("publishState: ERROR")
      print(cond)
    }, 
    warning=function(cond){print("publishState: WARNING")
      print(cond)}
  )
  
  dbDisconnect(conn)
}

updateSeed <- function(userID, gameID, seed) {
  conn <- getAWSConnection()
  queryTemplate <- "UPDATE gameTrack SET gameSeed=?id3 WHERE userID=?id1 AND gameID=?id2"
  query <- sqlInterpolate(conn, queryTemplate, id1=userID, id2=gameID, id3=seed)
  
  success <- F
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print("Seed Saved")
      success <- TRUE
    }, error=function(cond){print("updateSeed: ERROR")
      print(cond)
    }, 
    warning=function(cond){print("updateSeed: WARNING")
      print(cond)}
  )
  
  dbDisconnect(conn)
  
  return(success)
}

updateCashBalance <- function(userID, gameID, cashBalance) {
  conn <- getAWSConnection()
  
  queryTemplate <- "UPDATE gameTrack SET cashBalance=?id3 WHERE userID=?id1 AND gameID=?id2"
  query <- sqlInterpolate(conn, queryTemplate, id1=userID, id2=gameID, id3=cashBalance)
  
  success <- F
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print("final cash balance Saved")
      success <- TRUE
    }, error=function(cond){print("updateCashBalance: ERROR")
      print(cond)
    }, 
    warning=function(cond){print("updateCashBalance: WARNING")
      print(cond)}
  )
  
  dbDisconnect(conn)
  
  return(success)
}