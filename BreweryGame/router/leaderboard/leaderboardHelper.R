getLeaderboard <- function() {
  conn <- getAWSConnection()
  
  query <- paste0("SELECT u.userID, u.username, g.cashBalance, g.gameSeed FROM gameTrack g INNER JOIN userInfo u ON g.userID=u.userID ORDER BY g.cashBalance DESC LIMIT 10")
  result <- dbGetQuery(conn,query)
  #result should return a single row
  dbDisconnect(conn)
  return(result)
}
