getLeaderboardUser <- function(userID) {
  conn <- getAWSConnection()
  print(userID)
  query <- paste0("SELECT u.userID, u.username, g.cashBalance, g.gameSeed FROM gameTrack g LEFT JOIN userInfo u ON g.userID=u.userID WHERE g.userID=",userID, "  ORDER BY g.cashBalance DESC LIMIT 10")
  result <- dbGetQuery(conn,query)
  #result should return a single row
  dbDisconnect(conn)
  return(result)
}