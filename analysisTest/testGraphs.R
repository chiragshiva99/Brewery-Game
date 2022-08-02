library(ggplot2)
source("addParametersToDB.R")

getFromTrackTable <- function(table, userID, gameID) {
  conn <- getAWSConnection()
  
  queryTemplate <- paste("SELECT * FROM",table,"WHERE userID=?id2 AND gameID=?id3;")
  query <- sqlInterpolate(conn, queryTemplate, id2=userID, id3=gameID)
  
  result <- dbGetQuery(conn, query)
  
  dbDisconnect(conn)
  
  return(result)
}

demand <- getFromTrackTable("demandTrack", 1, 20)
demand$Beer <- ifelse(demand$beerID==1, "Beer 1", ifelse(demand$beerID==2, "Beer 2", "Beer 3"))

ggplot(data=demand, mapping=aes(gameDay, quantity, color=Beer)) +
  geom_segment(mapping = aes(x=gameDay, xend=gameDay, y=0, yend= quantity), size= 1) +
  geom_point(data=demand, size=3) +
  labs(title="Beer Demand", 
       subtitle="Demand Qty vs No. of Days", 
       x = "Number of Days",
       y = "Beer Quantity"
  )

ggplot(data=demand, mapping=aes(gameDay, quantity, fill= Beer)) +
  geom_bar(mapping = aes(x=gameDay, y=quantity), stat = "identity") +
  labs(title="Beer Demand", 
       subtitle="Demand Qty vs No. of Days", 
       x = "Number of Days",
       y = "Beer Quantity"
  )

ggplot(data=demand, mapping=aes(gameDay, quantity, fill= Beer)) +
  geom_bar(mapping = aes(x=gameDay, y=quantity), stat = "identity") +
  labs(title="Beer Demand", 
       subtitle="Demand Qty vs No. of Days", 
       x = "Number of Days",
       y = "Beer Quantity"
  )





tank <- getFromTrackTable("tankTrack", 1, 20)
tank$Beer <- ifelse(tank$beerID==1, "Beer 1", ifelse(tank$beerID==2, "Beer 2", "Beer 3"))

ggplot(tank, aes(gameDay, tankID, color=Beer)) + 
  geom_point(shape=15, size=10) + 
  scale_y_discrete(limits=c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))



