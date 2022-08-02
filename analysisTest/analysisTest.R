library(ggplot2)
library(vistime)
source("addParametersToDB.R")

getFromTrackTable <- function(table, userID, gameID) {
  conn <- getAWSConnection()
  
  queryTemplate <- paste("SELECT * FROM",table,"WHERE userID=?id2 AND gameID=?id3;")
  query <- sqlInterpolate(conn, queryTemplate, id2=userID, id3=gameID)
  
  result <- dbGetQuery(conn, query)
  
  dbDisconnect(conn)
  
  return(result)
}

tank <- getFromTrackTable("tankTrack", 1, 18)
tank

ggplot(tank, aes(gameDay, tankID)) + 
  geom_point(aes(color=beerID), shape=15, size=13) + 
  scale_y_discrete(limits=c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))

gg_vistime(tank)

tank

createVisTank <- function(tank) {
  visTank <- data.frame(matrix(nrow=0, ncol=4))
  colnames(visTank) <- c("event", "group", "start", "end")
  
  
  for (entry in 1:nrow(tank)) {
    data <- list()
    data$group <- as.character(tank[entry, "tankID"])
    data$event <- as.character(tank[entry, "beerID"])
    start <- tank[entry, "gameDay"]
    end <- tank[entry, "gameDay"]
    print(tank[entry,])
    print(visTank)
    index <- max(which((visTank$group == data$group) & (visTank$event == data$event)))
    print(index)
    if((nrow(subset(visTank, group==data$group))>0) & (visTank[index, "end"] + 1 == start)) {
      visTank[index, "end"] <- start
      next
    } else {
      data$start <- start
      data$end <- end
      visTank <- rbind(visTank, data)
    }
  }
  print(visTank)
  
  visTank$start <- as.Date(visTank$start, origin=origin)
  visTank$end <- as.Date(visTank$end, origin=origin)
  visTank <- visTank[order(visTank$group),]
  
  return(visTank)
}


?as.Date
origin <- "2022-07-22"

vistime(visTank)

tank
subset(tank, tankID == 1)
newTank <- tank[-c(4,5,7),]
newTank[c(9,11,14,17), "beerID"] <- 4 

newVis <- createVisTank(newTank)
vistime(newVis)


#data from demandTrack

getFromdemandTrackTable <- function(table, userID, gameID, beerID) {
  conn <- getAWSConnection()
  
  queryTemplate <- paste("SELECT * FROM",table,"WHERE userID=?id2 AND gameID=?id3 AND beerID=?id4;")
  query <- sqlInterpolate(conn, queryTemplate, id2=userID, id3=gameID, id4=beerID )
  
  result <- dbGetQuery(conn, query)
  
  dbDisconnect(conn)
  
  return(result)
}

demand <- getFromTrackTable("demandTrack", 1, 20)
demandBeer1 <- getFromdemandTrackTable("demandTrack", 1, 20, 1)
demandBeer2 <- getFromdemandTrackTable("demandTrack", 1, 20, 2)
demandBeer3 <- getFromdemandTrackTable("demandTrack", 1, 20, 3)


#It's overlapping so dont use 
ggplot(mapping=aes(gameDay, quantity)) +
  geom_segment(data= demandBeer1 , mapping = aes(x=gameDay, xend=gameDay, y=0, yend= quantity), color="red", size= 1) +
  geom_segment(data= demandBeer2 , mapping = aes(x=gameDay, xend=gameDay, y=0, yend= quantity), color="green", size= 1) +
  geom_segment(data= demandBeer3 , mapping = aes(x=gameDay, xend=gameDay, y=0, yend= quantity), color="blue", size= 1) +
  geom_point(data=demand, size=3) +
  labs(title="Beer Demand", 
       subtitle="Demand Qty vs No. of Days", 
       x = "Number of Days",
       y = "Beer Quantity",
       caption="Beer1=Red, Beer2=Green, Beer3= Blue"
  )

#create function to set other days to 0
  
  



