library(ggplot2)
library(ggalt)
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

tank <- getFromTrackTable("tankTrack", 1, 20)
tank

ggplot(tank, aes(gameDay, tankID)) + 
  geom_point(aes(color=beerID), shape=15, size=13) + 
  scale_y_discrete(limits=c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))


#gg_vistime(tank)

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
visTank <- createVisTank(tank)
vistime(visTank)



tank
subset(tank, tankID == 1)
newTank <- tank[-c(4,5,7),]
newTank[c(9,11,14,17), "beerID"] <- 4 

newVis <- createVisTank(newTank)
vistime(newVis)

#dumbbell plot
ggplot(data = newVis, mapping = aes(x= start, xend= end, y= group, color= event)) +
  geom_dumbbell(size=5,
                colour_x = "red", colour_xend = "black",
                dot_guide=TRUE, dot_guide_size=0.5, alpha= 0.7) +
  labs(title="Usage of Beer Tank", 
       subtitle="Time taken to brew each Beer", 
       x = "Date",
       y = "Tank No."
  )


#data from demandTrack

demand <- getFromTrackTable("demandTrack", 1, 20)
demand$Beer <- ifelse(demand$beerID==1, "Beer 1", ifelse(demand$beerID==2, "Beer 2", "Beer 3"))

ggplot(data=demand, mapping=aes(gameDay, quantity, color=Beer)) +
  geom_segment(mapping = aes(x=gameDay, xend=gameDay, y=0, yend= quantity), size= 1) +
  geom_point(data=demand, size=3) +
  #geom_line() +
  labs(title="Beer Demand", 
       subtitle="Demand Qty vs No. of Days", 
       x = "Number of Days",
       y = "Beer Quantity"
  )


demandBeer1 <- subset(demand, demand$beerID==1)
demandBeer2 <- subset(demand, demand$beerID==2)
demandBeer3 <- subset(demand, demand$beerID==3)

ggplot(data=demandBeer1, mapping=aes(gameDay, quantity, fill= Beer)) +
  geom_bar(mapping = aes(x=gameDay, y=quantity), stat = "identity") +
  labs(title="Demand for Beer 1", 
       subtitle="Demand Qty vs No. of Days", 
       x = "Number of Days",
       y = "Beer Quantity"
  )

ggplot(data=demandBeer2, mapping=aes(gameDay, quantity, fill= Beer)) +
  geom_bar(mapping = aes(x=gameDay, y=quantity), stat = "identity") +
  labs(title="Demand for Beer 2", 
       subtitle="Demand Qty vs No. of Days", 
       x = "Number of Days",
       y = "Beer Quantity"
  )

ggplot(data=demandBeer3, mapping=aes(gameDay, quantity, fill= Beer)) +
  geom_bar(mapping = aes(x=gameDay, y=quantity), stat = "identity") +
  labs(title="Demand for Beer 3", 
       subtitle="Demand Qty vs No. of Days", 
       x = "Number of Days",
       y = "Beer Quantity"
  )








