library(ggplot2)
library(ggdark)
library(plotly)
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

# --------------------------Money------------------------------
money <- getFromTrackTable("cashTrack", 1, 20)

ggplot(data=money, mapping=aes(gameDay, cashBalance)) +
  geom_step(size = 1, color = ifelse(money$cashBalance>=100000, "green", "red")) +
  geom_hline(mapping=aes(yintercept = 100000), color="grey", size= 2, alpha = 0.8) +
  geom_text(mapping=aes(0, y = 100000,label = "Initial Revenue", vjust = -1, hjust = 0), color = 'white') +
  labs(title="Cash Balance generated everyday", 
       x = "Game Day",
       y = "Cash Balance ($)"
  )+
  dark_theme_gray()
# ggplotly(a)

# --------------------------Tank Status------------------------------
tank <- getFromTrackTable("tankTrack", 1, 20)

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
  visTank$Beer <- ifelse(visTank$event==1, "Beer 1- IPA", ifelse(visTank$event==2, "Beer 2- Larger", "Beer 3- Stout"))
  visTank <- visTank[order(visTank$group),]
  
  return(visTank)
}

?as.Date
origin <- "2022-07-22"
visTank <- createVisTank(tank)
vistime(visTank)

#dumbbell plot
ggplot(data = visTank, mapping = aes(x= start, xend= end, y= group, color= Beer)) +
  geom_dumbbell(size=5,
                colour_x = "red", colour_xend = "grey",
                dot_guide=TRUE, dot_guide_size=0.5, alpha= 0.7) +
  labs(title="Usage of Beer Tank", 
       x = "Game Day",
       y = "Tank No."
  ) +
  dark_theme_gray()

# --------------------------Beer Inventory------------------------------
inventory <- getFromTrackTable("beerTrack", 1, 12)
inventory$Beer <- ifelse(inventory$beerID==1, "Beer 1- IPA", ifelse(inventory$beerID==2, "Beer 2- Larger", "Beer 3- Stout"))

ggplot(data=inventory, mapping=aes(gameDay, inventory, color=Beer)) +
  geom_step(size = 1) +geom_hline(mapping=aes(yintercept = 50), color="grey", size= 2, alpha = 0.8) +
  #coord_cartesian(xlim = c(0, 150), ylim = c(0, 250))+
  geom_text(mapping=aes(0, y = 50,label = "Recommended Brewing Point", vjust = -1, hjust = 0), color = 'white') +
  labs(title="Beer Inventory Level", 
       x = "Game Day",
       y = "Inventory"
  )+
  dark_theme_gray()

# --------------------------Beer Demand------------------------------
demand <- getFromTrackTable("demandTrack", 1, 20)
demand$Beer <- ifelse(demand$beerID==1, "Beer 1- IPA", ifelse(demand$beerID==2, "Beer 2- Larger", "Beer 3- Stout"))

ggplot(data=demand, mapping=aes(gameDay, quantity, color=Beer)) +
  geom_segment(mapping = aes(x=gameDay, xend=gameDay, y=0, yend= quantity), size= 1) +
  geom_point(data=demand, size=3) +
  labs(title="Beer Demand", 
       subtitle="Demand Qty vs No. of Days", 
       x = "Number of Days",
       y = "Beer Quantity"
  )+
  dark_theme_gray()

# BarGraphs for each Respective Beer
demandBeer1 <- subset(demand, demand$beerID==1)
demandBeer2 <- subset(demand, demand$beerID==2)
demandBeer3 <- subset(demand, demand$beerID==3)

ggplot(data=demandBeer1, mapping=aes(gameDay, quantity)) +
  geom_bar(mapping = aes(x=gameDay, y=quantity), stat = "identity", fill = "#EC9D00"#,color = "black"
           ) +
  labs(title="Demand for Beer 1", 
       x = "Game Day",
       y = "Beer Quantity"
  )+dark_theme_gray()

ggplot(data=demandBeer2, mapping=aes(gameDay, quantity, fill= Beer)) +
  geom_bar(mapping = aes(x=gameDay, y=quantity), stat = "identity", fill = "#FAE96F") +
  labs(title="Demand for Beer 2", 
       x = "Game Day",
       y = "Beer Quantity"
  )+dark_theme_gray()

ggplot(data=demandBeer3, mapping=aes(gameDay, quantity, fill= Beer)) +
  geom_bar(mapping = aes(x=gameDay, y=quantity), stat = "identity", fill = "#F6C101") +
  labs(title="Demand for Beer 3", 
       x = "Game Day",
       y = "Beer Quantity"
  )+dark_theme_gray()

# --------------------------Material Inventory Level------------------------------
material <- getFromTrackTable("materialTrack", 1, 20)
material$Material <- ifelse(material$materialID==1, "Malt", ifelse(material$materialID==2, "Hops", "Yeast"))

a <- dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
    theme(plot.title = element_text(family = "Fira Sans Condensed"),
          plot.background = element_rect(fill = "grey15"),
          panel.background = element_rect(fill = "grey10"),
          panel.grid.major = element_line(color = "grey50", size = 0.2),
          panel.grid.minor = element_line(color = "grey50", size = 0.2),
          legend.background = element_rect(fill = "grey10"),
          axis.ticks = element_blank(),
          legend.key = element_blank())


ggplot(data=material, mapping=aes(gameDay, inventory, color=Material)) +
  geom_step(size = 1) +geom_hline(mapping=aes(yintercept = 50), color="grey", size= 2, alpha = 0.8) +
  geom_text(mapping=aes(0, y = 100,label = "Recommended Raw-Material Reorder Point", vjust = 0, hjust = 0), color = 'white') +
  labs(title="Raw Material Inventory Level", 
       x = "Game Day",
       y = "Inventory"
  ) + a







