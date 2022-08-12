# Creates the dark theme for the plots 
# Done by Chirag
darkTheme <- dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        plot.background = element_rect(fill = "grey25"),
        panel.background = element_rect(fill = "grey15"),
        panel.grid.major = element_line(color = "grey80", size = 0.5),
        panel.grid.minor = element_line(color = "grey80", size = 0.5),
        legend.background = element_rect(fill = "grey10"),
        axis.ticks = element_blank(),
        legend.key = element_blank())

# Algorithm to convert data for plotting ## Gabriel
createVisTank <- function(tank) {
  visTank <- data.frame(matrix(nrow=0, ncol=4))
  colnames(visTank) <- c("event", "group", "start", "end")

  for (entry in 1:nrow(tank)) {
    data <- list()
    data$group <- tank[entry, "tankID"]
    data$event <- tank[entry, "beerID"]
    start <- tank[entry, "gameDay"]
    end <- tank[entry, "gameDay"]
    indexes <- which((visTank$group == data$group) & (visTank$event == data$event))
    # print("DEBUGGING")
    # print(indexes)
    if(identical(indexes, integer(0))) {
      data$start <- start
      data$end <- end
      visTank <- rbind(visTank, data)
      next
    }
    index <- max(indexes)
    # print(index)
    # print(visTank)
    # print(data)
    # print(paste0("Index ", entry))
    
    if(visTank[index, "end"] + 1 == start) {
      visTank[index, "end"] <- start
      next
    } else {
      data$start <- start
      data$end <- end
      visTank <- rbind(visTank, data)
    }
  }
  
  visTank <- visTank[order(visTank$group),]
  
  return(visTank)
}


