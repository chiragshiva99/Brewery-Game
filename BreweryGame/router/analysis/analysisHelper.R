darkTheme <- dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        plot.background = element_rect(fill = "grey25"),
        panel.background = element_rect(fill = "grey15"),
        panel.grid.major = element_line(color = "grey80", size = 0.5),
        panel.grid.minor = element_line(color = "grey80", size = 0.5),
        legend.background = element_rect(fill = "grey10"),
        axis.ticks = element_blank(),
        legend.key = element_blank())

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


