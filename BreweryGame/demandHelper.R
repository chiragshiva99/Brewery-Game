
## Demand
generateDemand <- function(customer, iia, normParam, days, beer, maxWait) {
  demand <- data.frame(matrix(nrow=0, ncol=6))
  colnames(demand) <- c("Customer", "Beer", "Quantity", "Day", "actualDay", "maxWait")
  cumDay <- 0
  while(cumDay < days) {
    qty <- ceiling(rnorm(1, mean=normParam[1], sd=normParam[2]))
    if (qty <= 0) {
      next
    }
    dayToNextOrder <- rexp(1, rate=(1/iia))
    cumDay <- cumDay + dayToNextOrder
    order <- data.frame(Customer=customer, Beer=beer, Quantity=qty, Day=ceiling(cumDay), actualDay=cumDay, maxWait=maxWait)
    demand <- rbind(demand, order)
  }
  demand <- demand[-c(nrow(demand)),]
  demand
}

generateTotalDemand <- function(table,days=100) {
  totalDemand <-   demand <- data.frame(matrix(nrow=0, ncol=6))
  colnames(demand) <- c("Customer","Beer", "Quantity", "Day", "actualDay", "maxWait")
  for (i in 1:nrow(table)){
    indivDemand <- generateDemand(table[i, "customerName"], table[i,"meanArrivalTime"], c(table[i,"mean"], table[i, "sd"]),days, table[i, "beerName"], table[i, "waitTime"])
    print(indivDemand)
    totalDemand <- rbind(totalDemand, indivDemand)
  }
  print(totalDemand)
  totalDemand
}