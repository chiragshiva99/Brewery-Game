calculateCost <- function(costInfo, mat, supplier, qty){
  costStuff <- costInfo %>% subset(materialName==mat) %>% subset(supplierName==supplier)
  cost <- qty*costStuff[1, "variableCost"] + costStuff[1, "fixedCost"]
  
  as.double(cost)
}

orderMaterial <- function(general, material, costInfo, matChosen, quantity, supplier) {
  newEntry <- data.frame(Material=matChosen, Quantity=quantity, Days=0, Supplier=supplier, daysToComplete=costInfo[which(costInfo$materialName == matChosen), "daysToComplete"])
  
  cost <- calculateCost(costInfo, matChosen, supplier, quantity)
  
  if (cost <= general$money) {
    material$rawMatOrder <- rbind(material$rawMatOrder, newEntry)
    general$money <- general$money - calculateCost(costInfo, matChosen, supplier, quantity)
  }
  print("GETS HERE")
  return(list(general, material))
}
