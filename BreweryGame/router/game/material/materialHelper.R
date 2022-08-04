
calculateCost <- function(costInfo, mat, supplier, qty){
  print(costInfo)
  print(mat)
  print(supplier)
  print(qty)
  costStuff <- costInfo %>% subset(materialName==mat) %>% subset(supplierName==supplier)
  cost <- qty*costStuff[1, "variableCost"] + costStuff[1, "fixedCost"]
  as.double(cost)
}
