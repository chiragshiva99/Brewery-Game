## Module to display beer Inventory
beerInvUI <- function(id) {
  ns <- NS(id)
  tableOutput(ns("beerInv"))
}

beerInvServer <- function(id, beer) {
  moduleServer(
    id,
    function(input, output, session) {
      output$beerInv <- renderTable({beer$beerInv %>% rename(Beer=name, Quantity=qty)})
    }
  )
}