matInvUI <- function(id) {
  ns <- NS(id)
  tableOutput(ns("matInv"))
}

matInvServer <- function(id, material) {
  moduleServer(
    id,
    function(input, output, session) {
      output$matInv <- renderTable({
        material$rawMatQty %>% rename(Material=name, Quantity=qty)
      })
    }
  )
}