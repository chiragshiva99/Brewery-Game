customerLostUI <- function(id) {
  ns <- NS(id)
  tabBox(
    id="customerLost",
    status="danger",
    solidHeader=TRUE,
    collapsible=F,
    width=NULL,
    tabPanel(
      title="Total Customer Lost",
      uiOutput(ns("lostSales")),
      # valueBoxOutput(ns("lostSales"), width=12),
    ),
    tabPanel(
      title="Lost By Beer",
      uiOutput(ns("lostSalesPerBeer"))
      # infoBoxOutput(ns("lostSalesPerBeer"), width=12),
    )
  )

  
}

customerLostServer <- function(id, demand) {
  moduleServer(
    id,
    function(input, output, session) {
      output$lostSales <- renderUI({
        paste(demand$lostCust, "Customers Lost")
      })
      
      output$lostSalesPerBeer <- renderTable({
        select(demand$lostPerBeer, -stockOut) %>% rename(Beer=name, Quantity=lostQty)
      })
      # output$lostSales <- renderValueBox({
      #   valueBox(
      #     demand$lostCust,
      #     "Customers Lost",
      #     color="danger"
      #   )
      # })
      # 
      # output$lostSalesPerBeer <- renderInfoBox({
      #   infoBox(
      #     title="Lost Sales By Beer",
      #     renderTable({select(demand$lostPerBeer, -stockOut) %>% rename(Beer=name, Quantity=lostQty)})
      #   )
      # })
    }
  )
}