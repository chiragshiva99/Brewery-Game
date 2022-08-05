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
      
      output$lostSalesPerBeer <- renderUI({
        beerCount <- nrow(demand$lostPerBeer)
        
        div(
          tags$table(class="table table-sm",
                     # style=""
                     tags$thead(
                       tags$tr(
                         lapply(1:beerCount, function(i) {
                           tags$th(
                             style=paste0("width: ", round(100/beerCount, 2),"%"),
                             demand$lostPerBeer[i, "name"]
                           )
                         })
                       )
                     ),
                     tags$tbody(
                       tags$tr(
                         lapply(1:beerCount, function(i) {
                           tags$td(
                             style=paste0("width: ", round(100/beerCount, 2),"%"),
                             demand$lostPerBeer[i, "lostQty"]
                           )
                         })
                       )
                     )
           )
        )
      })
      
    }
  )
}