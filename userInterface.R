# Game interface
gameInterface <- div(
          fluidRow(
            column(2, offset=1,
                   actionButton("reset", "Reset Game"),
                   htmlOutput("gameStatus")
            ),
            column(2, offset=6,
                   actionButton("advance", "Advance: Next Day")
                   
            )),
          fluidRow(
            column(2,h3("Money"),
                   htmlOutput("money"), offset=1),
            column(2, h3("Day"),
                   htmlOutput("day")),
            column(2, h3("Lost Sales"),
                   htmlOutput("lostSales"),
                   htmlOutput("lostSalesPerBeer")
            ),
            column(2,h3("Raw Materials"),
                   htmlOutput("rawMatQty")
            ),
            column(2, h3("Beer Inventory"),
                   htmlOutput("beerQty"))),
          fluidRow(
            column(3, h4("Material Orders"),
                   actionButton("purchase", "Purchase"),
                   fluidRow(
                     htmlOutput("currentOrders")
                   ),offset=1
            ),
            column(3, h4("Brewery Tanks"),
                   actionButton("brew", "Brew"),
                   fluidRow(
                     htmlOutput("tankInfo")
                   )
            ),
            column(4, h4("Customer Demand"),
                   htmlOutput("custDemand"))
          )
)

