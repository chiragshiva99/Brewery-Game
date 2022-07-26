# Analysis Page
analysisInterface <- tabItem(
  tabName = "analysisTab",
  fluidRow(
    h1("Analysis!!"),
    box(
      title="Money",
      "Includes things like, cash balance"
    ),
    box(
      title="Lost Customers",
      "Includes all lost customers and by beer also"
    ),
    box(
      title="Beer inventory levels",
      "Tracks all beer inventory, in tank and outside"
    ),
    box(
      title="Beer Demand",
      "Shows beer Demand over entire game"
    ),
    box(
      title="Material inventory Levels",
      "All material inventory, in delivery and on hand"
    )
    
  )
  
)