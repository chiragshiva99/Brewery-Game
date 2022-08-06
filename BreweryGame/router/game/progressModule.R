progressModuleUI <- function(id) {
  ns <- NS(id)
  div(
    h2("Progress Tracker"),
    tabBox(
      id=ns("progTab"),
      width=NULL,
      collapsible=F,
      tabPanel(
        title="Material Orders",
        matProgModuleUI(ns("material"))
      ),
      tabPanel(
        title="Tanks",
        beerTankModuleUI(ns("beer"))
      ),
      tabPanel(
        title="Total Demand",
        totalDemandModuleUI(ns("totalDemand"))
      )
      
    )
  )
}

progressModuleServer <- function(id, material, beer, demand, general, beerInfo, customerInfo, customerDemand, tanks, AUTO) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      matProgModuleServer("material", material)
      AUTO <- beerTankModuleServer("beer", beer, tanks, AUTO, general)
      totalDemandModuleServer("totalDemand", demand, beerInfo, customerInfo)
      
      return(AUTO)
    }
  )
}