progressModuleUI <- function(id) {
  ns <- NS(id)
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
      title="Demand",
      customerDemandModuleUI(ns("demand"))
    )
  )
}

progressModuleServer <- function(id, material, beer, demand) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      matProgModuleServer("material", material)
      beerTankModuleServer("beer", beer)
      customerDemandModuleServer("demand", demand)
      
    }
  )
}