invModuleUI <- function(id) {
  ns <- NS(id)
  tabBox(
    id=ns("inv"),
    title="Inventory",
    width=NULL,
    collapsible=F,
    tabPanel(
      title="All",
      div(
        h4("Raw Materials"),
        matInvUI(ns("material")),
        br(),
        h4("Beers"),
        beerInvUI(ns("beer"))
      )
    ),
    tabPanel(
      title="Material",
      matInvUI(ns("materialSub"))
    ),
    tabPanel(
      title="Beer",
      beerInvUI(ns("beerSub"))
    )
  )
}

invModuleServer <- function(id, beer, material) {
  moduleServer(
    id,
    function(input, output, session) {
      matInvServer("material", material)
      beerInvServer("beer", beer)
      matInvServer("materialSub", material)
      beerInvServer("beerSub", beer)
    }
  )
}