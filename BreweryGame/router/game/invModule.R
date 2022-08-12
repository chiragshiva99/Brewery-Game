## Module for the rendering of the inventory box
invModuleUI <- function(id) {
  ns <- NS(id)
  div(
    h2("Inventory"),
    tabBox(
      id=ns("inv"),
      width=NULL,
      collapsible=F,
      solidHeader=TRUE,
      status="teal",
      tabPanel(
        title="All",
        div(
          h4("Raw Materials"),
          dropdownButton(
            label="More Info",
            tableOutput(ns("materialDropdown")),
            circle = F, status = "info",
            icon = icon("circle-info"), width = "300px",
            tooltip = tooltipOptions(title = "Click to see More Info!")
          ),
          
          matInvUI(ns("material")),
          br(),
          h4("Beers"),
          dropdownButton(
            label="More Info",
            htmlOutput(ns("beerDropdown")),
            circle = F, status = "info",
            icon = icon("circle-info"), width = "400px",
            tooltip = tooltipOptions(title = "Click to see More Info!")
          ),
          beerInvUI(ns("beer"))
        )
      ),
      tabPanel(
        title="Material",
        dropdownButton(
          label="More Info",
          tableOutput(ns("materialDropdown2")),
          circle = F, status = "info",
          icon = icon("circle-info"), width = "300px",
          tooltip = tooltipOptions(title = "Click to see More Info!")
        ),
        matInvUI(ns("materialSub"))
      ),
      tabPanel(
        title="Beer",
        dropdownButton(
          label="More Info",
          htmlOutput(ns("beerDropdown2")),
          circle = F, status = "info",
          icon = icon("circle-info"), width = "400px",
          tooltip = tooltipOptions(title = "Click to see More Info!")
        ),
        beerInvUI(ns("beerSub"))
      )
    )
  )
}

invModuleServer <- function(id, beer, material, beerInfo, materialInfo) {
  moduleServer(
    id,
    function(input, output, session) {
      output$beerDropdown <- renderTable({
        
        beerInfo %>% select(-beerID) %>% rename(Beer=name, "Revenue/unit"=revenue, "Lead Time"=daysToComplete, "Stock-Out Cost/unit"=stockOut, "Holding Cost/unit"=holdingCost) 
      })
      
      output$materialDropdown <- renderTable(
        expr=materialInfo %>% select(-materialID) %>% rename(Material=name, "Holding Cost/unit"=holdingCost),
        digits=3
      )
      
      output$beerDropdown2 <- renderTable({
        
        beerInfo %>% select(-beerID) %>% rename(Beer=name, "Revenue/unit"=revenue, "Lead Time"=daysToComplete, "Stock-Out Cost/unit"=stockOut, "Holding Cost/unit"=holdingCost) 
      })
      
      output$materialDropdown2 <- renderTable(
        expr=materialInfo %>% select(-materialID) %>% rename(Material=name, "Holding Cost/unit"=holdingCost),
        digits=3
      )
      
      matInvServer("material", material)
      beerInvServer("beer", beer)
      matInvServer("materialSub", material)
      beerInvServer("beerSub", beer)
    }
  )
}