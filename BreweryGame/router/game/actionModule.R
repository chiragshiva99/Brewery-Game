# Tabs for the action box Module in the middle of the page

## Done by Gabriel
actionModuleUI <- function(id) {
  ns <- NS(id)
  div(
    fluidRow(
      column(
        width=4,
        h2("Actions")
      ),
      column(
        width=4,
        dropdownButton(
          label="Supplier List",
          htmlOutput(ns("supplierFilter")),
          htmlOutput(ns("supplierList")),
          circle = F, status = "info",
          icon = icon("shopping-cart"), width = "400px",
          tooltip = tooltipOptions(title = "Click to see Suppliers!")
        )
      ),
      column(
        width=4,
        dropdownButton(
          label="Recipe List",
          htmlOutput(ns("beerRecipes")),
          circle = F, status = "info",
          icon = icon("beer"), width = "300px",
          tooltip = tooltipOptions(title = "Click to see Beer Recipes!")
        )
      )
    ),
    uiOutput(ns("actionTab"))
  )

}

actionModuleServer <- function(id, general, beer, beerInfo, beerReq, material, costInfo, disabled, AUTO, demand, customerInfo, customerDemand, materialInfo) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      selected <- reactiveValues(tab=NULL)
      
      output$actionTab <- renderUI({

        actionTabs <- list()

        actionTabs[[1]] <- tabPanel(
          title="Purchase Material",
          matPurchaseModuleUI(ns("material"), unique(costInfo[,"materialName"]))
        )
        
        actionTabs[[2]] <- tabPanel(
          title="Brew Beer",
          beerBrewModuleUI(ns("beer"), beer$tanks[,"Tank"], beer$beerInv[, "name"])
        )
        
        actionTabs[[3]] <- tabPanel(
          title="Serve Customers",
          customerDemandModuleUI(ns("customer"))
        )
        
        actionTabs[[4]] <- tabPanel(
          title="Automate",
          automateModuleUI(ns("automate"))
        )

        return(tabBox(id=ns("action"), width=NULL, collapsible=F, solidHeader=TRUE,status="maroon", .list=actionTabs, selected=selected$tab))
      })
      
      # Renders beer Recipe dropdown
      output$beerRecipes <- renderTable({
        materials <- unique(beerReq[,"materialName"])
        beers <- unique(beerReq[,"beerName"])
        requirements <- as.data.frame(matrix(nrow=0, ncol=(1+length(materials))))
        colnames(requirements) <- c("Beer", materials)
        
        for(drink in beers) {
          data <- list()
          data[["Beer"]] <- drink
          for(mat in materials) {
            idx <- which((beerReq$beerName==drink)& (beerReq$materialName==mat))
            data[[mat]] <- beerReq[idx, "qty"]
          }
          requirements <- rbind(requirements, data)
        }
        
        requirements
      })
      
      # Renders supplier options dropdown
      output$supplierFilter <- renderUI({
        materialList <- material$rawMatQty[,"name"]
        materialList <- c("All", materialList)
        radioGroupButtons(
          inputId=ns("materialSelect"),
          label="Filter by Material",
          choices = materialList,
          selected = "All",
          justified=T,
          checkIcon = list(
            yes = icon("ok", lib="glyphicon")
          )
        )
      })
      
      # Renders list of suppliers
      output$supplierList <- renderTable({
        materialFilter <- input$materialSelect
        if(is.null(materialFilter)) {
          supplierInfo <- costInfo %>% rename(Material=materialName)
        } else if (materialFilter == "All") {
          supplierInfo <- costInfo %>% rename(Material=materialName)
        } else {
          supplierInfo <- costInfo %>% subset(materialName==materialFilter) %>% select(-materialName)
        }
        
        
        supplierInfo <- supplierInfo %>% rename(Supplier=supplierName, "Order Cost"=fixedCost, "Unit Cost"=variableCost, "Lead Time"=daysToComplete)
        supplierInfo
      })
      
      # Modules within the action Module
      matPurchaseModuleServer("material", general, material, costInfo, disabled, selected)
      beerBrewModuleServer("beer", beer, material, beerInfo, beerReq, disabled, general, selected)
      customerDemandModuleServer("customer", demand, general, beer, beerInfo, customerInfo, customerDemand, AUTO, selected)
      AUTO <- automateModuleServer("automate", AUTO, materialInfo, beerInfo, costInfo, selected)
      
      return(AUTO)
    }
  )
}