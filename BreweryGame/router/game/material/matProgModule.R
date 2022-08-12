
# Module to track the progress of material Orders
matProgModuleUI <- function(id) {
  ns <- NS(id)
  div(
    htmlOutput(ns("currentOrders"))
  )
}

matProgModuleServer <- function(id, material) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Generates current orders depending on the data
      output$currentOrders <- renderUI({
        if(nrow(material$rawMatOrder) == 0) {
          matStuff <- h3("No Material Orders Currently")
        } else {
          matStuff <- div(
            tags$table(class="table table-striped table-sm",
                       tags$thead(
                         tags$tr(
                           tags$th(style="width: 25%",
                                   strong("Material")
                           ),
                           tags$th(style="width: 20%",
                                   strong("Quantity")
                           ),
                           tags$th(style="width: 25%",
                                   strong("Days Left")
                           ),
                           tags$th(style="width: 30%",
                                   strong("Supplier")
                           )
                         )
                       ),
                       tags$tbody(
                         lapply(1:nrow(material$rawMatOrder), function(i) {
                           div(
                             tags$tr(
                               tags$td(style="width: 25%",
                                       material$rawMatOrder[i, "Material"]
                               ),
                               tags$td(style="width: 20%",
                                       material$rawMatOrder[i, "Quantity"]
                               ),
                               tags$td(style="width: 25%",
                                       (material$rawMatOrder[i, "daysToComplete"] - material$rawMatOrder[i, "Days"])
                               ),
                               tags$td(style="width: 30%",
                                       material$rawMatOrder[i, "Supplier"]
                               )
                             ),
                             tags$tr(
                               tags$td(colspan="4", style="width: 20%",
                                       progressBar(id = ns(paste0("pb", rownames(material$rawMatOrder)[i])), value = 100*(as.integer(material$rawMatOrder[i, "Days"])/as.integer(material$rawMatOrder[i, "daysToComplete"])), status = "success", size = "xs")
                               )
                             )
                           )
                         })
                       )
            )
          )
        }
        return(matStuff)
      })
    }
  )
}
  
  
  
