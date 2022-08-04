automateModuleUI <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("autoSwitch")),
    uiOutput(ns("autoMaterial")),
    uiOutput(ns("materialInput")),
    uiOutput(ns("autoBeer")),
    uiOutput(ns("beerInput"))
    
  )
}

automateModuleServer <- function(id, AUTO) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$allAuto, {
        if(!is.null(input$allAuto)) {
          AUTO$all <- input$allAuto
        }
      })
      
      output$autoSwitch <- renderUI({
        materialSwitch(
          inputId = ns("allAuto"),
          label = "Automate All", 
          value = AUTO$all,
          status = "success",
          right=T
        )
      })
      
      observeEvent(input$materialAuto, {
        if(!is.null(input$materialAuto)) {
          AUTO$material <- input$materialAuto
        }
      })
      
      output$autoMaterial <- renderUI({
        materialSwitch(
          inputId = ns("materialAuto"),
          label = "Automate Material Order", 
          value = AUTO$material,
          status = "success",
          right=T
        )
      })
      
      observeEvent(input$beerAuto, {
        if(!is.null(input$beerAuto)) {
          AUTO$beer <- input$beerAuto
        }
      })
      
      output$autoBeer <- renderUI({
        materialSwitch(
          inputId = ns("beerAuto"),
          label = "Automate Beer Brewing", 
          value = AUTO$beer,
          status = "success",
          right=T
        )
      })
      return(AUTO)
    }
  )
}