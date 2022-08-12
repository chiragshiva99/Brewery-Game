tankImage <- function(tanks, i) {
  tankCond <- as.list(tanks[i, ])
  state <- "empty"
  if(!is.na(tankCond$DaysInTank)) {
    if (tankCond$DaysInTank >= tankCond$daysToComplete) {
      state <- "ready"
    } else if(tankCond$Beer != "Empty") {
      state <- "full"
    }
  }
  
  return(state)
}

beerTankModuleUI <- function(id) {
  ns <- NS(id)
  div(
    h4("Each Tank produces 100 Beers"),
    uiOutput(ns("autoSwitch")),
    uiOutput(ns("tank"))
  )
}

beerTankModuleServer <- function(id, beer, tanks, AUTO, general) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$beerStoreAuto, {
        if(!is.null(input$beerStoreAuto)) {
          AUTO$beerStore <- input$beerStoreAuto
        }
      })
      
      output$autoSwitch <- renderUI({
        materialSwitch(
          inputId = ns("beerStoreAuto"),
          label = "Auto Storage", 
          value = (AUTO$beerStore | AUTO$all),
          status = "success",
          right=T
        )
      })
      
      output$tank <- renderUI({
        div(
          tags$table(class="table table-striped table-sm",
                     tags$thead(
                       tags$tr(
                         tags$th(style="width: 30%",
                                 strong("Tank")
                         ),
                         tags$th(style="width: 20%",
                                 strong("Beer")
                         ),
                         tags$th(style="width: 30%",
                                 strong("Days In Tank")
                         ),
                         tags$th(style="width: 20%"
                         )
                       )
                     ),
                     tags$tbody(
                       lapply(1:nrow(beer$tanks), function(i) {
                         div(
                           tags$tr(
                             tags$td(style="width: 30%",
                                     tags$img(src = paste0("tanks/",i, tankImage(beer$tanks, i),".png"),height="50px")
                             ),
                             tags$td(style="width: 20%",
                                     beer$tanks[i, 2]
                             ),
                             tags$td(style="width: 20%",
                                     beer$tanks[i, 3]
                             ),
                             tags$td(style="width: 30%",
                                     htmlOutput(ns(paste0("tank", i)))
                             )
                           ),
                           tags$tr(
                             tags$td(colspan="4", style="width: 20%",
                                     htmlOutput(ns(paste0("pgOut", i)))
                             )
                           )
                         )
                       })
                     )
                  )
                )
        
      })
      
      
      lapply(1:nrow(tanks), function(i) {
        output[[paste0("pgOut", i)]] <- renderUI({
          if(!is.na(beer$tanks[i, "DaysInTank"])) {
            return(
              progressBar(id = ns(paste0("pb", beer$tanks[i, 1])),
                          value = 100*(as.integer(beer$tanks[i, "DaysInTank"])/as.integer(beer$tanks[i, "daysToComplete"])),
                          status = "warning", size = "xs")
            )
          } else {
            return()
          }
        })
      })
      
      lapply(1:nrow(tanks), function(i) {
        output[[paste0("tank", i)]] <- renderUI({
          if(is.na(beer$tanks[i, "DaysInTank"])) {
            return()
          }
          
          # if(general$action >= general$maxAction) {
          #   return()
          # }
          
          if(beer$tanks[i, "DaysInTank"] >= beer$tanks[i, "daysToComplete"]) {
            return(
              actionBttn(
                inputId=ns(paste0("tankStore", i)),
                label="Store",
                style="fill",
                color="success",
                size="xs",
                block=T
              )
            )
            
            return()
          }
        })
      })
      
      lapply(1:nrow(tanks), function(i) {
        observeEvent(input[[paste0("tankStore", i)]], {
          if(general$action >= general$maxAction) {
            return(
              sendSweetAlert(
                session=session,
                title="Max Actions Taken Today!",
                text=NULL,
                type="warning"
              )
            )
          }
          
          
          beerIdx <- which(beer$beerInv["name"] == beer$tanks[i, "Beer"])
          beer$beerInv[beerIdx, "qty"] <- beer$beerInv[beerIdx, "qty"] + beer$tanks[i, "tankSize"]
          general$action <- general$action + 1
          beer$tanks[i, "Beer"] <- "Empty"
          beer$tanks[i, "DaysInTank"] <- NA
          beer$tanks[i, "daysToComplete"] <- NA
        }) 
      })

      
      return(AUTO)
    }
  )
}