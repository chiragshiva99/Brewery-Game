loadPrevState <- function(id, gameID) {
  prevGameState <- reactiveVals()
  dataTables <- list(beer="beerTrack", mat="materialTrack", cash="cashTrack", demand="demandTrack", tank="tankTrack")
  
  for (table in names(dataTables)) {
    print(table)
    
    ### Request and add it to the database
  }
}