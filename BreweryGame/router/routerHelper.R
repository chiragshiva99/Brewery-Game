## Incomplete function that was intended to allow for continuing of game
## Done by Gabriel

loadPrevState <- function(id, gameID) {
  prevGameState <- reactiveVals()
  dataTables <- list(beer="beerTrack", mat="materialTrack", cash="cashTrack", demand="demandTrack", tank="tankTrack")
  
  for (table in names(dataTables)) {
    print(table)
    
    ### Request and add it to the database
  }
}