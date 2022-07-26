## Both
addNewEntry <- function(infoDF, row, value) {
  # Check if na
  added <- F
  if (is.na(infoDF[row, 1])){
    infoDF[row, 1] <- value
    infoDF[row, 2] <- 0
    added <- T
  }

  list(infoDF, added)
}

vector.is.empty <- function(x) {
  return(length(x) ==0 )
}

incrementDays <- function(frameInfo, col) {
  frameInfo[col] <- frameInfo[col] + 1
  frameInfo
}


