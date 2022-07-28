pwd <- rstudioapi::askForPassword("AWS database password")
options(AWSPassword=pwd)
rm(pwd)

getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "project016",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    port = 3306,
    username = "project016",
    password = getOption("AWSPassword"))
  conn
}

##Add a dataframe of values to DB
addToTable <- function(table,dfin){
  conn <- getAWSConnection()
  columns <-  paste0(colnames(dfin), collapse=", ")
  dfin[sapply(dfin, is.character)] <- lapply(dfin[sapply(dfin, is.character)], function(x) paste0("\"",x,"\""))
  print(dfin)
  for (i in 1:nrow(dfin)){
    values <- paste(dfin[i,], collapse=", ")
    query <- paste("INSERT INTO", table, "(", columns, ") VALUES (", values, ")")
    #print(query) #for debug
    success <- FALSE
    tryCatch(
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        print("Value added")
        print(i)
        success <- TRUE
      }, error=function(cond){print("update: ERROR")
        print(cond)
        break
      }, 
      warning=function(cond){print("update: WARNING")
        print(cond)
        break
      },
      finally = {}
    )
  }
  dbDisconnect(conn)
}
