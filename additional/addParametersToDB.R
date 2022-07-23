pwd <- rstudioapi::askForPassword("AWS database password")
options(AWSPassword=pwd)

library("data.table")

source("usePackages.R")
pkgnames <- c("DBI", "tidyverse", "RMySQL")
loadPkgs(pkgnames)
source("dbHelper.R")

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


demandParam <- read.csv('parameters/demandParams.csv', encoding="UTF-8")
beerParam <- read.csv('parameters/beerParams.csv', encoding="UTF-8")
supplierNames <- read.csv('parameters/supplierNames.csv')
processNames <- read.csv('parameters/processNames.csv')
materialNames <- read.csv('parameters/materialNames.csv')
tableNames <- read.csv('parameters/tableNames.csv')
supplierParam <- read.csv('parameters/supplierParams.csv')
conditionParam <- read.csv('parameters/conditionParams.csv')
beerReqParam <- read.csv('parameters/beerReqParams.csv')
conditionExtra <- read.csv('parameters/conditionExtra.csv')

customerNames$name <- paste0("\"",customerNames$name, "\"")

customerNames <- read.csv('parameters/customerNames.csv')
customerNames[sapply(customerNames, is.character)] <- lapply(customerNames[sapply(customerNames, is.character)], #lapply over all character column names
   function(x) paste0("\"",x,"\""))
addBackslash <- function(x){paste0("\"",x,"\"")}
customerNames %>% mutate_if(is.character(col), function(col) paste0("\"",col, "\""))
addToTable("customerNames", customerNames)
addToTable("beerParameters", beerParam)
addToTable("supplierNames", supplierNames)
addToTable("processNames", processNames)
addToTable("materialNames", materialNames)

conditionParam$tankSize <- ""
conditionParam$costs <- ""
addToTable("conditionParameters", conditionParam)
addToTable("supplierParameters", supplierParam)
addToTable("beerReqParameters", beerReqParam)
addToTable("tableNames", tableNames)
addToTable("conditionExtra", conditionExtra)
addToTable("demandParameters", demandParam)

result <- getStartQty(1, "beerParameters")
result <- getStartQty(1, "materialNames")
result <- getBeerReq()

killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}


credentials <- data.frame(
  userID=c(1),
  username = c("G"),
  password   = sapply(c("mypass"),sodium::password_store),
  prevGameID = c(-1),
  curGameID = c(-1),
  stringsAsFactors = F
)

addToTable("userInfo", credentials)
