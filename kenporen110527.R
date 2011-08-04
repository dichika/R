setwd("Q:/Pln/Analysis")

library(RSQLite)
dbname <- "KenpoDBNo6_2010.db"

driver <- dbDriver("SQLite")
con <- dbConnect(driver,dbname)

list1 <- dbListTables(con)

tbl <- dbGetQuery(con, paste("SELECT NO from ", list1[i], " limit 5;", sep=""))
print(tbl)

?dbGetQuery