library(batade)
library(ggplot2)
library(stringr)
library(googleVis)
library(digest)
setwd("Q:/Pln/Analysis/01_hokensya/01_kenpo/10_ŽO•H“d‹@/01_data")

#monshin
m2009 <- read.csv("m2009.csv", as.is=TRUE, header=FALSE)
m2010 <- read.csv("m2010.csv", as.is=TRUE, header=FALSE)



#treemap‚ÅŠTŠÏ”cˆ¬

#1‘w
major <- ddply(m2009, .(kigou), summarise, count=sum(!is.na(mannum)))
major$Child <- str_c("c", major$kigou)
major$Parent <- "mitsubishi"
major$color <- 1:nrow(major)
major <- rbind(major, c(NA, sum(major$count), "mitsubishi", NA, 100))

#2‘wŽO•H“d‹@i’n•û‹æ•ªj
minor <- ddply(m2009[m2009$kigou==110, ], .(kinmuchikubun), summarise, count=sum(!is.na(mannum)))
minor$Child <- str_c("m", minor$kinmuchikubun)
minor$Parent <- "c110"
minor$color <- 1:nrow(minor)
colnames(minor) <- colnames(major)
pdata <- rbind(major, minor)


#3‘wŽO•H“d‹@
minor2 <- ddply(m2009[m2009$kigou==110, ], .(kinmuchikubun, yakushoku), summarise, count=sum(!is.na(mannum)))
minor2$Child <- minor2$yakushoku
minor2$Parent <- str_c("m", minor2$kinmuchikubun)
minor2$color <- 1:nrow(minor2)
minor2$yakushoku <- NULL
minor2$Child <- str_c(minor2$Child, minor2$Parent)
colnames(minor2) <- colnames(major)
pdata <- rbind(pdata, minor2)

#2‘wŽO•H“d‹@ˆÈŠO
minor3 <- ddply(m2009[m2009$kigou!=110, ], .(kigou, yakushoku), summarise, count=sum(!is.na(mannum)))
minor3$Child <- minor3$yakushoku
minor3$Parent <- str_c("c", minor3$kigou)
minor3$color <- 1:nrow(minor3)
minor3$yakushoku <- NULL
minor3$Child <- str_c(minor3$Child, minor3$Parent)
colnames(minor3) <- colnames(major)
pdata <- rbind(pdata, minor3)


T1 <- gvisTreeMap(data=pdata, idvar="Child", parentvar="Parent", sizevar="count", colorvar="color")
T1$html$header <- gsub("utf-8", "CP932", T1$html$header)
plot(T1)


mktrmd <- function(data, f, s, t, countid){
res <- data.frame(table(data$f, data$s, data$t))
}

f <<- diamonds$cut
s <<- diamonds$color 
countid <<- diamonds$x
mktrmd <- function(data){
res <- ddply(data, .(f,s), summarise, sum(!is.na(countid)))
}

