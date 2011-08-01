library(ggplot2)
library(stringr)
library(googleVis)
library(RColorBrewer)
source("http://aoki2.si.gunma-u.ac.jp/R/src/map.R", encoding="euc-jp") #青木先生の地図関数（同一ディレクトリに白地図データ必要）
#source("Q:/Pln/Analysis/999_code/getInfection110727.R") DB取得に必要

###

setwd("D:/downloads")
res <- read.csv("infection110727.csv", as.is=TRUE)
pref <- read.csv("pref.csv", as.is=TRUE)

resSmp <- merge(res, pref)

ress <- subset(resSmp, variable=="結核" & pref!="総数")
qu <- quantile(ress$value)[2:4]
ress$st <- cut(ress$value, breaks=c(-Inf, qu, Inf), labels=1:4, right=FALSE)
mapcolor <- brewer.pal(9,"BuGn")[c(1,3,5,9)]
pal <- data.frame(st=1:4, col=mapcolor, stringsAsFactors=FALSE)
ress1 <- merge(ress, pal)


#動きを把握するには塗り分けは使えない→色は1つにして円の大きさで表現する→Rgooglemapsでやるか？
for(w in 1:52){
  w <- ifelse(w<10, formatC(digits=1, w, flag="0"), w)
  ress1s <- subset(ress1, week==w & year=="2010")
  ltxt <- c(str_c("-", qu[1]), str_c(qu[1], "-", qu[2]), str_c(qu[2], "-", qu[3]), str_c(qu[3], "-"))
  setwd("D:/downloads/output")
  png(str_c(w, ".png"))
  setwd("D:/downloads")
  map(code=ress1s$num, col=ress1s$col)
  legend("topleft", ltxt, fill=mapcolor)
  legend("top", sprintf("20%s年 %s週", y, w), bty="n")
  dev.off()
  }

#googleVisで表現
#gsmp <- gvisGeoMap(data=ress, "prefen", "value", "pref", option=list(region="JP", width=1200, height=600))
#gsmp$html$header <- gsub("utf-8", "CP932", gsmp$html$header)
#plot(gsmp)

