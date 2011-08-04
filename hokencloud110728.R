library(RMeCab)
library(ggplot2)
library(arules)
library(hwriter)
library(batade)
library(wordcloud)
source("Q:/Pln/Analysis/999_code/hoken_func.R")
source("Q:/Pln/Analysis/999_code/wordplot.R") #品詞ごとのtreemap

setwd("Q:/Pln/Analysis/05_analysis/006_hokensidou")

data <- read.csv("plan110713.csv")
length(unique(data$参加者名))
length(unique(data$担当サポーター))
data$siensya <- 1#as.numeric(data$担当サポーター)
data$参加者名 <- NULL

tmpdir <- tempdir()
setwd(tmpdir)

for(num in 1:s_num){
  mat <- ftr_mat(data[data$siensya==num, 3])
  item<- c("名詞", "動詞", "形容詞", "副詞")
  res <- NULL
    for(class in item){
      res0 <- output(mat, class)#エラーの場合
      if(is.null(res0)){next}else{
        res <- rbind(res, res0[[1]])
        png(paste(sep="", "支援者", num, "_", class, ".png"))
        print(res0[[2]])
        dev.off()
        }
      }
  write.csv(res, paste("Q:/Pln/Analysis/05_analysis/006_hokensidou/110728/支援者", num, "_result.csv", sep=""), row.names=FALSE)
  }

data[,3] <- as.character(data[,3])

txconv <- function(data, coln=3, siensya=4){
    df <- NULL
    for(s in 1:max(data[, siensya])){
      for(i in 1:nrow(data)){
        tx <- data[i, coln]
        val <- unlist(RMeCabC(tx, mypref=1))#原形で表示
        atr <- attr(val, "names")
        df0 <- data.frame(val, atr, s)
        df <- rbind(df, df0)
      }
    }
    return(df)
  }
res <- txconv(data, 3)
res.d <- ddply(res, .(atr, val, siensya), summarise, count=sum(!is.na(val))
res.d[-(res.d$atr %in% c("接頭詞", "フィラー", "助詞", "助動詞", "連体詞")),]
res.ds$atr <- as.character(res.ds$atr)
res.ds <- subset(res.d, atr!="接頭詞" & atr!="助詞" &atr!="助動詞" &atr!="連体詞" & atr!="フィラー" & atr!="記号" & atr!="動詞")
library(RColorBrewer)
pal2 <- brewer.pal(8,"Dark2")
wc <- function(hinsi){
res.ds <- subset(res.d, atr==hinsi)
wordcloud(res.ds$val, res.ds$count, scale=c(4,.5),min.freq=1,
max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2, family="JP5")
}
res2 <- txconv(data)
res.d2 <- ddply(res, .(atr, val, siensya), summarise, count=sum(!is.na(val))

