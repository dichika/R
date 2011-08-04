library(batade)
library(ggplot2)
library(stringr)
library(googleVis)


#関数

#最新年度(2010年度)の健康分布を事業所単位でカウントする

mkkbunpu <- function(data){
  kb <- ddply(data, .(kigou, haihu, kenkoubunpuSEG), summarise, count=sum(!is.na(bangou)))
  res1 <- reshape(kb, idvar=c("kigou", "haihu"), timevar="kenkoubunpuSEG", direction="wide")
  res1[,-c(1,2)] <- apply(res1[,-c(1,2)], 2, function(x)ifelse(is.na(x), 0, x))

  grp <- ddply(data, .(group, kenkoubunpuSEG), summarise, count=sum(!is.na(bangou)))
  res2 <- reshape(grp, idvar="group", timevar="kenkoubunpuSEG", direction="wide")
  res2[,-1] <- apply(res2[,-1], 2, function(x)ifelse(is.na(x), 0, x))
  res2 <- data.frame(kigou=res2[,1], haihu=999, res2[,-1])

  zentai <- data.frame(kigou="全体", haihu=999, t(colSums(res1[,-c(1,2)])))
  res1 <- rbind(res1, res2, zentai)

  sums <- rowSums(res1[,-c(1,2)])
  per <- round(res1[,-c(1,2)]*100 / sums, 1)
  colnames(per) <- gsub("count", "per", colnames(per))

  res1 <- cbind(res1, per)
  res1$all <- sums
  invisible(res1)
  }



#健康分布の移動状況(2010年度の結果から2009年度を遡ることに注意)

mkido <- function(data1, data2){
  kenshin <- merge(data1, data2, by=c("kigou", "bangou", "haihu"))
  kbido <- ddply(kenshin, .(kigou, haihu, kenkoubunpuSEG.y, kenkoubunpuSEG.x), summarise, count=sum(!is.na(bangou)))
  colnames(kbido) <- c("kigou", "haihu", "s10", "s09", "count")
  kbido <- kbido[order(kbido$s10, kbido$s09),]
  kbido$seg <- str_c(kbido$s10, kbido$s09, sep=".")
  kbido$s09 <- NULL
  kbido$s10 <- NULL
  res <- reshape(kbido, idvar=c("kigou", "haihu"), timevar=c("seg"), direction="wide")
  res[,-c(1,2)] <- apply(res[,-c(1,2)], 2, function(x)ifelse(is.na(x), 0, x))

  kbido2 <- ddply(kenshin, .(group.x, kenkoubunpuSEG.y, kenkoubunpuSEG.x), summarise, count=sum(!is.na(bangou)))
  colnames(kbido2) <- c("group", "s10", "s09", "count")
  kbido2 <- kbido2[order(kbido2$s10, kbido2$s09),]
  kbido2$seg <- str_c(kbido2$s10, kbido2$s09, sep=".")
  kbido2$s09 <- NULL
  kbido2$s10 <- NULL
  res2 <- reshape(kbido2, idvar="group", timevar=c("seg"), direction="wide")
  res2[,-1] <- apply(res2[,-1], 2, function(x)ifelse(is.na(x), 0, x))
  res2 <- data.frame(kigou=res2[,1], haihu=999, res2[,-1])

  zentai <- data.frame(kigou="全体", haihu=999, t(colSums(res[,-c(1,2)])))
  res3 <- rbind(res, res2, zentai)

  for(c in seq(3,59,by=8)){
    per <- round(apply(res3[, c:(c+7)], 2, function(x)ifelse(x>0, x * 100 / rowSums(res3[, c:(c+7)]), 0)), 1)
    colnames(per) <- gsub("count", "per", colnames(per))
    res3 <- cbind(res3, per)
    }
  invisible(res3)
  }


#問診→悪い生活習慣の割合を算出し非リスク・リスクで分ける
#心配なのでテスト必要

shuukeiSmp <- function(data){
  res <- ddply(data, .(kigou, haihu, risk), summarise, 
                Q1=mean(Q1, na.rm=TRUE), 
                Q4=mean(Q4, na.rm=TRUE), 
                Q8=mean(Q8, na.rm=TRUE),
                Q10=mean(Q10, na.rm=TRUE),
                Q11=mean(Q11, na.rm=TRUE))
  res <- reshape(res, timevar="risk", idvar=c("kigou", "haihu"), direction="wide")

  res2 <- ddply(data, .(group, risk), summarise, 
                Q1=mean(Q1, na.rm=TRUE), 
                Q4=mean(Q4, na.rm=TRUE), 
                Q8=mean(Q8, na.rm=TRUE),
                Q10=mean(Q10, na.rm=TRUE),
                Q11=mean(Q11, na.rm=TRUE))
  res2 <- reshape(res2, timevar="risk", idvar="group", direction="wide")
  res2 <- data.frame(kigou=res2[,1], haihu=999, res2[,-1])
  
  res3 <- ddply(data, .(risk), summarise, id="全体",
                Q1=mean(Q1, na.rm=TRUE), 
                Q4=mean(Q4, na.rm=TRUE), 
                Q8=mean(Q8, na.rm=TRUE),
                Q10=mean(Q10, na.rm=TRUE),
                Q11=mean(Q11, na.rm=TRUE))
  res3 <- reshape(res3, timevar="risk", idvar="id", direction="wide")
  res3$id <- NULL
  res3 <- data.frame(kigou="全体", haihu=999, res3)

  res <- rbind(res, res2, res3)
  return(res)
  }

##############################################################################
##集計

#####入力

setwd("Q:/Pln/Analysis/01_hokensya/01_kenpo/10_三菱電機/01_data")

kc2009 <- read.csv("kc2009.csv", as.is=TRUE)
kc2010 <- read.csv("kc2010.csv", as.is=TRUE)

mc2009 <- read.csv("mc2009.csv", as.is=TRUE)
mc2010 <- read.csv("mc2010.csv", as.is=TRUE)

mc2009$bmi <- NULL
mc2010$bmi <- NULL
mc2009$age <- NULL
mc2010$age <- NULL
mc2009$sex <- NULL
mc2010$sex <- NULL
mc2009$group <- NULL
mc2010$group <- NULL


#問診・健診ともにデータがある者のみを集計対象とする

d2009 <- merge(kc2009, mc2009, by=c("kigou", "bangou", "haihu"))
d2010 <- merge(kc2010, mc2010, by=c("kigou", "bangou", "haihu"))


#2010年度時点で100名以上のデータがある事業所に絞り込む→問診でも共通のリストを使用

jg <- ddply(d2010, .(kigou, haihu), summarise, count=sum(!is.na(bangou)))
jg <- subset(jg, count>=100)
jg$count <- NULL

d2009 <- merge(d2009, jg)
d2010 <- merge(d2010, jg)


#####集計

res1 <- mkkbunpu(d2010)
res2 <- mkido(d2010, d2009)

res3 <- merge(shuukeiSmp(d2009), shuukeiSmp(d2010), by=c("kigou", "haihu"))
colnames(res3) <- gsub("x", "2009", colnames(res3))
colnames(res3) <- gsub("y", "2010", colnames(res3))



#######出力

#事業所・配布コードと突合して事業所一覧の形にして出力する
setwd("Q:/Pln/Analysis/01_hokensya/01_kenpo/10_三菱電機/01_data")
code <- read.csv("code.csv", as.is=TRUE)


jconv <- function(data, code){
  res <- merge(code, data, all.y=TRUE)
  res$j <- ifelse(is.na(res$j2), res$j1, str_c(res$j1, res$j2, sep=" "))  
  res$j1 <- NULL
  res$j2 <- NULL
  res$kigou <- NULL
  res$haihu <- NULL
  res$count <- NULL
  res <- data.frame(res$j, res[,-ncol(res)])
  invisible(res)
  }

res11 <- jconv(res1, code)
res21 <- jconv(res2, code)
res31 <- jconv(res3, code)

setwd("Q:/Pln/Analysis/01_hokensya/01_kenpo/10_三菱電機/02_output")
write.csv(res11, "健康分布2010.csv", row.names=FALSE)
write.csv(res21, "健康分布移動.csv", row.names=FALSE)
write.csv(res31, "問診経年.csv", row.names=FALSE)
