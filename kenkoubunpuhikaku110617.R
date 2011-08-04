library(ggplot2)
source("Q://Pln//Analysis//999_code//mkhtml.R")
setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/02_chihou/3月健康分布作成/")

result <- read.csv("test.csv", stringsAsFactors=FALSE)
result1 <- ddply(result, .(支部番号, 肥満判定, 総合リスク判定), summarise, count=length(性別))
all <- ddply(result, .(支部番号), summarise, all=length(性別))
result1 <- merge(result1, all, all.x=TRUE)
result1$prop <- round(result1$count*100 / result1$all, 1)

result <- read.csv("testall.csv", stringsAsFactors=FALSE) 
result <- melt(result, id.var="name")
resultall <- subset(result, variable=="all")
result <- subset(result, variable!="all")

result1 <- merge(result, resultall, all.x=TRUE, by="name")
result1[,4] <- NULL
result1$肥満判定 <- "肥満"
result1[grep("非肥満", result1$variable.x),]$肥満判定 <- "非肥満"
colnames(result1) <- c("name", "risk", "count", "all", "肥満判定")
result1$risk <- gsub("非肥満", "", result1$risk)
result1$risk <- gsub("肥満", "", result1$risk)
result1$prop <- round(result1$count*100 / result1$all, 1)

ggplot(data=result1, aes(reorder(name, all)))+
geom_bar(subset=.(肥満判定=="肥満"), aes(y=prop, fill=risk), stat="identity")+
geom_bar(subset=.(肥満判定=="非肥満"), aes(y=-prop, fill=risk), stat="identity")+
theme_bw() + geom_hline(yintercept = 0, colour = "grey90") + scale_y_continuous("各セグメントの割合", formatter="scientific", limits=c(-100,100))+
opts(axis.text.x=theme_text(angle=90))