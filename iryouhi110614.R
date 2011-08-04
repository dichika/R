#サンプル医療費データの傾向を確かめる
library(ggplot2)
library(googleVis)
source("Q:\\Pln\\Analysis\\999_code\\mkhtml.R")

tmpd <- tempdir()
setwd(tmpd)

df <- read.csv("D:/downloads/2005b.csv", as.is=TRUE)
df <- subset(df, complete.cases(df))
dfm <- melt(df, id.vars="区分")
dfm$variable <- as.Date(gsub("X", "", paste(dfm$variable, "01", sep="")), format="%Y%m%d")
dfm$区分 <- as.character(dfm$区分)
ggplot(dfm, aes(x=variable, y=value, group=区分)) + geom_line(aes(colour=区分)) + opts(axis.text.x=theme_text(angle=90))

res <- gvisMotionChart(dfm, timevar="variable", idvar="区分", date.format="%Y-%m-%d", chartid="sample")
res$html$header <- gsub("charset=utf-8", "charset=shift-jis", res$html$header)
plot(res) #一度プロットする必要あり
gvisMURL <- paste(sprintf("http://127.0.0.1:%s/custom/googleVis/", tools:::httpdPort), res$chartid, ".html", sep="")

df <- data.frame("医療機関の施設数", "LL", stringsAsFactors=FALSE)
df[2,] <- c("2000-2008年の施設数変遷：区分別", "M")
df[3,] <- c("ここはあとで編集する", "S")
df[4,] <- c(gvisMURL, "S")
df[5,] <- c("copyright:dichika", "M")

mkhtml("output.html", df)

shell("output.html")#描画する際にRが起動している必要あり

#問題点
#保存しなおすとHTMLが別ファイルで保存されてしまいgoogle visualization APIが無効になる→

library(XLConnect)
wb <- loadWorkbook("D:/downloads/01.xls") #ファイル（ワークブック）を指定する
getSheets(wb) #ファイル内のシート一覧を確認

getData <- function(wb, sheet){
  data <- readWorksheet(wb, sheet=sheet) #シートを指定して読み込む
  date <- as.Date(paste(sep="", as.numeric(data[12:nrow(data),2]), "01"), format="%Y%m%d")
  value <- as.integer(data[12:nrow(data),3])
  res <- data.frame(date, value)
  colnames(res) <- c("date", sheet)
  invisible(res)
  }

res <- merge(getData(wb, "件数"), getData(wb, "日数"))

data <- readWorksheet(wb, sheet="医療費")
result <- data.frame(do.call("rbind", strsplit(split="E", data[12:nrow(data),3])), stringsAsFactors=FALSE)
result$医療費 <- as.numeric(result[,1]) * 10 ^ as.numeric(result[,2])

res$医療費 <- result$医療費

M <- res
M <- melt(M, id.vars="date")

res <- gvisMotionChart(M, timevar="date", idvar="variable", date.format="%Y-%m-%d", chartid="sample")
res$html$header <- gsub("charset=utf-8", "charset=shift-jis", res$html$header)
plot(res) #一度プロットする必要あり
gvisMURL <- paste(sprintf("http://127.0.0.1:%s/custom/googleVis/", tools:::httpdPort), res$chartid, ".html", sep="")

df <- data.frame("医療機関の施設数", "LL", stringsAsFactors=FALSE)
df[2,] <- c("2000-2008年の施設数変遷：区分別", "M")
df[3,] <- c("ここはあとで編集する", "S")
df[4,] <- c(gvisMURL, "S")
df[5,] <- c("copyright:dichika", "M")
mkhtml("output.html", df)
shell("output.html")#描画する際にRが起動している必要あり

