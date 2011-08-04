library(ggplot2)
library(googleVis)
library(XLConnect)
source("Q:\\Pln\\Analysis\\999_code\\mkhtml.R")

setwd("D:/downloads")
wb <- loadWorkbook("D:/downloads/01.xls") #ファイル（ワークブック）を指定する
getSheets(wb) #ファイル内のシート一覧を確認

#データ読み込み関数（医療費だけ指数表示で読み込まれるので手を加える）
getData <- function(wb, sheet){
  data <- readWorksheet(wb, sheet=sheet) #シートを指定して読み込む
  date <- as.Date(paste(sep="", as.numeric(data[12:nrow(data),2]), "01"), format="%Y%m%d")
  if(sheet=="医療費"){
    result <- data.frame(do.call("rbind", strsplit(split="E", data[12:nrow(data),3])), stringsAsFactors=FALSE)
    value <- as.numeric(result[,1]) * 10 ^ as.numeric(result[,2])
    }else{
    value <- as.integer(data[12:nrow(data),3])
    }
  res <- data.frame(date, value)
  colnames(res) <- c("date", sheet)
  invisible(res)
  }

data <- merge(getData(wb, "件数"), getData(wb, "日数"))
data <- merge(data, getData(wb, "医療費"))

data.m <- melt(data, id.vars="date")

res1 <- gvisMotionChart(data.m, timevar="date", idvar="variable", date.format="%Y-%m-%d", chartid="sample")
res1$html$header <- gsub("charset=utf-8", "charset=shift-jis", res$html$header)
plot(res1)
#gvisMURL <- paste(sprintf("http://127.0.0.1:%s/custom/googleVis/", tools:::httpdPort), res$chartid, ".html", sep="")

data.ms <- subset(data.m, variable=="医療費")

res2 <- gvisAnnotatedTimeLine(data.ms, datevar="date", numvar="value", idvar="variable", date.format="%Y-%m-%d", chartid="sample")
res2$html$header <- gsub("charset=utf-8", "charset=shift-jis", res$html$header)
plot(res2)
