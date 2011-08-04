###鳥取県保険者協議会のデータから統一データベースを作成

#国保

setwd("Q:/Pln/Analysis/01_hokensya/08_kyougikai/02_data/kokuho")
if(file.exists("thumbs.db"))file.remove("thumbs.db")

data <- NULL
for(i in 1:length(dir())){
  data0 <- read.csv(dir()[i], header=TRUE, as.is=TRUE, skip=1)
  data0 <- data0[-nrow(data0),]
  name <- colnames(data0)
  colnames(data0) <- paste(sep="", "V", 1:length(data0))
  data <- rbind(data, data0)
  }
colnames(data) <- name

#その他
setwd("Q:/Pln/Analysis/01_hokensya/08_kyougikai/02_data/")
if(file.exists("thumbs.db"))file.remove("thumbs.db")

data2 <- NULL
for(i in grep("csv",dir())){
  data20 <- read.csv(dir()[i], header=FALSE, as.is=TRUE, skip=1)
  colnames(data20) <- paste(sep="", "V", 1:length(data20))
  data2 <- rbind(data2, data20)
  }
colnames(data2) <- name

data <- rbind(data, data2)

setwd("Q:/Pln/Analysis/01_hokensya/08_kyougikai/02_data/analysis")
write.csv(data, "tottorikyougikai110721.csv", row.names=FALSE)