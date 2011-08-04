library(ggplot2)
library(googleVis)
setwd("Q:/Pln/Analysis/01_hokensya/02_kokuho/02_hachioji/2011")
data <- read.csv("hachihoken110613.csv", as.is=TRUE)
data$発送日 <- as.Date(data$発送日)
data[,16:28] <- data[,16:28]*100
data$id <- NULL

for(count in 2:ncol(data)){
filename <- paste(count, ".png", sep="")
p <- ggplot(data, aes(x=発送日, y=data[,count])) + geom_line() + geom_point() + ylab(colnames(data[count]))
png(filename)
print(p)
dev.off()
}


data0 <- data.frame("八王子市保健指導利用勧奨", "LL", 0, stringsAsFactors=FALSE)
for(count in c(2, 16:ncol(data))){
txt <- c(colnames(data[count]), "M", 0)
img <- c(paste(sep="", count, ".png"), "S", 1)
data0 <- rbind(data0, txt)
data0 <- rbind(data0, img)
}
colnames(data0) <- NULL

mkhtml("八王子市保健指導利用勧奨結果110613.html", data0, foot=FALSE)