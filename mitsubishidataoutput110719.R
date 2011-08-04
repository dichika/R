library(batade)
library(ggplot2)
library(stringr)
library(googleVis)
library(digest)

source("Q:/Pln/Analysis/999_code/mitsubishidataconv110719.R") #健診・問診変換関数

setwd("Q:/Pln/Analysis/01_hokensya/01_kenpo/10_三菱電機/01_data")

###Input

#kenshin
k2009 <- read.csv("k2009.csv", as.is=TRUE)
k2010 <- read.csv("k2010.csv", as.is=TRUE)

#monshin
m2009 <- read.csv("m2009.csv", as.is=TRUE, header=FALSE)
m2010 <- read.csv("m2010.csv", as.is=TRUE, header=FALSE)


###Convert

#kenshin
kc2009 <- convk(k2009)
kc2010 <- convk(k2010)

#事業所コード
code <- ddply(k2010,.(記号,配布,事業所名称,配布名称),summarise, count=sum(!is.na(保険者名)))
colnames(code) <- c("kigou", "haihu", "j1", "j2","count")
add <- data.frame(kigou=c("関連会社", "研究所_製作所", "本社・支社", "全体"), haihu=999, j1=c("関連会社", "研究所_製作所", "本社・支社", "全体"), j2=NA, count=NA)
code <- rbind(code, add)
code$haihu <- ifelse(is.na(code$haihu), 999, code$haihu)


#問診
mc2009 <- convm(m2009)
mc2010 <- convm(m2010)


###Output

setwd("Q:/Pln/Analysis/01_hokensya/01_kenpo/10_三菱電機/01_data")

write.csv(kc2009, "kc2009.csv", row.names=FALSE)
write.csv(kc2010, "kc2010.csv", row.names=FALSE)

write.csv(code, "code.csv", row.names=FALSE)

write.csv(mc2009, "mc2009.csv", row.names=FALSE)
write.csv(mc2010, "mc2010.csv", row.names=FALSE)