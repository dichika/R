library(ggplot2)
library(digest)
library(stringr)

setwd("Q:/Pln/Analysis/01_hokensya/04_suishin/2011/01_data")
if(file.exists("thumbs.db"))file.remove("thumbs.db")

data <- NULL
for(i in 1:length(dir())){
  data0 <- read.csv(dir()[i], header=TRUE, as.is=TRUE)
  name <- colnames(data0)
  colnames(data0) <- paste(sep="", "V", 1:length(data0))
  data <- rbind(data, data0)
  }
colnames(data) <- name
nrow(data)

data$生年月日 <- as.Date(as.character(data$生年月日), format="%Y/%m/%d")
data$健診受診日 <- as.Date(as.character(data$健診受診日), format="%Y/%m/%d")


#日立
hitachi1 <- read.csv("Q:/Pln/Analysis/01_hokensya/04_suishin/2011/00_original/H21結果_日立健保_20110630.csv", header=FALSE, as.is=TRUE)
hitachi2 <- read.csv("Q:/Pln/Analysis/01_hokensya/04_suishin/2011/00_original/H22結果_日立健保_20110630.csv", header=FALSE, as.is=TRUE)

convH <- function(data, colname=name){
  colnames(data) <- colname
  data$生年月日 <- as.Date(as.character(data$生年月日), format="%Y%m%d")
  data$健診受診日 <- as.Date(as.character(data$健診受診日), format="%Y%m%d")
  invisible(data)
  }

hitachi1 <- convH(hitachi1)
hitachi2 <- convH(hitachi2)

data <- rbind(data, hitachi1, hitachi2)

###健診データの処理

#属性
hname <- gsub("健康保険組合|健保", "", data$保険者名, perl=TRUE)
birth <- data$生年月日
jushinbi <- data$健診受診日
nendo <- ifelse(months(jushinbi) %in% c("1月","2月","3月"), as.numeric(substr(as.character(jushinbi), start=1, stop=4))-1, as.numeric(substr(as.character(jushinbi), start=1, stop=4)))

age <- as.numeric(trunc((as.Date(paste(sep="-", nendo+1, "03-31")) - birth)/365.25))
age5 <- as.numeric(trunc(age/5)*5)
data$性別 <- gsub("男|男性", 1, data$性別)
data$性別 <- gsub("女|女性", 2, data$性別)
id <- apply(data[,1:2], 1, digest)


#検査値
height <- as.numeric(data$身長)
weight <- as.numeric(data$体重)
waist <- as.numeric(data$腹囲)
bmi <- as.numeric(data$ＢＭＩ)
sbp <- as.numeric(data$血圧_収縮期)
dbp <- as.numeric(data$血圧_拡張期)
tg <- as.numeric(data$中性脂肪)
hdl <- as.numeric(data$HDLコレステロール)
ldl <- as.numeric(data$LDLコレステロール)
fbg <- as.numeric(data$空腹時血糖)
hba1c <- as.numeric(data$HbA1c)
got <- as.numeric(data$GOT)
gpt <- as.numeric(data$GPT)
ggtp <- as.numeric(data$γ.GTP)

#問診の処理
data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))] <- apply(data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))], 2, function(x)gsub("はい|1|速い", 1, x))
data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))] <- apply(data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))], 2, function(x)gsub("いいえ|2|ふつう|遅い", 0, x))
data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))] <- apply(data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))], 2, function(x)as.numeric(x))

data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))] <- apply(data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))], 2, function(x)gsub("はい|1", 0, x))
data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))] <- apply(data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))], 2, function(x)gsub("いいえ|2", 1, x))
data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))] <- apply(data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))], 2, function(x)as.numeric(x))


#健診項目・問診の揃い状況＋血糖項目有無チェック
df <- data.frame(stringsAsFactors=FALSE, hname, id, sex=data$性別, birth, jushinbi, nendo, age, age5, waist, bmi, sbp, dbp, tg, hdl, ldl, got, gpt, ggtp, fbg, hba1c, drBP=data$服薬_血圧, drBG=data$服薬_血糖, drLP=data$服薬_脂質, SM=data$喫煙)
table(df$hname)
df <- subset(df, age>=40 & age<=64)
df <- subset(df, complete.cases(df[,-c(19,20)]) & (as.numeric(!is.na(fbg)) + as.numeric(!is.na(hba1c)))>=1)
table(df$hname)

#検査項目リスク設定（保健指導・受診勧奨）
waisth <- ifelse(((df$waist >= 85) & (df$sex==1))|((df$waist >= 90) & (df$sex==2)), 1, 0)
bmih <- ifelse(df$bmi >= 25, 1, 0)
sbph <- ifelse(df$sbp >= 130,1,0)
sbpj <- ifelse(df$sbp >= 140,1,0)
dbph <- ifelse(df$dbp >= 85,1,0)
dbpj <- ifelse(df$dbp >= 90,1,0)
hdlh <- ifelse(-df$hdl >= -39,1,0)
hdlj <- ifelse(-df$hdl >= -34,1,0)
ldlh <- ifelse(df$ldl >= 130,1,0)
ldlj <- ifelse(df$ldl >= 140,1,0)
tgh <- ifelse(df$tg >= 150,1,0)
tgj <- ifelse(df$tg >= 300,1,0)
fbgh <- ifelse(df$fbg >= 100,1,0)
fbgj <- ifelse(df$fbg >= 126,1,0)
hba1ch <- ifelse(df$hba1c >= 5.2,1,0)
hba1cj <- ifelse(df$hba1c >= 6.1,1,0)
ggtph <- ifelse(df$ggtp >= 51,1,0)
ggtpj <- ifelse(df$ggtp >= 101,1,0)
goth <- ifelse(df$got >= 31,1,0)
gotj <- ifelse(df$got >= 51,1,0)
gpth <- ifelse(df$gpt >= 31,1,0)
gptj <- ifelse(df$gpt >= 51,1,0)



#階層化リスク設定
kaisoukaBP <- ifelse((sbph + dbph)>=1, 1, 0)
kaisoukaBG <- ifelse(is.na(hba1ch), fbgh, hba1ch)
kaisoukaLP <- ifelse((tgh + hdlh)>=1, 1, 0)
kaisoukaSM <- ifelse(df$SM==1,1,0)
kaisoukaOB <- ifelse((waisth + bmih)>0, 2, 1)

#受診勧奨リスク設定
jushinBP <- ifelse((sbpj + dbpj)>=1, 1, 0)
jushinBG <- ifelse(is.na(hba1cj), fbgj, hba1cj)
jushinLP <- ifelse((ldlj)>=1, 1, 0)

#階層化リスク個数設定
kaisoukaCOUNT <- kaisoukaBP + kaisoukaBG + kaisoukaLP + kaisoukaSM
kaisoukaJUSHIN <- ifelse(is.na(fbgj), sbpj + dbpj + hdlj + ldlj + tgj + ggtpj + gptj + gotj + hba1cj, 
                         sbpj + dbpj + hdlj + ldlj + tgj + ggtpj + gptj + gotj + fbgj)

#階層化リスク組合せ設定
kaisoukaKUMIAWASE <- paste(sep="|", ifelse(kaisoukaSM==1, "SM", ""), ifelse(kaisoukaBP==1, "BP", ""), ifelse(kaisoukaBG==1, "BG", ""), ifelse(kaisoukaLP==1, "LP", "")) 


#健康分布フラグ設定

kenkoubunpuRISK <- rep(NA, nrow(df))
for(i in 1:nrow(df)){
  if(sum(df[i, grep("dr",colnames(df))], na.rm=TRUE)>0){
    kenkoubunpuRISK[i] <- 4
    } else if(kaisoukaJUSHIN[i] > 0){
    kenkoubunpuRISK[i] <- 3
    } else if(kaisoukaCOUNT[i] > 0){
    kenkoubunpuRISK[i] <- 2
    } else {
    kenkoubunpuRISK[i] <- 1}
  }
kenkoubunpuSEG <- 10*kaisoukaOB + kenkoubunpuRISK

df <- data.frame(df, kaisoukaBP, kaisoukaBG, kaisoukaLP, kaisoukaSM, kaisoukaOB, jushinBP, jushinBG, jushinLP, kaisoukaJUSHIN, kaisoukaCOUNT, kaisoukaKUMIAWASE, kenkoubunpuRISK, kenkoubunpuSEG)
d2009 <- subset(df, nendo==2009)
d2010 <- subset(df, nendo==2010)

setwd("Q:/Pln/Analysis/01_hokensya/04_suishin/2011/02_output")

df <- merge(d2009, d2010, by=c("id","hname","birth","sex"))

#write.csv(df, "dall.csv", row.names=FALSE)
