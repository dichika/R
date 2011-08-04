library(batade)
library(ggplot2)
library(stringr)
library(googleVis)


setwd("Q:/Pln/Analysis/01_hokensya/01_kenpo/10_三菱電機/01_data")

#########Input

#kenshin
k2009 <- read.csv("k2009.csv", as.is=TRUE)
k2010 <- read.csv("k2010.csv", as.is=TRUE)

#monshin
m2009 <- read.csv("m2009.csv", as.is=TRUE, header=FALSE)
m2010 <- read.csv("m2010.csv", as.is=TRUE, header=FALSE)

colnames(m2009) <- c("kigou", "kinmuchikubun", "haihu", "shozoku", "mannum", "age", "sex", "stressP", str_c("Q", 1:11), "yakushoku", "shokushu", "kinmukeitai", "height", "weight", "bmi")
colnames(m2010) <- c("kigou", "kinmuchikubun", "haihu", "shozoku", "mannum", "age", "sex", "stressP", str_c("Q", 1:11), "yakushoku", "shokushu", "kinmukeitai", "height", "weight", "bmi")

m2009$yakushoku <- ifelse(nchar(m2009$yakushoku)==0, "その他", m2009$yakushoku)　#空白をその他で置換
m2010$yakushoku <- ifelse(nchar(m2010$yakushoku)==0, "その他", m2010$yakushoku)

m2009$kinmuchikubun <- ifelse(is.na(m2009$kinmuchikubun), 999, m2009$kinmuchikubun) #NAを999で置換
m2010$kinmuchikubun <- ifelse(is.na(m2010$kinmuchikubun), 999, m2010$kinmuchikubun)


#treemapで概観把握

#1層
major <- ddply(m2009, .(kigou), summarise, count=sum(!is.na(mannum)))
major$Child <- str_c("c", major$kigou)
major$Parent <- "mitsubishi"
major$color <- 1:nrow(major)
major <- rbind(major, c(NA, sum(major$count), "mitsubishi", NA, 100))
#plot(gvisTreeMap(data=major, idvar="Child", parentvar="Parent", sizevar="count", colorvar="color"))

#2層三菱電機（地方区分）
minor <- ddply(m2009[m2009$kigou==110, ], .(kinmuchikubun), summarise, count=sum(!is.na(mannum)))
minor$Child <- str_c("m", minor$kinmuchikubun)
minor$Parent <- "c110"
minor$color <- 1:nrow(minor)
colnames(minor) <- colnames(major)
pdata <- rbind(major, minor)
#plot(gvisTreeMap(data=pdata, idvar="Child", parentvar="Parent", sizevar="count", colorvar="color"))

#3層三菱電機
minor2 <- ddply(m2009[m2009$kigou==110, ], .(kinmuchikubun, yakushoku), summarise, count=sum(!is.na(mannum)))
minor2$Child <- minor2$yakushoku
minor2$Parent <- str_c("m", minor2$kinmuchikubun)
minor2$color <- 1:nrow(minor2)
minor2$yakushoku <- NULL
minor2$Child <- str_c(minor2$Child, minor2$Parent)
colnames(minor2) <- colnames(major)
pdata <- rbind(pdata, minor2)

#2層三菱電機以外
minor3 <- ddply(m2009[m2009$kigou!=110, ], .(kigou, yakushoku), summarise, count=sum(!is.na(mannum)))
minor3$Child <- minor3$yakushoku
minor3$Parent <- str_c("c", minor3$kigou)
minor3$color <- 1:nrow(minor3)
minor3$yakushoku <- NULL
minor3$Child <- str_c(minor3$Child, minor3$Parent)
colnames(minor3) <- colnames(major)
pdata <- rbind(pdata, minor3)


T1 <- gvisTreeMap(data=pdata, idvar="Child", parentvar="Parent", sizevar="count", colorvar="color")
T1$html$header <- gsub("utf-8", "CP932", T1$html$header)
plot(T1)

#

convk <- function(data){
require(digest)

data$生年月日 <- as.Date(as.character(data$生年月日), format="%Y/%m/%d")
data$健診受診日 <- as.Date(as.character(data$健診受診日), format="%Y/%m/%d")


#属性
hname <- data$事業所名
kigou <- data$記号
birth <- data$生年月日
jushinbi <- data$健診受診日
nendo <- ifelse(months(jushinbi) %in% c("1月","2月","3月"), as.numeric(substr(as.character(jushinbi), start=1, stop=4))-1, as.numeric(substr(as.character(jushinbi), start=1, stop=4)))

age <- as.numeric(trunc((as.Date(paste(sep="-", nendo+1, "03-31")) - birth)/365.25))
age5 <- as.numeric(trunc(age/5)*5)
data$性別 <- gsub("男|男性", 1, data$性別)
data$性別 <- gsub("女|女性", 2, data$性別)
id <- apply(data[,c(1,6)], 1, digest)


#検査値
height <- as.numeric(data$身長)
weight <- as.numeric(data$体重)
waist <- as.numeric(data$腹囲)
bmi <- as.numeric(data$BMI)
sbp <- as.numeric(data$血圧_収縮期)
dbp <- as.numeric(data$血圧_拡張期)
tg <- as.numeric(data$中性脂肪)
hdl <- as.numeric(data$HDL.コレステロール)
ldl <- as.numeric(data$LDL.コレステロール)
fbg <- as.numeric(data$空腹時血糖)
hba1c <- as.numeric(data$HbA1c)
got <- as.numeric(data$GOT)
gpt <- as.numeric(data$GPT)
ggtp <- as.numeric(data$γ.GPT)

#問診の処理
data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))] <- apply(data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))], 2, function(x)gsub("はい|1|速い", 1, x))
data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))] <- apply(data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))], 2, function(x)gsub("いいえ|2|ふつう|遅い", 0, x))
data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))] <- apply(data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))], 2, function(x)as.numeric(x))

data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))] <- apply(data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))], 2, function(x)gsub("はい|1", 0, x))
data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))] <- apply(data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))], 2, function(x)gsub("いいえ|2", 1, x))
data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))] <- apply(data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))], 2, function(x)as.numeric(x))


#健診項目・問診の揃い状況＋血糖項目有無チェック
df <- data.frame(stringsAsFactors=FALSE, hname, haihumeishou=data$配布名称, id, sex=data$性別, birth, jushinbi, nendo, age, age5, waist, bmi, sbp, dbp, tg, hdl, ldl, got, gpt, ggtp, fbg, hba1c, drBP=data$服薬_血圧, drBG=data$服薬_血糖, drLP=data$服薬_脂質, SM=data$喫煙)
#df <- subset(df, age>=40 & age<=64)
#df <- subset(df, complete.cases(df[,-c(19,20)]) & (as.numeric(!is.na(fbg)) + as.numeric(!is.na(hba1c)))>=1)


#検査項目リスク設定（保健指導・受診勧奨）
waisth <- ifelse(((df$waist >= 85) & (df$sex==1))|((df$waist >= 90) & (df$sex==2)), 1, 0)
bmih <- ifelse(df$bmi >= 25, 1, 0)
sbph <- ifelse(df$sbp >= 130, 1, 0)
sbpj <- ifelse(df$sbp >= 140, 1, 0)
dbph <- ifelse(df$dbp >= 85, 1, 0)
dbpj <- ifelse(df$dbp >= 90, 1, 0)
hdlh <- ifelse(-df$hdl >= -39, 1, 0)
hdlj <- ifelse(-df$hdl >= -34, 1, 0)
ldlh <- ifelse(df$ldl >= 130, 1, 0)
ldlj <- ifelse(df$ldl >= 140, 1, 0)
tgh <- ifelse(df$tg >= 150, 1, 0)
tgj <- ifelse(df$tg >= 300, 1, 0)
fbgh <- ifelse(df$fbg >= 100, 1, 0)
fbgj <- ifelse(df$fbg >= 126, 1, 0)
hba1ch <- ifelse(df$hba1c >= 5.2, 1, 0)
hba1cj <- ifelse(df$hba1c >= 6.1, 1, 0)
ggtph <- ifelse(df$ggtp >= 51, 1, 0)
ggtpj <- ifelse(df$ggtp >= 101, 1, 0)
goth <- ifelse(df$got >= 31, 1, 0)
gotj <- ifelse(df$got >= 51, 1, 0)
gpth <- ifelse(df$gpt >= 31, 1, 0)
gptj <- ifelse(df$gpt >= 51, 1, 0)

waisth <- ifelse(is.na(waisth), 0, waisth)
bmih <- ifelse(is.na(bmih), 0, bmih)
sbph <- ifelse(is.na(sbph), 0, sbph)
sbpj <- ifelse(is.na(sbpj), 0, sbpj)
dbph <- ifelse(is.na(dbph), 0, dbph)
dbpj <- ifelse(is.na(dbpj), 0, dbpj)
hdlh <- ifelse(is.na(hdlh), 0, hdlh)
hdlj <- ifelse(is.na(hdlj), 0, hdlj)
ldlh <- ifelse(is.na(ldlh), 0, ldlh)
ldlj <- ifelse(is.na(ldlj), 0, ldlj)
tgh <- ifelse(is.na(tgh), 0, tgh)
tgj <- ifelse(is.na(tgj), 0, tgj)
fbgh <- ifelse(is.na(fbgh), 0, fbgh)
fbgj <- ifelse(is.na(fbgj), 0, fbgj)
hba1ch <- ifelse(is.na(hba1ch), 0, hba1ch)
hba1cj <- ifelse(is.na(hba1cj), 0, hba1cj)
ggtph <- ifelse(is.na(ggtph), 0, ggtph)
ggtpj <- ifelse(is.na(ggtpj), 0, ggtpj)
goth <- ifelse(is.na(goth), 0, goth)
gotj <- ifelse(is.na(gotj), 0, gotj)
gpth <- ifelse(is.na(gpth), 0, gpth)
gptj <- ifelse(is.na(gptj), 0, gptj)

#階層化リスク設定
kaisoukaBP <- ifelse((sbph + dbph)>=1, 1, 0)
kaisoukaBG <- ifelse(is.na(hba1ch), fbgh, hba1ch)
kaisoukaLP <- ifelse((tgh + hdlh)>=1, 1, 0)
kaisoukaSM <- ifelse(df$SM==1,1,0)
kaisoukaSM <- ifelse(is.na(kaisoukaSM), 0, kaisoukaSM)
kaisoukaOB <- ifelse((waisth + bmih)>0, 2, 1)

#受診勧奨リスク設定
jushinBP <- ifelse((sbpj + dbpj)>=1, 1, 0)
jushinBG <- ifelse(is.na(hba1cj), fbgj, hba1cj)
jushinLP <- ifelse((ldlj)>=1, 1, 0)

#階層化リスク個数設定
kaisoukaCOUNT <- kaisoukaBP + kaisoukaBG + kaisoukaLP + kaisoukaSM
#kaisoukaJUSHIN <- ifelse(is.na(fbgj), sbpj + dbpj + hdlj + ldlj + tgj + ggtpj + gptj + gotj + hba1cj, 
#                         sbpj + dbpj + hdlj + ldlj + tgj + ggtpj + gptj + gotj + fbgj)
kaisoukaJUSHIN <- sbpj + dbpj + hdlj + ldlj + tgj + ggtpj + gptj + gotj + hba1cj + fbgj 


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
invisible(df)
}

###問診と健診の突合

convm <- function(data){
data$id <- apply(data[,c(1,5)], 1, digest)
data[,9:19] <- apply(data[,9:19], 2, function(x)ifelse(is.na(x), 0, x))
invisible(data)
}

mc2009 <- convm(m2009)
mc2010 <- convm(m2010)


kc2009 <- convk(k2009)
kc2010 <- convk(k2010)




######集計

#100名以上の事業所リスト
jg <- ddply(kc2010, .(hname, haihumeishou), summarise, count=sum(!is.na(id)))
jg <- subset(jg, count>=100)
jg$count <- NULL
jg <- rbind(jg, data.frame(hname="全体平均",haihumeishou=""))

#最新年度(2010年度)の健康分布を事業所単位でカウントする

kb <- ddply(kc2010, .(hname, haihumeishou, kenkoubunpuSEG), summarise, count=sum(!is.na(id)))
res1 <- reshape(kb, idvar=c("hname", "haihumeishou"), timevar="kenkoubunpuSEG", direction="wide")
res1[,-c(1,2)] <- apply(res1[,-c(1,2)], 2, function(x)ifelse(is.na(x), 0, x))
zentai <- data.frame(hname="全体平均", haihumeishou="", t(colSums(res1[,-c(1,2)])))
res1 <- rbind(res1, zentai)
sums <- rowSums(res1[,-c(1,2)])
per <- round(res1[,-c(1,2)]*100 / sums, 1)
colnames(per) <- gsub("count", "per", colnames(per))
res1 <- cbind(res1, per)
res1$all <- sums

res1 <- merge(res1, jg) #100名以上の事業所のみ抽出
res1$name <- str_c(res1$hname, res1$haihumeishou)



#健康分布の移動状況(2010年度の結果から2009年度を遡ることに注意)

kenshin <- merge(kc2009, kc2010, by="id")
kbido <- ddply(kenshin, .(hname.x, haihumeishou.x, kenkoubunpuSEG.y, kenkoubunpuSEG.x), summarise, count=sum(!is.na(id)))
colnames(kbido) <- c("hname", "haihumeishou", "s10", "s09", "count")
kbido <- kbido[order(kbido$s10, kbido$s09),]
kbido$seg <- str_c(kbido$s10, kbido$s09, sep=".")
kbido$s09 <- NULL
kbido$s10 <- NULL
res2 <- reshape(kbido, idvar=c("hname", "haihumeishou"), timevar=c("seg"), direction="wide")
res2[,-c(1,2)] <- apply(res2[,-c(1,2)], 2, function(x)ifelse(is.na(x), 0, x))
zentai2 <- data.frame(hname="全体平均", haihumeishou="", t(colSums(res2[,-c(1,2)])))
res2 <- rbind(res2, zentai2)

for(c in seq(3,59,by=8)){
per <- round(apply(res2[, c:(c+7)], 2, function(x)ifelse(x>0, x * 100 / rowSums(res2[, c:(c+7)]), 0)), 1)
colnames(per) <- gsub("count", "per", colnames(per))
res2 <- cbind(res2, per)
}

res2 <- merge(res2, jg) #100名以上の事業所のみ抽出
res2$name <- str_c(res2$hname, res2$haihumeishou)

#問診（リスク者・未リスク者）



#######出力
setwd("Q:/Pln/Analysis/01_hokensya/01_kenpo/10_三菱電機/02_output")
write.csv(res1, "健康分布2010.csv", row.names=FALSE)
write.csv(res2, "健康分布移動.csv", row.names=FALSE)