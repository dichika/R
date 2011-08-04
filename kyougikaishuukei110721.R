library(ggplot2)
library(batade)
library(digest)
library(stringr)
library(googleVis)
#source("Q:/Pln/Analysis/999_code/KyougikaiDataMake110721.R")

#データ読み込み
setwd("Q:/Pln/Analysis/01_hokensya/08_kyougikai/02_data/analysis")

data <- read.csv("tottorikyougikai110721.csv", as.is=TRUE)
data$生年月日 <- gsub("-", "", data$生年月日)
data$健診受診日 <- gsub("-", "", data$健診受診日)
data$生年月日 <- as.Date(as.character(data$生年月日), format="%Y%m%d")
data$健診受診日 <- as.Date(as.character(data$健診受診日), format="%Y%m%d")


###健診データの処理

#属性
hname <- gsub("共済組合", "共済", data$保険者名, perl=TRUE)
kubun <- gsub("被保険者|本人", "被保険者", data$被保険者_被扶養者, perl=TRUE)
kubun <- ifelse(kubun!="被保険者", "被扶養者", kubun)
birth <- data$生年月日
jushinbi <- data$健診受診日
nendo <- ifelse(months(jushinbi) %in% c("1月","2月","3月"), as.numeric(substr(as.character(jushinbi), start=1, stop=4))-1, as.numeric(substr(as.character(jushinbi), start=1, stop=4)))

age <- as.numeric(trunc((as.Date(paste(sep="-", nendo+1, "03-31")) - birth)/365.25))
age5 <- as.numeric(trunc(age/5)*5)
data$性別 <- gsub("男|男性", 1, data$性別)
data$性別 <- gsub("女|女性", 2, data$性別)
#id <- apply(data[,1:2], 1, digest)


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
data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))] <- apply(data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))], 2, function(x)gsub("はい|1|速い|服薬あり", 1, x))
data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))] <- apply(data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))], 2, function(x)gsub("いいえ|2|ふつう|遅い|服薬なし", 0, x))
data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))] <- apply(data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))], 2, function(x)as.numeric(x))

data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))] <- apply(data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))], 2, function(x)gsub("はい|1", 0, x))
data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))] <- apply(data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))], 2, function(x)gsub("いいえ|2", 1, x))
data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))] <- apply(data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))], 2, function(x)as.numeric(x))


#健診項目・問診の揃い状況＋血糖項目有無チェック
df <- data.frame(stringsAsFactors=FALSE, hname, sex=as.numeric(data$性別), kubun, birth, jushinbi, nendo, age, age5, waist, bmi, sbp, dbp, tg, hdl, ldl, got, gpt, ggtp, fbg, hba1c, drBP=data$服薬_血圧, drBG=data$服薬_血糖, drLP=data$服薬_脂質, SM=data$喫煙)
#df <- subset(df, age>=40 & age<=64) #年齢制限は加えない
#df <- subset(df, complete.cases(df[,-c(19,20)]) & (as.numeric(!is.na(fbg)) + as.numeric(!is.na(hba1c)))>=1)
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

#NAを0に変換
naconv <- function(x)ifelse(is.na(x), 0, x)

waisth <- naconv(waisth)
bmih <- naconv(bmih)
sbph <- naconv(sbph)
sbpj <- naconv(sbpj)
dbph <- naconv(dbph)
dbpj <- naconv(dbpj)
hdlh <- naconv(hdlh)
hdlj <- naconv(hdlj)
ldlh <- naconv(ldlh)
ldlj <- naconv(ldlj)
tgh <- naconv(tgh)
tgj <- naconv(tgj)
fbgh <- naconv(fbgh)
fbgj <- naconv(fbgj)
hba1ch <- naconv(hba1ch)
hba1cj <- naconv(hba1cj)
ggtph <- naconv(ggtph)
ggtpj <- naconv(ggtpj)
goth <- naconv(goth)
gotj <- naconv(gotj)
gpth <- naconv(gpth)
gptj <- naconv(gptj)



#階層化リスク設定
kaisoukaBP <- ifelse((sbph + dbph)>=1, 1, 0)
kaisoukaBG <- ifelse((fbgh + hba1ch)>=1, 1, 0)
kaisoukaLP <- ifelse((tgh + hdlh)>=1, 1, 0)
kaisoukaSM <- ifelse(df$SM==1,1,0)
kaisoukaSM <- naconv(kaisoukaSM)
kaisoukaOB <- ifelse((waisth + bmih)>0, 2, 1)

#受診勧奨リスク設定
jushinBP <- ifelse((sbpj + dbpj)>=1, 1, 0)
jushinBG <- ifelse((fbgj + hba1cj)>=1, fbgj, hba1cj)
jushinLP <- ifelse((ldlj)>=1, 1, 0)

#階層化リスク個数設定
kaisoukaCOUNT <- ifelse((kaisoukaBP + kaisoukaBG + kaisoukaLP)>=1, kaisoukaBP + kaisoukaBG + kaisoukaLP + kaisoukaSM, 0)
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

#######################################################################



#性年齢ピラミッド

mkAgepyramid <- function(df){
  all <- ddply(df, .(hname), summarise, count=sum(!is.na(hname))) #基本属性
  attr_age <- ddply(df, .(hname), summarise, age_mean=round(mean(age, na.rm=TRUE),1)) #平均年齢
  attr_sex <- ddply(df, .(hname, sex), summarise, age_mean=mean(age, na.rm=TRUE), count=sum(!is.na(age))) #性別ごとの平均年齢

  attr_age5 <- ddply(df, .(hname, sex, age5), summarise, count=sum(!is.na(hname)))
  attr_age5 <- merge(attr_age5, all, by="hname")
  attr_age5 <- merge(attr_age5, attr_age, by="hname")

  attr_age5$prop <- round(attr_age5$count.x *100 / attr_age5$count.y, 1) #全体に対する割合
  attr_age5$sex <- ifelse(as.numeric(attr_age5$sex)==1, "男性", "女性")

  agepyramid <- ggplot(data=attr_age5, aes(age5))+
    geom_bar(subset=.(sex=="男性"), aes(y=prop, fill=factor(sex, levels=c("女性","男性"))), stat="identity")+
    geom_bar(subset=.(sex=="女性"), aes(y=-prop, fill=factor(sex, levels=c("女性","男性"))), stat="identity")+
    geom_hline(yintercept = 0, colour = "grey90") + opts(axis.text.x=theme_text(angle=90)) +coord_flip() +
    xlab("") + ylab("各セグメントの割合") + ylim(c(-50,50))+
    facet_wrap(~hname) + scale_fill_brewer(name="sex", palette="Set1") 
  AgeCount <- data.frame(hname=attr_age5$hname, count=attr_age5$count.y, age=attr_age5$age_mean)
  AgeCount$gl <- "国保"
  for(i in 1:nrow(AgeCount)){
    if((isTRUE(as.logical(grep("共済", AgeCount$hname[i]))))){
      AgeCount$gl[i] <- "共済"
      } else if (isTRUE(as.logical(grep("協会けんぽ", AgeCount$hname[i])))){
      AgeCount$gl[i] <- "協会けんぽ"
      } else if (isTRUE(as.logical(grep("全体", AgeCount$hname[i])))){
      AgeCount$gl[i] <- "全体"
      } else {
      AgeCount$gl[i] <- "国保"
      }
    }
  agecount <- ggplot(data=AgeCount, aes(x=count, y=age, label=hname)) + geom_point() + geom_text(aes(color=gl), size=3) + xlab("受診者数") + ylab("平均年齢") + opts(legend.position="none")
  return(list(agepyramid, agecount))
  }

#RColorBrewer::display.brewer.all(n=8, exact.n=FALSE)

attr_age5$hname2 <- paste(sep="", attr_age5$hname, "(", attr_age5$count.y, "人 平均年齢：", attr_age5$age_mean,")")

#####健康分布

mkKenkoubunpu <- function(df){
  kenpo <- ddply(df, .(hname, kaisoukaOB, kenkoubunpuRISK), summarise, count=sum(!is.na(hname)))
  kenpoall <- ddply(df, .(hname), summarise, count=sum(!is.na(hname)))
  result1 <- merge(kenpo, kenpoall, by="hname")
  colnames(result1) <- c("hname", "kaisoukaOB","kenkoubunpuRISK", "count", "all")

  all <- ddply(df, .(kaisoukaOB, kenkoubunpuRISK), summarise, count=sum(!is.na(hname)))
  all <- data.frame(hname="全体", all, Freq=nrow(df))
  colnames(all) <- c("hname", "kaisoukaOB","kenkoubunpuRISK", "count", "all")

  result1 <- rbind(result1, all)

  result1$prop <- round(result1$count*100 / result1$all, 1)
  result1$seg <- str_c(ifelse(result1$kaisoukaOB==1, "A", "B"), result1$kenkoubunpuRISK)
  result1$year <- "2010"

  p1 <- ggplot(data=result1, aes(hname))+
    geom_bar(subset=.(kaisoukaOB==2), aes(y=prop, fill=factor(kenkoubunpuRISK)), stat="identity")+
    geom_bar(subset=.(kaisoukaOB==1), aes(y=-prop, fill=factor(kenkoubunpuRISK)), stat="identity")+
    geom_hline(yintercept = 0, colour = "grey90") + opts(axis.text.x=theme_text(angle=90)) +coord_flip() +
    xlab("") + ylab("各セグメントの割合")
  
  #肥満者の割合

  result2 <- ddply(result1, .(hname, kaisoukaOB), summarise, prop=sum(prop))
  result2$g <- ifelse(result2$hname=="全体", "0", "1")

  p2 <- ggplot(data=result2, aes(reorder(as.character(hname), prop)))+
    geom_bar(subset=.(kaisoukaOB==2), aes(y=prop, fill=g), stat="identity")+
    coord_flip() + xlab("") + ylab("肥満者の割合") + opts(legend.position="none")

  return(list(p1, p2))
  }


####服薬率×コントロール率

mkJushinDrugRate <- function(df){

  #受診勧奨者服薬率
  dr1 <- data.frame(ddply(df[df$jushinBP==1, ], .(hname), summarise, dr_rate=mean(drBP, na.rm=TRUE)), risk="血圧")
  dr2 <- data.frame(ddply(df[df$jushinBG==1, ], .(hname), summarise, dr_rate=mean(drBG, na.rm=TRUE)), risk="血糖")
  dr3 <- data.frame(ddply(df[df$jushinLP==1, ], .(hname), summarise, dr_rate=mean(drLP, na.rm=TRUE)), risk="脂質")
  dr1all <- data.frame(hname="全体", dr_rate=mean(df[df$jushinBP==1, ]$drBP, na.rm=TRUE), risk="血圧")
  dr2all <- data.frame(hname="全体", dr_rate=mean(df[df$jushinBG==1, ]$drBG, na.rm=TRUE), risk="血糖")
  dr3all <- data.frame(hname="全体", dr_rate=mean(df[df$jushinLP==1, ]$drLP, na.rm=TRUE), risk="脂質")

  result3 <- rbind(dr1, dr2, dr3, dr1all, dr2all, dr3all)
  result3$g <- ifelse(result3$hname=="全体", "0", "1")

  p3 <- ggplot(result3, aes(x=reorder(hname, dr_rate)))+geom_bar(subset=.(risk=="血圧"), aes(y=dr_rate, fill=g), stat="identity") + coord_flip() + scale_y_continuous(formatter="percent", name="受診勧奨者服薬率") + opts(legend.position="none") + xlab("血圧")
  p4 <- ggplot(result3, aes(x=reorder(hname, dr_rate)))+geom_bar(subset=.(risk=="血糖"), aes(y=dr_rate, fill=g), stat="identity") + coord_flip() + scale_y_continuous(formatter="percent", name="受診勧奨者服薬率") + opts(legend.position="none") + xlab("血糖")
  p5 <- ggplot(result3, aes(x=reorder(hname, dr_rate)))+geom_bar(subset=.(risk=="脂質"), aes(y=dr_rate, fill=g), stat="identity") + coord_flip() + scale_y_continuous(formatter="percent", name="受診勧奨者服薬率") + opts(legend.position="none") + xlab("脂質")

  #服薬者コントロール率(コントロールされている割合であることに注意)

  ju1 <- data.frame(ddply(df[df$drBP==1, ], .(hname), summarise, ju_rate=1-mean(jushinBP, na.rm=TRUE)), risk="血圧")
  ju2 <- data.frame(ddply(df[df$drBG==1, ], .(hname), summarise, ju_rate=1-mean(jushinBG, na.rm=TRUE)), risk="血糖")
  ju3 <- data.frame(ddply(df[df$drLP==1, ], .(hname), summarise, ju_rate=1-mean(jushinLP, na.rm=TRUE)), risk="脂質")
  ju1all <- data.frame(hname="全体", ju_rate=1-mean(df[df$drBP==1, ]$jushinBP, na.rm=TRUE), risk="血圧")
  ju2all <- data.frame(hname="全体", ju_rate=1-mean(df[df$drBG==1, ]$jushinBG, na.rm=TRUE), risk="血糖")
  ju3all <- data.frame(hname="全体", ju_rate=1-mean(df[df$drLP==1, ]$jushinLP, na.rm=TRUE), risk="脂質")

  result4 <- rbind(ju1, ju2, ju3, ju1all, ju2all, ju3all)
  result4$g <- ifelse(result4$hname=="全体", "0", "1")
  result4$gl <- "国保"
  for(i in 1:nrow(result4)){
    if((isTRUE(as.logical(grep("共済", result4$hname[i]))))){
      result4$gl[i] <- "共済"
      } else if (isTRUE(as.logical(grep("協会けんぽ", result4$hname[i])))){
      result4$gl[i] <- "協会けんぽ"
      } else if (isTRUE(as.logical(grep("全体", result4$hname[i])))){
      result4$gl[i] <- "全体"
      } else {
      result4$gl[i] <- "国保"
      }
    }
  result4 <- subset(result4, complete.cases(result4))
  p6 <- ggplot(result4, aes(x=reorder(hname, ju_rate)))+geom_bar(subset=.(risk=="血圧"), aes(y=ju_rate, fill=g), stat="identity") + coord_flip() + scale_y_continuous(formatter="percent", name="服薬者コントロール率") + opts(legend.position="none") + xlab("血圧")
  p7 <- ggplot(result4, aes(x=reorder(hname, ju_rate)))+geom_bar(subset=.(risk=="血糖"), aes(y=ju_rate, fill=g), stat="identity") + coord_flip() + scale_y_continuous(formatter="percent", name="服薬者コントロール率") + opts(legend.position="none") + xlab("血糖")
  p8 <- ggplot(result4, aes(x=reorder(hname, ju_rate)))+geom_bar(subset=.(risk=="脂質"), aes(y=ju_rate, fill=g), stat="identity") + coord_flip() + scale_y_continuous(formatter="percent", name="服薬者コントロール率") + opts(legend.position="none") + xlab("脂質")

  result5 <- merge(result3, result4)
  p91 <- ggplot(data=result5, aes(x=dr_rate, y=ju_rate, label=hname)) + geom_point(subset=.(risk=="血圧"))+ geom_text(subset=.(risk=="血圧"), aes(color=gl), size=3) + scale_y_continuous(formatter="percent", name="服薬者コントロール率") + scale_x_continuous(formatter="percent", name="受診勧奨者服薬率") + opts(legend.position="none")
  p92 <- ggplot(data=result5, aes(x=dr_rate, y=ju_rate, label=hname)) + geom_point(subset=.(risk=="血糖"))+ geom_text(subset=.(risk=="血糖"), aes(color=gl), size=3) + scale_y_continuous(formatter="percent", name="服薬者コントロール率") + scale_x_continuous(formatter="percent", name="受診勧奨者服薬率") + opts(legend.position="none")
  p93 <- ggplot(data=result5, aes(x=dr_rate, y=ju_rate, label=hname)) + geom_point(subset=.(risk=="脂質"))+ geom_text(subset=.(risk=="脂質"), aes(color=gl), size=3) + scale_y_continuous(formatter="percent", name="服薬者コントロール率") + scale_x_continuous(formatter="percent", name="受診勧奨者服薬率") + opts(legend.position="none")

  return(list(p3, p4, p5, p6, p7, p8, p91, p92, p93))
  }



################出力
#レポート作成

df2009 <- subset(df, nendo==2009)

res1 <- mkAgepyramid(df2009)
res2 <- mkKenkoubunpu(df2009)
res3 <- mkJushinDrugRate(df2009)

setwd("Q:/Pln/Analysis/01_hokensya/08_kyougikai/03_output")
#画像の生成

png("性年齢別ピラミッド.png", width=800, height=600)
res1[[1]]
dev.off()

png("人数年齢.png", width=800, height=600)
res1[[2]]
dev.off()

png("健康分布.png", width=800, height=600)
res2[[1]]
dev.off()

png("肥満.png", width=800, height=600)
res2[[2]]
dev.off()

png("服薬血圧.png", width=800, height=600)
res3[[1]]
dev.off()

png("服薬血糖.png", width=800, height=600)
res3[[2]]
dev.off()

png("服薬脂質.png", width=800, height=600)
res3[[3]]
dev.off()

png("コント血圧.png", width=800, height=600)
res3[[4]]
dev.off()

png("コント血糖.png", width=800, height=600)
res3[[5]]
dev.off()

png("コント脂質.png", width=800, height=600)
res3[[6]]
dev.off()

png("コント×服薬血圧.png", width=800, height=600)
res3[[7]]
dev.off()

png("コント×服薬血糖.png", width=800, height=600)
res3[[8]]
dev.off()

png("コント×服薬脂質.png", width=800, height=600)
res3[[9]]
dev.off()

#レイアウト指定
mat <- rbind(c(sprintf("推進する会（%s）時点", Sys.Date()),"LL"),
             c("性年齢構成","L"),
             c("性年齢別ピラミッド.png","L"),
             c("平均年齢と健診受診者人数","L"),
             c("人数年齢.png","L"),
             c("肥満者の割合","L"),
             c("肥満.png","L"),
             c("健康分布比較(←非肥満　肥満→)","L"),
             c("健康分布.png","L"),
             c("服薬率：血圧","L"),
             c("服薬血圧.png","L"),
             c("服薬率：血糖","L"),
             c("服薬血糖.png","L"),
             c("服薬率：脂質","L"),
             c("服薬脂質.png","L"),
             c("コントロール：血圧","L"),
             c("コント血圧.png","L"),
             c("コントロール：血糖","L"),
             c("コント血糖.png","L"),
             c("コントロール：脂質","L"),
             c("コント脂質.png","L"),
             c("コントロール×服薬率：血圧","L"),
             c("コント×服薬血圧.png","L"),
             c("コントロール×服薬率：血糖","L"),
             c("コント×服薬血糖.png","L"),
             c("コントロール×服薬率：脂質","L"),
             c("コント×服薬脂質.png","L"),
             c("ここにメッセージを入力します","S"))

#レポート生成
mkhtml("report.html", mat, foot=TRUE)

#性年齢構成、健康分布、肥満比較、健康分布比較