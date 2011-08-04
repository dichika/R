library(ggplot2)
#複数ある健診データと属性データをそれぞれ読み込み、結合する

setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/001_original/KENSHIN")#健診データの作成
dat <- NULL
for(i in 1:length(list.files())){
  fname <- list.files()[i]
  dat0 <- read.csv(fname, as.is=TRUE, skip=1)
  dat <- rbind(dat, dat0)
  }

setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/001_original/HITOBETSU")
hito <- NULL
for(i in 1:length(list.files())){
  fname <- list.files()[i]
  hito0 <- read.csv(fname, as.is=TRUE, skip=2)
  hito <- rbind(hito, hito0)
  }

hito$性別 <- ifelse(hito$性別=="女",2,1)
hito$生年月日 <- as.Date(hito$生年月日, format="%Y/%m/%d")
dat$生年月日 <- as.Date(as.character(dat$生年月日), "%Y%m%d")

hito$記号 <- gsub("=", "", hito$記号)
dat$記号 <- gsub("=", "", dat$記号)

hito$番号 <- as.numeric(gsub("=", "", hito$番号))
dat$番号 <- as.numeric(gsub("=", "", dat$番号))

setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/001_original")
list.files()

data <- merge(dat, hito, by.x=c("記号", "番号", "カナ氏名", "性別", "生年月日"), by.y=c("記号", "番号", "カナ氏名", "性別", "生年月日"))

data <- subset(data, 特定健康診査受診者数=="対象")

data$postcode <- gsub("-", "", data$郵便番号)

#郵便番号から住所を結合

pdata <- read.csv("Q:/Pln/Analysis/KEN_ALLmod.CSV", as.is=TRUE, header=FALSE)
pdata$postcode <- formatC(pdata$V3, flag="0", width=7)
pdata2 <- pdata[, c("V7", "postcode")]
colnames(pdata2) <- c("pref", "postcode")
data2 <- merge(data, pdata2, all.x=TRUE)
data2[is.na(data2$pref),]$郵便番号

setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/001_original")
write.csv(data2, "data.csv", row.names=FALSE)



#############################################################################
#マージしたものを入力
setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/001_original")

data <- read.csv("data.csv", as.is=TRUE)
#data <- data[, c(1:185,188:191) ]#年度末年齢も含めている

#任意継続×本人・家族

hihokensya <- data$本人.家族区分

#検査項目抽出(ヘモグロビンは除いている)
#検査値に関して文字が入っている場合があるので要対応
  sel <- function(x)max(as.numeric(x), na.rm=TRUE)
  height <- as.numeric(data$身長)
  weight <- as.numeric(data$体重)
  waist <- apply(data[, grep("腹囲", colnames(data))], 1, sel)#自己測定も含む
  bmi <- as.numeric(data$BMI)
  sbp <- apply(data[, grep("収縮期血圧", colnames(data))], 1, sel)
  dbp <- apply(data[, grep("拡張期血圧", colnames(data))], 1, sel)
  hdl <- apply(data[, grep("HDL", colnames(data))], 1, sel)
  ldl <- apply(data[, grep("LDL", colnames(data))], 1, sel)
  tg <- apply(data[, grep("中性脂肪", colnames(data))], 1, sel)
  fbg <- apply(data[, grep("空腹時血糖", colnames(data))], 1, sel)
  hba1c <- apply(data[, grep("HbA1c", colnames(data))], 1, sel)
  ggtp <- apply(data[, grep("γ.GT", colnames(data))], 1, sel)
  got <- apply(data[, grep("GOT", colnames(data))], 1, sel)
  gpt <- apply(data[, grep("GPT", colnames(data))], 1, sel)

  #年齢算出（属性データに含まれる年度末年齢を使用することに変更）
  #age <- as.numeric(trunc((as.Date(base, "%Y%m%d") - data$生年月日)/365.25))
  age <- data$年度末年齢
  #5歳刻み年齢フラグ設定
  age5 <- as.numeric(trunc(age/5)*5)

#分析用データ出力関数
output <- function(data, base="20100331"){
  sel <- function(x)max(as.numeric(x), na.rm=TRUE)
  height <- as.numeric(data$身長)
  weight <- as.numeric(data$体重)
  waist <- apply(data[, grep("腹囲", colnames(data))], 1, sel)#自己測定も含む
  bmi <- as.numeric(data$BMI)
  sbp <- apply(data[, grep("収縮期血圧", colnames(data))], 1, sel)
  dbp <- apply(data[, grep("拡張期血圧", colnames(data))], 1, sel)
  hdl <- apply(data[, grep("HDL", colnames(data))], 1, sel)
  ldl <- apply(data[, grep("LDL", colnames(data))], 1, sel)
  tg <- apply(data[, grep("中性脂肪", colnames(data))], 1, sel)
  fbg <- apply(data[, grep("空腹時血糖", colnames(data))], 1, sel)
  hba1c <- apply(data[, grep("HbA1c", colnames(data))], 1, sel)
  ggtp <- apply(data[, grep("γ.GT", colnames(data))], 1, sel)
  got <- apply(data[, grep("GOT", colnames(data))], 1, sel)
  gpt <- apply(data[, grep("GPT", colnames(data))], 1, sel)

  #年齢算出（属性データに含まれる年度末年齢を使用することに変更）
  #age <- as.numeric(trunc((as.Date(base, "%Y%m%d") - data$生年月日)/365.25))
  #age <- data$年度末年齢 #2009年度データにしかないので使わない
  #5歳刻み年齢フラグ設定
  age5 <- as.numeric(trunc(age/5)*5)

  #分析用データ出力
#  outputdata <- data.frame(stringsAsFactors=FALSE, 記号=data$記号, 番号=data$番号, 性別=data$性別, 生年月日=data$生年月日, height, weight, waist, bmi, sbp, dbp, tg, hdl, ldl, got, gpt, ggtp, fbg, hba1c, data$服薬1.血圧., data$服薬2.血糖., data$服薬3.脂質., data$喫煙)
#  invisible(outputdata)
#  }

#outputdata <- output(data)
#head(outputdata)
#setwd("Q:/Pln/Analysis/05_analysis/001_riskprediction/002_work")
#write.csv(outputdata, "sigakudata.csv", row.names=FALSE)

#検査項目リスク設定（保健指導・受診勧奨）
waisth <- ifelse(((waist >= 85) & (data$性別==1))|((waist >= 90) & (data$性別==2)), 1, 0)
bmih <- ifelse(bmi >= 25, 1, 0)
sbph <- ifelse(sbp >= 130,1,0)
sbpj <- ifelse(sbp >= 140,1,0)
dbph <- ifelse(dbp >= 85,1,0)
dbpj <- ifelse(dbp >= 90,1,0)
hdlh <- ifelse(-hdl >= -39,1,0)
hdlj <- ifelse(-hdl >= -34,1,0)
ldlh <- ifelse(ldl >= 130,1,0)
ldlj <- ifelse(ldl >= 140,1,0)
tgh <- ifelse(tg >= 150,1,0)
tgj <- ifelse(tg >= 300,1,0)
fbgh <- ifelse(fbg >= 100,1,0)
fbgj <- ifelse(fbg >= 126,1,0)
hba1ch <- ifelse(hba1c >= 5.2,1,0)
hba1cj <- ifelse(hba1c >= 6.1,1,0)
ggtph <- ifelse(ggtp >= 51,1,0)
ggtpj <- ifelse(ggtp >= 101,1,0)
goth <- ifelse(got >= 31,1,0)
gotj <- ifelse(got >= 51,1,0)
gpth <- ifelse(gpt >= 31,1,0)
gptj <- ifelse(gpt >= 51,1,0)


#階層化リスク設定
kaisoukaBP <- ifelse((sbph + dbph)>=1, 1, 0)
kaisoukaBG <- ifelse((fbgh + hba1ch)>=1, 1, 0)
kaisoukaLP <- ifelse((tgh + hdlh)>=1, 1, 0)
kaisoukaSM <- ifelse(data$喫煙==1,1,0)
kaisoukaOB <- ifelse((waisth + bmih)>0, 2, 1)

#階層化リスク個数設定
kaisoukaCOUNT <- kaisoukaBP + kaisoukaBG + kaisoukaLP + kaisoukaSM
kaisoukaJUSHIN <- sbpj + dbpj + hdlj + ldlj + tgj + fbgj + hba1cj + ggtpj + gptj + gotj
#階層化リスク組合せ設定
kaisoukaKUMIAWASE <- paste(sep="|", ifelse(kaisoukaSM==1, "SM", ""), ifelse(kaisoukaBP==1, "BP", ""), ifelse(kaisoukaBG==1, "BG", ""), ifelse(kaisoukaLP==1, "LP", "")) 

#生活習慣該当フラグ(一部質問については「いいえ」が該当になることに注意)
#最初にNAを除いた上で該当者は1非該当者は0に変換
#問診項目の抽出
monshin <- data[, c(158, 161, 164, 167:185)]
monshinCOLNAME <- colnames(monshin)
monshin <- apply(monshin, 2, function(x)as.numeric(x))#飲酒量はNAでなく空白になっているのでNAに変換
colnames(monshin) <- monshinCOLNAME

#はいが該当者の場合
combart_hai <- function(x)ifelse(x>=2, 0, x)#変換関数
monshin[, -c(10:12, 19:20, 22)] <- apply(monshin[, -c(10:12, 19:20, 22)], 2, combart_hai)

#いいえが該当者、もしくは飲酒量（2以上が飲酒量問題あり）の場合
combart_iie <- function(x)ifelse(x>=2, 1, x-1)#変換関数
monshin[, c(10:12, 19:20, 22)] <- apply(monshin[,c(10:12, 19:20, 22)], 2, combart_iie)

#健康分布フラグ設定

kenkoubunpuRISK <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
if(sum(monshin[i, grep("服薬",colnames(monshin))], na.rm=TRUE)>0){
  kenkoubunpuRISK[i] <- 4
  } else if(kaisoukaJUSHIN[i] > 0){
  kenkoubunpuRISK[i] <- 3
  } else if(kaisoukaCOUNT[i] > 0){
  kenkoubunpuRISK[i] <- 2
  } else {
  kenkoubunpuRISK[i] <- 1}
}
kenkoubunpuSEG <- 10*kaisoukaOB + kenkoubunpuRISK

#######################################################################
#集計

#健康分布
#健康分布割合算出
KENKOUBUNPU <- data.frame(table(hihokensya, kenkoubunpuSEG))
HIHOLEVEL <- ddply(KENKOUBUNPU, .(hihokensya), summarise, Freq=sum(Freq))
result1 <- merge(KENKOUBUNPU, HIHOLEVEL, by.x="hihokensya", by.y="hihokensya")

result1all <- data.frame(hihokensya="全体", table(kenkoubunpuSEG), Freq.y=sum(is.finite(kenkoubunpuSEG)))
colnames(result1all) <- c("hihokensya", "kenkoubunpuSEG", "Freq.x", "Freq.y")
result1 <- rbind(result1, result1all)
result1$prop <- round(result1$Freq.x / result1$Freq.y, 3)

#支援×階層化リスク組合せ
#階層化リスク内訳割合算出

KAISOUKARISK <- data.frame(table(level=data$保健指導レベル, kaisoukaKUMIAWASE))
HOKENLEVEL <- data.frame(table(level=data$保健指導レベル))
result2 <- merge(KAISOUKARISK, HOKENLEVEL, by.x="level", by.y="level")
result2$prop <- round(result2$Freq.x / result2$Freq.y, 3)
KAISOUKACOUNT <- data.frame(table(kaisoukaKUMIAWASE, kaisoukaCOUNT))
KAISOUKACOUNT <- subset(KAISOUKACOUNT, Freq != 0)[,-3]
result2 <- merge(result2, KAISOUKACOUNT, by.x="kaisoukaKUMIAWASE", by.y="kaisoukaKUMIAWASE")

#支援×生活習慣該当
#生活習慣割合算出
sien1 <- colSums(monshin[data$保健指導レベル==1,], na.rm=TRUE)
sien2 <- colSums(monshin[data$保健指導レベル==2,], na.rm=TRUE)
sien3 <- colSums(monshin[data$保健指導レベル==3,], na.rm=TRUE)
sienall1 <- colSums(is.finite(monshin[data$保健指導レベル==1,]), na.rm=TRUE)
sienall2 <- colSums(is.finite(monshin[data$保健指導レベル==2,]), na.rm=TRUE)
sienall3 <- colSums(is.finite(monshin[data$保健指導レベル==3,]), na.rm=TRUE)
result3 <- rbind(round(sien1/sienall1,3), round(sien2/sienall2,3), round(sien3/sienall3,3))

#年齢×腹囲(BMI)データ数×腹囲(BMI)合計(あとで平均を出すため)
#年齢別平均値算出

smp1 <- data.frame(hihokensya, sex=data$性別, age, age5, monshin, waist, bmi, sbp, sbph, sbpj, dbp, dbph, dbpj, hdl, hdlh, hdlj, ldl, ldlh, ldlj, tg, tgh, tgj, fbg, fbgh, fbgj, hba1c, hba1ch, hba1cj, ggtp, ggtph, ggtpj, got, goth, gotj, gpt, gpth, gptj, kaisoukaBP, kaisoukaBG, kaisoukaLP)
smp12 <- smp1[is.finite(smp1$bmi) & is.finite(smp1$waist), ]
res4wst <- ddply(smp12, .(hihokensya, sex, age), summarise, name="WAIST", mean=round(mean(waist, na.rm=TRUE),1), count=sum(is.finite(waist)))
res4bmi <- ddply(smp12, .(hihokensya, sex, age), summarise, name="BMI", mean=round(mean(bmi, na.rm=TRUE),1), count=sum(is.finite(bmi)))
result4 <- rbind(res4wst, res4bmi)

#服薬率×コントロール率算出
#区分×リスク項目×受診勧奨者数×（受診勧奨者かつ服薬）×服薬者数
res5BP <- data.frame(name="BP", table(hihokensya, drug=data$服薬1.血圧., jukan=sbpj))
res5BG <- data.frame(name="BG", table(hihokensya, drug=data$服薬2.血糖., jukan=hba1cj))
res5LP <- data.frame(name="LP", table(hihokensya, drug=data$服薬3.脂質., jukan=ldlj))
result5 <- rbind(res5BP, res5BG, res5LP)

#リスク別内訳算出
#リスク項目別データ数×保健指導レベルデータ数×受診勧奨レベルデータ数(あとで割合出すため)
SBP <- c(sum(is.finite(sbp)), sum(sbph, na.rm=TRUE)-sum(sbpj, na.rm=TRUE),sum(sbpj, na.rm=TRUE))
DBP <- c(sum(is.finite(dbp)), sum(dbph, na.rm=TRUE)-sum(dbpj, na.rm=TRUE),sum(dbpj, na.rm=TRUE))
HDL <- c(sum(is.finite(hdl)), sum(hdlh, na.rm=TRUE)-sum(hdlj, na.rm=TRUE),sum(hdlj, na.rm=TRUE))
LDL <- c(sum(is.finite(ldl)), sum(ldlh, na.rm=TRUE)-sum(ldlj, na.rm=TRUE),sum(ldlj, na.rm=TRUE))
TG <- c(sum(is.finite(tg)), sum(tgh, na.rm=TRUE)-sum(tgj, na.rm=TRUE),sum(tgj, na.rm=TRUE))
FBG <- c(sum(is.finite(fbg)), sum(fbgh, na.rm=TRUE)-sum(fbgj, na.rm=TRUE),sum(fbgj, na.rm=TRUE))
HBA1C <- c(sum(is.finite(hba1c)), sum(hba1ch, na.rm=TRUE)-sum(hba1cj, na.rm=TRUE),sum(hba1cj, na.rm=TRUE))
GGTP <- c(sum(is.finite(ggtp)), sum(ggtph, na.rm=TRUE)-sum(ggtpj, na.rm=TRUE),sum(ggtpj, na.rm=TRUE))
GOT <- c(sum(is.finite(got)), sum(goth, na.rm=TRUE)-sum(gotj, na.rm=TRUE),sum(gotj, na.rm=TRUE))
GPT <- c(sum(is.finite(gpt)), sum(gpth, na.rm=TRUE)-sum(gptj, na.rm=TRUE),sum(gptj, na.rm=TRUE))

result6 <- data.frame(rbind(SBP, DBP, HDL, LDL, TG, FBG, HBA1C, GGTP, GOT, GPT))
colnames(result6) <- c("COUNT", "HOKEN", "JUKAN")
result6$propHOKEN <- round(result6$HOKEN / result6$COUNT, 3)
result6$propJUKAN <- round(result6$JUKAN / result6$COUNT, 3)


#区分性年代別生活習慣内訳算出
#区分×性別×年代×データ数×生活習慣回答
head <- data.frame(table(hihokensya=smp1$hihokensya, sex=smp1$sex, age5=smp1$age5, gaitou=smp1$服薬1.血圧.))[,-5]#ヘッダ用のダミー
result7 <- data.frame(head, apply(smp1[,5:26], 2, function(x)table(smp1$hihokensya, smp1$sex, smp1$age5, x)))
#区分×性別×年代ごとの生活習慣有効回答者数
head2 <- data.frame(addmargins(table(smp1$hihokensya, smp1$sex, smp1$age5, smp1[,5])))[,-5]
result7all <- data.frame(head2, apply(smp1[,5:26], 2, function(x)addmargins(table(smp1$hihokensya, smp1$sex, smp1$age5, x))))
result7all <- subset(result7all, Var1!="Sum" & Var2!="Sum" & Var3!="Sum" & Var4=="Sum")
colnames(result7all) <- c( "hihokensya", "sex", "age5", "gaitou", colnames(result7all)[-c(1:4)])

result7 <- merge(result7, result7all, by.x=c("hihokensya", "sex", "age5"), by.y=c("hihokensya", "sex", "age5") )
result7 <- subset(result7, gaitou.x==1)


#区分性年代別リスク内訳算出
#区分×性別×年代×データ数×リスク内訳()

result8SBP <- ddply(smp1, .(hihokensya, sex, age5), summarise, data=sum(is.finite(sbp)), hoken=sum(sbph, na.rm=TRUE)-sum(sbpj, na.rm=TRUE), jukan=sum(sbpj, na.rm=TRUE), koumoku ="SBP")
result8DBP <- ddply(smp1, .(hihokensya, sex, age5), summarise, data=sum(is.finite(dbp)), hoken=sum(dbph, na.rm=TRUE)-sum(dbpj, na.rm=TRUE), jukan=sum(dbpj, na.rm=TRUE), koumoku ="DBP")
result8HDL <- ddply(smp1, .(hihokensya, sex, age5), summarise, data=sum(is.finite(hdl)), hoken=sum(hdlh, na.rm=TRUE)-sum(hdlj, na.rm=TRUE), jukan=sum(hdlj, na.rm=TRUE), koumoku ="HDL")
result8LDL <- ddply(smp1, .(hihokensya, sex, age5), summarise, data=sum(is.finite(ldl)), hoken=sum(ldlh, na.rm=TRUE)-sum(ldlj, na.rm=TRUE), jukan=sum(ldlj, na.rm=TRUE), koumoku ="LDL")
result8TG <- ddply(smp1, .(hihokensya, sex, age5), summarise, data=sum(is.finite(tg)), hoken=sum(tgh, na.rm=TRUE)-sum(tgj, na.rm=TRUE), jukan=sum(tgj, na.rm=TRUE), koumoku ="TG")
result8FBG <- ddply(smp1, .(hihokensya, sex, age5), summarise, data=sum(is.finite(fbg)), hoken=sum(fbgh, na.rm=TRUE)-sum(fbgj, na.rm=TRUE), jukan=sum(fbgj, na.rm=TRUE), koumoku ="FBG")
result8HBA1C <- ddply(smp1, .(hihokensya, sex, age5), summarise, data=sum(is.finite(hba1c)), hoken=sum(hba1ch, na.rm=TRUE)-sum(hba1cj, na.rm=TRUE), jukan=sum(hba1cj, na.rm=TRUE), koumoku ="HBA1C")
result8GGTP <- ddply(smp1, .(hihokensya, sex, age5), summarise, data=sum(is.finite(ggtp)), hoken=sum(ggtph, na.rm=TRUE)-sum(ggtpj, na.rm=TRUE), jukan=sum(ggtpj, na.rm=TRUE), koumoku ="GGTP")
result8GOT <- ddply(smp1, .(hihokensya, sex, age5), summarise, data=sum(is.finite(got)), hoken=sum(goth, na.rm=TRUE)-sum(gotj, na.rm=TRUE), jukan=sum(gotj, na.rm=TRUE), koumoku ="GOT")
result8GPT <- ddply(smp1, .(hihokensya, sex, age5), summarise, data=sum(is.finite(gpt)), hoken=sum(gpth, na.rm=TRUE)-sum(gptj, na.rm=TRUE), jukan=sum(gptj, na.rm=TRUE), koumoku ="GPT")

addPROP <- function(targ){
targ$hoken.prop <- round(targ$hoken / targ$data, 3)
targ$jukan.prop <- round(targ$jukan / targ$data, 3)
return(targ)}

result8SBP <- addPROP(result8SBP)
result8DBP <- addPROP(result8DBP)
result8HDL <- addPROP(result8HDL)
result8LDL <- addPROP(result8LDL)
result8TG <- addPROP(result8TG)
result8FBG <- addPROP(result8FBG)
result8HBA1C <- addPROP(result8HBA1C)
result8GGTP <- addPROP(result8GGTP)
result8GOT <- addPROP(result8GOT)
result8GPT <- addPROP(result8GPT)

result8 <- rbind(result8SBP, result8DBP, result8HDL, result8LDL, result8TG, result8FBG, result8HBA1C, result8GGTP, result8GOT, result8GPT)


#あとでリファクタリング
#assign()
#name <- "result8SBP"
#name <- gsub("result8", "", name)
#tolower(name)

#区分性年代別階層化リスク内訳算出
#区分×性別×年代×データ数×階層化リスク内訳
result9BP <- ddply(smp1, .(hihokensya, sex, age5), summarise, data=sum(is.finite(kaisoukaBP)), sum=sum(kaisoukaBP, na.rm=TRUE), prop=round(sum(kaisoukaBP, na.rm=TRUE)/sum(is.finite(kaisoukaBP)), 3), koumoku="kaisoukaBP")
result9BG <- ddply(smp1, .(hihokensya, sex, age5), summarise, data=sum(is.finite(kaisoukaBG)), sum=sum(kaisoukaBG, na.rm=TRUE), prop=round(sum(kaisoukaBG, na.rm=TRUE)/sum(is.finite(kaisoukaBG)), 3), koumoku="kaisoukaBG")
result9LP <- ddply(smp1, .(hihokensya, sex, age5), summarise, data=sum(is.finite(kaisoukaLP)), sum=sum(kaisoukaLP, na.rm=TRUE), prop=round(sum(kaisoukaLP, na.rm=TRUE)/sum(is.finite(kaisoukaLP)), 3), koumoku="kaisoukaLP")
result9 <- rbind(result9BP, result9BG, result9LP)

##########################################################
#出力
setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/output")
todaydir <- format(Sys.Date(), "%y%m%d")
dir.create(todaydir)
setwd(paste(sep="", "Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/output/", todaydir))

for(i in 1:9){
obj <- get(paste("result", i, sep=""))
oname <- paste("result", i, ".csv", sep="")
write.csv(obj, oname)
}

#健康分布プロット

plotKENKOUBUNPU <- function(data, kubun){
  d <- subset(data, hihokensya == kubun)
  nob <- sum(subset(d, as.numeric(kenkoubunpuSEG)<5)$prop)
  ob <- sum(subset(d, as.numeric(kenkoubunpuSEG)>=5)$prop)
  da <- c(nob, ob)#非肥満、肥満割合列
  df <- round(c(nob, ob, d$prop[4:1]/nob, d$prop[8:5]/ob),3)
  a  <- c(df[1], df[2])#肥満・非肥満を棒グラフの太さに設定する
  d  <- matrix(df[3:length(df)],nrow=4)#各セグメントを行列化
  name <- kubun
  f  <- paste(name,'.emf',sep='')#ファイルを用意、名前は1列目を使用
  win.metafile(f)
  barplot(d, beside=F, space=0, width=a, axes=FALSE)
  dev.off()
  }

for(kubun in c("全体", "本人", "家族")){
  plotKENKOUBUNPU(result1, kubun)
  }


#######################################################################################
##保健指導データの評価
#


#保健指導データと2009年度との結合
setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/001_original/HOKENSHIDOU")
hoken <- read.csv("HOKENSHIDOU110509.csv", as.is =TRUE)#HCCのデータのみ
head(data)
nrow(hoken)#1145
hoken$生年月日 <- as.Date(as.character(hoken$生年月日), "%Y/%m/%d")

#2009年度健診データ(outputdata)の加工
head(outputdata$記号)
head(記号)
outputdata$記号 <- gsub(perl=TRUE, "=", "", outputdata$記号)
outputdata$番号 <- as.numeric(gsub(perl=TRUE, "=", "", outputdata$番号))

hoken$番号 <- as.numeric(hoken$番号)
hoken$性別 <- ifelse(hoken$性別=="男性",1,2)
hoken <- merge(hoken, outputdata, all.x=TRUE, by.x=c("記号", "番号", "性別", "生年月日"), by.y=c("記号", "番号", "性別", "生年月日"))

#2008年度データとの結合
setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2010/original/UC1611_130021_8/130021/KENSHIN")

data2008 <- NULL
for(i in 1:length(list.files())){
  fname <- list.files()[i]
  data20080 <- read.csv(fname, as.is=TRUE, header=TRUE)
  data2008 <- rbind(data2008, data20080)
  }
colnames(data2008)
outputdata2008 <- output(data2008, base="20090331")
outputdata2008$生年月日 <- as.Date(as.character(outputdata2008$生年月日), "%Y%m%d")
outputdata2008$記号 <- gsub(perl=TRUE, "=", "", outputdata2008$記号)
outputdata2008$番号 <- as.numeric(gsub(perl=TRUE, "=", "", outputdata2008$番号))

hoken2 <- merge(hoken, outputdata2008, all.x=TRUE, by.x=c("記号", "番号", "性別", "生年月日"), by.y=c("記号", "番号", "性別", "生年月日"))
setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/001_original/HOKENSHIDOU")
write.csv(hoken2, "保健指導データ110526.csv")

##########################################################################################
##保健指導データ
##データ作成
#健診データ入力
setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/001_original")

data <- read.csv("data.csv", as.is=TRUE)
data$生年月日 <- as.Date(data$生年月日, "%Y-%m-%d")


#保健指導データ入力

setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/001_original/HOKENSHIDOU")
hoken <- read.csv(list.files(), as.is =TRUE)

for(i in 1:nrow(hoken)){
  if(hoken$保健指導実施機関名称[i]=="日本私立学校振興・共済事業団東京臨海病院"){
    hoken$kikan[i] <- 1
    }else if(hoken$保健指導実施機関名称[i]=="株式会社全国訪問健康指導協会"){
    hoken$kikan[i] <- 2
    }else{  hoken$kikan[i] <- 3}
    }

#臨海病院と訪問指導協会は除く
hoken <- subset(hoken, kikan==3)
nrow(hoken)

hoken$生年月日 <- as.Date(as.character(hoken$生年月日), "%Y%m%d")
hoken$番号 <- as.numeric(hoken$番号)
hoken2 <- merge(data, hoken, by.x=c("記号", "番号", "性別", "生年月日"), by.y=c("記号", "番号", "性別", "生年月日"))
write.csv(hoken2, "HOKENSHIDOUsonota.csv", row.names=FALSE)#一人重複が生じているので手作業で削除する（ヨシエノリオ）


############################################################################
#都道府県コードは今後使う
hoken2$postcode <- gsub("-", "", hoken2$郵便番号.y)

pdata <- read.csv("Q:/Pln/Analysis/KEN_ALL.CSV", as.is=TRUE, header=FALSE)
pdata$postcode <- formatC(pdata$V3, flag="0", width=7)

hoken3 <- merge(hoken2, pdata, all.x=TRUE)
ddply(hoken3, .(支援レベル, V7), summarise, mean_w_be = mean(体重, na.rm=TRUE), mean_w_af = mean(X6か月後の評価時の体重, na.rm=TRUE), count_w_be = sum(!is.na(体重)), count_w_af = sum(!is.na(X6か月後の評価時の体重)))
hoken3[is.na(hoken3$V7),]
write.csv(hoken3, "hokensidou.csv", row.names=FALSE)

################################################################

#分析にはこちらのコードを用いる

setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/001_original/HOKENSHIDOU")
hoken2 <- read.csv("HOKENSHIDOU110509.csv", as.is = TRUE)
hoken2$生年月日 <- as.Date(as.character(hoken2$生年月日), format="%Y/%m/%d")
hoken2$age <- as.numeric(trunc((as.Date("20100401", "%Y%m%d") - hoken2$生年月日)/365.25))
hoken2$age <- ifelse(hoken2$age >= 70, 70, hoken2$age)
hoken2$age10 <- ifelse(hoken2$age >= 40,  trunc(hoken2$age/10)*10, 40)

table(hoken2$age10, useNA="ifany")
library(ggplot2)

####保健指導参加者数
table(hoken2$支援レベル, useNA="ifany")

#支援レベル×機関別平均年齢
ddply(hoken3, .(支援レベル, kikan), summarise, mean_age = mean(age, na.rm=TRUE), count_w_af = sum(!is.na(wt_saishuu)))
#支援レベル×男性人数
ddply(hoken3, .(支援レベル, kikan), summarise, male_count = sum(性別=="男性"), count_w_af = sum(!is.na(wt_saishuu)))
 

####体重評価
hoken3 <- subset(hoken2, wt_shokai > 0 & wt_saishuu > 0)
#全体体重
ddply(hoken3, .(支援レベル), summarise, mean_w_be = mean(wt_shokai, na.rm=TRUE), mean_w_af = mean(wt_saishuu, na.rm=TRUE), count_w_be = sum(!is.na(wt_shokai)), count_w_af = sum(!is.na(wt_saishuu)))
#保健指導機関別体重
ddply(hoken3, .(支援レベル, kikan), summarise, mean_w_be = mean(wt_shokai, na.rm=TRUE), mean_w_af = mean(wt_saishuu, na.rm=TRUE), count_w_be = sum(!is.na(wt_shokai)), count_w_af = sum(!is.na(wt_saishuu)))
#性年代別体重
ddply(hoken3, .(支援レベル, 性別, age10), summarise, mean_w_be = mean(wt_shokai, na.rm=TRUE), mean_w_af = mean(wt_saishuu,, na.rm=TRUE), count_w_be = sum(!is.na(wt_shokai)), count_w_af = sum(!is.na(wt_saishuu)))
#地域別体重
#ddply(hoken3, .(支援レベル, V7), summarise, mean_w_be = mean(wt_shokai, na.rm=TRUE), mean_w_af = mean(wt_saishuu,, na.rm=TRUE), count_w_be = sum(!is.na(wt_shokai)), count_w_af = sum(!is.na(wt_saishuu)))



####腹囲評価
hoken4 <- subset(hoken2, fukui_shokai > 0 & fukui_saishuu > 0)
#全体腹囲
ddply(hoken4, .(支援レベル), summarise, mean_f_be = mean(fukui_shokai, na.rm=TRUE), mean_f_af = mean(fukui_saishuu, na.rm=TRUE), count_f_be = sum(!is.na(fukui_shokai)), count_f_af = sum(!is.na(fukui_saishuu)))
#保健指導機関別腹囲
ddply(hoken4, .(支援レベル, kikan), summarise, mean_f_be = mean(fukui_shokai, na.rm=TRUE), mean_f_af = mean(fukui_saishuu, na.rm=TRUE), count_f_be = sum(!is.na(fukui_shokai)), count_f_af = sum(!is.na(fukui_saishuu)))
#性年代別腹囲
ddply(hoken4, .(支援レベル, 性別, age10), summarise, mean_f_be = mean(fukui_shokai, na.rm=TRUE), mean_f_af = mean(fukui_saishuu,, na.rm=TRUE), count_f_be = sum(!is.na(fukui_shokai)), count_f_af = sum(!is.na(fukui_saishuu)))
#地域別腹囲
#ddply(hoken4, .(支援レベル, V7), summarise, mean_f_be = mean(fuku_shokai, na.rm=TRUE), mean_f_af = mean(fukui_saishuu,, na.rm=TRUE), count_f_be = sum(!is.na(fukui_shokai)), count_f_af = sum(!is.na(fukui_saishuu)))
