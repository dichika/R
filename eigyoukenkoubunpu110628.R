library(ggplot2)
library(batade)

###営業用健康分布は定義を確認###
#このコードは階層化項目のみを用いた定義となっている

setwd("D:/downloads")

data <- read.csv("002_営業用平均値算出110628.csv", as.is=TRUE)
date <- as.Date(data$健診年月日, format="%Y-%m-%d")

#数値変換用関数の定義
sel <- function(x)max(as.numeric(x), na.rm=TRUE)

waist <- as.numeric(data$腹囲.実測.)
bmi <- as.numeric(data$BMI)
sbp <- apply(data[, grep("収縮期血圧", colnames(data))], 1, sel)
dbp <- apply(data[, grep("拡張期血圧", colnames(data))], 1, sel)
hdl <- apply(data[, grep("ＨＤＬ", colnames(data))], 1, sel)
#ldl <- apply(data[, grep("LDL", colnames(data))], 1, sel)
tg <- apply(data[, grep("中性脂肪", colnames(data))], 1, sel)
fbg <- apply(data[, grep("空腹時血糖", colnames(data))], 1, sel)
hba1c <- apply(data[, grep("ＨｂＡ1ｃ", colnames(data))], 1, sel)
#ggtp <- apply(data[, grep("γ.GT", colnames(data))], 1, sel)
#got <- apply(data[, grep("GOT", colnames(data))], 1, sel)
#gpt <- apply(data[, grep("GPT", colnames(data))], 1, sel)

drugBP <- ifelse(data$服薬1.血圧==1, 1,0)
drugBG <- ifelse(data$服薬2.血糖==1, 1,0)
drugLP <- ifelse(data$服薬3.脂質==1, 1,0)

SMOKE <- ifelse(data$喫煙==1, 1, 0)

#年齢算出（属性データに含まれる年度末年齢を使用することに変更）
#age <- as.numeric(trunc((as.Date(base, "%Y%m%d") - data$生年月日)/365.25))
age <- data$年齢
#5歳刻み年齢フラグ設定
age5 <- as.numeric(trunc(age/5)*5)
sex <- ifelse(data$性別=="男", 1, 2)
df <- data.frame(sex, age, age5, waist, bmi, sbp, dbp, hdl, tg, fbg, hba1c, drugBP, drugBG, drugLP,SMOKE)
df <- df[is.finite(rowSums(df)),]
df$drug <- ifelse((df$drugBP + df$drugBG + df$drugLP)>=1, 1, 0)

#検査項目リスク設定（保健指導・受診勧奨）
waisth <- ifelse(((df$waist >= 85) & (df$sex==1))|((df$waist >= 90) & (df$sex==2)), 1, 0)
bmih <- ifelse(df$bmi >= 25, 1, 0)
sbph <- ifelse(df$sbp >= 130,1,0)
sbpj <- ifelse(df$sbp >= 140,1,0)
dbph <- ifelse(df$dbp >= 85,1,0)
dbpj <- ifelse(df$dbp >= 90,1,0)
hdlh <- ifelse(-df$hdl >= -39,1,0)
hdlj <- ifelse(-df$hdl >= -34,1,0)
#ldlh <- ifelse(df$ldl >= 130,1,0)
#ldlj <- ifelse(df$ldl >= 140,1,0)
tgh <- ifelse(df$tg >= 150,1,0)
tgj <- ifelse(df$tg >= 300,1,0)
fbgh <- ifelse(df$fbg >= 100,1,0)
fbgj <- ifelse(df$fbg >= 126,1,0)
hba1ch <- ifelse(df$hba1c >= 5.2,1,0)
hba1cj <- ifelse(df$hba1c >= 6.1,1,0)
#ggtph <- ifelse(df$ggtp >= 51,1,0)
#ggtpj <- ifelse(df$ggtp >= 101,1,0)
#goth <- ifelse(df$got >= 31,1,0)
#gotj <- ifelse(df$got >= 51,1,0)
#gpth <- ifelse(df$gpt >= 31,1,0)
#gptj <- ifelse(df$gpt >= 51,1,0)


#階層化リスク設定
kaisoukaBP <- ifelse((sbph + dbph)>=1, 1, 0)
kaisoukaBG <- ifelse(is.na(hba1ch), fbgh, hba1ch)
kaisoukaLP <- ifelse((tgh + hdlh)>=1, 1, 0)
kaisoukaSM <- ifelse(df$SMOKE==1,1,0)
kaisoukaOB <- ifelse((waisth + bmih)>0, 2, 1)


#階層化リスク個数設定
kaisoukaCOUNT <- kaisoukaBP + kaisoukaBG + kaisoukaLP + kaisoukaSM
kaisoukaJUSHIN <- ifelse(is.na(fbgj), sbpj + dbpj + hdlj + tgj  + hba1cj, 
                         sbpj + dbpj + hdlj + tgj + fbgj)

#階層化リスク組合せ設定
kaisoukaKUMIAWASE <- paste(sep="|", ifelse(kaisoukaSM==1, "SM", ""), ifelse(kaisoukaBP==1, "BP", ""), ifelse(kaisoukaBG==1, "BG", ""), ifelse(kaisoukaLP==1, "LP", "")) 


#健康分布フラグ設定

kenkoubunpuRISK <- rep(NA, nrow(df))
for(i in 1:nrow(df)){
  if(sum(df[i, grep("drug",colnames(df))], na.rm=TRUE)>0){
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
#健康分布
#健康分布割合算出

KENKOUBUNPU <- data.frame(table(df$hname, kaisoukaOB, kenkoubunpuRISK))
KBALL <- ddply(KENKOUBUNPU, .(Var1, kaisoukaOB), summarise, Freq=sum(Freq))
result1 <- merge(KENKOUBUNPU, KBALL, by.x=c("Var1","kaisoukaOB"), by.y=c("Var1","kaisoukaOB"))
colnames(result1)[1] <- "hname"

all <- data.frame(hname="全体", table(kaisoukaOB, kenkoubunpuRISK))
result1oball <- ddply(all, .(hname, kaisoukaOB), transform, Freq.sum=sum(Freq))
colnames(result1oball)[4:5] <- c("Freq.x", "Freq.y")
result1 <- rbind(result1, result1oball)
result1$prop <- round(result1$Freq.x*100 / result1$Freq.y, 1)
#str(result1$kenkoubunpuRISK) <- as.numeric(as.character(result1$kenkoubunpuRISK))

p1 <- ggplot(data=result1, aes(hname))+theme_bw()+
  geom_bar(subset=.(kaisoukaOB==2), aes(y=prop, fill=kenkoubunpuRISK), stat="identity")+
  geom_bar(subset=.(kaisoukaOB==1), aes(y=-prop, fill=kenkoubunpuRISK), stat="identity")+
  geom_hline(yintercept = 0, colour = "grey90") + opts(axis.text.x=theme_text(angle=90)) +coord_flip() +
  xlab("") + ylab("各セグメントの割合")

kd <- data.frame(table(kenkoubunpuSEG))
kd$prop <- kd$Freq / sum(kd$Freq)

plotKENKOUBUNPU <- function(data, kubun=NULL){
  d <- data #subset(data, hihokensya == kubun)
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

plotKENKOUBUNPU(kd)
write.csv(kd, "output.csv", row.names=FALSE)

#レポート作成
setwd("Q:/Pln/Analysis/01_hokensya/04_suishin/2011/02_output")
#画像の生成
png("健康分布.png", width=800, height=600)
p1
dev.off()

png("肥満.png", width=800, height=600)
p2
dev.off()


#レイアウト指定
mat <- rbind(c("title","LL"),c("title","L"),c("健康分布.png","L"),c("ここにメッセージを入力します","S"))

#レポート生成
mkhtml("sample.html", mat, foot=TRUE)
