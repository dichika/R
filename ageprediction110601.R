#######################################################################
##重回帰を用いた年齢予測110203

#110302性別モデルに変更
#110322QUPiOデータを用いた仕様に変更
#110323性別モデルから年齢層別モデルに変更


###モデル作成前処理
library(ggplot2)
library(sampling)
library(mvtnorm)
library(robustbase)

set.seed(1)

dir <- "Q:/Pln/Analysis/05_analysis/001_riskprediction/002_work"
setwd(dir)

#クリーニング用コード読み込み
#source("C:/Users/h1030/Documents/Revolution/Project0/qcleaning.R")

#クリーニング済データ
tdc <- read.csv("qdata.csv")

##本日付フォルダに保存場所変更
cdir <- paste(Sys.Date(), sep="")
dir.create(cdir)
place <- paste(dir, cdir, sep="/")
setwd(place)

#内臓脂肪データ
#身体測定データを使う
#血圧データの平均値を使う

setwd(dir)
dvfa <- read.csv("vfa.csv", as.is=TRUE)
dvfa <- subset(dvfa, drug1==2 & drug2==2 & drug3==2 & vfa>0)
#dvfa$BP<- ifelse(dvfa$sbp>0 & dvfa$dbp>0, (dvfa$sbp + dvfa$dbp)/2, NA) 
#dvfa$height <- NULL
#dvfa$waist <- NULL
#dvfa$weight <- NULL
dvfa$hb <- NULL
#dvfa$bmi <- NULL
#dvfa$sbp <- NULL
#dvfa$dbp <- NULL

nrow(dvfa)
colnames(dvfa)
dvfa <- dvfa[,-c(grep("drug|smoke", colnames(dvfa)))]
dvfa$tg <- log(dvfa$tg)
dvfa$gpt <- log(dvfa$gpt)
dvfa$got <- log(dvfa$got)
dvfa$ggtp <- log(dvfa$ggtp)

pred <- lmrob(data=dvfa, vfa~.)
pred
cor(pred$fitted.values, pred$model$vfa)

qdata <- read.csv("qdatanew.csv", as.is = TRUE)
head(qdata)

#年齢層別・性別に多変量正規分布から乱数を発生、中性脂肪および肝機能は対数変換
#BMIは係数が負になってしまうので、推定した内臓脂肪で置換する
#血圧は収縮期血圧と拡張期血圧を平均する
#テストデータには身体測定データも含めた私学データを使用(110421)
#テストデータは40歳未満のデータも含むクピオデータに変更

sigaku <- read.csv("qdata110422.csv", as.is=TRUE)
#colnames(sigaku) <- c("sex", "age", "height", "weight", "waist", "bmi", "sbp", "dbp", "tg", "hdl", "ldl", "got", "gpt", "ggtp", "fbg", "hba1c", "drugBP", "drugBG", "drugLP", "smoking")
sigaku$sex <- ifelse( sigaku$sex== "男", 1, 2 )
nrow(sigaku[is.finite(rowSums(sigaku)), ])#89000件
sigaku <- subset(sigaku, drugBP ==2 & drugBG ==2 & drugLP ==2)
sigaku <- sigaku[is.finite(rowSums(sigaku)), -c(grep("drug|smok", colnames(sigaku)))]

sigaku$BP<- ifelse(sigaku$sbp>0 & sigaku$dbp>0, (sigaku$sbp + sigaku$dbp)/2, NA)
#sigaku$BP <- NULL
#sigaku$vfa <- predict(newdata=sigaku, pred)

#sigaku$gpt <- log(sigaku$gpt)
sigaku$tg <- log(sigaku$tg)
sigaku$got <- log(sigaku$got)
sigaku$ggtp <- log(sigaku$ggtp)

sigaku$height <- NULL
sigaku$weight<- NULL
sigaku$bmi <- NULL
#sigaku$hb <- NULL
sigaku$hba1c <- NULL
sigaku$gpt <- NULL

sigaku$sbp <- NULL
sigaku$dbp <- NULL
sigaku$vfa <- NULL

lm1 <- lm(data=sigaku, age~.)
summary(lm1)

lm2 <- lm(data=sigaku[sigaku$age >= 40,], age~.)
summary(lm2)

lm3 <- lmrob(data=sigaku[sigaku$age < 40,], age~.)
summary(lm3)


#35歳以下の分散共分散行列で多変量正規分布を作成
rand_as <-  function(sex=0, n=1000, age=35, data=sigaku){
    part <- subset(data, age <= age & sex==sex)
    part$tg <- log(part$tg)
	part$gpt <- log(part$gpt)
	part$got <- log(part$got)
	part$ggtp <- log(part$ggtp)
	sigma <- cov(part[-1])
    mu <- mean(part[-1])
    data_smp <- data.frame(rmvnorm(n=n, mean=mu, sigma=sigma))
    data_smp$sex <- sex
    invisible(data_smp)
    }


model_data <- rbind(rand_as(sex=0), rand_as(sex=1))
model_data$BP<- ifelse(model_data$sbp>0 & model_data$dbp>0, (model_data$sbp + model_data$dbp)/2, NA)
model_data$height <- NULL
model_data$weight <- NULL

model_data$bmi <- NULL
model_data$gpt <- NULL
model_data$sbp <- NULL
model_data$dbp <- NULL
young_lm <- lm(data=model_data, age~.)
summary(young_lm)
young_lm2 <- lmrob(data=model_data, age~.)
summary(young_lm2)

#サンプリング(cube法を使用、性年齢比を反映する形で調整、10分の1をテストデータとしてサンプリング)

X <- cbind(tdc$sex, tdc$age)#注：samplecube関数はvectorでしか受け付けない
p <- rep(0.1, nrow(tdc))
sample <- samplecube(X, p,1,FALSE)
dat <- tdc[sample==0, ]
dat2 <- tdc[sample==1, ]

####################################################################################
#モデルを作成
#データ傾向を確認する（プロット）
dat.m <- melt(dat, id.vars=c("sex", "age"), direction="long")
dat.sum <- ddply(dat.m, .(sex, age, variable), summarise, mean=mean(value))
p <- ggplot(data=dat.sum, aes(x=age, y=mean, group=factor(sex))) + geom_line(aes(colour=factor(sex))) + geom_point(aes(colour=factor(sex)), size=2) + geom_point(colour="grey90", size=1) + facet_wrap(~variable, scales="free_y")

pdf("sex-age-mean.pdf", family="Japan1GothicBBB", paper="a4r",width=9.5,height=7)
print(p)
dev.off()


#肝機能には対数変換行う
dat[,9:11] <- log(dat[,9:11])
dat2[,9:11] <- log(dat2[,9:11])

#Inf等を除く→is.finite判定は各列に対して行う必要があるのでrowSumsの方がよさそう
dat <- dat[rowSums(dat)>0,]
dat2 <- dat2[rowSums(dat2)>0,]



#重回帰
#sdat <- dat[, -c(3, 6, 10, 13)]#線形関係にないbmi, tg, gpt, hbをモデルから抜いてみる
agepredict <- glm(data=dat, age~.)
#agepredicts <- glm(data=sdat, age~.)

##標準化偏回帰係数の出力
#標準偏差ベクトルの作成
sdd <- c(0, apply(dat[,-2], 2, sd))
std_coef <- coef(agepredict)*sdd/sd(dat$age)

#sdd2 <- c(0, apply(sdat[,-2], 2, sd))
#std_coef2 <- coef(agepredicts)*sdd/sd(sdat$age)

#どの検査項目から改善させるかに関してはインパクト（対数変換×標準化後の重み）×減らしやすさ（分布からの外れ具合）で算出


#予測式ログ
sink(paste(sep="", "年齢予測モデル詳細", format(Sys.Date(), "%y%m%d"), ".txt"), append=TRUE)
print(agepredict)#予測式をプリント
sink()

#テストデータによる予測

#年齢が5歳以上上の人と-5~0、0~5歳の人、5歳以上下回る人の4段階で傾向(箱ヒゲ)を確認する
for( s in 1:2){
	dat2t <- dat2[dat2$sex == s, ]
	
	dat2t$agep <- predict(get(paste("agepredict", s+2, sep="")), newdata=dat2t)#この場合交互作用は含まない
	dat2t$agediff <- dat2t$agep-dat2t$age
	dat2t$agedl<- cut(dat2t$agediff, breaks=c(-Inf,-5,0,5,Inf), labels=c("-5未満","-5以上0未満","0以上5未満","5以上"))
	dat2t$ages <- trunc(dat2t$age/10)*10
	assign(paste("table", s, sep="_"), table(dat2t$ages))
	for(ag in c(40,50,60)){
		for(obs in 1:16){
		dat2ts <- subset(dat2t, ages==ag)[,c(obs,17)]
		colnames(dat2ts) <- c("obs","agedl")
		p <- ggplot(dat2ts, aes(x=agedl,y=obs)) + geom_boxplot() + opts(title =paste(ifelse(s==1, "男性交互作用なし", "女性交互作用なし"),ag,"_", colnames(df[obs])))
		pdf(file=paste(ifelse(s==1, "男性交互作用なし", "女性交互作用なし"), ag, "_", colnames(df[obs]),".pdf",sep=""), family="Japan1GothicBBB",paper="a4r",width=9.5,height=7)
		print(p)
		dev.off()
		}
	}
}






#####################################################################

#3年目における受診勧奨リスクフラグ
#3年目における保健指導リスクフラグ
tdc$hdl3 <- -tdc$hdl3#処理を統一するために一時的にhdlを負値に変換

list <- c("sbpj", "dbpj", "tgj", "hdlj", "fbgj", "hba1cj", "sbpr", "dbpr", "tgr", "hdlr", "fbgr", "hba1cr")
list2 <- c("sbp3", "dbp3", "tg3", "hdl3", "fbg3", "hba1c3", "sbp3", "dbp3", "tg3", "hdl3", "fbg3", "hba1c3")
list3 <- c("140", "90", "300", "-34", "126", "6.1","130", "85", "150", "-39", "100", "5.2")

txt <- paste(sep="", "tdc$", list, " <- as.numeric(cut(tdc$", list2, ", right=FALSE, breaks=c(-Inf, ", list3,", Inf), labels=c(0,1)))-1")
eval(parse(text=txt))


######################################################################
#QUPiOデータクリーニング

dir <- "Q:/Pln/Analysis/05_analysis/001_riskprediction/002_work"
setwd(dir)
qdata <- read.csv("qdatanew.csv", as.is = TRUE)

sel <- function(x)max(as.numeric(x), na.rm=TRUE)

height <- as.numeric(qdata$身長)
weight <- as.numeric(qdata$体重)
waist <- as.numeric(qdata$腹囲.実測.)
bmi <- as.numeric(qdata$BMI)
sbp <- apply(qdata[, grep("収縮期血圧", colnames(qdata))], 1, sel)#検査項目・生活習慣項目抽出
dbp <- apply(qdata[, grep("拡張期血圧", colnames(qdata))], 1, sel)
hdl <- apply(qdata[, grep("HDL|ＨＤＬ", colnames(qdata))], 1, sel)
ldl <- apply(qdata[, grep("LDL|ＬＤＬ", colnames(qdata))], 1, sel)
tg <- apply(qdata[, grep("中性脂肪", colnames(qdata))], 1, sel)
fbg <- apply(qdata[, grep("空腹時血糖", colnames(qdata))], 1, sel)
#hba1c <- apply(qdata[, grep("HbA1c", colnames(qdata))], 1, sel)
ggtp <- apply(qdata[, grep("γ.GT", colnames(qdata))], 1, sel)
got <- apply(qdata[, grep("GOT", colnames(qdata))], 1, sel)
gpt <- apply(qdata[, grep("GPT", colnames(qdata))], 1, sel)
res <- data.frame(sex=qdata$性別, age=qdata$年齢, height, weight, waist, bmi, sbp, dbp, tg, hdl, ldl, got, gpt, ggtp, fbg, hba1c, drugBP=qdata$服薬1.血圧., drugBG=qdata$服薬2.血糖., drugLP=qdata$服薬3.脂質.,smoke=qdata$喫煙)
res$hba1c <- NULL

#データ出力
#setwd("Q:/Pln/Analysis/05_analysis/001_riskprediction/002_work")
write.csv(res, "qdata110422.csv", row.names=FALSE)