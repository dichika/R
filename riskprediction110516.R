#このコードが最新（110527）
#保健指導値になるリスクを予測および閾値の設定
#HbA1cと空腹時血糖値のそれぞれを用いたモデルを作る
#腹囲を補完する→別に単年データから作成する
#年齢予測についてはBMI等の重複するパラメータを除く

today <- format(Sys.Date(), "%y%m%d")

###モデル作成前処理
library(ggplot2)
library(sampling)
set.seed(1)
dir <- "Q:/Pln/Analysis/05_analysis/001_riskprediction/002_work"
setwd(dir)

#data reading
testdata <- read.csv("testdata.csv", header=T)

#保存場所変更
cdir <- paste(Sys.Date(), sep="")
dir.create(cdir)
place <- paste(dir, cdir, sep="/")
setwd(place)

cdir2 <- paste("riskp", sep="")
dir.create(cdir2)
place <- paste(place, cdir2, sep="/")
setwd(place)

#予測段階で未服薬の人間を抽出
tdc <- subset(testdata, complete.cases(testdata))
tdc <- subset(tdc, tdc$med==0 & tdc$med2==0)


#3年目における受診勧奨リスクフラグ
tdc$sbpj <- as.numeric(cut(tdc$sbp3, right=FALSE, breaks=c(-Inf, 140, Inf), labels=c(0,1)))-1
tdc$dbpj <- as.numeric(cut(tdc$dbp3, right=FALSE, breaks=c(-Inf, 90, Inf), labels=c(0,1)))-1
tdc$tgj <- as.numeric(cut(tdc$tg3, right=FALSE, breaks=c(-Inf, 300, Inf), labels=c(0,1)))-1
tdc$hdlj <- (as.numeric(cut(tdc$hdl3, right=FALSE, breaks=c(-Inf, 34, Inf), labels=c(1,0)))-2)*(-1)
tdc$fbgj <- as.numeric(cut(tdc$fbg3, right=FALSE, breaks=c(-Inf, 126, Inf), labels=c(0,1)))-1
tdc$hba1cj <-as.numeric( cut(tdc$hba1c3, right=FALSE, breaks=c(-Inf, 6.1, Inf), labels=c(0,1)))-1

#3年目における保健指導リスクフラグ
tdc$sbpr <- as.numeric(cut(tdc$sbp3, right=FALSE, breaks=c(-Inf, 130, Inf), labels=c(0,1)))-1
tdc$dbpr <- as.numeric(cut(tdc$dbp3, right=FALSE, breaks=c(-Inf, 85, Inf), labels=c(0,1)))-1
tdc$tgr <- as.numeric(cut(tdc$tg3, right=FALSE, breaks=c(-Inf, 150, Inf), labels=c(0,1)))-1
tdc$hdlr <- (as.numeric(cut(tdc$hdl3, right=FALSE, breaks=c(-Inf, 39, Inf), labels=c(1,0)))-2)*(-1)
tdc$fbgr <- as.numeric(cut(tdc$fbg3, right=FALSE, breaks=c(-Inf, 100, Inf), labels=c(0,1)))-1
tdc$hba1cr <-as.numeric( cut(tdc$hba1c3, right=FALSE, breaks=c(-Inf, 5.2, Inf), labels=c(0,1)))-1

#サンプリング(cube法を使用、性年齢比を反映する形で調整、10分の1をサンプリング)
#保健指導リスクの両方の割合が同等になるように
X <- cbind(tdc$sex, tdc$age, tdc$sbpr, tdc$dbpr, tdc$tgr, tdc$hdlr, tdc$fbgr, tdc$hba1cr)#samplecube関数はvectorでしか受け付けない
p <- rep(0.1, nrow(tdc))
sample <- samplecube(X, p,1,FALSE)
df <- tdc[sample==0, ]
df2 <- tdc[sample==1, ]

#各データ状況サマリー
sink("モデルおよびテストデータ処理前状況.txt")
str(df)
summary(df)
str(df2)
summary(df2)
sink()

#性別年齢構成グラフ
pdf(file="性年齢構成.pdf", family="Japan1GothicBBB",paper="a4r",width=9.5,height=7)
ggplot(data=df,aes(x=age))+geom_histogram(colour = "darkgreen", fill = "grey90",binwidth=1)+facet_wrap(~sex) + opts(title="modeldata")
ggplot(data=df2,aes(x=age))+geom_histogram(colour = "darkgreen", fill = "grey90",binwidth=1)+facet_wrap(~sex) + opts(title="testdata")
dev.off()

#各項目分布状況（各検査項目ごとにboxplot）
for(parameter in 4:14){ 
	df_b1 <- df[,c(2,parameter)]
	colnames(df_b1) <- c("sex","koumoku")
	df_b1$year <- 1
	df_b2 <- df[,c(2,parameter+15)]
	colnames(df_b2) <- c("sex","koumoku")
	df_b2$year <- 2
	df_b3 <- df[,c(2,parameter+30)]
	colnames(df_b3) <- c("sex","koumoku")
	df_b3$year <- 3
	df_b <- rbind(df_b1,df_b2,df_b3)
	plot_koumoku <- ggplot(data=df_b, aes(x=as.factor(df_b$year), y=koumoku)) + geom_boxplot() + facet_wrap(~sex) + xlab("year") +ylab(colnames(df[parameter]))
	name_koumoku <- paste(colnames(df[parameter]), ".pdf", sep="")
	pdf(file=name_koumoku, family="Japan1GothicBBB",paper="a4r",width=9.5,height=7)
	print(plot_koumoku)
	dev.off()
	}

#モデルデータのみ外れ値処理（上下位1%を抽出、最小値で置換）
for(out in c(3:14,18:29,33:44)){
	ord <- round(length(df[,out])/100)
	mn <- max(head(df[order(df[,out], decreasing=FALSE),][,out],n=ord))
	mx <- min(head(df[order(df[,out], decreasing=TRUE),][,out],n=ord))
	df[,out] <- ifelse(df[,out]>mx, mx, df[,out])
	df[,out] <- ifelse(df[,out]<mn, mn, df[,out])
	}

#外れ値処理後各項目分布状況（各検査項目ごとにboxplot）
for(parameter in 4:14){ 
	df_b1 <- df[,c(2,parameter)]
	colnames(df_b1) <- c("sex","koumoku")
	df_b1$year <- 1
	df_b2 <- df[,c(2,parameter+15)]
	colnames(df_b2) <- c("sex","koumoku")
	df_b2$year <- 2
	df_b3 <- df[,c(2,parameter+30)]
	colnames(df_b3) <- c("sex","koumoku")
	df_b3$year <- 3
	df_b <- rbind(df_b1,df_b2,df_b3)
	plot_koumoku <- ggplot(data=df_b, aes(x=as.factor(df_b$year), y=koumoku)) + geom_boxplot() + facet_wrap(~sex) + xlab("year") +ylab(colnames(df[parameter]))
	name_koumoku <- paste(colnames(df[parameter]), "処理後.pdf", sep="")
	pdf(file=name_koumoku, family="Japan1GothicBBB",paper="a4r",width=9.5,height=7)
	print(plot_koumoku)
	dev.off()
	}
sink("モデルデータ処理後状況.txt")
summary(df)
sink()

#########################################################################################
#リスク予測
#予測データは目的変数を一つずつ入れ替えていく(性別、年齢は初年度のみとしないとランク落ちする)
#空腹時血糖値とHbA1cはどちらか一方を使用する


riskdata <- NULL
for(FBG in 0:1){
for(pred in 46:57){
	for(s in 1:2){
	data <- subset(df[,c(1:14,18:29,pred)], complete.cases(df[,c(1:14,18:29,pred)]))
	data2 <- subset(df2[,c(1:14,18:29,pred)], complete.cases(df2[,c(1:14,18:29,pred)]))
	dsex <- subset(data, data$sex==s)
	dsex2 <- subset(data2, data2$sex==s)
	colnames(dsex) <- c("age", "sex", "bmi", "sbp", "dbp", "tg", "hdl", "ldl", "got", "gpt", "ggtp", "fbg", "hba1c", "hb", "bmi2", "sbp2", "dbp2", "tg2", "hdl2", "ldl2", "got2", "gpt2", "ggtp2", "fbg2", "hba1c2", "hb2", "risk")

	#ロジスティック回帰(男女別にモデル作成、説明変数から性は抜いておくこと)
	targ <- paste(sep="", ifelse(FBG==0, "fbg", "hba1c"), '.*')
       dsexl <- dsex[,-c(2, grep(targ, colnames(dsex)))]#sexとFBG=0の場合空腹時血糖を抜く
	#dsexl <- dsex[,-2]#sexを抜く
	log <- glm(data=dsexl, family=binomial, dsexl$risk~.)
	
	#予測式を再現するためにログを残す
	seibetsu <- ifelse(s==1,"男性","女性")
       logname <- paste(sep="", "モデル詳細", today, ".txt")
	sink(logname, append=TRUE)
	print(seibetsu)#対象となった性別をプリント
	print(colnames(df[pred]))#対象となった検査項目をプリント
	print(log)#予測式をプリント
	sink()

	#出力（ifelseの中身が逆になっていることに注意）
       coefname <- paste(sep="", "riskcoef", today, ".csv")
       write.table(t(c(seibetsu, colnames(df[pred]),  ifelse(FBG==0, "hba1c", "fbg"), log$coef)), coefname, append=TRUE, row.names=FALSE)

	#テストデータによる予測
	dsex2$risk <- predict(log, newdata=dsex2, type="response")
	
	#10歳刻みフラグ
	dsex2$age10 <- cut(dsex2$age, right=FALSE, breaks=c(-Inf, 30, 40, 50, 60, 70, Inf), labels=c("20", "30", "40", "50", "60", "70"))
	
	#性および年齢10歳刻みで特異度、感度、陰性的中率、陽性的中率を算出
	ddd2 <- NULL
		for(q in seq(40, 60, by=10)){
			dage <- subset(dsex2, dsex2$age10==q)
			for(c in seq(0.01, 1, by=0.01)){
				dage$riskt <- ifelse(dage$risk>=c, 1, 0)
				tb <- table(dage$riskt, dage[,27])
				tokuido <- tb[1]/(tb[1] + tb[2])
				kando <- tb[4]/(tb[3] + tb[4])
				inteki <- tb[1]/(tb[1] + tb[3])
				youteki <- tb[4]/(tb[2] + tb[4])
				ddd1 <- data.frame(s, q, c,  BGtype=ifelse(FBG==0, "hba1c", "fbg"), tokuido, kando, inteki, youteki, risk=colnames(df[pred]))
				ddd2 <- rbind(ddd2, ddd1)
				}
			}
	riskdata <- rbind(riskdata, ddd2)#リスク値を蓄積
	##グラフ描画
	ddd2$age_s <- factor(ddd2$q)
	#感度（再現率）と陽性的中率（精度）→再現率を横軸、精度を縦軸
	qq1 <- ggplot(data=ddd2, aes(x=kando, y=youteki, group=age_s) )+ geom_line(aes(colour=age_s))+ opts(title = paste(colnames(df[pred]),ifelse(s==1,"男","女"),"感度×陽性的中率",sep=""))
	name <- paste(place, "/", colnames(df[pred]), ifelse(s==1,"男","女"), ifelse(FBG==0, "hba1c", "fbg"), "処理後感度×陽性的中率.pdf", sep="")
	pdf(file=name, family="Japan1GothicBBB",paper="a4r",width=9.5,height=7)
	print(qq1)
	dev.off()	
	
	#特異度と陰性的中率→特異度を横軸、陰性的中率を縦軸
	qq2 <- ggplot(data=ddd2, aes(x=tokuido, y=inteki, group=age_s) )+ geom_line(aes(colour=age_s))+ opts(title = paste(colnames(df[pred]),ifelse(s==1,"男","女"),"特異度×陰性的中率",sep=""))
	name <- paste(place, "/", colnames(df[pred]), ifelse(s==1,"男","女"),  ifelse(FBG==0, "hba1c", "fbg"), "処理後特異度×陰性的中率.pdf", sep="")
	pdf(file=name, family="Japan1GothicBBB",paper="a4r",width=9.5,height=7)
	print(qq2)
	dev.off()	

	}
}
}
write.csv(riskdata, "riskscore.csv",row.names=FALSE)

#陽性的中率0.7以上のうち最小のc（対数オッズ）を抽出する

riskdata2 <- subset(riskdata, youteki>=0.7)#陽性的中率0.7以上のデータを抽出
colnames(riskdata2) <- c("s", "q", "c", "BGtype", "tokuido", "kando", "inteki", "youteki", "name")
cd <- ddply(riskdata2, .(s,q,name), transform, min_youteki=min(youteki,na.rm=TRUE))#陽性的中率最小列を付加
cd <- cd[cd$youteki==cd$min_youteki,]#陽性的中率最小列と陽性的中率が等しいデータを抽出
cd <- ddply(cd, .(s,q,name), summarise, c=min(c))#さらに最小のcを抽出
c_score_data <- merge(riskdata2, cd)
fname <- paste(sep="", "c_score_data", today, ".csv")
write.csv(c_score_data, fname,row.names=FALSE)

#項目間精度比較
ddd2 <- NULL
for(pred in 46:57){
	for(s in 1:2){
	data <- subset(df[,c(1:14,18:29,pred)], complete.cases(df[,c(1:14,18:29,pred)]))
	data2 <- subset(df2[,c(1:14,18:29,pred)], complete.cases(df2[,c(1:14,18:29,pred)]))
	dsex <- subset(data, data$sex==s)
	dsex2 <- subset(data2, data2$sex==s)
	colnames(dsex) <- c("age", "sex", "bmi", "sbp", "dbp", "tg", "hdl", "ldl", "got", "gpt", "ggtp", "fbg", "hba1c", "hb", "bmi2", "sbp2", "dbp2", "tg2", "hdl2", "ldl2", "got2", "gpt2", "ggtp2", "fbg2", "hba1c2", "hb2", "risk")

	#ロジスティック回帰(男女別にモデル作成、説明変数から性は抜いておくこと)
	dsexl <- dsex[,-2]#sexを抜く
	log <- glm(data=dsexl, family=binomial, dsexl$risk~.)
	
	#テストデータによる予測
	dsex2$risk <- predict(log, newdata=dsex2, type="response")
	
	#10歳刻みフラグ
	dsex2$age10 <- cut(dsex2$age, right=FALSE, breaks=c(-Inf, 30, 40, 50, 60, 70, Inf), labels=c("20", "30", "40", "50", "60", "70"))
	
	#性および年齢10歳刻みで特異度、感度、陰性的中率、陽性的中率を算出
			dage <- dsex2#書き換えがめんどくさいのでコピーしている
			for(c in seq(0.01, 1, by=0.01)){
				dage$riskt <- ifelse(dage$risk>=c, 1, 0)
				tb <- table(dage$riskt, dage[,27])
				tokuido <- tb[1]/(tb[1] + tb[2])
				kando <- tb[4]/(tb[3] + tb[4])
				inteki <- tb[1]/(tb[1] + tb[3])
				youteki <- tb[4]/(tb[2] + tb[4])
				ddd1 <- data.frame(kensa = colnames(df[pred]), s, c, tokuido, kando, inteki, youteki)
				ddd2 <- rbind(ddd2, ddd1)
				}
			
	}
}

##グラフ描画
ddd2$項目名 <- as.factor(ddd2$kensa)
#感度（再現率）と陽性的中率（精度）→再現率を横軸、精度を縦軸
qq1 <- ggplot(data=ddd2, aes(x=kando, y=youteki, group=項目名) )+ geom_line(aes(colour=項目名))+ opts(title = "感度×陽性的中率") +facet_wrap(~s)
#特異度と陰性的中率→特異度を横軸、陰性的中率を縦軸
qq2 <- ggplot(data=ddd2, aes(x=tokuido, y=inteki, group=項目名) )+ geom_line(aes(colour=項目名))+ opts(title = "特異度×陰性的中率") +facet_wrap(~s)

pdf(file="02_検査項目間予測結果比較.pdf", family="Japan1GothicBBB",paper="a4r",width=9.5,height=7)
print(qq1)
print(qq2)
dev.off()

##リスク予測の予測式算出
#予測データは目的変数を一つずつ入れ替えていく(性別、年齢は初年度のみ含めないとランク落ちする)

model_coef <- NULL
for(pred in 46:51){
	for(s in 1:2){
	data <- subset(df[,c(1:14,18:29,pred)], complete.cases(df[,c(1:14,18:29,pred)]))
	data2 <- subset(df2[,c(1:14,18:29,pred)], complete.cases(df2[,c(1:14,18:29,pred)]))
	dsex <- subset(data, data$sex==s)
	dsex2 <- subset(data2, data2$sex==s)
	colnames(dsex) <- c("age", "sex", "bmi", "sbp", "dbp", "tg", "hdl", "ldl", "got", "gpt", "ggtp", "fbg", "hba1c", "hb", "bmi2", "sbp2", "dbp2", "tg2", "hdl2", "ldl2", "got2", "gpt2", "ggtp2", "fbg2", "hba1c2", "hb2", "risk")

	#ロジスティック回帰(男女別にモデル作成、説明変数から性は抜いておくこと)
	dsexl <- dsex[,-2]#sexを抜く
	log <- glm(data=dsexl, family=binomial, dsexl$risk~.)
	
	#予測式を再現するためにログを残す
	seibetsu <- ifelse(s==1,"男性","女性")
	sink("モデル詳細.txt", append=TRUE)
	print(seibetsu)#対象となった性別をプリント
	print(colnames(df[pred]))#対象となった検査項目をプリント
	print(log)#予測式をプリント
	sink()
	
	#テストデータによる予測
	dsex2$risk <- predict(log, newdata=dsex2, type="response")
	
	#10歳刻みフラグ
	dsex2$age10 <- cut(dsex2$age, right=FALSE, breaks=c(-Inf, 30, 40, 50, 60, 70, Inf), labels=c("20", "30", "40", "50", "60", "70"))

	
	#モデル式
	model_coef1 <- data.frame(t(log$coef))
	model_coef1$sex<- s
	model_coef1$name<- colnames(df[pred])
	model_coef <- rbind(model_coef, model_coef1)
	}
	
	}


#発症確率の算出（オッズではない）→血糖項目に関して値がおかしいのでやり直す必要がある
df_new <- NULL
for(s in 1:2){
	df_s <- subset(df, df$sex == s)
	model_coef_s <- subset(model_coef, model_coef$sex == s) 
	for(cf in 1:6){
	df_sm <- data.matrix(df_s[,c(1, 3:14, 18:29)])#行列化
	coef_m <- data.matrix(model_coef[cf, 2:26])#行列化
	a <- ( df_sm %*% t(coef_m) ) + model_coef[1, 1]#回帰係数と説明変数の積を求める
	num <- cf + 51
	df_s[, num ] <- 100/ (1 + exp(-a))#ロジットの定義から発生確率を求めて新規列に入力
	}
df_new <- rbind(df_new, df_s)
}

colnames(df_new) <- c(colnames(df), "sbpp", "dbpp", "tgp", "hdlp", "fbgp", "hba1cp") #各確率列名の付与

#各種データの出力
write.csv(model_coef, "モデル式データ.csv", row.names=FALSE)
write.csv(df_new, "リスク予測確率付き健診データ.csv", row.names=FALSE)



