#######################################################################
##重回帰を用いた腹囲予測110602

#身長体重を持っている場合に腹囲を予測し、他の予測モデルに用いる

###モデル作成前処理
library(hwriter)

set.seed(1)

dir <- "Q:/Pln/Analysis/05_analysis/001_riskprediction/002_work"
setwd(dir)

#クリーニング用コード読み込み
#source("C:/Users/h1030/Documents/Revolution/Project0/qcleaning.R")

#クリーニング済データ
sigaku <- read.csv("qdata110422.csv", as.is=TRUE)


#中性脂肪および肝機能は対数変換
#BMIは腹囲とかぶるのでモデルから抜く

#テストデータには身体測定データも含めた私学データを使用(110421)

sigaku$sex <- ifelse( sigaku$sex== "男", 1, 2 )
nrow(sigaku[is.finite(rowSums(sigaku)), ])
sigaku <- subset(sigaku, drugBP ==2 & drugBG ==2 & drugLP ==2) #未服薬者を抽出
sigaku <- sigaku[is.finite(rowSums(sigaku)), -c(grep("drug|smoke", colnames(sigaku)))]


sigaku$gpt <- log(sigaku$gpt)
sigaku$tg <- log(sigaku$tg)
sigaku$got <- log(sigaku$got)
sigaku$ggtp <- log(sigaku$ggtp)

sigaku$bmi <- NULL

result <- lm(data=sigaku, waist~.)

correlation_value <- cor(result$fitted.values, sigaku$waist)
plot(result$fitted.values, sigaku$waist)

##結果保存
cdir <- paste(Sys.Date(), sep="")
if(all(is.na(match(dir(), cdir))))dir.create(cdir)
place <- paste(dir, cdir, sep="/")
setwd(place)

title <- "腹囲予測モデル"
p <- openPage(paste(sep="", title, ".html"), charset="CP932")
hwrite(title, p, br=TRUE, heading=1)
hwrite(sprintf("使用データ数 %s件", nrow(sigaku)), p, br=TRUE)
hwrite("モデル：線形重回帰", p, br=TRUE)
hwrite(paste(sep="=", "相関係数", round(correlation_value, 3)), p, br=TRUE)
plot(sigaku$waist, result$fitted.values, xlab="腹囲 実測値", ylab="腹囲 予測値")
hwriteImage(paste(sep="", title, "1.png"), p, capture=TRUE, br=TRUE)
hwrite("回帰係数一覧", p, br=TRUE, heading=3)
hwrite("tg, gpt, got, ggtpは自然対数変換していることに注意", p, br=TRUE)
hwrite(round(result$coef,3), p, br=TRUE)
closePage(p, splash=FALSE)

shell(paste(sep="", title, ".html"))
