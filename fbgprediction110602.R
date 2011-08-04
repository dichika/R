#######################################################################
##重回帰を用いたfbg予測110602

#HbA1cのみを持っている場合に空腹時血糖を予測し、他の予測モデルに用いる
#HbA1cのみでほとんどは予測できる
#モデルを作成した後、回帰係数およびモデル式を記述する

###モデル作成前処理
library(hwriter)

set.seed(1)

dir <- "Q:/Pln/Analysis/05_analysis/001_riskprediction/002_work"
setwd(dir)
bg <- read.csv("bgdata110602.csv")

bg <- subset(bg, med == 0) #未服薬者を抽出
bg <- bg[is.finite(rowSums(bg)), -c(grep("med", colnames(bg)))]

#外れ値を除く
bg <- bg[bg$hba1c<20,]
bg <- bg[bg$fbg<400,]

#中性脂肪および肝機能は対数変換し、infiniteなものは除く
bg$gpt <- log(bg$gpt)
bg$tg <- log(bg$tg)
bg$got <- log(bg$got)
bg$ggtp <- log(bg$ggtp)
bg <- bg[is.finite(rowSums(bg)), ]

#空腹時血糖予測モデル作成
result <- lm(data=bg, fbg~.)

correlation_value <- cor(result$fitted.values, bg$fbg)
plot(result$fitted.values, bg$fbg)

bg[result$fitted.values - bg$fbg >= 50, ]
result$fitted.values[result$fitted.values - bg$fbg >= 50]
##結果保存
cdir <- paste(Sys.Date(), sep="")
if(all(is.na(match(dir(), cdir))))dir.create(cdir)
place <- paste(dir, cdir, sep="/")
setwd(place)

title <- "空腹時血糖予測モデル"
p <- openPage(paste(sep="", title, ".html"), charset="CP932")
hwrite(title, p, br=TRUE, heading=1)
hwrite(sprintf("使用データ数 %s件", nrow(bg)), p, br=TRUE)
hwrite("モデル：線形重回帰", p, br=TRUE)
plot(bg$fbg, result$fitted.values, xlab="空腹時血糖 実測値", ylab="空腹時血糖 予測値")
hwrite(paste(sep="=", "相関係数", round(correlation_value, 3)), p, br=TRUE)
hwriteImage(paste(sep="", title, ".png"), p, capture=TRUE, br=TRUE)
hwrite("回帰係数一覧", p, br=TRUE, heading=3)
hwrite("tg, gpt, got, ggtpは自然対数変換していることに注意", p, br=TRUE)
hwrite(round(result$coef,3), p, br=TRUE)
closePage(p, splash=FALSE)

shell(paste(sep="", title, ".html"))