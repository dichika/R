

######保健指導テキストマイニング
##各支援者の初回面接における癖（抽象的な行動計画を立てやすい等）を抽出する
##各支援者の学習効果を定量化する→対象者の均一性と行動計画の均一性を比較


###行動計画データの正規化について
#事前にエクセルで全角英数字については半角化を行っておく：mystr関数で）
#漢数字の変換(一→1, 万→0000)
#万歩計→歩数計
#エスカレーター・エレベーター→エスカレータ・エレベータ

##(paperよりインサイト)
#長期分析の要件→解説内容の順番や段落構成等が定式化されている
#共起解析＋主成分分析＋回帰分析
#訓練データでの説明力評価→他分析器との予測力比較→運用テスト(外挿)

library(RMeCab)
library(ggplot2)
library(arules)
library(hwriter)

setwd("Q:/Pln/Analysis/05_analysis/006_hokensidou")
source("Q:/Pln/Analysis/999_code/hoken_func.R")


##集計用データの作成

d <- read.csv("data2.csv", as.is=TRUE)#セルフモニタリングデータ
dj <- read.csv("dj.csv", as.is=TRUE)#実績データ
shokai <- read.csv("djmensetsusya110513.csv", as.is=TRUE)#初回面接者名（積極的支援、私学、2009年度）
dj <- merge(dj, shokai)

today <- paste(sep="/", getwd(), format(Sys.Date(), "%y%m%d"))
dir.create(today)
setwd(today)


dat <- d[d$V2>=10, ]#セルフモニタリングデータから行動計画のみ抽出



#実績データで支援者を仮に決める（5人とした）
s_num <- 5 #支援者数

#if(nrow(dj) %% s_num == 0){
#  dj$siensya <- rep(1:s_num, nrow(dj) %/% s_num)
#  }else{
#  dj$siensya <- c(rep(1:s_num, nrow(dj) %/% s_num), seq(1, nrow(dj)%%s_num))
#  }
  

#行動計画と実績データを結合
data <- merge( dat, dj, by.x="V1", by.y="index" )





##支援者ごとの頻度表およびグラフを出力(名詞-副詞)
#数詞、数助詞が上位にくるので要対応
#名詞の出力がおかしいので要対応110513→data.frameで因子型変換を抑制しないとggplot2に反映されてしまう
#途中で出力が止まるので要対応110513→データが少ない支援者の場合副詞、形容詞のデータが少ないとエラーが出てしまうので
#画像＋HTML出力を行ってレポート化する

dir.create("output")
setwd(paste(sep="/", today, "output"))


for(num in 1:s_num){
  mat <- ftr_mat(data[data$siensya==num, ]$V3)
  item<- c("名詞", "動詞", "形容詞", "副詞")
  res <- NULL
    for(class in item){
      res0 <- output(mat, class)#エラーの場合
      if(is.null(res0)){next}else{
        res <- rbind(res, res0[[1]])
        pdf(paste(sep="", "支援者", num, "_", class, ".pdf"), family="Japan1GothicBBB")
        print(res0[[2]])
        dev.off()
        }
      }
  write.csv(res, paste("支援者", num, "_result.csv", sep=""), row.names=FALSE)
  }

setwd(today)


##########支援者ごとの特徴的な行動計画の抽出

#共通データの作成
#支援者ごとの行動計画をテキストファイルに出力して処理に用いる


dir.create("doc2")
setwd(paste(sep="", today, "/doc2"))

for(count in 1:s_num){
write(data[data$siensya==count, ]$V3, paste(sep="", getwd(), "/", count, ".txt"))
}

setwd(today)

#特徴語を含む行動計画を抽出する
#特徴語(TFIDFが高い)を抽出して、その言葉を含む行動計画を抽出
#(新規)特徴語を「多く」含む行動計画を抽出→
#(新規)重みづけされた特徴語を「多く」含む行動計画を抽出→
#行動計画単位で、加算値・最大値・平均値を算出

#特徴語を含む行動計画を抽出する関数（引数は「行動計画フォルダ」「支援者の番号」「抽出する行動計画の数」）

#plan <- plan_feat(place="doc2", num=1, n=3)#名前で抽出できるように改変？

#print(plan)

#特徴的な共起（行動計画単位）を含む行動計画を抽出する
#引数は、支援者数、支援者番号、行動計画数、単体、行動計画データ

plan2_colloc <- plan_feat2(s_num=s_num, siensya=1, n=10, tango=0, plandata=data)
plan2_single <- plan_feat2(s_num=s_num, siensya=1, n=10, tango=1, plandata=data)

print(plan2_colloc)#特徴的な組合せ
print(plan2_single)#特徴的な単語

##支援者ごとの行動計画の均質性の比較
#行動計画集合に対してエントロピーを算出→語の頻度×log2（語の頻度）
#研修効果による標準化が進むとエントロピーが低くなる可能性あり→時期による変化をみるとよい

ENTROPY <- docDF("doc2", weight="idf4*norm", type=1) #コサイン正規化をかけている
colSums(ENTROPY[,-(1:3)])


####各種スコアの算出

###行動計画ごとのスコア算出
#スコアランキングによる行動計画抽出は同一のものが出る可能性があるので重複を削除する必要あり

##具体性スコアの算出
#一旦テキストファイルに吐きだす(いずれはテンポラリファイルで処理したい)
#テキストファイルを読み込んでRMeCabTextにかけて品詞情報を抽出
#具体性スコアを算出(数詞+1点、固有名詞+1点、副詞-1点、助数詞+1点)
#具体性スコアは効果に関する重要度で決定したい
#具体性を行動計画の長さ（文字数）で補正したものも用意する


data$gutaisei <- gutaiseiAdd(data=data, plancol=3)
data$gutaisei_mod <- data$gutaisei/nchar(data$V3)

setwd("Q:/Pln/Analysis/05_analysis/006_hokensidou")


##効果的スコアの算出
#効果的であるとは、「体重が減っている」×「多く寄与している」

#減量率に対する行動計画の寄与度を利用者ごとに算出→該当計画を守れた日数/全計画を守れた日数
start <- 4 #行動計画記録列の最初の列
end <- 368 #行動計画記録列の最後の列

data$ok <- rowSums(data[, start:end], na.rm=TRUE) #該当計画を守れた日数
person_ok <- ddply(data, .(V1), summarise, person_ok = sum(ok, na.rm=TRUE)) #利用者ごとののべ守れた日数

res <- merge(data, person_ok, all.x=TRUE)
res$kiyodo <- round( res$ok / res$person_ok, 3)
res$score_kouka <- -res$weight_rate * res$kiyodo #減量しているほど高くできるように負に変換している


##支援者ごとの評価
scoreSUMMARY(res)

#支援者ごとの具体性・効果的ベスト・ワースト10の算出
siensyaBESTWORST10(res, siensya=3)


##参加者ごとの具体性の評価
#具体性の最大値を、参加者の具体性とする
sankasya_smp <- ddply(res, .(V1), summarise, max_weight_rate=max(weight_rate, na.rm=TRUE), mean_gutaisei=mean(gutaisei, na.rm=TRUE), mean_gutaisei_mod=mean(gutaisei_mod, na.rm=TRUE))
sankasya_smp <- sankasya_smp[is.finite(sankasya_smp$max_weight_rate),]

cor(sankasya_smp$max_weight_rate, sankasya_smp$mean_gutaisei)
cor(sankasya_smp$max_weight_rate, sankasya_smp$mean_gutaisei_mod) #文字数で補正した方が若干相関は高い




####################################################################################
#ここから下は残骸


##食事・運動・その他のカテゴリ分類
#カテゴリをランダムフォレストで決める
#教師データから行動計画ごとの素性マトリックスを作成


#素性マトリックス作成関数

ftr_mat <- function(vec){
    res <- as.list(rep(NA, length(vec)))
    for(i in 1:length(vec)){
        tx <- vec[i]
        if ( nchar(tx) > 1){
        val <- unlist(RMeCabC(tx, mypref=1))#原形で表示
        atr <- attr(val, "names")
        res[[i]] <- unique(paste(sep = "-", val, atr))
        } else {
        next
        }}
    res.im <- as(res, "itemMatrix")
    data.frame(as(res.im, "matrix"))
    }

train <- read.csv("train_plan.csv", stringsAsFactors = FALSE)
tra <- ftr_mat(train$plan)

tra <- data.frame(category = factor(train$category), tra)

library(randomForest)
rf <- randomForest(data=tra, category~., ntree=10000)



#教師データを含むテストデータから行動計画ごとの素性マトリックスを作成しテストデータを抜いて予測
testvec <- data$V3

test <- ftr_mat(testvec)
test <- test[ 101:nrow(test), colnames(test) %in% colnames(tra)]#学習データの範囲でテストデータから列を抽出
rf_pred <- predict( rf, newdata=test )

testvec[rf_pred==0]
testvec[rf_pred==1]
testvec[rf_pred==2]



#行動計画単位で形態素に分割→arulesでJaccard係数を単語間で算出し単語間距離とする→cytoscapeで表示

#itemMatrixを作成
res <- as.list(rep(NA, nrow(data)))
for(i in 1:nrow(data)){
  tx <- data$V3[i]
  val <- unlist(RMeCabC(tx, mypref=1))#原形で表示
  atr <- attr(val, "names")
  res[[i]] <- unique(paste(sep = "-", val, atr))
  }

res.im <- as(res, "itemMatrix")
mat <- as(res.im, "matrix")
mat <- mat[, -grep("-記号", colnames(mat))]
mat <- mat[, -grep("-助詞", colnames(mat))]
mat2 <- mat[,colSums(mat)>=5]

#Jaccard係数を算出
res.dist <- dissimilarity(t(mat2))
res.dist.mat <- as.matrix(res.dist)
?dissimilarity

#グラフ構造として出力
gob <- graph.adjacency(res.dist.mat, mode="undirected", weight=TRUE)
edgelist <- data.frame(from=gob[[3]], to=gob[[4]], weight=gob[[9]][[4]])
edgelist <- subset(edgelist, weight<0.9, )
hist(edgelist$weight)
write.csv(edgelist, "g_plan.csv",row.names=FALSE)

#行動計画をnode名として出力
name <- data.frame(0:(ncol(mat2)-1), do.call("rbind", strsplit(split="-", colnames(mat2))))
colnames(name) <- c("num", "word", "class")
write.csv(name, "name_plan.csv",row.names=FALSE)











###コーパス比較
####減量成功コーパスと失敗コーパスを作成し比較する
##仮説→減量するかしないかは本人のモチベーションで、たくさん減量できるかは計画次第？

#実績データから対象を抽出（動機付け支援、男性）
dj <- dj[!is.na(dj$shuryou_weight) & dj$sex=="M" & dj$level=="douki",]


#行動計画データから少なくとも50%以上は守れている行動計画のみ抽出
count <- function(x)sum(x, na.rm=TRUE)/sum(!is.na(x))
d <- subset(d, d[,2]>=10 & apply(d[, -c(1:3)], 1, count)>=0.7)

#djにおいて5%以上減量者にフラグ、dにmerge→32人抽出(うち11人が5%以上減量)
dj$success <- ifelse(dj$weight_rate <= -0.05, 1, 0)

data <- merge(d[,c(1,3)], dj[,c(1, ncol(dj))], by.x="V1", by.y="index")


#成功コーパスと失敗コーパスを作成し一旦出力(区切り文字は。にする)
success <- paste(data[data$success==1,2], collapse="。")
lost <- paste(data[data$success==0,2], collapse="。")

setwd("Q:/Pln/Analysis/05_analysis/006_hokensidou/doc")
write(success, "success.txt", sep="")
write(lost, "lost.txt", sep="")



#再度データ入力
setwd("Q:/Pln/Analysis/05_analysis/006_hokensidou")
docd <- docDF("doc", type=1, weight="tf*norm") #形態素、TFのみ+コサイン正規化
str(docd)

#docd.m <- melt(docd, id.vars=c("TERM", "POS1", "POS2"))
#head(docd.m)
#str(docd.m)
#docd.m$variable <- factor(gsub(".txt", "", as.character(docd.m$variable)), levels=c("success", "lost"))

head(docd)
docd$diff <- docd[,5] - docd[,4]

#グラフのパラメータ設定
theme_update(theme_grey())

##品詞ごとに語の比較を行う
#名詞
ggplot(data=docd[!docd$TERM=="。" & docd$POS1=="名詞" & docd$diff>=0.02,], aes(x=reorder(TERM, diff), y=diff)) + geom_bar(stat="identity") + coord_flip()
ggplot(data=docd[!docd$TERM=="。" & docd$POS1=="名詞" & docd$diff<=-0.02,], aes(x=reorder(TERM, diff), y=diff)) + geom_bar(stat="identity") + coord_flip()
#数
ggplot(data=docd.m[!docd.m$TERM=="|" & docd.m$POS2=="数" ,], aes(x=reorder(TERM, value), y=value, group=variable)) + geom_line(aes(colour=variable), stat="identity") + coord_flip()
#副詞
ggplot(data=docd.m[docd.m$POS1=="副詞",], aes(x=reorder(TERM, value), y=value, group=variable)) + geom_line(aes(colour=variable), stat="identity") + coord_flip()
#形容詞
ggplot(data=docd.m[docd.m$POS1=="形容詞",], aes(x=reorder(TERM, value), y=value, group=variable)) + geom_line(aes(colour=variable), stat="identity") + coord_flip()
#助詞
ggplot(data=docd.m[docd.m$POS1=="助詞",], aes(x=reorder(TERM, value), y=value, group=variable)) + geom_line(aes(colour=variable), stat="identity") + coord_flip()
#助動詞
ggplot(data=docd.m[docd.m$POS1=="助動詞",], aes(x=reorder(TERM, value), y=value, group=variable)) + geom_line(aes(colour=variable), stat="identity") + coord_flip()
#動詞(フォントを小さく)
base_size=8
theme_update(axis.text.y = theme_text(size=base_size * 0.8, colour = "grey50", hjust = 1, lineheight = 0.9))
ggplot(data=docd.m[docd.m$POS1=="動詞",], aes(x=reorder(TERM, value), y=value, group=variable)) + geom_line(aes(colour=variable), stat="identity") + coord_flip()
theme_update(theme_grey())
#接頭詞
ggplot(data=docd.m[docd.m$POS1=="接頭詞",], aes(x=reorder(TERM, value), y=value, group=variable)) + geom_line(aes(colour=variable), stat="identity") + coord_flip()

#名詞
ggplot(data=docd[!docd$TERM=="。" & docd$POS1=="名詞", ], aes(x=lost.txt, y=success.txt, group=POS2, label=TERM)) + geom_text(size=3, aes(colour=POS2)) + opts(legend.position="none") + ylim(0,0.05)+ xlim(0,0.05) + geom_abline()
#数
ggplot(data=docd[!docd$TERM=="。" & docd$POS2=="数", ], aes(x=lost.txt, y=success.txt, group=POS2, label=TERM)) + geom_text(size=3, aes(colour=POS2)) + opts(legend.position="none") + ylim(0,0.05)+ xlim(0,0.05) + geom_abline()
#副詞
ggplot(data=docd[docd$POS1=="副詞", ], aes(x=lost.txt, y=success.txt, group=POS2, label=TERM)) + geom_text(size=3, aes(colour=POS2)) + opts(legend.position="none") + geom_abline()
#形容詞
ggplot(data=docd[docd$POS1=="形容詞", ], aes(x=lost.txt, y=success.txt, group=POS2, label=TERM)) + geom_text(size=3, aes(colour=POS2)) + opts(legend.position="none") + geom_abline()




##共起状況について有意なものを各群においてピックアップ
setwd("Q:/Pln/Analysis/05_analysis/006_hokensidou")
docN <- docDF("doc", type=1, N=2, pos=c("名詞", "動詞", "副詞", "形容詞", "助詞"), weight="tf*norm")
head(docN, n=100)
table(docN$POS1)
#各群に出現している語のうち、差が大きいものをピックアップ
docN$diff <- docN$success.txt-docN$lost.txt
docN1 <- docN[docN$diff>0.04 & docN$success.txt>0 & docN$lost.txt>0, ]
ggplot(data=docN1, aes(x=reorder(TERM, diff), y=diff)) + geom_bar(position="dodge", stat="identity") + coord_flip()

#POS2ごとに頻度を比較して差が大きいものをピックアップ
docNPOS2 <- ddply(docN, .(POS2), summarise, sum_suc=sum(success.txt), sum_los=sum(lost.txt))
docNPOS2$diff <- docNPOS2$sum_suc - docNPOS2$sum_los
docNPOS21 <- docNPOS2[docNPOS2$diff>0, ]

ggplot(data=docNPOS21, aes(x=reorder(POS2, diff), y=diff)) + geom_bar(position="dodge", stat="identity") + coord_flip()
#ggplot(data=docN, aes(x=lost.txt, y=success.txt, group=POS2, label=TERM)) + geom_text(aes(colour=POS2)) + geom_abline() + opts(legend.position="none")
#係助詞-自立
docN[docN$POS2=="一般-一般",]

suc_g <- data.frame(matrix(unlist(strsplit(docN$TERM, split="-")), ncol=2, byrow=TRUE), value = docN$success.txt, stringsAsFactors=FALSE)
los_g <- data.frame(matrix(unlist(strsplit(docN$TERM, split="-")), ncol=2, byrow=TRUE), value = docN$lost.txt, stringsAsFactors=FALSE)

setwd("Q:/Pln/Analysis/05_analysis/006_hokensidou")
write.csv(suc_g, "suc_g.csv", row.names=FALSE)
write.csv(los_g, "los_g.csv", row.names=FALSE)

##達成した行動計画中に、数詞が含まれている行動計画を含んでいる

#発表用
#名詞
ggplot(data=docd[!docd$TERM=="。" & docd$POS1=="名詞" & docd$diff>=0.02,], aes(x=reorder(TERM, diff), y=diff)) + geom_bar(stat="identity") + coord_flip()
ggplot(data=docd[!docd$TERM=="。" & docd$POS1=="名詞" & docd$diff<=-0.02,], aes(x=reorder(TERM, diff), y=diff)) + geom_bar(stat="identity") + coord_flip()

#動詞
ggplot(data=docd[docd$POS1=="動詞" & docd$diff>=0.01,], aes(x=reorder(TERM, diff), y=diff)) + geom_bar(stat="identity") + coord_flip()
ggplot(data=docd[docd$POS1=="動詞" & docd$diff<=-0.01,], aes(x=reorder(TERM, diff), y=diff)) + geom_bar(stat="identity") + coord_flip()

#共起状況
ggplot(data=docN1, aes(x=reorder(TERM, diff), y=diff)) + geom_bar(position="dodge", stat="identity") + coord_flip()
docN1N <- docN[docN$diff< -0.04 & docN$success.txt>0 & docN$lost.txt>0, ]
ggplot(data=docN1N, aes(x=reorder(TERM, diff), y=diff)) + geom_bar(position="dodge", stat="identity") + coord_flip()



