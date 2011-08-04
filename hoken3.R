

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
library(batade)
library(wordcloud)

setwd("Q:/Pln/Analysis/05_analysis/006_hokensidou")
source("Q:/Pln/Analysis/999_code/hoken_func.R")
source("Q:/Pln/Analysis/999_code/wordplot.R") #品詞ごとのtreemap


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


#dir.create("output")
#setwd(paste(sep="/", today, "output"))
tmpdir <- tempdir()
setwd(tmpdir)

for(num in 1:s_num){
  mat <- ftr_mat(data[data$siensya==num, ]$V3)
  item<- c("名詞", "動詞", "形容詞", "副詞")
  res <- NULL
    for(class in item){
      res0 <- output(mat, class)#エラーの場合
      if(is.null(res0)){next}else{
        res <- rbind(res, res0[[1]])
        png(paste(sep="", "支援者", num, "_", class, ".png"))
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

#print(plan2_colloc)#特徴的な組合せ
#print(plan2_single)#特徴的な単語



####各種スコアの算出

###行動計画ごとのスコア算出
#スコアランキングによる行動計画抽出は同一のものが出る可能性があるので重複を削除する必要あり

##具体性スコアの算出
#一旦テキストファイルに吐きだす(いずれはテンポラリファイルで処理したい)
#テキストファイルを読み込んでRMeCabTextにかけて品詞情報を抽出
#具体性スコアを算出(数詞+1点、固有名詞+1点、副詞-1点、助数詞+1点, 辞書レベルでルール処理→「半分」)
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


##支援者ごとのスコア比較
scoreSUMMARY(res)

#支援者ごとの具体性・効果的ベスト・ワースト10の算出
siensyaBESTWORST10(res, siensya=1)


#品詞および形態素のtreemap
wordplot(data)

####htmlレポートの出力

library(hwriter)
#適当におしゃれなcssを拾ってくる

#http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/blitzer/jquery-ui.css
#https://ajax.googleapis.com/ajax/libs/jquery/1.6.0/jquery.js
#https://ajax.googleapis.com/ajax/libs/jqueryui/1.8.12/jquery-ui.js

num <- 1
siensya <- num
planresult <- siensyaBESTWORST10(res, siensya=1)
colnames(plan2_colloc) <- c("単語1","単語2","特徴的な行動計画")

p <- openPage("result1.html", dirname=tmpdir, link.javascript='https://ajax.googleapis.com/ajax/libs/jquery/1.6.0/jquery.js', 
              link.css='C:/Users/h1030/Desktop/source/style.css',
              lang='ja', charset="shift-jis")
hwrite(paste("支援者", planresult[[1]], "さんの結果です", sep=""), p, heading=1)

noun <- hwriteImage(paste("支援者", num, "_名詞.png", sep=""), onclick="this.hide()", image.border=0, width=300)
#adj <- hwriteImage(paste("支援者", num, "_形容詞.png", sep=""), onclick="this.hide()", image.border=0, width=300)
adv <- hwriteImage(paste("支援者", num, "_副詞.png", sep=""), onclick="this.hide()", image.border=0, width=300)

graph <- hwrite(c(noun, adv), border=0)
plans <- hwrite(plan2_colloc, row.names=FALSE, row.bgcolor="#b9c9fe", table.id="hor-minimalist-a")

hwrite(c(graph, plans), p, border=0)

hwrite(hmakeTag("button", "次へ"), p, link="result2.html")
closePage(p, splash=FALSE)

p <- openPage("result2.html", dirname=tmpdir, link.javascript='https://ajax.googleapis.com/ajax/libs/jquery/1.6.0/jquery.js', 
              link.css='C:/Users/h1030/Desktop/source/style.css', 
              lang='ja', charset="shift-jis")
hwrite(paste("サポーター", planresult[[1]], "さんの結果です", sep=""), p, heading=1)
table_g <- hwrite(data.frame("具体性ベスト10"=planresult[[2]][[2]],"具体性ワースト10"=planresult[[3]][[2]]), row.bgcolor="#b9c9fe", table.id="hor-minimalist-a")
table_k <- hwrite(data.frame("効果的ベスト10"=planresult[[4]][[2]],"効果的ワースト10"=planresult[[5]][[2]]), row.bgcolor="#b9c9fe", table.id="hor-minimalist-a")
hwrite(c(table_g, table_k), p, border=0)
hwrite(hmakeTag("button", "前へ"), p, link="result1.html")
closePage(p, splash=FALSE)

shell(file.path(tmpdir, "result1.html"))


##参加者ごとの具体性の評価
#具体性の最大値を、参加者の具体性とする
#sankasya_smp <- ddply(res, .(V1), summarise, max_weight_rate=max(weight_rate, na.rm=TRUE), mean_gutaisei=mean(gutaisei, na.rm=TRUE), mean_gutaisei_mod=mean(gutaisei_mod, na.rm=TRUE))
#sankasya_smp <- sankasya_smp[is.finite(sankasya_smp$max_weight_rate),]

#cor(sankasya_smp$max_weight_rate, sankasya_smp$mean_gutaisei)
#cor(sankasya_smp$max_weight_rate, sankasya_smp$mean_gutaisei_mod) #文字数で補正した方が若干相関は高い

#参加者と行動計画のマッチ
#行動計画の遵守状況（day0から1カ月時点での守れた日数、守れた間隔：


###支援者ごとの行動計画の均質性の比較(今後の研究課題)→時期による変化を確認する

#行動計画集合に対してエントロピーを算出→語の頻度×log2（語の頻度）
#研修効果による標準化が進むとエントロピーが低くなる可能性あり→時期による変化をみるとよい
#対象者の均質性と行動計画の均質性を比較する必要あり

#ENTROPY <- docDF("doc2", weight="idf4*norm", type=1) #コサイン正規化をかけている
#colSums(ENTROPY[,-(1:3)])
