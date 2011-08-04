library(googleVis)
library(hwriter)
library(ggplot2)


address <- "Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/PROSAPO/01_DATA"
setwd(address)

###メルマガの時系列開封率を比較

#とりあえず読み込んでデータを結合
filenum <- length(dir())

df <- NULL
  for(i in 1:filenum){
    df0 <- read.csv(dir()[i], as.is=TRUE)
    df0 <- df0[order(df0$アクセス日時),]
    df0$送付時 <- df0$アクセス日時[1]
    df <- rbind(df, df0)
    }

setwd("Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/PROSAPO/02_MATOME")
df2 <- read.csv("haishin.csv", as.is=TRUE)

df2$配信日 <- as.Date(df2$配信日)
setwd(address)

df$アクセス日時 <- as.POSIXlt(df$アクセス日時)
df$timing <- format(df$アクセス日時, "%Y/%m/%d")
df$name <- as.Date(df$送付時)
df$経過日数 <- as.numeric(difftime(as.Date(df$アクセス日時), df$name, unit="days"))


####日別×配信日時別カウント

#データ作成
df_plot <- ddply(df, .(name, timing), summarise, count=length(メールアドレス))
df_plot$cums <- unlist(by(df_plot[,"count"], df_plot[, "name"], cumsum))
df_plot <- merge(df_plot, df2, by.x="name", by.y="配信日", all.x=TRUE)
df_plot$name <- paste(sep="", format(as.POSIXct(df_plot$name), "%m%d"), "送付")
df_plot$開封率 <- round(df_plot$cums*100 / df_plot$送信成功数,1)


#google Visualization APIを利用してプロット
res_plot <- gvisAnnotatedTimeLine(df_plot,  datevar = "timing", numvar="count", idvar = "name", options=list(width=1000, height=500, displayExactValues=TRUE, legendPosition="newRow", thickness=3))
res_plot$html$header <- gsub("charset=utf-8", "charset=shift-jis", res_plot$html$header)
res_plot$html$caption <- NULL
res_plot$html$footer <- NULL

####経過日数別開封率比較

#データ作成
df_plot2 <- ddply(df, .(name, 経過日数, 送付時), summarise, count=length(メールアドレス))
df_plot2$cums <- unlist(by(df_plot2[,"count"], df_plot[, "name"], cumsum))
df_plot2 <- merge(df_plot2, df2, by.x="name", by.y="配信日", all.x=TRUE)
df_plot2$name <- paste(sep="", format(as.POSIXct(df_plot2$name), "%m%d"), "送付")
df_plot2$開封率 <- round(df_plot2$cums*100 / df_plot2$送信成功数,1)
SEKISAN <- ggplot(data=df_plot2, aes(x=経過日数, y=開封率, group=name)) + geom_line(aes(color=name), size=2) + geom_point(aes(color=name), size=4) +ylab("開封率")


#テーブル表示
df_plot2 <- df_plot2[order(df_plot2$経過日数), ]#reshape前に並び替えておく
TABLE <- reshape(df_plot2[,c(1,2,3,6,8)], idvar=c("name", "送付時", "予定タイトル"), timevar="経過日数", direction="wide")
colnames(TABLE) <- gsub("開封率\\.", "", colnames(TABLE))
colnames(TABLE)[1] <- "送付日"
TABLE <- TABLE[order(TABLE$送付日, decreasing=TRUE),]
TABLE$送付曜日 <- weekdays(as.POSIXlt(TABLE$送付時))
TABLE <- TABLE[, c(1:3, ncol(TABLE), 4:(ncol(TABLE)-1))]
res_table <- gvisTable(TABLE, options=list(width=500, height=300))
res_table$html$header <- gsub("charset=utf-8", "charset=shift-jis", res_table$html$header)
res_table$html$caption <- NULL
res_table$html$footer <- NULL





###最新とその前のデータを取得して継続開封率を算出

data <- read.csv(dir()[filenum-1], as.is=TRUE)
data2 <- read.csv(dir()[filenum], as.is=TRUE)

data <- data[order(data$アクセス日時),]
data2 <- data2[order(data2$アクセス日時),]

data$アクセス日時 <- as.POSIXct(data$アクセス日時)
data2$アクセス日時 <- as.POSIXct(data2$アクセス日時)

lastdate <- format(data$アクセス日時[1], "%Y/%m/%d")
nowdate <- format(data2$アクセス日時[1], "%Y/%m/%d")

res1 <- length(unique(data$メールアドレス))
res2 <- length(unique(data2$メールアドレス))

res_keizoku <- nrow(merge(data, data2, by="メールアドレス"))


###健保ごとの開封傾向を確認する

setwd("Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/PROSAPO/03_HAISHIN")
df3 <- read.csv(dir()[length(dir())], as.is=TRUE)
df3 <- merge(df3, data2, by.x="email", by.y="メールアドレス", all.x=TRUE)
df3$アクセス有無 <- ifelse(!is.na(df3$アクセス日時), 1, 0)
df3 <- subset(df3, kenponame!="デモンストレーション健康保険組合")
df3$kenponame <- gsub("健康保険組合|共済組合|振興・共済事業団|保健衛生部健康推進課","",df3$kenponame)
res_kenpo <- ddply(df3, .(kenponame), summarise, 開封数=sum(アクセス有無), 送付数=length(アクセス有無))
res_kenpo$開封数 <- as.numeric(res_kenpo$開封数)
res_kenpo$送付数 <- as.numeric(res_kenpo$送付数)
res_kenpo$開封率 <- round(res_kenpo$開封数*100 / res_kenpo$送付数, 1)
res_kenpo <- res_kenpo[order(res_kenpo$開封率),]
res_kenpo <- rbind(res_kenpo, c("総数", sum(res_kenpo$開封数), sum(res_kenpo$送付数), round(sum(res_kenpo$開封数)*100/sum(res_kenpo$送付数), 1)))
res_kenpo[,2:4] <- apply(res_kenpo[,2:4], 2, as.numeric)
KAIFUU <- ggplot(res_kenpo, aes(x=reorder(kenponame, 開封率), y=開封率)) + geom_bar() + coord_flip() + xlab("")

#テーブル表示
res_table_kenpo <- gvisTable(res_kenpo, options=list(width=500, height=300))
res_table_kenpo$html$header <- gsub("charset=utf-8", "charset=shift-jis", res_table_kenpo$html$header)
res_table_kenpo$html$caption <- NULL
res_table_kenpo$html$footer <- NULL

setwd(address)



####開封時間×健保ごとの開封状況

res_souhu <- subset(df3, アクセス有無==1)
res_souhu$dayhour <- format(res_souhu$アクセス日時, "%m%d%H")
res_souhud <- ddply(res_souhu, .(kenponame, dayhour), summarise, count = length(kenponame))
res_souhuda <- ddply(res_souhu, .(kenponame), summarise, all = length(kenponame))
res_souhu <- merge(res_souhud, res_souhuda)
res_souhu$prop <- round(res_souhu$count*100/res_souhu$all, 1)
res_souhu <- merge(res_souhu, res_kenpo)

kenpolist <- unique(res_souhu$kenponame)
kenpolist1 <- kenpolist[1:floor(length(kenpolist)/2)]
kenpolist2 <- kenpolist[(floor(length(kenpolist)/2)+1):length(kenpolist)]

KAIFUUKENPO1 <- ggplot(data=res_souhu[res_souhu$kenponame%in%kenpolist1, ], aes(x=dayhour, y=prop, group=kenponame)) + geom_line(aes(colour=kenponame)) + geom_point(aes(colour=kenponame)) + facet_grid(kenponame~.) + opts(legend.position="none", axis.text.x=theme_text(angle=90)) + xlab("日時")
KAIFUUKENPO2 <- ggplot(data=res_souhu[res_souhu$kenponame%in%kenpolist2, ], aes(x=dayhour, y=prop, group=kenponame)) + geom_line(aes(colour=kenponame)) + geom_point(aes(colour=kenponame)) + facet_grid(kenponame~.) + opts(legend.position="none", axis.text.x=theme_text(angle=90)) + xlab("日時")


####出力

#HTML
outdir <- "Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/PROSAPO/output"
path <- paste(sep="", outdir, "/html/消さないで.html")
path2 <- paste(sep="", outdir, "/html/消さないで2.html")
path21 <- paste(sep="", outdir, "/html/消さないで21.html")
path3 <- paste(sep="", outdir, "/html/消さないで3.html")
path4 <- paste(sep="", outdir, "/html/消さないで4.html")

write(unlist(res_plot$html), path)
write(unlist(res_table$html), path2)
write(unlist(res_table_kenpo$html), path21)


#グラフ
outdir2 <- "Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/PROSAPO/output/image"
png(paste(sep="", outdir2, "/積算.png"))
SEKISAN #経過日数と積算開封率
dev.off()

png(paste(sep="", outdir2, "/開封率.png"))
KAIFUU #経過日数と積算開封率
dev.off()

png(paste(sep="", outdir2, "/健保開封傾向1.png"))
KAIFUUKENPO1
dev.off()

png(paste(sep="", outdir2, "/健保開封傾向2.png"))
KAIFUUKENPO2
dev.off()


#説明文
setwd("Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/PROSAPO/01_DATA")
time1 <- file.info(dir()[length(dir())])$mtime #開封データの取得日時を取得
title <- sprintf("メルマガ(プロサポ)開封状況(%s時点)", format(time1, "%Y/%m/%d %H:%M:%S"))

txt1 <- sprintf("前回( %s 送付)開封者数は %s 人です", lastdate, res1)
txt2 <- sprintf("今回( %s 送付)開封者数は %s 人です", nowdate, res2)
txt3 <- sprintf("前回送付分からの継続開封者数は %s パーセント( %s 人中 %s 人)です", round(res_keizoku * 100 / res1, 1), res1, res_keizoku)
txt31 <- "これまでの開封数"
txt4 <- "開封率×経過日数"
txt5 <- "健保別×開封率（母数は各健保の送付数）"
txt6 <- "健保別×時間別×開封時間内訳(母数は各健保の開封数)"
txt61 <- "横軸は日時→060817であれば6月8日17時"

#出力
setwd(outdir)

p <- openPage("result.html", charset="CP932", dirname=outdir, link.javascript="http://www.google.com/jsapi")
hwrite(hmakeTag("style",
".round{
  border-radius: 10px;
  border: 4px solid #009933;
  padding: 4px;
  width: 1200px;
  }
 .title{
  text-shadow: 3px 6px 8px #FFFFFF;
  background-color: #009933;
  border-radius: 10px;
  border: 4px solid #009933;
  padding: 4px;
  width: 1200px;
  }
",type="text/css"),p)
hwrite(title, p, heading=1, class="title")
hwrite(txt1, p, br=TRUE, heading=4)
hwrite(txt2, p, br=TRUE, heading=4)
hwrite(txt3, p, br=TRUE, heading=4)

hwrite(txt31, p, br=TRUE, heading=4, class="round")
hwrite(paste('<iframe src=', path, ' frameborder="0" width="1200" height="600" scrolling="no"></iframe>', sep=""), p, center=TRUE)
hwrite("ここをクリックするとコメントを書き込めます", p, div=TRUE, id="editable", contenteditable="true", br=TRUE)


hwrite(txt4, p, br=TRUE, heading=4, class="round")
GRAPH <- hwriteImage(paste(sep="", outdir2, "/積算.png"), width=600, height=400)
HTABLE <- hwrite(paste('<iframe src=', path2, ' frameborder="0" width="600" height="400" scrolling="no"></iframe>', sep=""))
hwrite(c(GRAPH, HTABLE), p, br=TRUE, border=0)
hwrite("ここをクリックするとコメントを書き込めます", p, div=TRUE, id="editable", contenteditable="true", br=TRUE)

hwrite(txt5, p, br=TRUE, heading=4, class="round")
GRAPH2 <- hwriteImage(paste(sep="", outdir2, "/開封率.png"), width=600, height=400)
HTABLE2 <- hwrite(paste('<iframe src=', path21, ' frameborder="0" width="600" height="400" scrolling="no"></iframe>', sep=""))
hwrite(c(GRAPH2, HTABLE2), p, br=TRUE, border=0)
hwrite("ここをクリックするとコメントを書き込めます", p, div=TRUE, id="editable", contenteditable="true", br=TRUE)

hwrite(txt6, p, br=TRUE, heading=4, class="round")
hwrite(txt61, p, br=TRUE)
KEIKOU1 <- hwriteImage(paste(sep="", outdir2, "/健保開封傾向1.png"), width=600, height=400, br=TRUE)
KEIKOU2 <- hwriteImage(paste(sep="", outdir2, "/健保開封傾向2.png"), width=600, height=400, br=TRUE)
hwrite(c(KEIKOU1, KEIKOU2), p, br=TRUE, border=0)
hwrite("ここをクリックするとコメントを書き込めます", p, div=TRUE, id="editable", contenteditable="true", br=TRUE)

closePage(p, splash=FALSE)