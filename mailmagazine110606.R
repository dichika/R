library(googleVis)
library(hwriter)
library(ggplot2)


address <- "Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/01_PC"
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

setwd("Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/02_MATOME")
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

#直近5回分の結果のみグラフで表示
recent <- tail(df2$予定タイトル, n=5)
SEKISAN <- ggplot(data=df_plot2[df_plot2$予定タイトル%in%recent&df_plot2$経過日数<8,], aes(x=経過日数, y=開封率, group=name)) + geom_line(aes(color=name), size=2) + geom_point(aes(color=name), size=4) +ylab("開封率")


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
setwd(address)

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

setwd("Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/03_KENPO")
df3 <- read.csv(dir()[length(dir())], as.is=TRUE)
df3 <- merge(df3, data2, by.x="email", by.y="メールアドレス", all.x=TRUE)
df3$アクセス有無 <- ifelse(!is.na(df3$アクセス日時), 1, 0)
df3 <- subset(df3, kenponame!="デモンストレーション健康保険組合")
df3$kenponame <- gsub("健康保険組合|共済組合|振興・共済事業団|保健衛生部健康推進課","",df3$kenponame)

domain <- c("sangyokoyo", "jeed", "ehdo",
  "central-gd","goeidoboku","penta",
  "metro",
  "tomin",
  "mizuho",
  "ac.jp",
  "mitsui",
  "ni-net","oyc",
  "kao",
  "daiwa", "dir",
  "cas","nict",
  "soumu","stanfordalumni",
  "yamato", "ytckempo", "kuronekoyamato","yaw.co.jp",
  "mgc","japan-pionics","co-jsp",
  "gsk","viivhealthcare", 
  "hitachi","e-suruga",
  "diam", "dai-ichi",
  "knt")

  df3$place <- "家"
  df3[grep(paste(domain, collapse="|"),df3$email, perl=TRUE), ]$place <- "会社" #指定ドメインを含むアドレスを会社用アドレスとみなす


res_kenpo <- ddply(df3, .(kenponame), summarise, 開封数=sum(アクセス有無), 送付数=length(アクセス有無))
res_kenpo$開封率 <- round(res_kenpo$開封数*100 / res_kenpo$送付数, 1)
res_kenpo <- res_kenpo[order(res_kenpo$開封率, decreasing=TRUE),]
#res_kenpo <- rbind(res_kenpo, c("総数", sum(res_kenpo$開封数), sum(res_kenpo$送付数), round(sum(res_kenpo$開封数)*100/sum(res_kenpo$送付数), 1)))
#res_kenpo[,3:5] <- apply(res_kenpo[,3:5], 2, as.numeric)
KAIFUU <- ggplot(res_kenpo, aes(x=reorder(kenponame, 開封率))) + geom_bar(aes(y=開封率), stat="identity") + xlab("") +coord_flip()

res_kenpo2 <- ddply(df3, .(kenponame, place), summarise, 開封数=sum(アクセス有無), 送付数=length(アクセス有無))
res_kenpo2$開封率 <- round(res_kenpo2$開封数*100 / res_kenpo2$送付数, 1)
res_kenpo2 <- res_kenpo2[order(res_kenpo2$開封率, decreasing=TRUE),]

KAIFUU2 <- ggplot(res_kenpo2, aes(x=reorder(kenponame, 開封率))) +
  geom_bar(subset=.(place=="家"), aes(y=-開封率, fill=place)) + 
  geom_bar(subset=.(place=="会社"), aes(y=開封率, fill=place)) +
  geom_hline(yintercept=0, color="gray90")+
  xlab("") +coord_flip()+ylim(-100, 100)

#テーブル表示
res_table_kenpo <- gvisTable(res_kenpo, options=list(width=500, height=300))
res_table_kenpo$html$header <- gsub("charset=utf-8", "charset=shift-jis", res_table_kenpo$html$header)
res_table_kenpo$html$caption <- NULL
res_table_kenpo$html$footer <- NULL

res_table_kenpo2 <- gvisTable(res_kenpo2[order(res_kenpo2$place, res_kenpo2$開封率, res_kenpo2$kenponame , decreasing=TRUE),], options=list(width=500, height=300))
res_table_kenpo2$html$header <- gsub("charset=utf-8", "charset=shift-jis", res_table_kenpo$html$header)
res_table_kenpo2$html$caption <- NULL
res_table_kenpo2$html$footer <- NULL


setwd(address)

###健保ごとの開封率を確認する

#配送リストファイル読み込み
setwd("Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/03_KENPO")
haisou_num <- length(dir()) #配送リストの数

df31 <- NULL
for(i in 1:haisou_num){ #配送リスト読み込み
  df30 <- read.csv(dir()[i], as.is=TRUE) #健保別配送リスト
  df30$num <- i
  df31 <- rbind(df31, df30)
  }

#開封リストファイル読み込み
setwd(address)
kaihuu_num <- length(dir()) #開封リストの数

data21 <- NULL
for(i in (kaihuu_num-haisou_num+1):kaihuu_num){ #開封リスト読み込み
  data20 <- read.csv(dir()[i], as.is=TRUE) #健保別開封リスト
  data20$num <- i+haisou_num-kaihuu_num
  data20$date <- min(as.Date(substr(data20$アクセス日時, start=1, stop=10)))
  data21 <- rbind(data21, data20)
  }

num_list <- ddply(data21, .(date), summarise, num = mean(num))

df31 <- merge(df31, data21, by.x=c("email","num"), by.y=c("メールアドレス","num"), all.x=TRUE)
df31$アクセス有無 <- ifelse(!is.na(df31$アクセス日時), 1, 0)

df31 <- subset(df31, kenponame!="デモンストレーション健康保険組合")
df31$kenponame <- gsub("健康保険組合|共済組合|振興・共済事業団|保健衛生部健康推進課","",df31$kenponame) #健保名から「健保」を除く
res_kenpo2 <- ddply(df31, .(kenponame, num), summarise, 開封数=sum(アクセス有無), 送付数=length(アクセス有無))
res_kenpo2$開封数 <- as.numeric(res_kenpo2$開封数)
res_kenpo2$送付数 <- as.numeric(res_kenpo2$送付数)
res_kenpo2$開封率 <- round(res_kenpo2$開封数*100 / res_kenpo2$送付数, 1)
res_kenpo2 <- res_kenpo2[order(res_kenpo2$開封率, decreasing=TRUE),]
res_kenpo2[,2:4] <- apply(res_kenpo2[,2:4], 2, as.numeric)
recent_kenponame <- unique(df3$kenponame) #直近の健保リスト
res_kenpo2 <- subset(res_kenpo2, kenponame%in%recent_kenponame)
res_kenpo2 <- merge(res_kenpo2, num_list, all.x=TRUE)
res_kenpo2 <- ddply(res_kenpo2, .(kenponame), transform, 平均開封率=mean(開封率))
res_kenpo2$date <- as.POSIXct(res_kenpo2$date)
KAIFUU21 <- ggplot(res_kenpo2[res_kenpo2$平均開封率 >= 20, ], aes(x=date, y=開封率, group=kenponame)) + geom_line(aes(color=kenponame), size=2) +geom_point(aes(color=kenponame), size=4) +ylab("開封率") + scale_x_datetime(format="%m/%d")
KAIFUU22 <- ggplot(res_kenpo2[res_kenpo2$平均開封率 < 20 & res_kenpo2$平均開封率>=15, ], aes(x=date, y=開封率, group=kenponame)) + geom_line(aes(color=kenponame), size=2) +geom_point(aes(color=kenponame), size=4) +ylab("開封率") + scale_x_datetime(format="%m/%d")
KAIFUU23 <- ggplot(res_kenpo2[res_kenpo2$平均開封率 < 15 & res_kenpo2$平均開封率 >= 10, ], aes(x=date, y=開封率, group=kenponame)) + geom_line(aes(color=kenponame), size=2) +geom_point(aes(color=kenponame), size=4) +ylab("開封率") + scale_x_datetime(format="%m/%d")
KAIFUU24 <- ggplot(res_kenpo2[res_kenpo2$平均開封率 < 10, ], aes(x=date, y=開封率, group=kenponame)) + geom_line(aes(color=kenponame), size=2) +geom_point(aes(color=kenponame), size=4) +ylab("開封率") + scale_x_datetime(format="%m/%d")

#テーブル表示
res_kenpo2$date <- as.Date(res_kenpo2$date)
res_kenpo_kaihuu <- reshape(res_kenpo2[,c(2,5,6)], idvar="kenponame", timevar="date", direction="wide")
colnames(res_kenpo_kaihuu)[-1] <- gsub("開封率.", "", colnames(res_kenpo_kaihuu[-1]))
res_kenpo_kaihuu$平均開封率 <- round(rowMeans(res_kenpo_kaihuu[,-1]), 1)
res_kenpo_kaihuu <- res_kenpo_kaihuu[order(res_kenpo_kaihuu$平均開封率, decreasing=TRUE), ]
res_table_kaihuu <- gvisTable(res_kenpo_kaihuu, options=list(width=550, height=350))
res_table_kaihuu$html$header <- gsub("charset=utf-8", "charset=shift-jis", res_table_kenpo$html$header)
res_table_kaihuu$html$caption <- NULL
res_table_kaihuu$html$footer <- NULL

res_kenpo_kaihuu2 <- reshape(res_kenpo2[,c(2,3,6)], idvar="kenponame", timevar="date", direction="wide")
colnames(res_kenpo_kaihuu2)[-1] <- gsub("開封数.", "", colnames(res_kenpo_kaihuu2[-1]))
res_kenpo_kaihuu2 <- merge(res_kenpo_kaihuu2,res_kenpo_kaihuu, by="kenponame")
res_kenpo_kaihuu2 <- res_kenpo_kaihuu2[order(res_kenpo_kaihuu2$平均開封率, decreasing=TRUE), ]
res_kenpo_kaihuu2 <- res_kenpo_kaihuu2[,-grep("y|平均開封率",colnames(res_kenpo_kaihuu2))]
colnames(res_kenpo_kaihuu2)[-1] <- gsub(".x", "", colnames(res_kenpo_kaihuu2[-1]))
res_table_kaihuu2 <- gvisTable(res_kenpo_kaihuu2, options=list(width=550, height=350))
res_table_kaihuu2$html$header <- gsub("charset=utf-8", "charset=shift-jis", res_table_kenpo$html$header)
res_table_kaihuu2$html$caption <- NULL
res_table_kaihuu2$html$footer <- NULL


setwd(address)



####開封時間×健保ごとの開封状況

res_souhu <- subset(df3, アクセス有無==1)
res_souhu$dayhour <- format(res_souhu$アクセス日時, "%m%d%H")
res_souhud <- ddply(res_souhu, .(kenponame, dayhour), summarise, count = length(kenponame))
res_souhuda <- ddply(res_souhu, .(kenponame), summarise, all = length(kenponame))
res_souhu <- merge(res_souhud, res_souhuda)
res_souhu$prop <- round(res_souhu$count*100/res_souhu$all, 1)
res_souhu <- merge(res_souhu, res_kenpo)

KAIFUUKENPO1 <- ggplot(data=res_souhu[res_souhu$開封率>=20,], aes(x=dayhour, y=prop, group=kenponame)) + geom_line(aes(colour=kenponame)) + geom_point(aes(colour=kenponame)) + opts(legend.position=ifelse(length(unique(res_souhu$kenponame))>1,"none","right"), axis.text.x=theme_text(angle=90))+ facet_grid(kenponame~.) 
KAIFUUKENPO2 <- ggplot(data=res_souhu[res_souhu$開封率 < 20 & res_souhu$開封率>=15,], aes(x=dayhour, y=prop, group=kenponame)) + geom_line(aes(colour=kenponame)) + geom_point(aes(colour=kenponame)) + opts(legend.position=ifelse(length(unique(res_souhu$kenponame))>1,"none","right"), axis.text.x=theme_text(angle=90))+ facet_grid(kenponame~.)
KAIFUUKENPO3 <- ggplot(data=res_souhu[res_souhu$開封率 < 15 & res_souhu$開封率>=10,], aes(x=dayhour, y=prop, group=kenponame)) + geom_line(aes(colour=kenponame)) + geom_point(aes(colour=kenponame))  + opts(legend.position=ifelse(length(unique(res_souhu$kenponame))>1,"none","right"), axis.text.x=theme_text(angle=90))+ facet_grid(kenponame~.)
KAIFUUKENPO4 <- ggplot(data=res_souhu[res_souhu$開封率<10,], aes(x=dayhour, y=prop, group=kenponame)) + geom_line(aes(colour=kenponame)) + geom_point(aes(colour=kenponame))  + opts(legend.position=ifelse(length(unique(res_souhu$kenponame))>1,"none","right"), axis.text.x=theme_text(angle=90))+ facet_grid(kenponame~.)


###BMIおよびリスクごとの開封傾向を確認する

setwd("Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/04_RISK")

#リスクごとの開封傾向
df4 <- read.csv(dir()[length(dir())], as.is=TRUE)
df4$email <- gsub(" ", "", df4$email, perl=TRUE)
df4 <- subset(df4, kenponame!="デモンストレーション健康保険組合")
df4$kenponame <- gsub("健康保険組合|共済組合|振興・共済事業団|保健衛生部健康推進課","",df4$kenponame)
df4$bmi_st <- floor(df4$bmi)

test <- merge(df3, df4, all.x=TRUE)#配信リストとリスク者配信リストを結合
test$riskname <- ifelse(is.na(test$riskname), "なし", test$riskname)#リスク者配信リストにメールアドレスがない者は"なし"
test2 <- ddply(test, .(riskname, アクセス有無), summarise, count=length(riskname))#リスク×アクセスの集計
test3 <- ddply(test, .(riskname), summarise, all=length(riskname))#リスクごとの総計
test4 <- merge(test2, test3)#総計と集計を結合
test4$prop <- round(test4$count*100 / test4$all, 1)#割合を算出
test41 <- test4[test4$アクセス有無==1, ]#アクセスしている人のみ抽出
test41 <- subset(test41, complete.cases(test41))#NA除く
RISK <- ggplot(data=test41, aes(x=reorder(riskname, prop), y=prop)) + geom_bar(stat="identity") + xlab("") + ylab("開封率") + coord_flip()

#テーブル表示
test41 <- test41[order(test41$prop, decreasing=TRUE),]
test41[,2] <- NULL
colnames(test41) <- c("リスク", "開封数", "送付数", "開封率")
res_table2 <- gvisTable(test41, options=list(width=500, height=300))
res_table2$html$header <- gsub("charset=utf-8", "charset=shift-jis", res_table2$html$header)
res_table2$html$caption <- NULL
res_table2$html$footer <- NULL


#BMIごとの開封傾向
test5 <- ddply(test, .(bmi_st, アクセス有無), summarise, count=length(riskname))
test6 <- ddply(test, .(bmi_st), summarise, all=length(riskname))
test7 <- merge(test5, test6)
test7$prop <- round(test7$count * 100 / test7$all ,1)
test8 <- test7[test7$アクセス有無==1, ]
test8 <- subset(test8, complete.cases(test8))
BMI <- ggplot(data=test8[test8$bmi_st<=40,], aes(x=bmi_st, y=prop)) + geom_bar(stat="identity") + xlab("") + ylab("開封率") + coord_flip()

test9 <- test8[, -2]
colnames(test9) <- c("BMI", "開封数", "送付数", "開封率")

#テーブル表示
test9 <- test9[order(test9$BMI, decreasing=TRUE), ]
res_table3 <- gvisTable(test9, options=list(width=500, height=300))
res_table3$html$header <- gsub("charset=utf-8", "charset=shift-jis", res_table3$html$header)
res_table3$html$caption <- NULL
res_table3$html$footer <- NULL

###リスク/BMI別開封数経緯(2011/7/14追加)

#直近3回分の集計を実施

rdata <- NULL
for(c in 0:3){

  fnum <- nrow(df2)-c
  setwd("Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/03_KENPO")

  alist <- subset(df, name==df2[fnum,]$配信日)

  #配布リストの読み込み
  rnum <- length(dir())-c
  dfk1 <- read.csv(dir()[rnum], as.is=TRUE)  
  dfk1 <- subset(dfk1, kenponame!="デモンストレーション健康保険組合")
  dfk1$kenponame <- gsub("健康保険組合|共済組合|振興・共済事業団|保健衛生部健康推進課","",dfk1$kenponame)

  #リスク者リストの読み込み
  setwd("Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/04_RISK")
  dfr1 <- read.csv(dir()[rnum], as.is=TRUE)  
  dfr1 <- subset(dfr1, kenponame!="デモンストレーション健康保険組合")
  dfr1$kenponame <- gsub("健康保険組合|共済組合|振興・共済事業団|保健衛生部健康推進課","",dfr1$kenponame)
  dfr1$bmi_st <- floor(dfr1$bmi)

  #全配信リストとリスク者配信リストを結合
  rd <- merge(dfk1, dfr1, all.x=TRUE, by="email")
  rd$riskname <- ifelse(is.na(rd$riskname), "なし", rd$riskname) #リスク者配信リストにメールアドレスがない者は"なし"
  #アクセスリストと結合
  rd <- merge(rd, alist, all.x=TRUE, by.x="email", by.y="メールアドレス")
  rd$アクセス有無 <- ifelse(!is.na(rd$アクセス日時), 1, 0)
  rd2 <- ddply(rd, .(riskname, アクセス有無), summarise, count=length(riskname)) #リスク×アクセスの集計
  rd3 <- ddply(rd, .(riskname), summarise, all=length(riskname)) #リスクごとの総計
  rd4 <- merge(rd2, rd3)#総計と集計を結合
  rd4$prop <- round(rd4$count*100 / rd4$all, 1)#割合を算出
  rd41 <- rd4[rd4$アクセス有無==1, ]#アクセスしている人のみ抽出
  rd41 <- subset(rd41, complete.cases(rd41))#NA除く
  rd41$配信日 <- df2[fnum,]$配信日
  rdata <- rbind(rdata, rd41)
  }

rdata$アクセス有無 <- NULL
rdata$配信日2 <- as.character(rdata$配信日, format="%m/%d")
RISK2 <- ggplot(data=rdata, aes(x=配信日2, y=prop, group=riskname)) + geom_line(aes(color=riskname), size=2) + geom_point(aes(color=riskname), size=4) + xlab("送付日時") + ylab("開封率")

#テーブル表示
rdata$配信日2 <- NULL
colnames(rdata) <- c("リスク", "開封数", "送付数", "開封率", "配信日")
res_table4 <- gvisTable(rdata, options=list(width=500, height=300))
res_table4$html$header <- gsub("charset=utf-8", "charset=shift-jis", res_table2$html$header)
res_table4$html$caption <- NULL
res_table4$html$footer <- NULL


###時間別開封数（2011/06/15追加）

convert60 <- function(data){
  domain <- c("sangyokoyo", "jeed", "ehdo",
  "central-gd","goeidoboku","penta",
  "metro",
  "tomin",
  "mizuho",
  "ac.jp",
  "mitsui",
  "ni-net","oyc",
  "kao",
  "daiwa", "dir",
  "cas","nict",
  "soumu","stanfordalumni",
  "yamato", "ytckempo", "kuronekoyamato","yaw.co.jp",
  "mgc","japan-pionics","co-jsp",
  "gsk","viivhealthcare", 
  "hitachi","e-suruga",
  "diam", "dai-ichi",
  "knt")

  data$place <- "家"
  data[grep(paste(domain, collapse="|"),data$メールアドレス, perl=TRUE), ]$place <- "会社" #指定ドメインを含むアドレスを会社用アドレスとみなす

  data$hour <- as.numeric(format(data$アクセス日時, "%Y%m%d%H%M"))
  res <- ddply(data, .(place, hour), summarize, count=length(hour))
  res$hour <- as.POSIXct(as.character(res$hour), format="%Y%m%d%H%M")
  res$diff <- as.numeric(res$hour-min(res$hour))/60
  #res$diff <- as.numeric((res$hour-min(res$hour)))
  res <- res[res$diff <= 60, ]
  res$積算 <- 0
  res[res$place=="家",]$積算 <- cumsum(res[res$place=="家",]$count)
  res[res$place=="会社",]$積算 <- cumsum(res[res$place=="会社",]$count)
  invisible(res)
  }

data21 <- convert60(data2)#今回のデータ
data22 <- convert60(data)#前回のデータ

#成績が良かったデータ
dataG <- read.csv(paste(sep="", address, "/opencount_2011100393.csv"), as.is=TRUE)
dataG <- dataG[order(dataG$アクセス日時),]
dataG$アクセス日時 <- as.POSIXct(dataG$アクセス日時)
data23 <- convert60(dataG)

data21$timing <- paste("今回(", substr(min(data21$hour), 6, 10), ")送付", sep="")
data22$timing <- paste("前回(", substr(min(data22$hour), 6, 10), ")送付", sep="")
data23$timing <- paste("開封率過去最高(", substr(min(data23$hour), 6, 10), ")送付", sep="")
final <- rbind(data21, data22, data23)
JIKANKAIFUU <- ggplot(data=final, aes(x=diff, y=積算, group=timing)) + geom_line(size=2, aes(colour=timing)) + geom_point(aes(color=timing), size=4) + xlab("送付後経過時間(分)") + ylab("積算開封数") + opts(title="送付後1時間") + facet_wrap(~place)

###一日開封数

convert24 <- function(data){
  domain <- c("sangyokoyo", "jeed", "ehdo",
  "central-gd","goeidoboku","penta",
  "metro",
  "tomin",
  "mizuho",
  "ac.jp",
  "mitsui",
  "ni-net","oyc",
  "kao",
  "daiwa", "dir",
  "cas","nict",
  "soumu","stanfordalumni",
  "yamato", "ytckempo", "kuronekoyamato","yaw.co.jp",
  "mgc","japan-pionics","co-jsp",
  "gsk","viivhealthcare", 
  "hitachi","e-suruga",
  "diam", "dai-ichi",
  "knt")

  data$place <- "家"
  data[grep(paste(domain, collapse="|"),data$メールアドレス, perl=TRUE), ]$place <- "会社" #指定ドメインを含むアドレスを会社用アドレスとみなす

  data$hour <- as.numeric(format(data$アクセス日時, "%Y%m%d%H"))
  res <- ddply(data, .(hour, place), summarize, count=length(hour))
  res$hour <- as.POSIXlt(as.character(res$hour), format="%Y%m%d%H")
  res$diff <- as.numeric((res$hour-min(res$hour))/3600)
  res <- res[res$diff < 24, ]
  res$積算 <- 0
  res[res$place=="家",]$積算 <- cumsum(res[res$place=="家",]$count)
  res[res$place=="会社",]$積算 <- cumsum(res[res$place=="会社",]$count)
  invisible(res)
  }

data211 <- convert24(data2)#今回のデータ
data221 <- convert24(data)#前回のデータ
data231 <- convert24(dataG)
data211$timing <- paste("今回(", substr(min(data211$hour), 6, 10), ")送付", sep="")
data221$timing <- paste("前回(", substr(min(data221$hour), 6, 10), ")送付", sep="")
data231$timing <- paste("開封率過去最高(", substr(min(data231$hour), 6, 10), ")送付", sep="")
final2 <- rbind(data211, data221, data231)
JIKANKAIFUUDAY <- ggplot(data=final2, aes(x=diff, y=積算, group=timing)) + geom_line(size=2, aes(colour=timing)) + geom_point(aes(color=timing), size=4) + xlab("送付後経過時間(時)") + ylab("積算開封数") + opts(title="送付後24時間") + facet_wrap(~place)



####出力

#HTML
outdir <- "Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/output"
path <- paste(sep="", outdir, "/html/消さないで.html")
path2 <- paste(sep="", outdir, "/html/消さないで2.html")
path21 <- paste(sep="", outdir, "/html/消さないで21.html")
path211 <- paste(sep="", outdir, "/html/消さないで211.html")
path22 <- paste(sep="", outdir, "/html/消さないで22.html")
path23 <- paste(sep="", outdir, "/html/消さないで23.html")
path3 <- paste(sep="", outdir, "/html/消さないで3.html")
path4 <- paste(sep="", outdir, "/html/消さないで4.html")

write(unlist(res_plot$html), path)
write(unlist(res_table$html), path2)
write(unlist(res_table_kenpo$html), path21)
write(unlist(res_table_kenpo2$html), path211)
write(unlist(res_table_kaihuu$html), path22)
write(unlist(res_table_kaihuu2$html), path23)
write(unlist(res_table4$html), path3)
write(unlist(res_table3$html), path4)


#グラフ
outdir2 <- "Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/output/image"

png(paste(sep="", outdir2, "/時間別開封数.png"))
JIKANKAIFUU #経過時間（1時間）と開封数
dev.off()

png(paste(sep="", outdir2, "/時間別開封数日.png"))
JIKANKAIFUUDAY #経過時間（24時間）と開封数
dev.off()

png(paste(sep="", outdir2, "/積算.png"))
SEKISAN #経過日数と積算開封率
dev.off()

png(paste(sep="", outdir2, "/開封率.png"))
KAIFUU #健保別開封率
dev.off()

png(paste(sep="", outdir2, "/開封率2.png"))
KAIFUU2 #健保×家・会社別開封率
dev.off()

png(paste(sep="", outdir2, "/健保開封率1.png"))
KAIFUU21 #平均開封率が20%以上の健保
dev.off()

png(paste(sep="", outdir2, "/健保開封率2.png"))
KAIFUU22 #平均開封率が15%以上の健保
dev.off()

png(paste(sep="", outdir2, "/健保開封率3.png"))
KAIFUU23 #平均開封率が10%以上の健保
dev.off()

png(paste(sep="", outdir2, "/健保開封率4.png"))
KAIFUU24 #平均開封率が10%未満の健保
dev.off()

png(paste(sep="", outdir2, "/健保開封傾向1.png"))
KAIFUUKENPO1 #開封率が20%以上の健保
dev.off()

png(paste(sep="", outdir2, "/健保開封傾向2.png"))
KAIFUUKENPO2 #開封率が15%以上の健保
dev.off()

png(paste(sep="", outdir2, "/健保開封傾向3.png"))
KAIFUUKENPO3 #開封率が10%以上の健保
dev.off()

png(paste(sep="", outdir2, "/健保開封傾向4.png"))
KAIFUUKENPO4 #開封率が10%未満の健保
dev.off()

png(paste(sep="", outdir2, "/bmi.png"))
BMI #BMIと開封率
dev.off()

png(paste(sep="", outdir2, "/risk.png"))
RISK2 #riskと開封率
dev.off()



#説明文
setwd("Q:/Dev/20_サービス開発運営部/01_プロジェクト/1006-001_ishikawa_メールマガジン反応/work/01_PC")
#time1 <- file.info(dir()[length(dir())])$mtime #開封データの取得日時を取得
time1 <- max(data2$アクセス日時)
title <- sprintf("メルマガ(パソナビ)開封状況(%s時点)", format(time1, "%Y/%m/%d %H:%M:%S"))

txt1 <- sprintf("前回( %s 送付)開封者数(人)", lastdate)
txt11 <- sprintf("     %s", res1)
txt2 <- sprintf("今回( %s 送付)開封者数(人)", nowdate)
txt21 <- sprintf("     %s", res2)

txt3 <- sprintf("前回送付分からの継続開封者数は %s パーセント( %s 人中 %s 人)です", round(res_keizoku * 100 / res1, 1), res1, res_keizoku)

txt31 <- "送付後1時間/24時間時点の開封数"
txt4 <- "開封率×経過日数（グラフは直近5回分のみ表示）"
txt5 <- "健保別×開封率（母数は各健保の送付数）"
txt51 <- "注意：会社ごとのセキュリティに左右される可能性（たとえば第一生命）あり→健保別開封傾向をみて判断する"
txt511 <- "健保別×開封率(家・会社別)"
txt52 <- "健保別開封率推移"
txt6 <- "健保別×時間別×開封時間内訳(母数は各健保の開封数)"
txt61 <- "横軸は日時→060817であれば6月8日17時"
txt7 <- "リスク別開封率(直近4回分)"
txt8 <- "BMI別開封率（リスクを持っている人のみであることに注意）"

#出力
setwd(outdir)

p <- openPage("result.html", charset="CP932", dirname=outdir, link.javascript="http://www.google.com/jsapi")
hwrite(hmakeTag("style","

 .title{
  text-shadow: 3px 6px 8px #FFFFFF;
  background-color: #009933;
  border-radius: 10px;
  /*border: 4px solid #009933;*/
  padding: 4px;
  background:-webkit-gradient(linear, left top, left bottom, from(#FFFFFF), to(#009933));
  }
 .round{
  border-radius: 10px;
  /*border: 4px solid #009933;*/
  padding: 4px;
  background:-webkit-gradient(linear, left top, left bottom, from(#FFFFFF), to(#009933));
  }
 .result{
  font-size: 500%
  }
",type="text/css"),p)
hwrite(title, p, heading=1, class="title")
hwrite(txt1, p)
hwrite(txt11, p, br=TRUE, class="result")
hwrite(txt2, p)
hwrite(txt21, p, br=TRUE, class="result")
hwrite(txt3, p, br=TRUE, heading=4)

#これまでの開封数→今は表示していない
#hwrite(paste('<iframe src=', path, ' frameborder="0" width="1200" height="600" scrolling="no"></iframe>', sep=""), p, center=TRUE)

#日別積算開封率
hwrite(txt4, p, br=TRUE, heading=4, class="round")
GRAPH <- hwriteImage(paste(sep="", outdir2, "/積算.png"), width=600, height=400)
HTABLE <- hwrite(paste('<iframe src=', path2, ' frameborder="0" width="600" height="400" scrolling="no"></iframe>', sep=""))
hwrite(c(GRAPH, HTABLE), p, br=TRUE, border=0, center=TRUE)
hwrite("ここをクリックするとコメントを書き込めます", p, div=TRUE, id="editable", contenteditable="true", br=TRUE)

#時間別積算開封数
hwrite(txt31, p, br=TRUE, heading=4, class="round")
hwriteImage( c(paste(sep="", outdir2, "/時間別開封数.png"),paste(sep="", outdir2, "/時間別開封数日.png")) , p, p, br=TRUE, border=0, center=TRUE)
hwrite("ここをクリックするとコメントを書き込めます", p, div=TRUE, id="editable", contenteditable="true", br=TRUE)

#健保別開封率
hwrite(txt5, p, br=TRUE, heading=4, class="round")
hwrite(txt51, p, br=TRUE, heading=4)
GRAPH2 <- hwriteImage(paste(sep="", outdir2, "/開封率.png"), width=600, height=400)
HTABLE2 <- hwrite(paste('<iframe src=', path21, ' frameborder="0" width="600" height="400" scrolling="no"></iframe>', sep=""))
hwrite(c(GRAPH2, HTABLE2), p, br=TRUE, border=0, center=TRUE)
hwrite("ここをクリックするとコメントを書き込めます", p, div=TRUE, id="editable", contenteditable="true", br=TRUE)

#健保別開封率(家・会社)
hwrite(txt511, p, br=TRUE, heading=4, class="round")

GRAPH211 <- hwriteImage(paste(sep="", outdir2, "/開封率2.png"), width=600, height=400)
HTABLE211 <- hwrite(paste('<iframe src=', path211, ' frameborder="0" width="600" height="400" scrolling="no"></iframe>', sep=""))
hwrite(c(GRAPH211, HTABLE211), p, br=TRUE, border=0, center=TRUE)
hwrite("ここをクリックするとコメントを書き込めます", p, div=TRUE, id="editable", contenteditable="true", br=TRUE)


#健保別開封率比較→これまでの開封率を比較する
hwrite(txt52, p, br=TRUE, heading=4, class="round", class="round")
hwrite("", p, br=TRUE)
GRAPH21 <- hwriteImage(paste(sep="", outdir2, "/健保開封率1.png"), width=600, height=400)
GRAPH31 <- hwriteImage(paste(sep="", outdir2, "/健保開封率2.png"), width=600, height=400)
GRAPH41 <- hwriteImage(paste(sep="", outdir2, "/健保開封率3.png"), width=600, height=400)
GRAPH51 <- hwriteImage(paste(sep="", outdir2, "/健保開封率4.png"), width=600, height=400)
HTABLE21 <- hwrite(paste('<iframe src=', path22, ' frameborder="0" width="600" height="400" scrolling="no"></iframe>', sep=""))
HTABLE22 <- hwrite(paste('<iframe src=', path23, ' frameborder="0" width="600" height="400" scrolling="no"></iframe>', sep=""))
mat <- rbind(c("平均開封率20%以上", "平均開封率15-19.9%"), 
      c(GRAPH21, GRAPH31), 
      c("平均開封率10-14.9%", "平均開封率10%未満"),
      c(GRAPH41, GRAPH51),
      c("開封率", "開封数"),
      c(HTABLE21, HTABLE22))
hwrite(mat, p, br=TRUE, border=0, center=TRUE)
hwrite("ここをクリックするとコメントを書き込めます", p, div=TRUE, id="editable", contenteditable="true", br=TRUE)




#健保別開封分布
hwrite(txt6, p, br=TRUE, heading=4, class="round", class="round")
hwrite(txt61, p, br=TRUE)
hwrite("", p, br=TRUE)
GRAPH2 <- hwriteImage(paste(sep="", outdir2, "/健保開封傾向1.png"), width=600, height=400)
GRAPH3 <- hwriteImage(paste(sep="", outdir2, "/健保開封傾向2.png"), width=600, height=400)
GRAPH4 <- hwriteImage(paste(sep="", outdir2, "/健保開封傾向3.png"), width=600, height=400)
GRAPH5 <- hwriteImage(paste(sep="", outdir2, "/健保開封傾向4.png"), width=600, height=400)
mat <- rbind(c("開封率20%以上", "開封率15-19.9%"), c(GRAPH2, GRAPH3))
mat <- rbind(mat, c("開封率10-14.9%", "開封率10%未満"))
mat <- rbind(mat, c(GRAPH4, GRAPH5))
hwrite(mat, p, br=TRUE, border=0, center=TRUE)
hwrite("ここをクリックするとコメントを書き込めます", p, div=TRUE, id="editable", contenteditable="true", br=TRUE)

#リスク別開封率
hwrite(txt7, p, br=TRUE, heading=4, class="round")
GRAPH6 <- hwriteImage(paste(sep="", outdir2, "/risk.png"), width=600, height=400)
HTABLE3 <- hwrite(paste('<iframe src=', path3, ' frameborder="0" width="600" height="400" scrolling="no"></iframe>', sep=""))
hwrite(c(GRAPH6, HTABLE3), p, br=TRUE, border=0, center=TRUE)
hwrite("ここをクリックするとコメントを書き込めます", p, div=TRUE, id="editable", contenteditable="true", br=TRUE)

#BMI別開封率
hwrite(txt8, p, br=TRUE, heading=4, class="round")
GRAPH7 <- hwriteImage(paste(sep="", outdir2, "/bmi.png"), width=600, height=400)
HTABLE4 <- hwrite(paste('<iframe src=', path4, ' frameborder="0" width="600" height="400" scrolling="no"></iframe>', sep=""))
hwrite(c(GRAPH7, HTABLE4), p, br=TRUE, border=0, center=TRUE)
hwrite("ここをクリックするとコメントを書き込めます", p, div=TRUE, id="editable", contenteditable="true", br=TRUE)

closePage(p, splash=FALSE)