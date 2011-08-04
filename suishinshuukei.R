library(ggplot2)
library(batade)
library(digest)
library(stringr)
library(googleVis)

#集計

setwd("Q:/Pln/Analysis/01_hokensya/04_suishin/2011/02_output")

df <- read.csv("dall.csv", as.is=TRUE)

df$hname <- gsub("富士ﾌｲﾙﾑｸﾞﾙｰﾌﾟ", "富士フイルムグループ", df$hname)
df$hname <- gsub("ＴＤＫ", "TDK", df$hname)
df$hname <- gsub("ヤマト", "ヤマトグループ", df$hname)

#######################################################################

#基本属性
all <- ddply(df, .(hname), summarise, count=sum(!is.na(id)))
attr_age <- ddply(df, .(hname), summarise, age2010_mean=round(mean(age.y, na.rm=TRUE),1))
attr_sex <- ddply(df, .(hname, sex), summarise, age2009_mean=mean(age.x, na.rm=TRUE), count2009=sum(!is.na(age.x)), age2010_mean=mean(age.y, na.rm=TRUE), count2010=sum(!is.na(age.y)))

#性年齢ピラミッド
attr_age5 <- ddply(df, .(hname, sex, age5.y), summarise, count=sum(!is.na(id)))
attr_age5 <- merge(attr_age5, all, by="hname")
attr_age5 <- merge(attr_age5, attr_age, by="hname")

attr_age5$prop <- round(attr_age5$count.x *100 / attr_age5$count.y, 1) #全体に対する割合
attr_age5$sex <- ifelse(as.numeric(attr_age5$sex)==1, "男性", "女性")
attr_age5$hname2 <- paste(sep="", attr_age5$hname, "(", attr_age5$count.y, "人 平均年齢：", attr_age5$age2010_mean,")")

agepyramid <- ggplot(data=attr_age5, aes(age5.y))+
  geom_bar(subset=.(sex=="男性"), aes(y=prop, fill=factor(sex, levels=c("女性","男性"))), stat="identity")+
  geom_bar(subset=.(sex=="女性"), aes(y=-prop, fill=factor(sex, levels=c("女性","男性"))), stat="identity")+
  geom_hline(yintercept = 0, colour = "grey90") + opts(axis.text.x=theme_text(angle=90)) +coord_flip() +
  xlab("") + ylab("各セグメントの割合") + ylim(c(-50,50))+
  facet_wrap(~hname) + scale_fill_brewer(name="sex", palette="Set1") 




#####健康分布

#健康分布2010

kenpo <- ddply(df, .(hname, kaisoukaOB.y, kenkoubunpuRISK.y), summarise, count=sum(!is.na(id)))
kenpoall <- ddply(df, .(hname), summarise, count=sum(!is.na(id)))
result1 <- merge(kenpo, kenpoall, by="hname")
colnames(result1) <- c("hname", "kaisoukaOB","kenkoubunpuRISK", "count", "all")

all <- ddply(df, .(kaisoukaOB.y, kenkoubunpuRISK.y), summarise, count=sum(!is.na(id)))
all <- data.frame(hname="全体", all, Freq=nrow(df))
colnames(all) <- c("hname", "kaisoukaOB","kenkoubunpuRISK", "count", "all")

result1 <- rbind(result1, all)

result1$prop <- round(result1$count*100 / result1$all, 1)
result1$seg <- str_c(ifelse(result1$kaisoukaOB==1, "A", "B"), result1$kenkoubunpuRISK)
result1$year <- "2010"

p1 <- ggplot(data=result1, aes(hname))+
  geom_bar(subset=.(kaisoukaOB==2), aes(y=prop, fill=factor(kenkoubunpuRISK)), stat="identity")+
  geom_bar(subset=.(kaisoukaOB==1), aes(y=-prop, fill=factor(kenkoubunpuRISK)), stat="identity")+
  geom_hline(yintercept = 0, colour = "grey90") + opts(axis.text.x=theme_text(angle=90)) +coord_flip() +
  xlab("") + ylab("各セグメントの割合")

#肥満者の割合

result2 <- ddply(result1, .(hname, kaisoukaOB), summarise, prop=sum(prop))
result2$g <- ifelse(result2$hname=="全体", "0", "1")

p2 <- ggplot(data=result2, aes(reorder(as.character(hname), prop)))+
  geom_bar(subset=.(kaisoukaOB==2), aes(y=prop, fill=g), stat="identity")+
  coord_flip() + xlab("") + ylab("肥満者の割合") + opts(legend.position="none")


#健保ごとの2年間の変化
kenpo2009 <- ddply(df, .(hname, kaisoukaOB.x, kenkoubunpuRISK.x), summarise, count=sum(!is.na(id)))
kenpoall2009 <- ddply(df, .(hname), summarise, count=sum(!is.na(id)))
result1b <- merge(kenpo2009, kenpoall2009, by="hname")
colnames(result1b) <- c("hname", "kaisoukaOB","kenkoubunpuRISK", "count", "all")

all2009 <- ddply(df, .(kaisoukaOB.x, kenkoubunpuRISK.x), summarise, count=sum(!is.na(id)))
all2009 <- data.frame(hname="全体", all2009, Freq=nrow(df))
colnames(all2009) <- c("hname", "kaisoukaOB","kenkoubunpuRISK", "count", "all")

result1b <- rbind(result1b, all2009)

result1b$prop <- round(result1b$count*100 / result1b$all, 1)
result1b$seg <- str_c(ifelse(result1b$kaisoukaOB==1, "A", "B"), result1b$kenkoubunpuRISK)
result1b$year <- "2009"

result1c <- rbind(result1, result1b)
ggplot(data=result1c, aes(x=seg, group=year)) + geom_bar(subset=.(hname=="日立製作所"), aes(y=prop, fill=year), stat="identity", position="dodge")



####服薬率×コントロール率

#受診勧奨者服薬率

dr1 <- data.frame(ddply(df[df$jushinBP.y==1, ], .(hname), summarise, dr_rate=mean(drBP.y)), risk="血圧")
dr2 <- data.frame(ddply(df[df$jushinBG.y==1, ], .(hname), summarise, dr_rate=mean(drBG.y)), risk="血糖")
dr3 <- data.frame(ddply(df[df$jushinLP.y==1, ], .(hname), summarise, dr_rate=mean(drLP.y)), risk="脂質")
dr1all <- data.frame(hname="全体", dr_rate=mean(df[df$jushinBP.y==1, ]$drBP.y), risk="血圧")
dr2all <- data.frame(hname="全体", dr_rate=mean(df[df$jushinBG.y==1, ]$drBG.y), risk="血糖")
dr3all <- data.frame(hname="全体", dr_rate=mean(df[df$jushinLP.y==1, ]$drLP.y), risk="脂質")

result3 <- rbind(dr1, dr2, dr3, dr1all, dr2all, dr3all)
result3$g <- ifelse(result3$hname=="全体", "0", "1")

p3 <- ggplot(result3, aes(x=reorder(hname, dr_rate)))+geom_bar(subset=.(risk=="血圧"), aes(y=dr_rate, fill=g), stat="identity") + coord_flip() + scale_y_continuous(formatter="percent", name="受診勧奨者服薬率") + opts(legend.position="none") + xlab("血圧")
p4 <- ggplot(result3, aes(x=reorder(hname, dr_rate)))+geom_bar(subset=.(risk=="血糖"), aes(y=dr_rate, fill=g), stat="identity") + coord_flip() + scale_y_continuous(formatter="percent", name="受診勧奨者服薬率") + opts(legend.position="none") + xlab("血糖")
p5 <- ggplot(result3, aes(x=reorder(hname, dr_rate)))+geom_bar(subset=.(risk=="脂質"), aes(y=dr_rate, fill=g), stat="identity") + coord_flip() + scale_y_continuous(formatter="percent", name="受診勧奨者服薬率") + opts(legend.position="none") + xlab("脂質")

#服薬者コントロール率(コントロールされている割合であることに注意)

ju1 <- data.frame(ddply(df[df$drBP.y==1, ], .(hname), summarise, ju_rate=1-mean(jushinBP.y)), risk="血圧")
ju2 <- data.frame(ddply(df[df$drBG.y==1, ], .(hname), summarise, ju_rate=1-mean(jushinBG.y)), risk="血糖")
ju3 <- data.frame(ddply(df[df$drLP.y==1, ], .(hname), summarise, ju_rate=1-mean(jushinLP.y)), risk="脂質")
ju1all <- data.frame(hname="全体", ju_rate=1-mean(df[df$drBP.y==1, ]$jushinBP.y), risk="血圧")
ju2all <- data.frame(hname="全体", ju_rate=1-mean(df[df$drBG.y==1, ]$jushinBG.y), risk="血糖")
ju3all <- data.frame(hname="全体", ju_rate=1-mean(df[df$drLP.y==1, ]$jushinLP.y), risk="脂質")

result4 <- rbind(ju1, ju2, ju3, ju1all, ju2all, ju3all)
result4$g <- ifelse(result4$hname=="全体", "0", "1")

p6 <- ggplot(result4, aes(x=reorder(hname, ju_rate)))+geom_bar(subset=.(risk=="血圧"), aes(y=ju_rate, fill=g), stat="identity") + coord_flip() + scale_y_continuous(formatter="percent", name="服薬者コントロール率") + opts(legend.position="none") + xlab("血圧")
p7 <- ggplot(result4, aes(x=reorder(hname, ju_rate)))+geom_bar(subset=.(risk=="血糖"), aes(y=ju_rate, fill=g), stat="identity") + coord_flip() + scale_y_continuous(formatter="percent", name="服薬者コントロール率") + opts(legend.position="none") + xlab("血糖")
p8 <- ggplot(result4, aes(x=reorder(hname, ju_rate)))+geom_bar(subset=.(risk=="脂質"), aes(y=ju_rate, fill=g), stat="identity") + coord_flip() + scale_y_continuous(formatter="percent", name="服薬者コントロール率") + opts(legend.position="none") + xlab("脂質")

result5 <- merge(result3, result4)
p91 <- ggplot(data=result5, aes(x=dr_rate, y=ju_rate, label=hname)) + geom_point(subset=.(risk=="血圧"), aes(color=g))+ geom_text(subset=.(risk=="血圧")) + scale_y_continuous(formatter="percent", name="服薬者コントロール率") + scale_x_continuous(formatter="percent", name="受診勧奨者服薬率") + opts(legend.position="none")
p92 <- ggplot(data=result5, aes(x=dr_rate, y=ju_rate, label=hname)) + geom_point(subset=.(risk=="血糖"), aes(color=g))+ geom_text(subset=.(risk=="血糖")) + scale_y_continuous(formatter="percent", name="服薬者コントロール率") + scale_x_continuous(formatter="percent", name="受診勧奨者服薬率") + opts(legend.position="none")
p93 <- ggplot(data=result5, aes(x=dr_rate, y=ju_rate, label=hname)) + geom_point(subset=.(risk=="脂質"), aes(color=g))+ geom_text(subset=.(risk=="脂質")) + scale_y_continuous(formatter="percent", name="服薬者コントロール率") + scale_x_continuous(formatter="percent", name="受診勧奨者服薬率") + opts(legend.position="none")

####健保別年齢別平均値比較

result6 <- ddply(df, .(hname, sex, age5.y), summarise, m_sbp=mean(sbp.y, na.rm=TRUE), m_dbp=mean(sbp.y, na.rm=TRUE), m_tg=mean(tg.y, na.rm=TRUE), m_hdl=mean(hdl.y, na.rm=TRUE), m_ldl=mean(ldl.y, na.rm=TRUE), m_fbg=mean(fbg.y, na.rm=TRUE), m_hba1c=mean(hba1c.y, na.rm=TRUE), m_got=mean(got.y, na.rm=TRUE), m_gpt=mean(gpt.y, na.rm=TRUE), m_ggtp=mean(ggtp.y, na.rm=TRUE))
result6 <- melt(result6, id.var=c("hname", "sex", "age5.y"))
result6all <- ddply(df, .(sex, age5.y), summarise, m_sbp=mean(sbp.y, na.rm=TRUE), m_dbp=mean(sbp.y, na.rm=TRUE), m_tg=mean(tg.y, na.rm=TRUE), m_hdl=mean(hdl.y, na.rm=TRUE), m_ldl=mean(ldl.y, na.rm=TRUE), m_fbg=mean(fbg.y, na.rm=TRUE), m_hba1c=mean(hba1c.y, na.rm=TRUE), m_got=mean(got.y, na.rm=TRUE), m_gpt=mean(gpt.y, na.rm=TRUE), m_ggtp=mean(ggtp.y, na.rm=TRUE))
result6all <- melt(result6all, id.var=c("sex", "age5.y"))
result6all <- data.frame(hname="全体", result6all, stringsAsFactors=FALSE)
result6 <- rbind(result6, result6all)

#male
p10 <- ggplot(data=result6, aes(x=age5.y, y=value, group=hname)) + geom_line(subset=.(sex==1), aes(color=hname)) + facet_wrap(~variable, scales="free_y")

#female
p11 <- ggplot(data=result6, aes(x=age5.y, y=value, group=hname)) + geom_line(subset=.(sex==2), aes(color=hname)) + facet_wrap(~variable, scales="free_y")

#motion chartは時間表示の調整が必要
library(googleVis)
result6c <- cast(result6)
result6c$dummy <- result6c$age5.y
M1 <- gvisMotionChart(data=result6c[result6c$sex==1,], idvar="hname", timevar="dummy")
M1$html$header <- gsub("charset=utf-8", "charset=CP932", M1$html$header)
plot(M1)



################出力
#レポート作成
setwd("Q:/Pln/Analysis/01_hokensya/04_suishin/2011/02_output")
#画像の生成

png("性年齢別ピラミッド.png", width=800, height=600)
agepyramid
dev.off()

png("健康分布.png", width=800, height=600)
p1
dev.off()

png("肥満.png", width=800, height=600)
p2
dev.off()

png("血圧受勧服薬.png", width=800, height=600)
p3
dev.off()

png("血糖受勧服薬.png", width=800, height=600)
p4
dev.off()

png("脂質受勧服薬.png", width=800, height=600)
p5
dev.off()

png("血圧コントロール.png", width=800, height=600)
p6
dev.off()

png("血糖コントロール.png", width=800, height=600)
p7
dev.off()

png("脂質コントロール.png", width=800, height=600)
p8
dev.off()

png("コント×服薬血圧.png", width=800, height=600)
p91
dev.off()

png("コント×服薬血糖.png", width=800, height=600)
p92
dev.off()

png("コント×服薬脂質.png", width=800, height=600)
p93
dev.off()

#レイアウト指定
mat <- rbind(c(sprintf("推進する会（%s）時点", Sys.Date()),"LL"),
             c("性年齢構成","L"),
             c("性年齢別ピラミッド.png","L"),
             c("肥満者の割合","L"),
             c("肥満.png","L"),
             c("健康分布比較(←非肥満　肥満→)","L"),
             c("健康分布.png","L"),
             c("血圧受診勧奨服薬","L"),
             c("血圧受勧服薬.png","L"),
             c("血糖受診勧奨服薬","L"),
             c("血糖受勧服薬.png","L"),
             c("脂質受診勧奨服薬","L"),
             c("脂質受勧服薬.png","L"),
             c("血圧コントロール","L"),
             c("血圧コントロール.png","L"),
             c("血糖コントロール","L"),
             c("血糖コントロール.png","L"),
             c("脂質コントロール","L"),
             c("脂質コントロール.png","L"),
             c("コントロール×服薬率：血圧","L"),
             c("コント×服薬血圧.png","L"),
             c("コントロール×服薬率：血糖","L"),
             c("コント×服薬血糖.png","L"),
             c("コントロール×服薬率：脂質","L"),
             c("コント×服薬脂質.png","L"),
             c("ここにメッセージを入力します","S"))

#レポート生成
mkhtml("sample.html", mat, foot=TRUE)

#性年齢構成、健康分布、肥満比較、健康分布比較