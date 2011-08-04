
library(ggplot2)
library(batade)

#保健指導データ

setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/001_original/HOKENSHIDOU")

hoken2 <- read.csv("HOKENSHIDOU110509.csv", as.is = TRUE)
hoken2$生年月日 <- as.Date(as.character(hoken2$生年月日), format="%Y/%m/%d")
hoken2$age <- as.numeric(trunc((as.Date("20100401", "%Y%m%d") - hoken2$生年月日)/365.25))
hoken2$age <- ifelse(hoken2$age >= 70, 70, hoken2$age)
hoken2$age10 <- ifelse(hoken2$age >= 40,  trunc(hoken2$age/10)*10, 40)

nrow(hoken2)
table(hoken2$age10, useNA="ifany")

###############################################################

#健診データ


conv <- function(data){
  sel <- function(x)max(as.numeric(x), na.rm=TRUE)
  #height <- as.numeric(data$身長)
  weight <- as.numeric(data$体重)
  waist <- as.numeric(data$腹囲.実測.)
  bmi <- as.numeric(data$BMI)
  sbp <- apply(data[, grep("収縮期血圧", colnames(data))], 1, sel)
  dbp <- apply(data[, grep("拡張期血圧", colnames(data))], 1, sel)
  hdl <- apply(data[, grep("HDL|ＨＤＬ", colnames(data))], 1, sel)
  ldl <- apply(data[, grep("LDL|ＬＤＬ", colnames(data))], 1, sel)
  tg <- apply(data[, grep("中性脂肪", colnames(data))], 1, sel)
  fbg <- apply(data[, grep("空腹時血糖", colnames(data))], 1, sel)
  hba1c <- apply(data[, grep("HbA1c|ＨｂＡ1ｃ", colnames(data))], 1, sel)
  ggtp <- apply(data[, grep("γ.GT", colnames(data))], 1, sel)
  got <- apply(data[, grep("GOT", colnames(data))], 1, sel)
  gpt <- apply(data[, grep("GPT", colnames(data))], 1, sel)
  age <- data[, grep("年齢|年度末年齢", colnames(data))]
  age5 <- as.numeric(trunc(age/5)*5)
  drug <- apply(data[, grep("服薬", colnames(data))], 1, sum)
  res <- data.frame(生年月日=data$生年月日, 性別=data$性別, age, age5, weight, waist, bmi, sbp, dbp, hdl, ldl, tg, fbg, hba1c, ggtp, got, gpt, level=data$保健指導レベル名称, drug)
  invisible(res)
  }


#2009年度データ
data1 <- read.csv("D:/downloads/004_私学2009年度健診データ110701.csv", as.is=TRUE)
data1$健診年月日 <- as.Date(data1$健診年月日)
data1 <- data1[data1$健診年月日>="2009-04-01" & data1$健診年月日<"2010-04-01",]

#2010年度データ
data2 <- read.csv("D:/downloads/003_私学2010年度健診データ110701.csv", as.is=TRUE)
data2$健診年月日 <- as.Date(data2$健診年月日)
data2 <- data2[data2$健診年月日>="2010-04-01" & data2$健診年月日<"2011-04-01",]

d2009 <- conv(data1)
d2009$USER_ID <- data1$USER_ID
d2009$受診日 <- data1$健診年月日


d2010 <- conv(data2)
d2010$USER_ID <- data2$USER_ID
d2010$受診日 <- data2$健診年月日

#IDと記号番号を結合
code <- read.csv("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/001_original/私学有効データ.csv", as.is=TRUE)
colnames(code) <- c("USER_ID", "記号", "番号", "生年月日")
code$生年月日 <- as.Date(as.character(code$生年月日), format="%Y%m%d")

kenshin <- merge(d2009, d2010, by="USER_ID")

kenshin <- merge(kenshin, code)

#結合
kenshin[,2] <- NULL
kenshin <- subset(kenshin, drug.x==6 & drug.y==6) #非服薬のみ抽出

#保健指導対象者以外の変化もみたい場合は末尾のコードへ
final <- merge(hoken2, kenshin, by=c("記号","番号","生年月日"))



final$d_day <- as.numeric(final$受診日.y - final$受診日.x)

attach(final)
final$d_weight <- weight.y - weight.x
final$d_waist <- waist.y - waist.x
final$d_bmi <- bmi.y - bmi.x
final$d_sbp <- sbp.y - sbp.x
final$d_dbp <- dbp.y - dbp.x
final$d_hdl <- hdl.y - hdl.x
final$d_ldl <- ldl.y - ldl.x
final$d_tg <- tg.y - tg.x
final$d_fbg <- fbg.y - fbg.x
final$d_hba1c <- hba1c.y - hba1c.x
final$d_ggtp <- ggtp.y - ggtp.x
final$d_got <- got.y - got.x
final$d_gpt <- gpt.y - gpt.x
final$per3 <- ifelse( (wt_shokai - wt_saishuu)/wt_shokai >= 0.03, 1, 0)
final$per5 <- ifelse( (wt_shokai - wt_saishuu)/wt_shokai >= 0.05, 1, 0)
detach(final)



res1 <- ddply(final, .(kikan, 支援レベル), summarise, 
  m_weight = mean(d_weight[is.finite(d_weight)]),
  m_weightper = round(mean(d_weight[is.finite(d_weight)]*100 / weight.y[is.finite(d_weight)]), 1),
  m_waist = mean(d_waist[is.finite(d_waist)]),
  m_bmi = mean(d_bmi[is.finite(d_bmi)]),
  m_sbp = mean(d_sbp[is.finite(d_sbp)]),
  m_dbp = mean(d_dbp[is.finite(d_dbp)]),
  m_hdl = mean(d_hdl[is.finite(d_hdl)]),
  m_ldl = mean(d_sbp[is.finite(d_ldl)]),
  m_tg = mean(d_tg[is.finite(d_tg)]),
  m_fbg = mean(d_fbg[is.finite(d_fbg)]),
  m_hba1c = mean(d_hba1c[is.finite(d_hba1c)]),
  m_ggtp = mean(d_ggtp[is.finite(d_ggtp)]),
  m_got = mean(d_got[is.finite(d_got)]),
  m_gpt = mean(d_gpt[is.finite(d_gpt)]),
  count_weight = length(d_weight[is.finite(d_weight)]),
  count_waist = length(d_waist[is.finite(d_waist)]),
  count_bmi = length(d_bmi[is.finite(d_bmi)]),
  count_sbp = length(d_sbp[is.finite(d_sbp)]),
  count_dbp = length(d_dbp[is.finite(d_dbp)]),
  count_hdl = length(d_hdl[is.finite(d_hdl)]),
  count_ldl = length(d_sbp[is.finite(d_ldl)]),
  count_tg = length(d_tg[is.finite(d_tg)]),
  count_fbg = length(d_fbg[is.finite(d_fbg)]),
  count_hba1c = length(d_hba1c[is.finite(d_hba1c)]),
  count_ggtp = length(d_ggtp[is.finite(d_ggtp)]),
  count_got = length(d_got[is.finite(d_got)]),
  count_gpt = length(d_gpt[is.finite(d_gpt)])
  )

res2 <- ddply(final, .(kikan, 支援レベル, 性別), summarise, 
  m_weight = mean(d_weight[is.finite(d_weight)]),
  m_weightper = round(mean(d_weight[is.finite(d_weight)]*100 / weight.y[is.finite(d_weight)]), 1),
  m_waist = mean(d_waist[is.finite(d_waist)]),
  m_bmi = mean(d_bmi[is.finite(d_bmi)]),
  m_sbp = mean(d_sbp[is.finite(d_sbp)]),
  m_dbp = mean(d_dbp[is.finite(d_dbp)]),
  m_hdl = mean(d_hdl[is.finite(d_hdl)]),
  m_ldl = mean(d_sbp[is.finite(d_ldl)]),
  m_tg = mean(d_tg[is.finite(d_tg)]),
  m_fbg = mean(d_fbg[is.finite(d_fbg)]),
  m_hba1c = mean(d_hba1c[is.finite(d_hba1c)]),
  m_ggtp = mean(d_ggtp[is.finite(d_ggtp)]),
  m_got = mean(d_got[is.finite(d_got)]),
  m_gpt = mean(d_gpt[is.finite(d_gpt)]),
  count_weight = length(d_weight[is.finite(d_weight)]),
  count_waist = length(d_waist[is.finite(d_waist)]),
  count_bmi = length(d_bmi[is.finite(d_bmi)]),
  count_sbp = length(d_sbp[is.finite(d_sbp)]),
  count_dbp = length(d_dbp[is.finite(d_dbp)]),
  count_hdl = length(d_hdl[is.finite(d_hdl)]),
  count_ldl = length(d_sbp[is.finite(d_ldl)]),
  count_tg = length(d_tg[is.finite(d_tg)]),
  count_fbg = length(d_fbg[is.finite(d_fbg)]),
  count_hba1c = length(d_hba1c[is.finite(d_hba1c)]),
  count_ggtp = length(d_ggtp[is.finite(d_ggtp)]),
  count_got = length(d_got[is.finite(d_got)]),
  count_gpt = length(d_gpt[is.finite(d_gpt)])
  )

res3 <- ddply(final, .(kikan, 支援レベル, 性別, age10), summarise, 
  m_weight = mean(d_weight[is.finite(d_weight)]),
  m_weightper = round(mean(d_weight[is.finite(d_weight)]*100 / weight.y[is.finite(d_weight)]), 1),
  m_waist = mean(d_waist[is.finite(d_waist)]),
  m_bmi = mean(d_bmi[is.finite(d_bmi)]),
  m_sbp = mean(d_sbp[is.finite(d_sbp)]),
  m_dbp = mean(d_dbp[is.finite(d_dbp)]),
  m_hdl = mean(d_hdl[is.finite(d_hdl)]),
  m_ldl = mean(d_sbp[is.finite(d_ldl)]),
  m_tg = mean(d_tg[is.finite(d_tg)]),
  m_fbg = mean(d_fbg[is.finite(d_fbg)]),
  m_hba1c = mean(d_hba1c[is.finite(d_hba1c)]),
  m_ggtp = mean(d_ggtp[is.finite(d_ggtp)]),
  m_got = mean(d_got[is.finite(d_got)]),
  m_gpt = mean(d_gpt[is.finite(d_gpt)]),
  count_weight = length(d_weight[is.finite(d_weight)]),
  count_waist = length(d_waist[is.finite(d_waist)]),
  count_bmi = length(d_bmi[is.finite(d_bmi)]),
  count_sbp = length(d_sbp[is.finite(d_sbp)]),
  count_dbp = length(d_dbp[is.finite(d_dbp)]),
  count_hdl = length(d_hdl[is.finite(d_hdl)]),
  count_ldl = length(d_sbp[is.finite(d_ldl)]),
  count_tg = length(d_tg[is.finite(d_tg)]),
  count_fbg = length(d_fbg[is.finite(d_fbg)]),
  count_hba1c = length(d_hba1c[is.finite(d_hba1c)]),
  count_ggtp = length(d_ggtp[is.finite(d_ggtp)]),
  count_got = length(d_got[is.finite(d_got)]),
  count_gpt = length(d_gpt[is.finite(d_gpt)])
  )




#重複者確認、3%-5%減少者それぞれで同様の表を出力
#HCCのみ

#3%減

resh1 <- ddply(final, .(kikan, per3, 支援レベル), summarise, 
  m_weight = mean(d_weight[is.finite(d_weight)]),
  m_weightper = round(mean(d_weight[is.finite(d_weight)]*100 / weight.y[is.finite(d_weight)]), 1),
  m_waist = mean(d_waist[is.finite(d_waist)]),
  m_bmi = mean(d_bmi[is.finite(d_bmi)]),
  m_sbp = mean(d_sbp[is.finite(d_sbp)]),
  m_dbp = mean(d_dbp[is.finite(d_dbp)]),
  m_hdl = mean(d_hdl[is.finite(d_hdl)]),
  m_ldl = mean(d_sbp[is.finite(d_ldl)]),
  m_tg = mean(d_tg[is.finite(d_tg)]),
  m_fbg = mean(d_fbg[is.finite(d_fbg)]),
  m_hba1c = mean(d_hba1c[is.finite(d_hba1c)]),
  m_ggtp = mean(d_ggtp[is.finite(d_ggtp)]),
  m_got = mean(d_got[is.finite(d_got)]),
  m_gpt = mean(d_gpt[is.finite(d_gpt)]),
  count_weight = length(d_weight[is.finite(d_weight)]),
  count_waist = length(d_waist[is.finite(d_waist)]),
  count_bmi = length(d_bmi[is.finite(d_bmi)]),
  count_sbp = length(d_sbp[is.finite(d_sbp)]),
  count_dbp = length(d_dbp[is.finite(d_dbp)]),
  count_hdl = length(d_hdl[is.finite(d_hdl)]),
  count_ldl = length(d_sbp[is.finite(d_ldl)]),
  count_tg = length(d_tg[is.finite(d_tg)]),
  count_fbg = length(d_fbg[is.finite(d_fbg)]),
  count_hba1c = length(d_hba1c[is.finite(d_hba1c)]),
  count_ggtp = length(d_ggtp[is.finite(d_ggtp)]),
  count_got = length(d_got[is.finite(d_got)]),
  count_gpt = length(d_gpt[is.finite(d_gpt)])
  )

resh1 <- subset(resh1, kikan=="東京臨海病院+HCC")

resh2 <- ddply(final, .(kikan, per3, 支援レベル, 性別), summarise, 
  m_weight = mean(d_weight[is.finite(d_weight)]),
  m_weightper = round(mean(d_weight[is.finite(d_weight)]*100 / weight.y[is.finite(d_weight)]), 1),
  m_waist = mean(d_waist[is.finite(d_waist)]),
  m_bmi = mean(d_bmi[is.finite(d_bmi)]),
  m_sbp = mean(d_sbp[is.finite(d_sbp)]),
  m_dbp = mean(d_dbp[is.finite(d_dbp)]),
  m_hdl = mean(d_hdl[is.finite(d_hdl)]),
  m_ldl = mean(d_sbp[is.finite(d_ldl)]),
  m_tg = mean(d_tg[is.finite(d_tg)]),
  m_fbg = mean(d_fbg[is.finite(d_fbg)]),
  m_hba1c = mean(d_hba1c[is.finite(d_hba1c)]),
  m_ggtp = mean(d_ggtp[is.finite(d_ggtp)]),
  m_got = mean(d_got[is.finite(d_got)]),
  m_gpt = mean(d_gpt[is.finite(d_gpt)]),
  count_weight = length(d_weight[is.finite(d_weight)]),
  count_waist = length(d_waist[is.finite(d_waist)]),
  count_bmi = length(d_bmi[is.finite(d_bmi)]),
  count_sbp = length(d_sbp[is.finite(d_sbp)]),
  count_dbp = length(d_dbp[is.finite(d_dbp)]),
  count_hdl = length(d_hdl[is.finite(d_hdl)]),
  count_ldl = length(d_sbp[is.finite(d_ldl)]),
  count_tg = length(d_tg[is.finite(d_tg)]),
  count_fbg = length(d_fbg[is.finite(d_fbg)]),
  count_hba1c = length(d_hba1c[is.finite(d_hba1c)]),
  count_ggtp = length(d_ggtp[is.finite(d_ggtp)]),
  count_got = length(d_got[is.finite(d_got)]),
  count_gpt = length(d_gpt[is.finite(d_gpt)])
  )

resh2 <- subset(resh2, kikan=="東京臨海病院+HCC")


resh3 <- ddply(final, .(kikan, per3, 支援レベル, 性別, age10), summarise, 
  m_weight = mean(d_weight[is.finite(d_weight)]),
  m_weightper = round(mean(d_weight[is.finite(d_weight)]*100 / weight.y[is.finite(d_weight)]), 1),
  m_waist = mean(d_waist[is.finite(d_waist)]),
  m_bmi = mean(d_bmi[is.finite(d_bmi)]),
  m_sbp = mean(d_sbp[is.finite(d_sbp)]),
  m_dbp = mean(d_dbp[is.finite(d_dbp)]),
  m_hdl = mean(d_hdl[is.finite(d_hdl)]),
  m_ldl = mean(d_sbp[is.finite(d_ldl)]),
  m_tg = mean(d_tg[is.finite(d_tg)]),
  m_fbg = mean(d_fbg[is.finite(d_fbg)]),
  m_hba1c = mean(d_hba1c[is.finite(d_hba1c)]),
  m_ggtp = mean(d_ggtp[is.finite(d_ggtp)]),
  m_got = mean(d_got[is.finite(d_got)]),
  m_gpt = mean(d_gpt[is.finite(d_gpt)]),
  count_weight = length(d_weight[is.finite(d_weight)]),
  count_waist = length(d_waist[is.finite(d_waist)]),
  count_bmi = length(d_bmi[is.finite(d_bmi)]),
  count_sbp = length(d_sbp[is.finite(d_sbp)]),
  count_dbp = length(d_dbp[is.finite(d_dbp)]),
  count_hdl = length(d_hdl[is.finite(d_hdl)]),
  count_ldl = length(d_sbp[is.finite(d_ldl)]),
  count_tg = length(d_tg[is.finite(d_tg)]),
  count_fbg = length(d_fbg[is.finite(d_fbg)]),
  count_hba1c = length(d_hba1c[is.finite(d_hba1c)]),
  count_ggtp = length(d_ggtp[is.finite(d_ggtp)]),
  count_got = length(d_got[is.finite(d_got)]),
  count_gpt = length(d_gpt[is.finite(d_gpt)])
  )

resh3 <- subset(resh3, kikan=="東京臨海病院+HCC")



#5%減

resh11 <- ddply(final, .(kikan, per5, 支援レベル), summarise, 
  m_weight = mean(d_weight[is.finite(d_weight)]),
  m_weightper = round(mean(d_weight[is.finite(d_weight)]*100 / weight.y[is.finite(d_weight)]), 1),
  m_waist = mean(d_waist[is.finite(d_waist)]),
  m_bmi = mean(d_bmi[is.finite(d_bmi)]),
  m_sbp = mean(d_sbp[is.finite(d_sbp)]),
  m_dbp = mean(d_dbp[is.finite(d_dbp)]),
  m_hdl = mean(d_hdl[is.finite(d_hdl)]),
  m_ldl = mean(d_sbp[is.finite(d_ldl)]),
  m_tg = mean(d_tg[is.finite(d_tg)]),
  m_fbg = mean(d_fbg[is.finite(d_fbg)]),
  m_hba1c = mean(d_hba1c[is.finite(d_hba1c)]),
  m_ggtp = mean(d_ggtp[is.finite(d_ggtp)]),
  m_got = mean(d_got[is.finite(d_got)]),
  m_gpt = mean(d_gpt[is.finite(d_gpt)]),
  count_weight = length(d_weight[is.finite(d_weight)]),
  count_waist = length(d_waist[is.finite(d_waist)]),
  count_bmi = length(d_bmi[is.finite(d_bmi)]),
  count_sbp = length(d_sbp[is.finite(d_sbp)]),
  count_dbp = length(d_dbp[is.finite(d_dbp)]),
  count_hdl = length(d_hdl[is.finite(d_hdl)]),
  count_ldl = length(d_sbp[is.finite(d_ldl)]),
  count_tg = length(d_tg[is.finite(d_tg)]),
  count_fbg = length(d_fbg[is.finite(d_fbg)]),
  count_hba1c = length(d_hba1c[is.finite(d_hba1c)]),
  count_ggtp = length(d_ggtp[is.finite(d_ggtp)]),
  count_got = length(d_got[is.finite(d_got)]),
  count_gpt = length(d_gpt[is.finite(d_gpt)])
  )

resh11 <- subset(resh11, kikan=="東京臨海病院+HCC")

resh21 <- ddply(final, .(kikan, per5, 支援レベル, 性別), summarise, 
  m_weight = mean(d_weight[is.finite(d_weight)]),
  m_weightper = round(mean(d_weight[is.finite(d_weight)]*100 / weight.y[is.finite(d_weight)]), 1),
  m_waist = mean(d_waist[is.finite(d_waist)]),
  m_bmi = mean(d_bmi[is.finite(d_bmi)]),
  m_sbp = mean(d_sbp[is.finite(d_sbp)]),
  m_dbp = mean(d_dbp[is.finite(d_dbp)]),
  m_hdl = mean(d_hdl[is.finite(d_hdl)]),
  m_ldl = mean(d_sbp[is.finite(d_ldl)]),
  m_tg = mean(d_tg[is.finite(d_tg)]),
  m_fbg = mean(d_fbg[is.finite(d_fbg)]),
  m_hba1c = mean(d_hba1c[is.finite(d_hba1c)]),
  m_ggtp = mean(d_ggtp[is.finite(d_ggtp)]),
  m_got = mean(d_got[is.finite(d_got)]),
  m_gpt = mean(d_gpt[is.finite(d_gpt)]),
  count_weight = length(d_weight[is.finite(d_weight)]),
  count_waist = length(d_waist[is.finite(d_waist)]),
  count_bmi = length(d_bmi[is.finite(d_bmi)]),
  count_sbp = length(d_sbp[is.finite(d_sbp)]),
  count_dbp = length(d_dbp[is.finite(d_dbp)]),
  count_hdl = length(d_hdl[is.finite(d_hdl)]),
  count_ldl = length(d_sbp[is.finite(d_ldl)]),
  count_tg = length(d_tg[is.finite(d_tg)]),
  count_fbg = length(d_fbg[is.finite(d_fbg)]),
  count_hba1c = length(d_hba1c[is.finite(d_hba1c)]),
  count_ggtp = length(d_ggtp[is.finite(d_ggtp)]),
  count_got = length(d_got[is.finite(d_got)]),
  count_gpt = length(d_gpt[is.finite(d_gpt)])
  )

resh21 <- subset(resh21, kikan=="東京臨海病院+HCC")


resh31 <- ddply(final, .(kikan, per5, 支援レベル, 性別, age10), summarise, 
  m_weight = mean(d_weight[is.finite(d_weight)]),
  m_weightper = round(mean(d_weight[is.finite(d_weight)]*100 / weight.y[is.finite(d_weight)]), 1),
  m_waist = mean(d_waist[is.finite(d_waist)]),
  m_bmi = mean(d_bmi[is.finite(d_bmi)]),
  m_sbp = mean(d_sbp[is.finite(d_sbp)]),
  m_dbp = mean(d_dbp[is.finite(d_dbp)]),
  m_hdl = mean(d_hdl[is.finite(d_hdl)]),
  m_ldl = mean(d_sbp[is.finite(d_ldl)]),
  m_tg = mean(d_tg[is.finite(d_tg)]),
  m_fbg = mean(d_fbg[is.finite(d_fbg)]),
  m_hba1c = mean(d_hba1c[is.finite(d_hba1c)]),
  m_ggtp = mean(d_ggtp[is.finite(d_ggtp)]),
  m_got = mean(d_got[is.finite(d_got)]),
  m_gpt = mean(d_gpt[is.finite(d_gpt)]),
  count_weight = length(d_weight[is.finite(d_weight)]),
  count_waist = length(d_waist[is.finite(d_waist)]),
  count_bmi = length(d_bmi[is.finite(d_bmi)]),
  count_sbp = length(d_sbp[is.finite(d_sbp)]),
  count_dbp = length(d_dbp[is.finite(d_dbp)]),
  count_hdl = length(d_hdl[is.finite(d_hdl)]),
  count_ldl = length(d_sbp[is.finite(d_ldl)]),
  count_tg = length(d_tg[is.finite(d_tg)]),
  count_fbg = length(d_fbg[is.finite(d_fbg)]),
  count_hba1c = length(d_hba1c[is.finite(d_hba1c)]),
  count_ggtp = length(d_ggtp[is.finite(d_ggtp)]),
  count_got = length(d_got[is.finite(d_got)]),
  count_gpt = length(d_gpt[is.finite(d_gpt)])
  )

resh31 <- subset(resh31, kikan=="東京臨海病院+HCC")

setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/002_output")

#保健指導機関別結果
write.csv(res1, "result1.csv",row.names=FALSE)
write.csv(res2, "result2.csv",row.names=FALSE)
write.csv(res3, "result3.csv",row.names=FALSE)

#3%減少
write.csv(resh1, "resulth1.csv",row.names=FALSE)
write.csv(resh2, "resulth2.csv",row.names=FALSE)
write.csv(resh3, "resulth3.csv",row.names=FALSE)

#5%減少
write.csv(resh11, "resulth11.csv",row.names=FALSE)
write.csv(resh21, "resulth21.csv",row.names=FALSE)
write.csv(resh31, "resulth31.csv",row.names=FALSE)



########################################################
#保健指導参加者の階層化移動について

#final2 <- merge(kenshin, hoken2, by=c("記号","番号","生年月日"), all.x=TRUE)
setwd("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/002_output/data")
write.csv(final2, "Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/002_output/data/final2.csv", row.names=FALSE)


final2 <- read.csv(final2, "Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/002_output/data/final2.csv", as.is=TRUE)

final2$sidou <- ifelse(!is.na(final2$初回面接の実施日付), 1, 0)

#保健指導参加者と非参加者での改善率を比較する
df <- data.frame(table(final2$level.x, final2$level.y, final2$sidou))
hoken_res <- subset(df, Var1!="判定不能" & Var2!="判定不能" & Var2!="")
hoken_res$対象2009 <- ifelse(hoken_res$Var1=="なし",0,1)
hoken_res$対象2010 <- ifelse(hoken_res$Var2=="なし",0,1)
hoken_res <- ddply(hoken_res, .(Var3), transform, all=sum(Freq))
hoken_res$per_all <- round(hoken_res$Freq*100 / hoken_res$all, 1)
hoken_res2 <- hoken_res[hoken_res$対象2009==1, ]
kaizenritu <- ddply(hoken_res2, .(Var3, 対象2010), summarise, kaizen=sum(per_all))
kaizenritu <- kaizenritu[kaizenritu$対象2010==0, ]
kaizenritu$指導 <- ifelse(kaizenritu$Var3==1, "指導あり", "指導なし")
ggplot(kaizenritu, aes(x=reorder(指導, kaizen), y=kaizen, label=kaizen)) + geom_bar(stat="identity") + geom_text(aes(y=kaizen+2)) + coord_flip() +xlab("") + ylab("改善率")  + opts(title="参加有無×保健指導改善率")

#機関別の改善率を比較する
df2 <- data.frame(table(final2$level.x, final2$level.y, final2$kikan, useNA="ifany"))
hoken_res_kikan <- subset(df2, Var1!="判定不能" & Var2!="判定不能" & Var2!="")
hoken_res_kikan$対象2009 <- ifelse(hoken_res_kikan$Var1=="なし",0,1)
hoken_res_kikan$対象2010 <- ifelse(hoken_res_kikan$Var2=="なし",0,1)
hoken_res_kikan <- ddply(hoken_res_kikan, .(Var3, Var1), transform, all=sum(Freq))
hoken_res_kikan2 <- hoken_res_kikan[hoken_res_kikan$対象2009==1 & hoken_res_kikan$対象2010==0, ]
hoken_res_kikan2$kaizen <- round(hoken_res_kikan2$Freq*100 / hoken_res_kikan2$all, 1)
hoken_res_kikan2$kikan <- ifelse(is.na(hoken_res_kikan2$Var3), "非参加", as.character(hoken_res_kikan2$Var3))
ggplot(data=hoken_res_kikan2, aes(x=reorder(kikan, kaizen),label=kaizen)) + geom_bar(stat="identity", aes( y=kaizen)) + geom_text( aes(y=kaizen+5)) + facet_wrap(~Var1) + coord_flip() +xlab("") + ylab("改善率")  + opts(title="機関×保健指導改善率")