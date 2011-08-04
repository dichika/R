library(ggplot2)
setwd("C:/Users/h1030/Desktop")

data <- read.csv("検診＋問診データb.csv", as.is=TRUE)
str(data)
#各人ごとに健診年から最小値を引く
df <- ddply(data, .(参加者ID, プログラム.コース), transform, 経過=健診年-min(健診年))

#検査値データをまとめる

weight <- df$体重
bmi <- df$ＢＭＩ
waist <- df$腹囲.実測.
sbp <- apply(df[, grep("最高血圧", colnames(df))], 1, mean, na.rm=TRUE)
dbp <- apply(df[, grep("最低血圧", colnames(df))], 1, mean, na.rm=TRUE)
tg <- df$中性脂肪..トリグリセリド.
hdl <- df$HDLコレステロール
ldl <- df$LDLコレステロール
got <- df$GOT..AST.
gpt <- df$GP.T.ALT.
ggtp <- df$γ.GT..γ.GTP.
fbg <- df$空腹時血糖
hba1c <- df$HbA1c..ヘモグロビンA1c.
dr_BP <- ifelse(df$血圧を下げる薬の使用の有無.1.はい.2.いいえ==1, 1, 0)
dr_BG <- ifelse(df$インスリン注射又は血糖を下げる薬の使用の有無.1.はい.2.いいえ==1, 1, 0)
dr_LP <- ifelse(df$コレステロールを下げる薬の使用の有無.1.はい.2.いいえ==1, 1, 0)
dr <- apply(data.frame(dr_BP, dr_BG, dr_LP), 1, sum, na.rm=TRUE)

data2 <- data.frame(id=df$参加者ID, course=df$プログラム.コース, weight, bmi, waist, sbp, dbp, tg, hdl, ldl, got, gpt, ggtp, fbg, hba1c, dr, keika=df$経過)
keikaid <- ddply(data2, .(id), summarise, keika=max(keika))

#重複参加者・服薬者を除いてなおかつデータが揃っている人を評価
#ggplot(subset(data2, chouhuku==0 & dr==0), aes(x=keika, y=weight, group=id)) + geom_point(aes(color=id)) + geom_line(aes(color=id)) + opts(legend.position="none")
list1 <- subset(keikaid, keika==1)$id
smp <- subset(data2[data2$id %in% list1,], dr==0)
smp.r <- reshape(smp, timevar="keika", direction="wide")
attach(smp.r)
hist(weight.1 - weight.0, freq=FALSE)
hist(bmi.1 - bmi.0, freq=FALSE)
hist(sbp.1 - sbp.0, freq=FALSE)
hist(dbp.1 - dbp.0, freq=FALSE)
hist(tg.1 - tg.0, freq=FALSE)
hist(hdl.1 - hdl.0, freq=FALSE)
hist(got.1 - got.0, freq=FALSE)
hist(gpt.1 - gpt.0, freq=FALSE)
hist(ggtp.1 - ggtp.0, freq=FALSE)
hist(fbg.1 - fbg.0, freq=FALSE)
hist(hba1c.1 - hba1c.0, freq=FALSE)

detach(smp.r)
#
?hist