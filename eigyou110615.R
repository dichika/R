library(ggplot2)

setwd("D:/downloads")

data <- read.csv("002_‰c‹Æ—p•½‹Ï’lŽZo110615.csv", as.is=TRUE)
date <- as.Date(data$Œ’f”NŒŽ“ú, format="%Y-%m-%d")

  sel <- function(x)max(as.numeric(x), na.rm=TRUE)
#  height <- as.numeric(data$g’·)
#  weight <- as.numeric(data$‘Ìd)
#  waist <- apply(data[, grep("• ˆÍ", colnames(data))], 1, sel)#Ž©ŒÈ‘ª’è‚àŠÜ‚Þ
#  bmi <- as.numeric(data$BMI)
  sbp <- apply(data[, grep("ŽûkŠúŒŒˆ³", colnames(data))], 1, sel)
  dbp <- apply(data[, grep("Šg’£ŠúŒŒˆ³", colnames(data))], 1, sel)
  hdl <- apply(data[, grep("‚g‚c‚k", colnames(data))], 1, sel)
#  ldl <- apply(data[, grep("LDL", colnames(data))], 1, sel)
  tg <- apply(data[, grep("’†«Ž‰–b", colnames(data))], 1, sel)
  fbg <- apply(data[, grep("‹ó• ŽžŒŒ“œ", colnames(data))], 1, sel)
  hba1c <- apply(data[, grep("‚g‚‚‚`1‚ƒ", colnames(data))], 1, sel)
#  ggtp <- apply(data[, grep("ƒÁ.GT", colnames(data))], 1, sel)
#  got <- apply(data[, grep("GOT", colnames(data))], 1, sel)
#  gpt <- apply(data[, grep("GPT", colnames(data))], 1, sel)
drugBP <- ifelse(data$•ž–ò1.ŒŒˆ³==1, 1,0)
drugBG <- ifelse(data$•ž–ò2.ŒŒ“œ==1, 1,0)
drugLP <- ifelse(data$•ž–ò3.Ž‰Ž¿==1, 1,0)

SMOKE <- ifelse(data$‹i‰Œ==1, 1, 0)
  #”N—îŽZoi‘®«ƒf[ƒ^‚ÉŠÜ‚Ü‚ê‚é”N“x––”N—î‚ðŽg—p‚·‚é‚±‚Æ‚É•ÏXj
  #age <- as.numeric(trunc((as.Date(base, "%Y%m%d") - data$¶”NŒŽ“ú)/365.25))
  age <- data$”N—î
  #5Î‚Ý”N—îƒtƒ‰ƒOÝ’è
  age5 <- as.numeric(trunc(age/5)*5)
sex <- ifelse(data$«•Ê=="’j", 1, 2)
df <- data.frame(sex, age, age5, sbp, dbp, hdl, tg, fbg, hba1c, drugBP, drugBG, drugLP,SMOKE)
df2 <- df[is.finite(rowSums(df)),]
df2$drug <- ifelse((df2$drugBP + df2$drugBG + df2$drugLP)>=1, 1, 0)

df2$sbpr <- ifelse( df2$sbp >= 130, 1, 0)
df2$dbpr <- ifelse( df2$dbp >= 85, 1, 0)
df2$hdlr <- ifelse( df2$hdl < 40, 1, 0)
df2$tgr <- ifelse( df2$tg>=150, 1, 0)
df2$hba1cr <- ifelse( df2$hba1c>=5.2, 1, 0)
df2$fbgr <- ifelse( df2$fbg>=100, 1, 0)

df2$riskBP <- ifelse( (df2$sbpr + df2$sbpr) >= 1 ,1 ,0 )
df2$riskBG <- ifelse( (df2$hba1cr + df2$fbgr) >= 1 ,1 ,0 )
df2$riskLP <- ifelse( (df2$hdlr + df2$tgr) >= 1 ,1 ,0 )

result <- ddply(df2, .(sex, age5), summarize, 
  ŒŒˆ³ƒŠƒXƒNŠY“–—¦ = round(mean(riskBP, na.rm=TRUE)*100, 1),
  ŒŒ“œƒŠƒXƒNŠY“–—¦ = round(mean(riskBG, na.rm=TRUE)*100, 1),
  Ž‰Ž¿ƒŠƒXƒNŠY“–—¦ = round(mean(riskLP, na.rm=TRUE)*100, 1),
  •ž–ò—¦ = round(mean(drug, na.rm=TRUE)*100, 1), 
  ŒŒˆ³•ž–ò—¦ = round(mean(drugBP, na.rm=TRUE)*100, 1), 
  ŒŒ“œ•ž–ò—¦ = round(mean(drugBG, na.rm=TRUE)*100, 1), 
  Ž‰Ž¿•ž–ò—¦ = round(mean(drugLP, na.rm=TRUE)*100, 1), 
  ‹i‰Œ—¦ = round(mean(SMOKE, na.rm=TRUE)*100, 1),
  ŽûkŠúŒŒˆ³ƒŠƒXƒNŠY“–—¦ = round(mean(sbpr, na.rm=TRUE)*100, 1),
  Šg’£ŠúŒŒˆ³ƒŠƒXƒNŠY“–—¦ = round(mean(dbpr, na.rm=TRUE)*100, 1),
  HDLƒŠƒXƒNŠY“–—¦ = round(mean(hdlr, na.rm=TRUE)*100, 1),
  ’†«Ž‰–bƒŠƒXƒNŠY“–—¦ = round(mean(tgr, na.rm=TRUE)*100, 1),
  ‹ó• ŽžŒŒ“œƒŠƒXƒNŠY“–—¦ = round(mean(fbgr, na.rm=TRUE)*100, 1),
  HbA1cƒŠƒXƒNŠY“–—¦ = round(mean(hba1cr, na.rm=TRUE)*100, 1),
  l” = length(hba1c))

write.csv(result, "ƒNƒsƒIƒŠƒXƒNŠY“–—¦110616.csv", row.names=FALSE)