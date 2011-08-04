

#健診フラグ設定

convk <- function(data){
  #require(digest)
  code2 <- read.csv("Q:/Pln/Analysis/01_hokensya/01_kenpo/10_三菱電機/01_data/code2.csv", as.is=TRUE)

  data$生年月日 <- as.Date(as.character(data$生年月日), format="%Y/%m/%d")
  data$健診受診日 <- as.Date(as.character(data$健診受診日), format="%Y/%m/%d")


  #属性
  haihu <- data$配布
  haihu <- ifelse(is.na(haihu), 999, haihu) #NAを999で置換
  kigou <- data$記号
  birth <- data$生年月日
  jushinbi <- data$健診受診日
  nendo <- ifelse(months(jushinbi) %in% c("1月","2月","3月"), as.numeric(substr(as.character(jushinbi), start=1, stop=4))-1, as.numeric(substr(as.character(jushinbi), start=1, stop=4)))

  age <- as.numeric(trunc((as.Date(paste(sep="-", nendo+1, "03-31")) - birth)/365.25))
  age5 <- as.numeric(trunc(age/5)*5)
  data$性別 <- gsub("男|男性", 1, data$性別)
  data$性別 <- gsub("女|女性", 2, data$性別)
  bangou <- data$被保険者番号


  #検査値
  height <- as.numeric(data$身長)
  weight <- as.numeric(data$体重)
  waist <- as.numeric(data$腹囲)
  bmi <- as.numeric(data$BMI)
  sbp <- as.numeric(data$血圧_収縮期)
  dbp <- as.numeric(data$血圧_拡張期)
  tg <- as.numeric(data$中性脂肪)
  hdl <- as.numeric(data$HDL.コレステロール)
  ldl <- as.numeric(data$LDL.コレステロール)
  fbg <- as.numeric(data$空腹時血糖)
  hba1c <- as.numeric(data$HbA1c)
  got <- as.numeric(data$GOT)
  gpt <- as.numeric(data$GPT)
  ggtp <- as.numeric(data$γ.GPT)

  #問診の処理
  data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))] <- apply(data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))], 2, function(x)gsub("はい|1|速い", 1, x))
  data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))] <- apply(data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))], 2, function(x)gsub("いいえ|2|ふつう|遅い", 0, x))
  data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))] <- apply(data[, grep("服薬|喫煙|既往歴|貧血|体重変化|X1年の体重変化|食べ方|朝食", colnames(data))], 2, function(x)as.numeric(x))

  data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))] <- apply(data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))], 2, function(x)gsub("はい|1", 0, x))
  data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))] <- apply(data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))], 2, function(x)gsub("いいえ|2", 1, x))
  data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))] <- apply(data[, grep("運動習慣|歩行|歩行速度|睡眠|指導希望", colnames(data))], 2, function(x)as.numeric(x))


  #健診項目・問診の揃い状況＋血糖項目有無チェック
  df <- data.frame(stringsAsFactors=FALSE, kigou, haihu, bangou, sex=data$性別, birth, jushinbi, nendo, age, age5, waist, bmi, sbp, dbp, tg, hdl, ldl, got, gpt, ggtp, fbg, hba1c, drBP=data$服薬_血圧, drBG=data$服薬_血糖, drLP=data$服薬_脂質, SM=data$喫煙)
  #df <- subset(df, age>=40 & age<=64)
  #df <- subset(df, complete.cases(df[,-c(19,20)]) & (as.numeric(!is.na(fbg)) + as.numeric(!is.na(hba1c)))>=1)


  #検査項目リスク設定（保健指導・受診勧奨）
  waisth <- ifelse(((df$waist >= 85) & (df$sex==1))|((df$waist >= 90) & (df$sex==2)), 1, 0)
  bmih <- ifelse(df$bmi >= 25, 1, 0)
  sbph <- ifelse(df$sbp >= 130, 1, 0)
  sbpj <- ifelse(df$sbp >= 140, 1, 0)
  dbph <- ifelse(df$dbp >= 85, 1, 0)
  dbpj <- ifelse(df$dbp >= 90, 1, 0)
  hdlh <- ifelse(-df$hdl >= -39, 1, 0)
  hdlj <- ifelse(-df$hdl >= -34, 1, 0)
  ldlh <- ifelse(df$ldl >= 130, 1, 0)
  ldlj <- ifelse(df$ldl >= 140, 1, 0)
  tgh <- ifelse(df$tg >= 150, 1, 0)
  tgj <- ifelse(df$tg >= 300, 1, 0)
  fbgh <- ifelse(df$fbg >= 100, 1, 0)
  fbgj <- ifelse(df$fbg >= 126, 1, 0)
  hba1ch <- ifelse(df$hba1c >= 5.2, 1, 0)
  hba1cj <- ifelse(df$hba1c >= 6.1, 1, 0)
  ggtph <- ifelse(df$ggtp >= 51, 1, 0)
  ggtpj <- ifelse(df$ggtp >= 101, 1, 0)
  goth <- ifelse(df$got >= 31, 1, 0)
  gotj <- ifelse(df$got >= 51, 1, 0)
  gpth <- ifelse(df$gpt >= 31, 1, 0)
  gptj <- ifelse(df$gpt >= 51, 1, 0)

  waisth <- ifelse(is.na(waisth), 0, waisth)
  bmih <- ifelse(is.na(bmih), 0, bmih)
  sbph <- ifelse(is.na(sbph), 0, sbph)
  sbpj <- ifelse(is.na(sbpj), 0, sbpj)
  dbph <- ifelse(is.na(dbph), 0, dbph)
  dbpj <- ifelse(is.na(dbpj), 0, dbpj)
  hdlh <- ifelse(is.na(hdlh), 0, hdlh)
  hdlj <- ifelse(is.na(hdlj), 0, hdlj)
  ldlh <- ifelse(is.na(ldlh), 0, ldlh)
  ldlj <- ifelse(is.na(ldlj), 0, ldlj)
  tgh <- ifelse(is.na(tgh), 0, tgh)
  tgj <- ifelse(is.na(tgj), 0, tgj)
  fbgh <- ifelse(is.na(fbgh), 0, fbgh)
  fbgj <- ifelse(is.na(fbgj), 0, fbgj)
  hba1ch <- ifelse(is.na(hba1ch), 0, hba1ch)
  hba1cj <- ifelse(is.na(hba1cj), 0, hba1cj)
  ggtph <- ifelse(is.na(ggtph), 0, ggtph)
  ggtpj <- ifelse(is.na(ggtpj), 0, ggtpj)
  goth <- ifelse(is.na(goth), 0, goth)
  gotj <- ifelse(is.na(gotj), 0, gotj)
  gpth <- ifelse(is.na(gpth), 0, gpth)
  gptj <- ifelse(is.na(gptj), 0, gptj)

  #階層化リスク設定
  kaisoukaBP <- ifelse((sbph + dbph)>=1, 1, 0)
  kaisoukaBG <- ifelse(is.na(hba1ch), fbgh, hba1ch)
  kaisoukaLP <- ifelse((tgh + hdlh)>=1, 1, 0)
  kaisoukaSM <- ifelse(df$SM==1,1,0)
  kaisoukaSM <- ifelse(is.na(kaisoukaSM), 0, kaisoukaSM)
  kaisoukaOB <- ifelse((waisth + bmih)>0, 2, 1)

  #受診勧奨リスク設定
  jushinBP <- ifelse((sbpj + dbpj)>=1, 1, 0)
  jushinBG <- ifelse(is.na(hba1cj), fbgj, hba1cj)
  jushinLP <- ifelse((ldlj)>=1, 1, 0)

  #階層化リスク個数設定
  kaisoukaCOUNT <- ifelse((kaisoukaBP + kaisoukaBG + kaisoukaLP)>=1, kaisoukaBP + kaisoukaBG + kaisoukaLP + kaisoukaSM, 0)
  kaisoukaJUSHIN <- sbpj + dbpj + hdlj + ldlj + tgj + ggtpj + gptj + gotj + hba1cj + fbgj 


  #階層化リスク組合せ設定
  kaisoukaKUMIAWASE <- paste(sep="|", ifelse(kaisoukaSM==1, "SM", ""), ifelse(kaisoukaBP==1, "BP", ""), ifelse(kaisoukaBG==1, "BG", ""), ifelse(kaisoukaLP==1, "LP", "")) 


  #健康分布フラグ設定

  kenkoubunpuRISK <- rep(NA, nrow(df))
  for(i in 1:nrow(df)){
    if(sum(df[i, grep("dr",colnames(df))], na.rm=TRUE)>0){
      kenkoubunpuRISK[i] <- 4
      } else if(kaisoukaJUSHIN[i] > 0){
      kenkoubunpuRISK[i] <- 3
      } else if(kaisoukaCOUNT[i] > 0){
      kenkoubunpuRISK[i] <- 2
      } else {
      kenkoubunpuRISK[i] <- 1}
    }
  kenkoubunpuSEG <- 10*kaisoukaOB + kenkoubunpuRISK

  df <- data.frame(df, kaisoukaBP, kaisoukaBG, kaisoukaLP, kaisoukaSM, kaisoukaOB, jushinBP, jushinBG, jushinLP, kaisoukaJUSHIN, kaisoukaCOUNT, kaisoukaKUMIAWASE, kenkoubunpuRISK, kenkoubunpuSEG)
  df <- merge(df, code2, all.x=TRUE)
  df$group <- ifelse(is.na(df$group), "関連会社", df$group)
  df$risk <- ifelse(df$kenkoubunpuSEG==11, 0, 1)
  invisible(df)
  }

###問診のデータ変換

convm <- function(data){
  #require(digest)
  code2 <- read.csv("Q:/Pln/Analysis/01_hokensya/01_kenpo/10_三菱電機/01_data/code2.csv", as.is=TRUE)
  colnames(data) <- c("kigou", "kinmuchikubun", "haihu", "shozoku", "bangou", "age", "sex", "stressP", str_c("Q", 1:11), "yakushoku", "shokushu", "kinmukeitai", "height", "weight", "bmi")
  data$yakushoku <- ifelse(nchar(data$yakushoku)==0, "その他", data$yakushoku)　#空白をその他で置換
  data$haihu <- ifelse(is.na(data$haihu), 999, data$haihu) #NAを999で置換
  data$kinmuchikubun <- ifelse(is.na(data$kinmuchikubun), 999, data$kinmuchikubun) #NAを999で置換
  data[,9:19] <- apply(data[,9:19], 2, function(x)ifelse(is.na(x), 0, x))
  data$Q1 <- ifelse(data$Q1==2, 1, 0)
  #data$Q1 <- ifelse(data$Q1==1&&data$Q2==1&&data$Q3==1, 0, 1) #運動に関して複数条件を用いる場合
  data$Q4 <- ifelse(data$Q4==1, 1, 0)
  data$Q8 <- ifelse(data$Q8==1, 0, 1)
  data$Q10 <- ifelse(data$Q10==1, 0, 1)
  data$Q11 <- ifelse(data$Q11==2, 1, 0)
  data <- merge(data, code2, all.x=TRUE)
  data$group <- ifelse(is.na(data$group), "関連会社", data$group)
  invisible(data)
  }