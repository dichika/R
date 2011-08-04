####関数群####

#グラフ・表出力用関数
#素性マトリックスと品詞を引数にとってグラフと表を出力する
#data.frameで因子型変換を抑制しないとggplot2に反映されてしまう

output <- function(x, item){
    mat3 <- x[, grep(paste(item, ".", sep=""), colnames(x))]
    if(ncol(mat3)==0){
      return()
    }else{
    name <- do.call("rbind", strsplit(split="-", colnames(mat3)))[,1]
    name <- gsub(".+\\.","", name, perl=TRUE) #品詞情報を除く
    data <- data.frame(name, class=item, prop=round(colSums(mat3)/nrow(x), 3)*100, stringsAsFactors=FALSE)
    data <- data[order(data$prop, decreasing=TRUE), ]
    rownames(data) <- NULL
    if(item=="名詞"){
      stopwordlist <- paste(c("量", "秒", "時", "分", "回", "時間", "週", "日", "月", "以上", "未満", "以下", "膳", "杯", "もの", "⇒","000",145, 7000, 8000, 1,10,100,120,1300,15,150,2,20,200,2000,230,3,30,4,40,45,5,50,500,6,60,7,700,70000,720,730,8,80,9,9010000, 0, 00, 000, "kg", "カロリー", "ml", "mL", "kcal", "Kcal", "day", "g", "cc", "week","m","ータ"), collapse="|")
      data <- data[ -grep(stopwordlist, data$name, perl=TRUE), ]
      graphdata <- data[1:ifelse(nrow(data)>=10,10,nrow(data)), ]
      p <- ggplot(data=graphdata, aes(x=reorder(name, prop), y=prop)) + geom_bar() + coord_flip() + geom_text(aes(label=prop), hjust=0) + xlab("") + ylab("割合(%)") + opts(title=paste(sep="", item, "上位10位"))
      res <- list(data, p) 
      }else{
      graphdata <- data[1:ifelse(nrow(data)>=10,10,nrow(data)), ]
      p <- ggplot(data=graphdata, aes(x=reorder(name, prop), y=prop)) + geom_bar() + coord_flip() + geom_text(aes(label=prop), hjust=0) + xlab("") + ylab("割合(%)") + opts(title=paste(sep="", item, "上位10位"))
      res <- list(data, p) 
      }
   }}


#素性マトリックス作成関数
#行動計画文字列を引数にとって、品詞と結合したマトリックスを作成する

ftr_mat <- function(vec){
    res <- as.list(rep(NA, length(vec)))
    for(i in 1:length(vec)){
        tx <- vec[i]
        if ( nchar(tx) > 1){
          val <- unlist(RMeCabC(tx, mypref=1))#原形で表示
          atr <- attr(val, "names")
          res[[i]] <- unique(paste(sep = "-", atr, val))
          } else {
          next
        }}
    res.im <- as(res, "itemMatrix")
    data.frame(as(res.im, "matrix"))
    }

#特徴語を含む行動計画を抽出する関数（引数は「行動計画フォルダ」「支援者の番号」「抽出する行動計画の数」）

plan_feat <- function(place="doc2", num=1, n=3){
  mat_feat <- docDF(place, Genkei=1, type=1, pos=c("名詞", "動詞", "形容詞", "副詞"), weight="tf*idf*norm")
  mat_feat <- mat_feat[mat_feat$POS2!="数", ]
  vec <- head(mat_feat[order(mat_feat[,num+3], decreasing=TRUE) & rowSums(mat_feat[,4:length(mat_feat)])==mat_feat[,num+3], ], n)$TERM
  vec <- vec[nchar(vec) >= 2] #2文字以上の単語を抽出する
  vec_feat <- paste(vec, collapse="|")
  txt <- readLines(paste(sep="", getwd(), "/doc2/", num, ".txt"))
  p <- grep(vec_feat, txt, value=TRUE, perl = TRUE)
  invisible(list(plan=p, vec=vec))
  }


#特徴的な共起もしくは単語（各行動計画内）を含む行動計画を抽出する

plan_feat2 <- function(s_num, siensya, n=10, tango=0, plandata=data){
  result <- NULL #支援者別の共起語データを作成（語１・語２・頻度・支援者番号）
  for(num in 1:s_num){
    pl <- subset(plandata, siensya==num)$V3
    result0 <- colloc_plan(pl)
    result0$siensya <- num
    result <- rbind(result, result0)
    }
  result2 <- subset(result, V4 == tango) #共起組合せを抽出する際はFALSE(0)、単語頻度のみの場合はTRUE(1)
  result3 <- ddply(result2, .(Var1, Var2, siensya), summarise, freq = sum(Freq))
  result3 <- reshape(result3, idvar = c("Var1", "Var2"), timevar = "siensya", direction = "wide")
  result3[,1:2] <- apply(result3[,1:2], 2, function(x)gsub("^.*-", "", x, perl=TRUE))

  TFIDFdata <- result3[,-c(1,2)] #TFIDFを計算するためのデータ
  TF <- TFIDFdata / colSums(TFIDFdata,na.rm=TRUE) #TFを算出（列和で割る）
  colnames(TF) <- paste("TF",1:ncol(TF), sep=".")

  IDF <- log(5 / rowSums(!is.na(TFIDFdata),na.rm=TRUE)) #IDFを算出（行和で割る）

  TFIDF <- TF*IDF #TF*IDFを算出
  colnames(TFIDF) <- paste("TFIDF",1:ncol(TFIDF), sep=".")

  result4 <- cbind(result3, TF, stringsAsFactors=FALSE)
  result4 <- cbind(result4, IDF, stringsAsFactors=FALSE)
  result4 <- cbind(result4, TFIDF, stringsAsFactors=FALSE)

  #数詞、単位を除く
  stopwordlist <- paste(c("量", "秒", "時", "分", "回", "時間", "週", "日", "月", "以上", "未満", "以下", "膳", "杯", "もの", "枚", "⇒","000",145, 7000, 8000, 1,10,100,120,1300,15,150,2,20,200,2000,230,3,30,4,40,45,5,50,500,6,60,7,700,70000,720,730,8,80,9,9010000, 0, 00, 000, "kg", "カロリー", "ml", "mL", "kcal", "Kcal", "day", "g", "cc", "week","m","ータ"), collapse="|")
  result5 <- result4[ -grep(stopwordlist, result4$Var1, perl=TRUE), ]
  result5 <- result5[ -grep(stopwordlist, result4$Var2, perl=TRUE), ]

  invisible(kyouki_plan(dat=result5, num=siensya, n=n, plandata=plandata))
  }

#plan_feat2内の関数
#行動計画単位の共起頻度を抽出する関数（引数は行動計画ベクトル）

colloc_plan <- function(vec){
    res <- as.list(rep(NA, length(vec)))#行動計画を抽出し形態素に分割
    for(i in 1:length(vec)){
        tx <- vec[i]
        if ( nchar(tx) > 1){
          val <- unlist(RMeCabC(tx))#原形は使わない
          atr <- attr(val, "names")
          res[[i]] <- unique(paste(sep = "-", atr, val))
          } else {
          next
          }
        }
    res.im <- as(res, "itemMatrix")    #行動計画単位でのクロス集計表を作成する→下三角行列は0にして頻度0のものは除く
    tbl <- as.table(crossTable(res.im))
    mat <- as.matrix(tbl)
    mat[lower.tri(mat)] <- 0
    df <- data.frame(mat)
    df <- subset(df, df$Freq > 0)
    #df <- df[ intersect(grep("名詞|形容詞|動詞|副詞", df$Var1), grep("名詞|形容詞|動詞|副詞", df$Var2)), ]#この段階で記号は除く
    df <- df[ intersect(grep("名詞", df$Var1), grep("名詞", df$Var2)), ]#この段階で記号は除く+名詞に特徴ありそうなので絞る
        for(n in 1:nrow(df)){
        df[n, 4] <- identical(df[n,1], df[n,2])    #単語の単一頻度にはTRUE
        }
  invisible(df)
  }



#plan_feat2内の関数
#指定した支援者において該当する行動計画（多い単語・多い組合せ）を抽出
#引数は dat:支援者別共起頻度データ、num:支援者番号、n:抽出したい行動計画数、plandata:元となる行動計画データ

kyouki_plan <- function(dat, num, n, plandata=data){
  sorted.sub <- dat[ order(dat[, paste("TFIDF", num, sep=".")], decreasing=TRUE), ]#指定した支援者において総計をソート（Var1とVar2をとばしている）
  sorted.sub.head <- head( sorted.sub, n = n*2) #指定にしたがって上位N*2個の行動計画を抽出
  pl <- unique(subset(plandata, siensya==num)$V3)  #行動計画データから指定した支援者の行動計画を抽出
  res <- NULL
  for( m in 1:(n*2) ){
    res0 <- pl[intersect( grep(sorted.sub.head[m,1], pl, perl=TRUE), grep(sorted.sub.head[m,2], pl, perl=TRUE))][1]
    res <- c(res, res0)
    }
  plan.num <- as.numeric(factor(res))#重複除去の際に用いるために番号をつけておく（因子型を利用）
  df <- data.frame(key1 = sorted.sub.head$Var1, key2 = sorted.sub.head$Var2, plan = res, plan.num=plan.num)
  
  #各番号において一番の上の行動計画を抽出
  resu <- NULL
  for( c in 1:nlevels(factor(res))){
  resu0 <- df[df$plan.num==c, ][1,]
  resu <- rbind(resu, resu0)
  }
  final <- resu[1:n,]
  final$plan.num <- NULL
  invisible(final)
  }


#各行動計画に対する具体性スコア算出関数（初期値3）
gutaiseiCalc <- function(path){
    tx <- RMeCabText(path)
    tx_res <- do.call("rbind", tx)
    score_num <- sum(tx_res[,3] %in% "数")
    score_josuushi <- sum(tx_res[,4] %in% "助数詞")
    score_koyuu <- sum(tx_res[,3] %in% "固有名詞")
    score_fukushi <- -sum(tx_res[,2] %in% "副詞")
    score_other <- sum(tx_res[,1] %in% c("半分", "四分の一"))
    score <- 3 + score_num + score_koyuu + score_fukushi + score_josuushi + score_other #初期値3としている
    invisible(score)
    }



#具体性スコアを行動計画データに付加する関数


gutaiseiAdd <- function(data=data, plancol=3){
  ifelse(any(list.files() %in% "plans"), "", dir.create("plans"))
  score <- NULL
  for(count in 1:nrow(data)){
      num <- formatC(count, width=5, flag="0")
      path <- paste(sep="", getwd(), "/plans/p_", num, ".txt")
      #path <- tempdir()
      write(data[count, plancol] ,path)
      score0 <- gutaiseiCalc(path)
      score <- c(score, score0)
      }
  return(score)}

#支援者ごとの効果的スコア、具体性スコアの平均を算出する関数
#引数は具体性、寄与度、効果的スコアが付加された行動計画データ

scoreSUMMARY <- function(res){
  smp <- ddply(res, .(siensya), summarise, 
    mean_score_kouka = mean(score_kouka, na.rm=TRUE),
    mean_score_gutaisei = mean(gutaisei, na.rm=TRUE) )

  #各スコアにおける全体平均および標準偏差を算出
  kouka_sd_all <- sd(res$score_kouka, na.rm=TRUE)
  kouka_mean_all <- mean(res$score_kouka, na.rm=TRUE) 

  gutaisei_sd_all <- sd(res$gutaisei, na.rm=TRUE)
  gutaisei_mean_all <- mean(res$gutaisei, na.rm=TRUE) 

  #各スコアの偏差値を算出
  smp$kouka_hensachi <- 50 + (10 * (smp$mean_score_kouka - kouka_mean_all)) / kouka_sd_all
  smp$gutaisei_hensachi <- 50 + (10 * (smp$mean_score_gutaisei - gutaisei_mean_all)) / gutaisei_sd_all

  #支援者ごとの結果の出力→4・5についてNaNとなるので精査
  return(smp)
  }

#支援者各自のベストワーストな行動計画を出す関数
#引数は具体性、寄与度、効果的スコアが付加された行動計画データと支援者番号

siensyaBESTWORST10 <- function(res, siensya=1){
  sample_a <- res[res$siensya==siensya & !is.na(res$score_kouka),]

  #具体性のある目標ベスト10
  r1 <- list("具体性ベスト10", head( unique(sample_a[order(sample_a$gutaisei, decreasing=TRUE), ]$V3), n=10 ))
  #具体性のある目標ワースト10
  r2 <- list("具体性ワースト10", tail( unique(sample_a[order(sample_a$gutaisei, decreasing=TRUE), ]$V3), n=10 ))

  #効果的だった目標ベスト10
  r3 <- list("効果的ベスト10", head( unique(sample_a[order(sample_a$score_kouka, decreasing=TRUE), ])$V3, n=10 ))
  #効果的だった目標ワースト10
  r4 <- list("効果的ワースト10", tail( unique(sample_a[order(sample_a$score_kouka, decreasing=TRUE), ])$V3, n=10 ))
  return(list(sample_a$siensya_name[1], r1, r2, r3, r4))
  }
