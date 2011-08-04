

estim <- function(data, idvar=1, dayvar=2, pointvar=3, master, masteridvar=1, merge=TRUE){
　　require(ggplot2)
  colnames(data)[c(idvar, dayvar, pointvar)] <- c("id", "D", "P")
  datam <- melt(data, id.var=c("id", "D", "P"))

  #出現疾病リストを取得
  dlist <- unique(datam$value[!is.na(datam$value)])

  #レコード単位で疾病出現数、DN、PDN、全疾病の平均医療費M を求める
  datav <- ddply(datam, .(id, D, P), summarise, count=sum(!is.na(value)))
  datav$DN <- datav$D * (1+log(datav$count))
  datav$PDN <- datav$P / datav$DN
  M <- sum(datav$P, na.rm=TRUE)/sum(datav$DN, na.rm=TRUE)

  #出現疾病リストにしたがって医療費を推計する
  result <- NULL
  for(num in dlist){
    targ <- subset(datam, value==num)$id #疾病番号を指定
    targP <- mean(datav[datav$id%in%targ,]$PDN, na.rm=TRUE) #指定した疾病の平均点数
    targCount <- mean(datav[datav$id%in%targ,]$count, na.rm=TRUE) #指定した疾病の平均出現数
    EXP <- ifelse(targP>=M, log(targCount), targCount-1) #補正係数が入力されていない時は算出
    result0 <- data.frame(num, targP*((targP/M)^EXP), EXP, targP, targCount, M, stringsAsFactors=FALSE)
    colnames(result0) <- c("code", "estimate", "EXP", "targP", "targCount", "M")
    result <- rbind(result, result0)
    }

  #マスター（119コード）とのmerge
  if(merge==TRUE){
  resultFinal <- merge(master, result[order(result$code),], by.x=colnames(master)[masteridvar], by.y="code", all.x=TRUE)
  return(resultFinal)
  }else{
  return(result)
  }
}

####検証用シミュレーションデータ生成関数
###点数は

mkdummy <- function(n=100, disn=5, daylim=c(0,5)){
  #日数マトリクスの作成
  MAT <- data.frame(matrix(floor(runif(n=n*disn, min=daylim[1], max=daylim[2])), byrow=TRUE, nrow=n))
  MAT$D <- rowSums(MAT)

  #点数へ変換
  for(i in 1:disn){
    MAT[,i] <- i*1000*MAT[,i]
    }
  MAT$P <- rowSums(MAT[,1:disn])

  #疾病コードへ変換（1-119の通番を使用）
  for(i in 1:disn){
    MAT[,i] <- ifelse(MAT[,i]>0, i,NA)
    }
  id <- 1:n
  DUMMY <- data.frame(stringsAsFactors=FALSE, id, MAT$D, MAT$P, MAT[,1:disn]) 
  invisible(DUMMY)
  }


##シミュレーション
setwd("D:/downloads")

master <- read.csv("119.csv", as.is=TRUE, header=FALSE)

resd <- NULL

for(s in 1:100){
  set.seed(s)
  truedata <- seq(1000, 5000, length=5)
  SIM <- estim(mkdummy(), master=master, masteridvar=2, merge=FALSE)
  residual0 <- data.frame(truedata, residual=SIM$estimate-truedata)
  resd <- rbind(residual, residual0)
  }

ggplot(data=resd, aes(x=truedata, y=residual^2))+geom_point()

smp <- read.csv("sampledata.csv", as.is=TRUE, header=FALSE)
EST <- estim(smp, master=master, merge=FALSE)[,1:2]

conv <- function(data){
  for(i in 1:length(data)){
    for(j in 1:nrow(data)){
      data[j,i] <- ifelse(!is.na(data[j,i]), EST[EST$code==data[j,i],]$estimate, 0)
    }
  }
  return(data)
  }

df <- data.frame(POINTest=rowSums(conv( smp[,4:length(smp)])), POINTtrue=smp[,3])
ggplot(df, aes(x=POINTest, y=POINTtrue)) + geom_point()
mean((df$POINTest - df$POINTtrue)^2)