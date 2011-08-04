setwd("Q:/Pln/Analysis/05_analysis/010_fda")
library(fda)
library(MFDA)
library(ggplot2)

set.seed(1)

#カシオの血圧を関数クラスタリングでパターン化

df <- read.csv("sbp20.csv")
colnames(df) <- c("sex", "age", paste(sep = "", "sbp", 1:20))
df$index <- 1:nrow(df)

#仮に、初年度において23歳の血圧を追いかける
df23 <- subset(df, age==23)
df23m <- melt(df23, c("index", "age", "sex"))
head(df23m)
df23m$variable <- as.numeric(gsub("sbp", "", df23m$variable))
ggplot(data=df23m, aes(x=variable, y=value, group=index)) + geom_line()

#5年目の測定が異常なので6年目以降で（つまり29歳以降）
ggplot(data=df23m, aes(x=variable, y=value, group=index)) + geom_line(aes(colour=sex)) + xlim(6,20)


#関数クラスタリング
df23 <- subset(df23, complete.cases(df23))
my.clust <- MFclust(mat, minG=1, maxG=4) #カテゴリ変数は不可
mat <- as.matrix(df23[,-c(1:7, ncol(df23))])

testdata <- mat
my.clust<-try(MFclust(testdata, minG=3, maxG=5, nchain=1,iter.max=1))
cld <- data.frame(id=1:nrow(my.clust$clust.center), my.clust$clust.center)
cld <- melt(cld, "id")
cld$variable <- as.numeric(gsub("t", "", cld$variable))
p1 <- ggplot(data=cld, aes(x=variable, y=value, group=id)) + geom_line(size=2, aes(color=id))+ scale_colour_gradientn(colour = rainbow(5))

df23$clust <- my.clust$clust
df23m <- melt(df23, c("index", "age", "sex", "clust"))
df23m$variable <- as.numeric(gsub("sbp", "", df23m$variable))
table(df23$clust)
p2 <- ggplot(data=df23m[df23m$clust==3,], aes(x=variable, y=value, group=index)) + geom_line(aes(colour=sex)) + xlim(6,20) + facet_wrap(~clust)

print(p1)
windows()
print(p2)