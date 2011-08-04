#青木先生の関数
source("http://aoki2.si.gunma-u.ac.jp/R/src/map.R", encoding="euc-jp")

map <- function(code.list,                                             # 描画する都道府県コード
                density=NULL,                                           # ハッチングの 1 インチあたりの線密度
                color=NULL,
                main="")                                             # 塗り分けに使用する色
{
        map0 <- function(data, dens, color)                             # 描画関数
        {
                continue <- apply(data, 1, any)                         # 経度と緯度が共に 0 が描画の区切り
                plot(lon, lat, type = "n", axes=FALSE,                  # 大枠を決める
                        xlab="", ylab="", bty="n", asp=1, main=main)
                start <- 1
                k <- 0
                for (i in 2:nrow(data)) {
                        if (continue[i] == FALSE) {                     # 区切れ目
                                k <- k+1                                # 何番目の描画か
                                if (i-start == 4) {                     # 沖縄を描くときの区画線
                                        lines(data[start:(i-1),])
                                }
                                else {
                                        polygon(data[start:(i-1),], density=dens[k], col=color[k], border="black")
                                }
                                start <- i+1
                        }
                }
        }

# 関数本体

        for (i in seq(along=code.list)) {                               # 指定した全ての都道府県について
                if (code.list[i] %in% c(15, 28, 47)) {                  # 新潟県，兵庫県，沖縄県は描画パーツが 2 つ
                        code.list <- c(code.list, -code.list[i])        # コードリストの追加（目印とするために負の数で）
                        density <- c(density, density[i])               # 線密度と
                        color <- c(color, color[i])                     # 色も追加する
                }
        }
        code.list[code.list == -15] <- 48                               # 追加のコードリストに直す
        code.list[code.list == -28] <- 49
        code.list[code.list == -47] <- 50

        lon <- lat <- NULL
        for (i in code.list) {                                          # 指定した全ての都道府県について
                fn <- sprintf("jpn/%02i", i)                            # データファイル名を得る
                gwm <- matrix(scan(fn, quiet=TRUE), ncol=2, byrow=TRUE) # データを読む
                lon <- c(lon, gwm[,1], 0)                               # 経度を蓄積
                lat <- c(lat, gwm[,2], 0)                               # 緯度を蓄積
        }
        mlon <- min(lon[lon != 0])                                      # 経度の最小値
        mlat <- max(lat[lat != 0])                                      # 緯度の最大値
        lon <- ifelse(lon == 0, 0, lon-mlon+1)                          # 経度の調整
        lat <- ifelse(lat == 0, 0, mlat-lat+1)                          # 緯度の調整
        map0(cbind(as.integer(lon), as.integer(lat)), density, color)   # 描画関数を呼ぶ
}


library(ggplot2)

#白地図の場所を指定
setwd("D:/My Dropbox/011_map/japan_ver62/")
#データの読み込み
data <- read.csv("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/003_other/list110425.csv", as.is = TRUE)
data$対象者数st <- cut(data$対象者数, right=FALSE, breaks=c(1, 10, 50, 100, 500, Inf), labels=c("1-9", "10-49", "50-99", "100-499", "500-"))

sc_type <- data.frame(stringsAsFactors=FALSE, type=toupper(letters[1:11]), name=c("大学", "短大", "高校", "中学校", "小学校", "幼稚園", "養護学校等", "その他学校", "施設", "高専", "専門学校"))
data <- merge(data, sc_type, all.x=TRUE)

#県別プロット
data_summary <- ddply(data, .(pref), summarise, sum_jushin=sum(受診者数), sum_taishou=sum(対象者数))
jushinritsu <- 100 * round(data_summary$sum_jushin / data_summary$sum_taishou, 3)
num <- trunc((jushinritsu - 50) / 5) +1
color <- rev(heat.colors(6))[num]
age_st <- paste(seq(50, 75, 5), "%-", sep="")

map(1:47, color=color, main="全体")
legend(x=2.5, y=400, age_st, fill=rev(heat.colors(6)))
windows()

#学校種類別県別プロット
for(type in toupper(letters[1:11])){
data_summary <- ddply(data[data$type==type,], .(pref), summarise, sum_jushin=sum(受診者数), sum_taishou=sum(対象者数))
jushinritsu <- 100 * round(data_summary$sum_jushin / data_summary$sum_taishou, 3)
num <- trunc(jushinritsu/10) +1
color <- rev(heat.colors(11))[num]
age_st <- paste(seq(0, 100, 10), "%", sep="")
name <- data[data$type==type,]$name[1]
map(1:47, color=color, main=name)
legend(x=2.5, y=400, age_st, fill=rev(heat.colors(11)))
windows()
}
dev.off()

#学校種類別受診率
data_summary <- ddply(data, .(name, 対象者数st), summarise, median_size=median(対象者数), mean_size=mean(対象者数), sum_jushin=sum(受診者数), sum_taishou=sum(対象者数))
data_summary$jushinritsu <- 100 * round(data_summary$sum_jushin / data_summary$sum_taishou, 3)
data_summary
ggplot(data_summary, aes(x=対象者数st, y=jushinritsu)) + geom_bar(stat="identity") + facet_wrap(~name)

#学校種類別対象者数
ggplot(data, aes(x=対象者数st)) + geom_histogram() + xlab("対象者数") + coord_flip() + facet_wrap(~name)
addmargins(table(data$name, data$対象者数st))

#学校種類別占有率
data_summary <- ddply(data, .(name), summarise, median_size=median(対象者数), mean_size=mean(対象者数), sum_jushin=sum(受診者数), sum_taishou=sum(対象者数))
data_summary$jushinritsu <- 100 * round(data_summary$sum_jushin / data_summary$sum_taishou, 3)
data_summary$senyuuristu <- 100 * round(data_summary$sum_taishou / sum(data_summary$sum_taishou), 3)
ggplot(data_summary, aes(x=median_size, y=jushinritsu)) + geom_point(aes(size=senyuuristu))

#規模別占有率
data_summary <- ddply(data, .(対象者数st), summarise, count=sum(!is.na(対象者数)), median_size=median(対象者数), mean_size=mean(対象者数), sum_jushin=sum(受診者数), sum_taishou=sum(対象者数))
data_summary$jushinritsu <- 100 * round(data_summary$sum_jushin / data_summary$sum_taishou, 3)
data_summary$senyuuristu <- 100 * round(data_summary$sum_taishou / sum(data_summary$sum_taishou), 3)
ggplot(data_summary, aes(x=median_size, y=jushinritsu)) + geom_point(aes(size=senyuuristu))