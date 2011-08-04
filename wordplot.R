wordplot <- function(data, col=3, threshold=30){
require(googleVis)
require(stringr)
require(ggplot2)

smp <- data[nchar(data[,col])>0 ,col]

ans <- NULL
for(c in 1:length(smp)){
ans0 <- unlist(RMeCabC(smp[c]))
ans <- c(ans, ans0)
}

hinsi <- attr(ans, "names")
df <- data.frame(table(hinsi))
df$pa <- "行動計画"
df$col <- 1:nrow(df)
df$p <- round(df$Freq * 100 / sum(df$Freq), 1)
colnames(df) <- c("ch", "fr", "pa", "col", "p")

top <- data.frame(ch="行動計画", fr=sum(df$Freq), pa=NA, col=0, p=100)

df2 <- data.frame(hinsi, ans)
df2 <- data.frame(table(df2))
df2$color <- 1:nrow(df2)
df2$per <- round(df2$Freq * 100 / sum(df2$Freq), 1)
colnames(df2) <- c("pa", "ch", "fr", "col", "p")
df2 <- df2[,c(2,3,1,4,5)]

df3 <- rbind(top, df, df2[df2$fr>=threshold,])

df <- rbind(top, df, df2)

T1 <- gvisTreeMap(df3, idvar="ch", parentvar="pa", colorvar="col", sizevar="fr")
T1$html$header <- gsub("utf-8", "CP932", T1$html$header)
plot(T1)
}