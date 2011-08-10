dirplot2 <- function(path=getwd()){
  require(ggplot2)
  require(portfolio)
  require(stringr)
 
  setwd(path)
  df <- file.info(dir(recursive=TRUE))
  max_len <- max(unlist(lapply(strsplit(row.names(df), split="/"), length)))

  res <- data.frame(matrix(NA, ncol=max_len+1, nrow=nrow(df)))
  for(i in 1:nrow(df)){
    size <- df$size[i]
    ROW <- unlist(strsplit(row.names(df), split="/")[[i]])
    res[i, ] <- t(c(ROW, rep(NA, max_len-length(ROW)), size))
    }
  colnames(res) <- c(str_c("l", 1:(ncol(res)-1)), "size")
  res$size <- as.numeric(res$size)

  #layer0
  all <- data.frame(p=NA, id="all", size=sum(res[, ncol(res)]), stringsAsFactors=FALSE)

  #layer1
  l1 <- ddply(res, .(id=l1), summarise, size=sum(size))
  l1 <- data.frame(p="all", l1, stringsAsFactors=FALSE)
  #lay <- rbind(all, l1)

  #layer2 and so on
  lay <- NULL
  for(i in 1:(length(res)-2)){
    smp <- res[, c(i, i+1, length(res))]
    colnames(smp) <- c("p", "id", "size")
    smp <- subset(smp, complete.cases(smp))
    lay0 <- ddply(smp, . (p, id), summarise, size=sum(size))
    lay <- rbind(lay, lay0)
    }

  lay$color <- c(rep(100, 423), rep(-100, 422))
  l1$color <- 1:nrow(l1)
  l1 <- rbind(head(lay, 50), tail(lay,50))

  #グループごとに分割してプロット
  map.market(id=l1$id, area=l1$size, group=l1$p, color=l1$color, main="")

  #分割したグループ同士のつながりを可視化→樹形図？

  }