dirplot <- function(path=getwd()){
  require(ggplot2)
  require(googleVis)
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
  lay <- rbind(all, l1)

  #layer2 and so on
  for(i in 1:(length(res)-2)){
    smp <- res[, c(i, i+1, length(res))]
    colnames(smp) <- c("p", "id", "size")
    smp <- subset(smp, complete.cases(smp))
    lay0 <- ddply(smp, . (p, id), summarise, size=sum(size))
    lay <- rbind(lay, lay0)
    }

  lay$color <- 1:nrow(lay)
  trmp <- gvisTreeMap(lay, idvar="id", parentvar="p", colorvar="color",
                      options=list(width=600, height=500, fontSize=10))
  trmp$html$header <- gsub("utf-8", "CP932", trmp$html$header)
  plot(trmp)
  }