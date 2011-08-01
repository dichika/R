library(ggplot2)
library(stringr)

###ƒf[ƒ^æ“¾(5•ª‚­‚ç‚¢‚Åæ“¾‚Å‚«‚é)

res <- NULL
for(y in 9:11){
  y <- ifelse(y<10, formatC(digits=1, y, flag="0"), y)
  for(w in 1:52){
    w <- ifelse(w<10, formatC(digits=1, w, flag="0"), w)
    f <- sprintf("http://idsc.nih.go.jp/idwr/sokuho/20%s%s/20%s-%s-zensu.csv", y, w, y, w)

      if(ifelse(class(try(smp <- read.csv(f, skip=2, nrows=4), silent=TRUE)) == "try-error", FALSE, TRUE)){
  
      smp <- read.csv(f, skip=2, nrows=4)
      header <- c("pref", colnames(smp)[seq(2,length(colnames(smp)), by=2)])
  
      data <- read.csv(f, skip=3)
      data <- data.frame(data[,1], data[, grep("•ñ", colnames(data), perl=TRUE)])
      colnames(data) <- header
      res0 <- melt(data, id.var="pref")
      res0$week <- w
      res0$year <- str_c("20", y)
      res <- rbind(res, res0)
      }else{
      next
      }
    }
  }

setwd("D:/downloads")
write.csv(res, "infection110727.csv", row.names=FALSE)