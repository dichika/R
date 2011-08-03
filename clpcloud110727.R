#wordcloud‚ğ‚¿‚å‚Á‚Æ•ÏX‚·‚é
wordcloud2 <- function (words, freq, scale = c(4, 0.5), min.freq = 3, max.words = Inf, 
    random.order = TRUE, rot.per = 0.1, colors = "black", use.r.layout = FALSE, 
    ...) 
{
    tails <- "g|j|p|q|y"
    last <- 1
    nc <- length(colors)
    overlap <- function(x1, y1, sw1, sh1) {
        if (!use.r.layout) 
            return(wordcloud:::.overlap(x1, y1, sw1, sh1, boxes))
        s <- 0
        if (length(boxes) == 0) 
            return(FALSE)
        for (i in c(last, 1:length(boxes))) {
            bnds <- boxes[[i]]
            x2 <- bnds[1]
            y2 <- bnds[2]
            sw2 <- bnds[3]
            sh2 <- bnds[4]
            if (x1 < x2) 
                overlap <- x1 + sw1 > x2 - s
            else overlap <- x2 + sw2 > x1 - s
            if (y1 < y2) 
                overlap <- overlap && (y1 + sh1 > y2 - s)
            else overlap <- overlap && (y2 + sh2 > y1 - s)
            if (overlap) {
                last <<- i
                return(TRUE)
            }
        }
        FALSE
    }
    ord <- order(freq, decreasing = TRUE)
    words <- words[ord <= max.words]
    freq <- freq[ord <= max.words]
    if (random.order) 
        ord <- sample.int(length(words))
    else ord <- order(freq, decreasing = TRUE)
    words <- words[ord]
    freq <- freq[ord]
    words <- words[freq >= min.freq]
    freq <- freq[freq >= min.freq]
    thetaStep <- 0.1
    rStep <- 0.05
    plot.new()
    op <- par("mar")
    par(mar = c(0, 0, 0, 0))
    plot.window(c(0, 1), c(0, 1), asp = 1)
    normedFreq <- freq/max(freq)
    size <- (scale[1] - scale[2]) * normedFreq + scale[2]
    boxes <- list()
    for (i in 1:length(words)) {
        rotWord <- runif(1) < rot.per
        r <- 0
        theta <- runif(1, 0, 2 * pi)
        x1 <- 0.5
        y1 <- 0.5
        wid <- strwidth(words[i], cex = size[i], ...)
        ht <- strheight(words[i], cex = size[i], ...)
        if (grepl(tails, words[i])) 
            ht <- ht + ht * 0.2
        if (rotWord) {
            tmp <- ht
            ht <- wid
            wid <- tmp
        }
        isOverlaped <- TRUE
        while (isOverlaped) {
            if (!overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, 
                ht) && x1 - 0.5 * wid > 0 && y1 - 0.5 * ht > 
                0 && x1 + 0.5 * wid < 1 && y1 + 0.5 * ht < 1) {
                #cc <- ceiling(nc * normedFreq[i])
                #cc <- colors[cc]
                text(x1, y1, words[i], cex = size[i], offset = 0, 
                  srt = rotWord * 90, col = colors[i], ...)
                boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, 
                  y1 - 0.5 * ht, wid, ht)
                isOverlaped <- FALSE
            }
            else {
                if (r > sqrt(0.5)) {
                  warning(paste(words[i], "could not be fit on page. It will not be plotted."))
                  isOverlaped <- FALSE
                }
                theta <- theta + thetaStep
                r <- r + rStep * thetaStep/(2 * pi)
                x1 <- 0.5 + r * cos(theta)
                y1 <- 0.5 + r * sin(theta)
            }
        }
    }
    par(mar = op)
    invisible()
}

#clpcloud‚à­‚µè’¼‚µ
clpcloud <- function(type=NULL, min=1, ...){
  require(RMeCab)
  require(wordcloud)
  require(plyr)
  require(RColorBrewer)
  if( .Platform$OS.type=="unix"){
    txt <- read.delim(pipe("pbpaste"), as.is=TRUE, header=FALSE)  	
  }else{
    txt <- read.delim("clipboard", as.is=TRUE, header=FALSE)
  }
  res <- unlist(apply(txt,1,RMeCabC, mypref=1))
  #•Œ“™‚ğœ‚­
  res <- res[!(attr(res, "names") %in% c("•Œ", "‹L†", "•“®Œ", "Ú“ªŒ","˜A‘ÌŒ"))]
  if(is.null(type)){
  	res <- res
  } else {
  	res <- res[attr(res, "names") %in% type]
  }
  item <- data.frame(hinsi=attr(res, "names"), word=res, stringsAsFactors=FALSE)
  res <- ddply(item, .(hinsi, word), summarise, count=sum(!is.na(word)))
  if(is.null(type)){
    res$num <- as.numeric(as.factor(res$hinsi))
    pal <- data.frame(num=1:length(unique(res$hinsi)), 
                      pal=brewer.pal(length(unique(res$hinsi)),"Set1"),
                      stringsAsFactors=FALSE)
    res <- merge(res, pal)
    wordcloud2(res$word, res$count, min.freq=min, colors=res$pal, ...)
  } else {
    pal <- brewer.pal(9,"BuGn")[5:9]
    wordcloud(res$word, res$count, min.freq=min, colors=pal, ...)
  }
}

windowsFonts(JP1 = windowsFont("MS Mincho"),
             JP2 = windowsFont("MS Gothic"),
             JP3 = windowsFont("Arial Unicode MS"),
             JP4 = windowsFont("Meiryo UI"),
             JP5 = windowsFont("HGMaruGothicMPRO"))