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
  #•ŽŒ“™‚ðœ‚­
  res <- res[!(attr(res, "names") %in% c("•ŽŒ", "‹L†", "•“®ŽŒ", "Ú“ªŽŒ","˜A‘ÌŽŒ"))]
  if(is.null(type)){
  	res <- res
  } else {
  	res <- res[attr(res, "names") %in% type]
  }
  item <- data.frame(hinsi=attr(res, "names"), word=res, stringsAsFactors=FALSE)
  res <- ddply(item, .(hinsi, word), summarise, count=sum(!is.na(word)))
  res$count <- res$count
  pal <- brewer.pal(9,"BuGn")[5:9]
  wordcloud(res$word, res$count, min.freq=min, colors=pal, ...)
  }

windowsFonts(JP1 = windowsFont("MS Mincho"),
             JP2 = windowsFont("MS Gothic"),
             JP3 = windowsFont("Arial Unicode MS"),
             JP4 = windowsFont("Meiryo UI"),
             JP5 = windowsFont("HGMaruGothicMPRO"))