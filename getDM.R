
comparePlot <- function(urldata1, urldata2, data1name="data1", data2name="data2"){
  require(rdatamarket)
  require(googleVis)
  require(ggplot2)

  getDM <- function(url){
    dminfo(url)
    data <- dmseries(url)
    data <- data.frame(year=index(data), as.data.frame(data))
    data <- melt(data, id.vars="year")
    colnames(data) <- c("year", "country", "value")
    invisible(data)
    }
  data1 <- getDM(urldata1)
  data2 <- getDM(urldata2)
  result <- merge(data1, data2, by=c("year", "country"))
  result <- subset(result, complete.cases(result))
  colnames(result) <- c("year", "country", data1name, data2name)
  result$year <- as.Date(paste(sep="/", result$year, "01/01"))
  M <- gvisMotionChart(result, idvar="country", timevar="year")
  invisible(M)
  }
urldata1 <- "http://datamarket.com/data/set/15nl/gini-index#display=line&ds=15nl|ho8"
urldata2 <- "http://datamarket.com/data/set/15n6/health-expenditure-public-of-gdp#display=line&ds=15n6|hnt"
data1name <- "GINI"
data2name <- "HEperGDP"

M <- comparePlot(urldata1, urldata2, data1name, data2name)
ggd <- createGoogleGadget(M)
write(ggd,"sample.xml")