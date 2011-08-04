library(googleVis)
path <- "Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/001_original/test"
setwd(path)
data <- data.frame(id=row.names(file.info(dir())), size=file.info(dir())$size, parent="all")
namelist <- row.names(subset(file.info(dir()), isdir==TRUE))
while(length(namelist)>0)
  child <- paste(sep="/", path, namelist[i])
  setwd(child)
  data2 <- data.frame(id=row.names(file.info(dir())), size=file.info(dir())$size, parent=namelist[1])
  data[data$id=="test2",]$size <- sum(data2$size)

final <- rbind(data, data2)

final <- rbind(final, init)
final$col <- 1:nrow(final)
p <- gvisTreeMap(data=final, idvar="id", parentvar="parent", sizevar="size", colorvar="col")
plot(p)

init <- data.frame(id="all", size=sum(data$size), parent=NA)
df <- rbind(init, data)
df$col <- 1:nrow(df)
df$size[df$id=="all"] <- 1000
df$id <- as.character(df$id)
plot(gvisTreeMap(data=df, idvar="id", parentvar="parent", sizevar="size", colorvar=""))


library(GGally)
ggfluctuation2(table(movies$Action, movies$Comedy))
ggfluctuation2(table(movies$Action, movies$mpaa))
ggfluctuation2(table(movies[,c("Action", "mpaa")]))
ggfluctuation2(table(warpbreaks$breaks, warpbreaks$tension))

ggpairs(iris[,3:5])

ggpairs(
iris[,3:5],
upper = list(continuous = "density", combo = "box"),
lower = list(continuous = "points", combo = "dot"),
diag = list(continuous = "bar", discrete = "bar")
)

plotMatrix <- ggpairs(mtcars[,c("mpg","wt","cyl")], upper = "blank", title = "Custom Example")
# ggplot example taken from example(geom_text)
plot <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
plot <- plot + geom_text(aes(colour=factor(cyl)), size = 3) + scale_colour_discrete(l=40)
plotMatrix <- putPlot(plotMatrix, plot, 1, 2)
plotMatrix <- putPlot(plotMatrix, ggally_text("ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"), 1, 3)
plotMatrix


library(stringr)
fruit <- c("apple", "banana", "pear", "pinapple")
str_locate()
str_c("a",".csv")

sentences <- c("Jane saw a cat", "Jane sat down")
word(sentences, 1)