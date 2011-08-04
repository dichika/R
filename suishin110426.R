#推進する会のタイプ分け→生活習慣病死亡者数等を目的変数
#決定木
setwd("Q:/Pln/Analysis/01_hokensya/04_suishin")
library(party)
library(rpart)
library(tree)
library(partykit)
library(ggplot2)
df <- read.csv("prefdata110426.csv", as.is=TRUE)
plot(df[,-1])
plot(df$coloncdm, df$brinfarcdm)
cor(df$coloncdm, df$brinfarcdm)

ggplot(data=df, aes(x=reorder(pref, brinfarcdm))) + geom_bar(aes(y=brinfarcdm), stat="identity") + coord_flip() + xlab("") + ylab("") + opts(title = "brain infarction male", axis.text.y = theme_text(size=8))

brinfarc <- rpart(data=df[,-1], brinfarcdm~butter+katsuo+salt+gtea)
plot(as.party(brinfarc), tp_args = list(id = FALSE))
for(i in 1:nrow(df)){
if(df$salt[i] >= 2765.5){
  df$group[i] <- "A"}else if(df$salt[i] >= 2038.5 && df$butter[i] < 411.5){
  df$group[i] <- "B"}else if(df$salt[i] >= 2038.5 && df$butter[i] >= 411.5){
  df$group[i] <- "C"}else{
  df$group[i] <- "D"}
}
table(df$group)
ggplot(data=df, aes(x = salt, y = butter, label=pref)) + geom_text(aes(size=brinfarcdm)) + geom_vline(x=2038.5)  + geom_vline(x=2765.5) + geom_hline(y=411.5)
ggplot(data=df, aes(x = salt, y = butter, label=pref)) + geom_text(aes(size=brinfarcdm))