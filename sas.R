#SASデータをRに読み込む
ごく稀にSASデータでデータが公開されているようなf*ckin'な状況がある（たとえば[ここ|http://www.cdc.gov/brfss/technical_infodata/surveydata/1984.htm]
そういう時はforeignパッケージを用いて読み込む（今回はxpt形式のファイルを読み込んでいる）。

library(foreign)
df <- read.xport("C:/Users/h1030/Desktop/CNTY09xpt/CNTY09.xpt")
