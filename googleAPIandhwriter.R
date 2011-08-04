
library(googleVis)

###準備
#住所から緯度経度を得るための関数google maps geocoding APIを使用
getGIS <- function(locationdata){
  library(RCurl)
  library(XML)
  data <-  NULL
    for(count in 1:length(locationdata)){
　　  location <- locationdata[count]
　　　　  if(Sys.getlocale("LC_CTYPE")=="Japanese_Japan.932"){
　　　　  Encodelocation <-paste(c("",charToRaw(iconv(location,"CP932","UTF-8"))),collapse="%")
　　　　  }else{
　　　　  Encodelocation <-paste(c("",charToRaw(location)),collapse="%")
　　　　  }
　　  url <- paste("http://maps.google.com/maps/api/geocode/xml?address=",Encodelocation,"&sensor=false", sep="")
　　  xml <- getURL(url)
　　  lat <- as.numeric(xmlValue(xmlRoot(xmlTreeParse(xml))[["result"]][["geometry"]][["location"]][["lat"]]))
　　  lon <- as.numeric(xmlValue(xmlRoot(xmlTreeParse(xml))[["result"]][["geometry"]][["location"]][["lng"]]))
　　  data0 <- data.frame(lat=lat, lon=lon, stringsAsFactors=FALSE)
　　  data <- rbind(data, data0)
   }
 return(data)
 }


###地図を作る
address <- "東京都千代田区三崎町2-9-22"
mapdata <- cbind(address, getGIS(address))
mapdata$LatLong <- paste(round(mapdata$lat,3), round(mapdata$lon,3), sep=":")
M1 <- gvisMap(mapdata, "LatLong" , "address",
              options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE,
                           mapType="normal", useMapTypeControl=TRUE, zoomLevel=19,
                           width=500,height=250))

M1$html$chart["jsData"] <- gsub("35.701", mapdata$lat, M1$html$chart["jsData"], perl=TRUE)#jsonDataの緯度経度と文字コードを修正
M1$html$chart["jsData"] <- gsub("139.75", mapdata$lon, M1$html$chart["jsData"], perl=TRUE)
M1$html$header <- gsub("charset=utf-8", "charset=shift-jis", M1$html$header)
M1$html$caption <- NULL
M1$html$footer <- NULL

plot(M1)
URL <- paste(sprintf("http://127.0.0.1:%s/custom/googleVis/", tools:::httpdPort), M1$chartid, ".html", sep="")


###コラムを作る
data <- read.csv(as.is=TRUE, skip=16,"http://www.e-stat.go.jp/SG1/estat/GL71050103.do?_csvDownload_&fileId=000002764425&releaseCount=2", header=FALSE)
data <- subset(data, complete.cases(data))

data <- stack(list( 総医療費= as.numeric(gsub(",", "", data$V2)), "一人当たり医療費" = data$V4))
data$year <- rep(as.Date(paste(1954:2005, "0101", sep=""), format="%Y%m%d"), 2)
data$annotation <- data$values
D1 <- gvisAnnotatedTimeLine(data, datevar="year",
                           numvar="values", idvar="ind",
                           titlevar="annotation", annotationvar="",
                           options=list(displayAnnotations=FALSE,
                            legendPosition='newRow', scaleColumns='[0,1]',
                            width=500, height=250, scaleType='allmaximized'))

D1$html$header <- gsub("charset=utf-8", "charset=shift-jis", D1$html$header)
D1$html$caption <- NULL
D1$html$footer <- NULL

plot(D1)
URL2 <- paste(sprintf("http://127.0.0.1:%s/custom/googleVis/", tools:::httpdPort), D1$chartid, ".html", sep="")


###案内状をHTMLで出力する
library(hwriter)
tmpdir <- tempdir()
setwd(tmpdir)

p <- openPage("report.html", charset="shift-jis",
link.javascript="https://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.js")

##Javascriptの記述

hwrite(hmakeTag("script", '
$(function(){
    $(".open").click(function(){
     $("#slideBox").slideToggle("slow");
    });
});

var debug = document.getElementById("debug");
function onDragStart(event){
    debug.innerHTML = "Drag Start" + "<br>" +debug.innerHTML;
}
function onDrop(event){
    debug.innerHTML = "On Drop" + "<br>" +debug.innerHTML;
}
function onDragOver(event){
    debug.innerHTML = "On Drag Over" + "<br>" +debug.innerHTML;
}
function onDragEnter(event){
    debug.innerHTML = "On Drag Enter" + "<br>" +debug.innerHTML;
}
function onDragLeave(event){
    debug.innerHTML = "On Drag Leave" + "<br>" +debug.innerHTML;
}

', type="text/javascript"), table=FALSE,p)

##CSSの記述

hwrite(hmakeTag("style","
.open{

background: #fc6;

color: #fff;

cursor: pointer;

width:45px;

padding: 10px

}

#slideBox{

padding: 10px;

border: 1px #ccc solid;

display:none;

}

#drop{
       height: 150px; 
       width: 680px; 
       border: 5px; 
       padding: 10px; 
       background: #696969; 
       border-radius: 10px; 
       -moz-border-radius: 10px; 
       -webkit-border-radius: 10px; 
}
"),p)

hwrite("第９９回Tokyo.R忘年会のおしらせ", p, heading=1, br=TRUE)
#img <- hwriteImage("http://www.r-project.org/Rlogo.jpg", br=TRUE, width=400, height=200, draggable="TRUE")
img <- hwrite(hmakeTag("img", src="http://www.r-project.org/Rlogo.jpg", width=400, height=200, draggable="true", ondragstar="onDragStart(event)"),p)
tbl <- hwrite(data.frame(内容=c("日時：平成23年12月31日", "場所：サンマルクカフェ水道橋駅東口店", "会費：5000円")), row.names=FALSE, border=0)
but <- hwrite(hmakeTag("button", "参加する"), link="javascript:alert('バターデニッシュうめええええ！！！！！！！！')", br=TRUE)

hwrite(c(img, tbl, but),  p, border=0)

MAP <- hwrite(paste('<iframe src=', URL, ' frameborder="0" width="600" height="400" scrolling="no"></iframe>', sep=""))
BOTAN <- hwrite(hmakeTag("p", "ボタン", class="open"))
KORAMU <- hwrite(paste('<iframe src=', URL2, ' frameborder="0" width="600" height="400" scrolling="no"></iframe>', sep=""), div=TRUE, id="slideBox")

mat <- rbind(c("会場地図", "コラム（日本の医療費）"), c(MAP, BOTAN, KORAMU))
hwrite(mat, p, border=0)
hwrite("ここにドロップしてください", p, div=TRUE, id="drop", ondragenter="onDragEnter(event)", ondragover="onDragOver(event)", ondrop="onDrop(event)", ondragleave="onDragLeave(event)")
hwrite("", p, div=TRUE, id="debug")
closePage(p, splash=FALSE)

###シェルスクリプトからブラウザ起動をさせて出力を確認（Windows環境＆PATHが通っていることが必要）
shell("report.html")

**openpage関数
**hwriter関数
**hwriteImage関数
**hwrite
**closepage関数