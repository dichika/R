library(hwriter)


samplefmt <- read.csv("sample.csv", as.is=TRUE)

mkhtml <- function(filename, data, foot=TRUE, charset="CP932", lang="JP"){
  p <- openPage(filename, charset=charset, lang=lang)
  if(is.data.frame(data))data <- as.matrix(data)
  hwrite(hmakeTag("style","
  .LL{
  font-size: 300%;
  color: #FFFFFF;
  background-color: #009933;
  padding: 4px;
  }

  .L{
  font-size:200%;
  border: solid;
  border-width: 0px 0px 2px 0px;
  border-color: #009933;  
  padding: 4px;
  }

  .M{
  font-size: 100%;
  border: solid;
  border-width: 0px 0px 0px 5px;
  border-color: #009933;
  padding: 4px;
  }

  .S{
  padding: 4px;  
  }

  .foot{
  text-align: right;
  font-size: 100%;
  border: solid;
  border-width: 2px 0px 0px 0px;
  border-color: #009933;
  padding: 4px;  
  }

  "), p)
  hwrite(data[1,1], p, br=TRUE, contenteditable="TRUE", class="LL", div=TRUE)
  targ <- ifelse(foot, nrow(data)-1, nrow(data))
  for(i in 2:targ){
    ROW <- data[i,]
    if(!as.numeric(ROW[3])){
      hwrite(ROW[1], p, br=TRUE, contenteditable="TRUE", class=ROW[2], div=TRUE)
      }else{
      hwriteImage(ROW[1], p, br=TRUE, div=TRUE)
      }
  }  
  if(foot){
    hwrite(data[nrow(data),1], p, br=TRUE, contenteditable="TRUE", class="foot", div=TRUE)
    closePage(p, splash=FALSE)
    }else{
    closePage(p, splash=FALSE)
    }
  }
