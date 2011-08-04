

#' make HTML report with text and images
#' 
#' Make HTML report from rayout dataframe. This is a wrapper function of
#' "hwrite" in the "hwriter" package.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param filename character.A filename for output.
#' @param data 3 columns dataframe. left column:text or a image file name,
#'   middle column:choose a text size LL/L/M/S, right column:image file or not,
#'   image file=1, others =0. See Example.
#' @param foot logical. If TRUE, the bottom row is converted into foot note.
#' @param charset character. Default charset is "CP932".
#' @param lang character. Default language code is "JP".
#' @return NULL(a HTML file)
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' 
#' #prepare a image file.
#' png("sample.png")
#' plot(iris$Sepal.Length, iris$Sepal.Width)
#' dev.off()
#' 
#' #make a dataframe for the report rayout
#' df <- data.frame("This is a sample report.", "LL", stringsAsFactors=FALSE)
#' df[2,] <- c("This is a caption.", "L")
#' df[3,] <- c("This is a caption2.", "M")
#' df[4,] <- c("This is a text.", "S")
#' df[5,] <- c("sample.png", "S")
#' df[6,] <- c("This is a foot note", "S")
#' 
#' mkhtml("sample.html", df)
#' 
#' #If you use a web-browser supporting CSS3, you can edit text parts on it.
#' 
mkhtml <-
function(filename, data, foot=TRUE, charset="CP932", lang="JP"){
	require(hwriter)
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
    content <- unlist(strsplit(split="\\|", ROW[1]))
     if(length(content)>1){
         if(isTRUE(as.logical(grep(".*\\.png$|.*\\.jpg$|.*\\.jpeg$|.*\\.gif$|.*\\.tiff$", content, perl=TRUE))) && nchar(content) < 15){
         type <- "DI"
         }else if(isTRUE(as.logical(grep(".*\\.htm$|.*\\.html$", content, perl=TRUE))) && nchar(content) < 15){
         type <- "DH"
         }       
     }else{
       if(isTRUE(as.logical(grep(".*\\.png$|.*\\.jpg$|.*\\.jpeg$|.*\\.gif$|.*\\.tiff$", content, perl=TRUE))) && nchar(ROW[1]) < 15){
         type <- "I"
         }else if(isTRUE(as.logical(grep(".*\\.htm$|.*\\.html$", content, perl=TRUE)))){
         type <- "H"
         }else{
         type <- "T"
         }
     }
     switch(type,
       "T" = hwrite(content, p, br=TRUE, contenteditable="TRUE", class=ROW[2], div=TRUE),
       "I" = hwriteImage(content, p, br=TRUE, div=TRUE, center=TRUE, border=0),
       "DI" = hwriteImage(content, p, br=TRUE, div=TRUE, center=TRUE, border=0),
       "H" = hwrite(paste('<iframe src=', content, ' frameborder="0" width="1200" height="600" scrolling="no"></iframe>', sep=""), p, center=TRUE),
       "DH" = hwrite(c(paste('<iframe src=', content, ' frameborder="0" width="600" height="400" scrolling="no"></iframe>', sep=""),paste('<iframe src=', content, ' frameborder="0" width="600" height="400" scrolling="no"></iframe>', sep="")), p, center=TRUE),
       stop(message = "rayout error")
       )
    }
  if(foot){
    hwrite(data[nrow(data),1], p, br=TRUE, contenteditable="TRUE", class="foot", div=TRUE)
    closePage(p, splash=FALSE)
    }else{
    closePage(p, splash=FALSE)
    }
  }

