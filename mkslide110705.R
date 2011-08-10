setwd("D:/My Dropbox/batade/")

css <- "D:/My Dropbox/batade/CSSslide.css"
js <- "D:/My Dropbox/batade/CSSslide.js"


#presentationlist
lis <- list(
c("title1", "hogehoge"),
c("title2", "what's next","an example"),
c("title3", "kajougaki","a type","or so")
)

mkslide <- function(txt, file, css, js, charset="CP932"){
  require(hwriter)
  require(stringr)
  mkpage <- function(txt){
    title <- hwrite(txt[1], heading=1)
    if(length(txt)-1 == 1){
      hwrite(str_c(title, txt[2]), div=TRUE)
      }else{
      kajougaki <- str_c(hmakeTag("li",txt[-1]),collapse="")
      kajougaki <- str_c("<ul>", kajougaki, "</ul>")
      hwrite(str_c(title, kajougaki), div=TRUE)
      }
    }
  pages <- str_c(unlist(lapply(txt, mkpage)), collapse="")
  p <- openPage(file, 
           link.javascript = js,
           link.css = css,
           charset = charset)
  hwrite(pages, p)
  closePage(p, splash=FALSE)
  }

mkslide(lis, "sample2.html", css=css, js=js)