css <- "D:/My Dropbox/batade/CSSslide.css"
js <- "D:/My Dropbox/batade/CSSslide.js"

setwd("D:/My Dropbox/batade/")

#presentationlist
lis <- list(
c("�^�C�g��1", "�����ɂق��ق�"),
c("�^�C�g��2", "���ɂȂɂ����邩","���������`�̃��X�g��"),
c("�^�C�g��3", "�ӏ�����������Ƃ�����","���������`��","����Ȋ���")
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
      kajougaki <- str_c("<ul>",line, "</ul>")
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