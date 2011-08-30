toHATENA <- function(trg, localDBP="D:/My Dropbox/Public", pubDBP="http://dl.dropbox.com/u/956851"){
#make googlegadget and save to Dropbox

require(RCurl)
require(RHTMLForms)
require(RJSONIO)
XMLname <- paste(sep="", unlist(strsplit(split="\\.", trg))[1], ".xml")
url <- paste(sep="", "http://api.zoom.it/v1/content/?url=", paste(sep="/", pubDBP, trg))
resAPI <- fromJSON(getURL(url))
res <- sprintf('<Module><ModulePrefs title=""></ModulePrefs><Content type="html"><![CDATA[%s]]></Content></Module>', resAPI$embedHtml)
address <- paste(sep="/", localDBP, XMLname)
write(res, address)

#upload the XML as googlegadget

forms <-  getHTMLFormDescription("http://www.google.com/ig/submit")
fun <-  createFunction(forms[[2]])
webDBP <- paste(sep="/", pubDBP, XMLname)
fun(url = webDBP)

#hatena

reshatena <- paste(sep="", '<script src="http://www.gmodules.com/ig/ifr?url=', webDBP, '&amp;synd=open&amp;w=580&amp;h=450&amp;title=&amp;border=%23ffffff%7C3px%2C1px+solid+%23999999&amp;output=js"></script>')
out <- paste(sep="", unlist(strsplit(split="\\.", trg))[1], "code.txt")
hatenafilename <- paste(sep="/", localDBP, out)
write(reshatena, hatenafilename)
}