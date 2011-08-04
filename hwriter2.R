#HTMLレポートの機能まとめ
#通常の文章はhwriterでべた打ちする→CSSでスタイルは設定しておく
#googleAPIはガジェットにして公開する
→ createGoogleGadget関数でxml化→どこか（例えばgoogle site）にXML公開→igoogle経由でガジェット化してアドレスを取得→下の関数でHTMLに埋め込む

library(hwriter)
hwriteGadget <- fucntion(gadgetaddress, page=NULL, ...){
    content <- paste(sep="",'http://gmodules.com/ig/ifr?url=', gadgetaddress, '&synd=open&w=160&h=130&title=&border=%23ffffff%7C0px%2C0px+solid+%23ffffff&output=js')
    hwriter(hmakeTag("script", content), page, ...)
    }
