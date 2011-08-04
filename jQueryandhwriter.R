
###

library(hwriter)

###jQueryはgoogleAPIを利用

p <- openPage("report.html",
link.javascript="https://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.js", charset="shift-jis")

###Javascriptの記述

hwrite(hmakeTag("script", '//
$(function(){
    $(".open").click(function(){
     $("#slideBox").slideToggle("slow");
    });
});
//', type="text/javascript"), table=FALSE,p)

###CSSの記述

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
"),p)

###HTMLの記述

hwrite(hmakeTag("p", "ボタン", class="open"),p)
hwrite("いえーい!!!!!!!!!!!!!!!!!!", p, div=TRUE, id="slideBox")
hwrite("jQueryのソースはこちらのものを利用しています。", p, link="http://www.webcreatorbox.com/sample-jquery-tips20")
closePage(p, splash=FALSE)
shell("report.html")
