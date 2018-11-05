library(bitops)
library(RCurl)
library(XML)
library(stringr)
library(foreign)

key<-"江苏启东事件"          #筛选关键词
timeStart<-as.Date("2012-08-01")   #起始时间
timeEnd<-as.Date("2012-09-01")     #结束时间
interval<-2                          #筛选区间，1表示按小时，2表示日

#声明自己身份以及联系方式
useragent<-str_c(R.version$platform, R.version$version.string, sep = ", ")
httpheader<-c(from = "mail@trelig.cn")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  #设置工作路径为当前文件路径，只能在Rstudio中使用
if(!file.exists("./datafiles"))     #创建数据文件夹
  dir.create("./datafiles")
setwd("./datafiles")

datafetch<-function(parsed_doc){
  #if判断是否含有搜索结果
  if(xpathSApply(parsed_doc, path = "//div[@class = 'card-wrap']/div", xmlGetAttr, "class") == "card card-no-result s-pt20b40"){
      return()              #no->结束提取并返回
  }
  
  #xPath对用户昵称、正文内容、转发、评论、点赞量、时间记录到变量中
  #获取用户名列表
  userName<- xpathSApply(parsed_doc, path = "/html/body//div//a[@class = 'name']", xmlValue)
  #获取日期
  pubdate<- xpathSApply(parsed_doc, path = "/html/body//div//p[@class = 'from']/a[@target = '_blank']", xmlValue)
  #获取微博正文
  txtDetail<- xpathSApply(parsed_doc, path = "/html/body//div//p[@class = 'txt' and @node-type = 'feed_list_content']", xmlValue)
  #获取转发量之类的数据
  dataDetail<- xpathSApply(parsed_doc, path = "/html/body//div/ul/li/a[@action-type = 'login']",
                           xmlValue)
  #获取原文链接
  linkOri<-xpathSApply(parsed_doc, path = "/html/body//div//p[@class = 'from']/a[@target = '_blank']", xmlGetAttr, "href")
  #整理归类转发量之类的数据
  likenum<-NULL
  repostnum<-NULL
  commentnum<-NULL
  for (i in 1:length(dataDetail)){
    switch (i%%4 + 1,
            likenum<-c(likenum, as.numeric(dataDetail[i])),
            NULL,
            repostnum<-c(repostnum, as.numeric(sub('^...' , '', dataDetail[i]))),
            commentnum<-c(commentnum, as.numeric(sub('^...', '',  dataDetail[i])))
    )
  }
  tempframe<-data.frame(
    User = userName,
    Date = pubdate,
    Repost = repostnum,
    Comment = commentnum,
    Like = likenum,
    Link = linkOri,
    TXT = txtDetail,
    stringsAsFactors = FALSE
  )
  #write.csv(tempframe, file = "C:/Users/ZHU/Desktop/test2.csv")
  return(tempframe)
}

#解析html
getparsed_doc<-function(url_get){
  target_html<-getURL(url_get, useragent = useragent, httpheader = httpheader)#获取网页内容
  parsed_doc<-htmlParse(file = target_html)
  return(parsed_doc)
}

getnewurl<-function(key, timescope, npage){
  newurl<-str_c("https://s.weibo.com/weibo?q=", key, "&scope=ori&typeall=1&suball=1&timescope=custom:",timescope, "&Refer=g&page=", npage)#构造检索URL
  return(newurl)
}

#检查有无下一页
nextpage<-function(parsed_doc){
  urls<-xpathSApply(parsed_doc, path = "//div[@class = 'm-page']//span/ul/li/a",
                    xmlGetAttr, "href")           #获得页数的url
  if(is.null(urls)){
    return()
  }
  for (npage in 2:length(urls)){
    tempframe<-data.frame(NULL)
    urlpage<-getnewurl(key, timescope, npage)
    parsed_doc2<-getparsed_doc(urlpage)
    nexttemp<-datafetch(parsed_doc2)           #数据处理
    tempframe<-rbind(tempframe, nexttemp)    #合并到总数据帧
  }
  return(tempframe)
}

totalframe<-data.frame(
  User = NULL,
  Date = NULL,
  Repost = NULL,
  Comment = NULL,
  Like = NULL,
  Link = NULL,
  TXT = NULL,
  stringsAsFactors = FALSE
)                  #定义一个data.frame，用以包含一天/月的数据并一次性写入文件

logname<-str_c("messages", Sys.time(), ".log")

#循环改变搜索的时间范围
days<-as.character(seq(from = timeStart, to = timeEnd, by = 1))
for (i in 1:length(days)){
  sink(file = logname, append = TRUE, split=TRUE)
  tempmonth<-sub("^.....", "", days[i])
  monthnum<-as.numeric(sub("...$", "", tempmonth))
  
  
  if(interval == 1){           #筛选期间为小时
    for (hour in 0:23){
      npage<-1            #默认页数
      timescope<-str_c(days[i],"-", hour, ":", days[i], "-", hour+1)
      url_get<-getnewurl(key, timescope, npage)
      
      parsed_doc<-getparsed_doc(url_get)
      tempframe<-datafetch(parsed_doc)           #数据处理
      tempframe2<-nextpage(parsed_doc)                #检查是否有下一页并处理
      
      tempframe<-rbind(tempframe, tempframe2)
      hournumber<-nrow(tempframe)
      totalframe<-rbind(totalframe, tempframe)    #合并到总数据帧
      
      caution<-str_c(Sys.time(), ": 已处理", timescope, "的数据,共有 ", hournumber, " 项,等待3s")
      print(caution)                              #输出提示信息
      Sys.sleep(3)         #设置搜索间隔时间(避免被封)
      
      daynumber<-nrow(totalframe)
      filename<-str_c(key,"-",days[i], ".csv")
      daycaution<-str_c(Sys.time(), ": 已处理完一天的数据,共有 ",daynumber, " 项，等待6s！")
      print(daycaution)
      
      if(daynumber > 0){
        filecaution<-str_c(Sys.time(), ": 文件存储为 ", filename)
        print(filecaution)
        write.csv(totalframe, file = filename)            #保存文件为指定文件名
      }
      totalframe<-data.frame(NULL)           #清空totalframe
      Sys.sleep(6)
    }
  }else{                      #筛选区间为日
    npage<-1            #默认页数
    timescope<-str_c(days[i], "-00:", days[i+1], "-00")
    url_get<-getnewurl(key, timescope, npage)
    
    parsed_doc<-getparsed_doc(url_get)
    tempframe<-datafetch(parsed_doc)           #数据处理
    tempframe2<-nextpage(parsed_doc)                #检查是否有下一页
    
    tempframe<-rbind(tempframe, tempframe2)
    totalframe<-rbind(totalframe, tempframe)      #合并到总数据帧
    daycaution<-str_c(Sys.time(), ": 已处理完", days[i], "的数据，等待3秒")
    print(daycaution)
    Sys.sleep(3)
    
    #完成一天遍历，检测是否完成一个月    
    tempmonth2<-sub("^.....", "", days[i+1])
    monthnum2<-as.numeric(sub("...$", "", tempmonth2))
    if(monthnum != monthnum2){
      monthnumber<-nrow(totalframe)
      
      monthcaution<-str_c(Sys.time(), ": 已处理完", sub("...$", "", days[i]), "月的数据,共有 ",monthnumber, " 项，等待6s！")
      print(monthcaution)
      
      
      if(monthnumber > 0){
        filename<-str_c(key,"-",sub("...$", "", days[i]), ".csv")
        filecaution<-str_c(Sys.time(), ": 文件存储为 ", filename)
        print(filecaution)
        write.csv(totalframe, file = filename)            #保存文件为指定文件名
      }
      totalframe<-data.frame(NULL)                        #清空totalframe
      Sys.sleep(6)
    }
  }
  if((i+2) > length(days)){
    break()
  }
  sink()
}
endcaution<-str_c("任务完成，日志记录保存为 ", logname)
print(endcaution)
