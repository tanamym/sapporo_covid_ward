library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(lubridate)
repeat{
  while(wday(Sys.Date(),label = TRUE)=="月"){
    path<-"https://www.city.sapporo.jp/hokenjo/f1kansen/2019n-covhassei.html"
    RH<-read_html(path)
    tb<-RH%>%
      rvest::html_nodes("tbody")
    date<-
      RH%>%
      html_nodes("h3")%>%
      html_text()%>%
      data.frame()%>%
      filter(str_detect(.,"区ごとの発生状況"))%>%
      rename("Date"=".")%>%
      mutate(Date=str_remove(Date,"区ごとの発生状況（"),
             Date=str_remove(Date,"分）"))
    text<-
      tb[[7]]%>%
      rvest::html_nodes("tr")%>%
      rvest::html_text()
    K<-
      text[1]%>%
      strsplit("\r\n")%>%
      data.frame()
    colnames(K)<-"区名"
    K<-K%>%
      filter(str_detect(区名,"区"))%>%
      mutate(区名=str_remove_all(区名," "))
    N<-
      text[2]%>%
      strsplit("\r\n")%>%
      data.frame()
    colnames(N)<-"count"  
    N<-N%>%
      mutate(count=stri_trans_nfkc(count))%>%
      filter(str_detect(count,"[0-9]"))%>%
      filter(!str_detect(count,"\\(.+?\\)"))%>%
      mutate(count=str_remove_all(count," "))
    data1<-cbind(K,N)%>%
      mutate(count=ifelse(count=="1~4","4",count))%>%
      mutate(count=str_remove(count,","))%>%
      mutate(count=as.numeric(count))%>%
      mutate(D=date$Date[1])%>%
      mutate(Date1=str_sub(D,1,5),
             Date2=str_sub(D,-6,-1),
             Date1=str_remove(Date1,"～"),
             Date2=str_remove(Date2,"～"))%>%
      mutate(Date1=paste0("2022/",Date1),
             Date2=paste0("2022/",Date2),
             Date1=str_replace_all(Date1,"月","/"),
             Date1=str_remove_all(Date1,"日"),
             Date2=str_replace_all(Date2,"月","/"),
             Date2=str_remove_all(Date2,"日"))%>%
      select(D,Date1,Date2,区名,count)%>%
      mutate(D=str_replace_all(D,"月|年","/"),
             D=str_remove_all(D,"日"),
             D=paste0("2022/",D),
             D=str_replace(D,"～","～2022/"))
      # mutate(Date1=as.Date(Date1,"%Y年%m月%d日"),
      #        Date2=as.Date(Date2,"%Y年%m月%d日"))
    # data3<-rbind(read.csv("札幌市区別データ2021-09-13.csv"),
    #              read.csv("札幌市区別データ2021-09-20.csv"),
    #              read.csv("札幌市区別データ2021-09-27.csv"))
    data2<-data.frame()
    data2<-read.csv("札幌市区別データ2.csv",encoding="UTF-8")%>%
      mutate(D2=as.Date(Date1))%>%
      arrange(desc(D2))
    if(data1[1,1]!=data2[1,1]){
      data3<-rbind(data2%>%select(-D2),data1)
      write.csv(data3,"札幌市区別データ2.csv",
                row.names=F,fileEncoding="UTF-8")
      write.csv(data1,paste0("札幌市区別データ",Sys.Date(),".csv"),
                row.names=F)
      print("出力しました")
    }
    print(Sys.time())
    Sys.sleep(3600)
  }
}