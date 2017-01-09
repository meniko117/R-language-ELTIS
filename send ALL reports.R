# скопировать все полученные после расчетов файлы
flist <- list.files("Z:/Отчеты для R", full.names = TRUE)
file.copy(flist, "C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2", overwrite = TRUE)






library(sendmailR)
library(xtable)
#####send plain email


# создание диаграмм
# диаграммы типа "guage"
library(ggplot2)
setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')

#загрузка данных по оборачиваемости
BranchTurn<-read.csv("BranchTurn.csv")

#загрузка данных по продажам
salesAnalysis<- read.csv ("salesAnalysis.csv")
salesAnalysis<- as.matrix(salesAnalysis [1:2, 2:4])

#загрузка данных по стоимости излишков
surplusCost<- read.csv ("surplusHistory.csv")

gg.gauge <- function(pos,breaks=c(0,35,70,100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="forestgreen")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="red")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(round(breaks/35))))+
    annotate("text",x=0,y=0,label=pos/35,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}

setwd('//Srv20/ПЗУ/Смирнов/Prognozilla/')
png('turnMSK.png', width = 480, height = 250)
gg.gauge(BranchTurn[1,2]*35,breaks=c(0,35,70,100))
dev.off()

png('turnSPb.png', width = 480, height = 250)
gg.gauge(BranchTurn[2,2]*35,breaks=c(0,35,70,100))
dev.off()

png('turnNSK.png', width = 480, height = 250)
gg.gauge(BranchTurn[3,2]*35,breaks=c(0,35,70,100))
dev.off()






png('salesAnalysis.png',width = 800, height = 800)
barplot(salesAnalysis, main="Факт/План выполнения продаж",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(salesAnalysis), beside=TRUE)
dev.off()






png('surplusCost.png')
names(surplusCost)<- c("date", "МСК", "СПб", "НСК")
#df5<-head(df5)

surplusCost[,1]<- as.Date(surplusCost[,1])
legend_title<- "филиалы"

ggplot(surplusCost, aes(date)) +
  geom_line(aes(y = МСК,  colour = "МСК")) + 
  geom_line(aes(y = СПб, colour = "СПб")) + 
  geom_line(aes(y = НСК, colour = "НСК")) +
  ggtitle("Стоимость излишков, руб") +
  labs(x="Дни",y="Сумма, руб")+
  guides(color=guide_legend(title="филиалы"))

dev.off()














































#source ('C:/Documents and Settings/smirnov/Мои документы/Максим/R план/charts.R')



from <- "prognozilla@eltis.com"
to <- c( "m.smirnov@eltis.com", "serg-m@eltis.com", "o.grishakina@eltis.com", "m.floren@eltis.com", "maximov@eltis.com", "o.nazina@eltis.com", "i.ryabcovskaya@eltis.com",          
         "a.ochagov@eltis.com", "e.ilina@eltis.com", "r.antropov@eltis.com", "j.popkova@eltis.com", "e.kochetygova@eltis.com",
         "a.suhovsky@eltis.com", "v.litvinenkov@eltis.com", "a.osipova@eltis.com", "t.linnikova@eltis.com", "t.gan@eltis.com")

body <- iconv("график закупок", to = "KOI8-R")
subject <- iconv("Отчеты общие", to = "KOI8-R")

mailControl=list(smtpServer="xch01.eltis.intra")






setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')

s<- data.frame(matrix(nrow=2, ncol=2))
s<- read.csv("priorityProduction.csv", stringsAsFactors = FALSE)

k<- data.frame(matrix(nrow=2, ncol=2))
k<- read.csv("salesAnalysis.csv", stringsAsFactors = FALSE)
# colnames(s)<- c("количество", "дата")
# s[,]<- "план и факт"

# write.csv(s, 'C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2/enctest.csv', fileEncoding= "windows-1251")
# s<- read.table('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2/enctest.csv', sep = ",", encoding = "windows-1251", col.names=FALSE)

msg1 <- mime_part(paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
                        Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
                        <html xmlns="http://www.w3.org/1999/xhtml">
                        <head>
                        <meta http-equiv="Content-Type" content="text/html; charset=windows-1251" />
                        <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                        <style type="text/css">
                        </style>
                        </head>
                        
                        
                        
                        <body> ', print ("<h3>Добрый день! </h3> ", type = 'html') , 
                        #print( xtable(s), type = 'html'),
                        #print ("Привет!", type = 'html'), 
                        #print( xtable(s), type = 'html'), 
                        print("", type = 'html'), ', 
                        
                        <br>
                        <br>
                        
                        <h3>Ниже указаны основные показатели за текущее число. Выполнение плана продаж по филиалам.</h3>
                        (Взят план продаж из 1С в штуках, умножен на себестоимость каждой штуки. 
                        Также взяты данные по выполнению плана продаж на текущее число в штуках и умножены на себестоимость каждой штуки).  
                        
                        
                        
                        
                        
                        <h3> Соседний график показывает стоимость товаров на складе каждого филиала, которые мы относим к "излишкам", т.е. позиции, остатков по которым, хватает более, чем на 2 мес продаж.</h3>
                        <br>
                        ', print( xtable(k), type = 'html'), '
                        
                        <br>
                        
                        
                        
                        
                        
                        
                        <br>
                        <br>
                        <img src="file://Srv20/ПЗУ/Смирнов/Prognozilla//salesAnalysis.png" ALT="some text" WIDTH=500 HEIGHT=500>
                        <img src="file://Srv20/ПЗУ/Смирнов/Prognozilla//surplusCost.png" alt=""WIDTH=700 HEIGHT=500>
                        
                        
                        <br>
                        <br>
                        <h3>Ниже приведена таблица с указанием позиций, по которым какой-либо из филиалов испытывает дефицит. ЭТИ ПОЗИЦИИ являются ПРИОРИТЕТОМ для ПРОИЗВОДСТВА.</h3>
                        
                        <br>
                        ', print( xtable(s), type = 'html'), ', 
                        
                        <br>
                        <br>
                        <h3>Показатели оборачиваемости для каждого филиала в месяцах.  
                        Т.е. рассчитана стоимость товаров на складе и разделена на план продаж в стоимостном выражении. 
                        <br>
                        Показатель ок. 1 мес- хорошо, 2 мес - критически много.</h3>
                        
                        </body>
                        
                        <p>
                        <style type="text/css"> 
                        
                        .block {
                        text-align: center;
                        }
                        
                        .block li {
                        float: left;
                        padding-right: 20px;
                        }
                        
                        img1 {
                        width: 300px;
                        height: 300px;
                        }
                        .clear {
                        clear: both;
                        }
                        ul {
                        list-style: none
                        }
                        
                        .block-item p {
                        width: 10px;
                        }
                        
                        </style>
                        
                        <div class="block">
                        <ul>
                        
                        <li>
                        <div class="block-item">
                        <img src="file://Srv20/ПЗУ/Смирнов/Prognozilla//turnMSK.png" alt="" WIDTH=350 HEIGHT=180>
                        <h3> Москва </h3>
                        </div>
                        </li>
                        
                        <li>
                        <div class="block-item">
                        <img src="file://Srv20/ПЗУ/Смирнов/Prognozilla//turnSPb.png" alt="" WIDTH=350 HEIGHT=180>
                        <h3> СПб </h3> 
                        </div>
                        </li>
                        
                        <li>
                        <div class="block-item">
                        <img src="file://Srv20/ПЗУ/Смирнов/Prognozilla//turnNSK.png" alt="" WIDTH=350 HEIGHT=180>
                        <h3> НСК </h3> 
                        </div>
                        </li>
                        <li>
                        
                        
                        
                        <div class="clear"></div>
                        </ul>
                        </div>
                        
                        <html>
                        <br clear="all">
                        
                     
                        
                        <font color = "red"> 
                        <h3>Это письмо сформировано автоматически, отвечать на него не нужно.</h3>
                        
                        </body>
                        <IMG SRC="http://pastenow.ru/Upload/Paste/LKCU.gif" WIDTH=150 HEIGHT=120/> 
                        
                        
                        </html>
                        
                        
                        
                        
                        
                        </html>  '))




## Override content type.
msg1[["headers"]][["Content-Type"]] <- "text/html"

bodyWithAttachment <- list( msg1)
sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)












































library(sendmailR)
library(xtable)
#####send plain email

#source ('C:/Documents and Settings/smirnov/Мои документы/Максим/R план/charts.R')

to <- c( "m.smirnov@eltis.com", "r.antropov@eltis.com", "j.popkova@eltis.com", "e.kochetygova@eltis.com",  "m.floren@eltis.com", "t.gan@eltis.com")

from <- "prognozilla@eltis.com"

body <- iconv("график закупок", to = "KOI8-R")
subject <- iconv("Отчеты Москва", to = "KOI8-R")

mailControl=list(smtpServer="xch01.eltis.intra")






setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')

s<- data.frame(matrix(nrow=2, ncol=2))
s<- read.csv("deficitMSKfilter.csv", stringsAsFactors = FALSE)

k<- data.frame(matrix(nrow=2, ncol=2))
k<- read.csv("surplusCostMSK.csv", stringsAsFactors = FALSE)
k<- k [order(-k[,9]),] 
# colnames(s)<- c("количество", "дата")
# s[,]<- "план и факт"

# write.csv(s, 'C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2/enctest.csv', fileEncoding= "windows-1251")
# s<- read.table('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2/enctest.csv', sep = ",", encoding = "windows-1251", col.names=FALSE)

msg1 <- mime_part(paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
                        Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
                        <html xmlns="http://www.w3.org/1999/xhtml">
                        <head>
                        <meta http-equiv="Content-Type" content="text/html; charset=windows-1251" />
                        <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                        <style type="text/css">
                        </style>
                        </head>
                        
                        
                        
                        <body> ', print ("<h3>Добрый день! </h3> ", type = 'html') , 
                        #print( xtable(s), type = 'html'),
                        #print ("Привет!", type = 'html'), 
                        #print( xtable(s), type = 'html'), 
                        print("", type = 'html'), ', 
                        
                        <br>
                        <br>
                        
                        
                        
                        <br>
                        
                        
                        
                        <h3> Позиции, по которым возможно возникновение дефицита. В колонке "Плановый остаток через 4 нед" указана прогнозируемая величина дефицита. При необходмости предлагаю на эту величину разместить допзказ.</h3>
                        <br>
                        ', print( xtable(s), type = 'html'), '
                        
                        <br>
                        
                        
                        
                        
                        
                        <br>
                        <br>
                        <h3>Ниже указаны позиции, по которым оборачиваемость более 2 месяцев, т.е. остаток превышает месячные продажи более, чем в 2 раза. Требуется активизировать продажи, чтобы вернуть деньги в оборот.</h3>
                        
                        <br>
                        ', print( xtable(k), type = 'html'), ', 
                        
                        <br>
                        <br>
                        
                        
                        </body>
                        
                        
                        
                    
                        
                        <font color = "red"> 
                        <h3>Это письмо сформировано автоматически, отвечать на него не нужно.</h3>
                        
                        </body>
                        <IMG SRC="http://pastenow.ru/Upload/Paste/LKCU.gif" WIDTH=150 HEIGHT=120/> 
                        
                        
                        </html>
                        
                        
                        
                        
                        
                        </html>  '))




## Override content type.
msg1[["headers"]][["Content-Type"]] <- "text/html"

bodyWithAttachment <- list( msg1)
sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)





# Санкт-Петербург

to <- c( "m.smirnov@eltis.com", "a.ochagov@eltis.com", "e.ilina@eltis.com", "m.floren@eltis.com", "t.gan@eltis.com")
body <- iconv("график закупок", to = "KOI8-R")
subject <- iconv("Отчеты Санкт-Петербург", to = "KOI8-R")

mailControl=list(smtpServer="xch01.eltis.intra")






setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')

s<- data.frame(matrix(nrow=2, ncol=2))
s<- read.csv("deficitSPbfilter.csv", stringsAsFactors = FALSE)

k<- data.frame(matrix(nrow=2, ncol=2))
k<- read.csv("surplusCostSPb.csv", stringsAsFactors = FALSE)
k<- k [order(-k[,9]),] 
# colnames(s)<- c("количество", "дата")
# s[,]<- "план и факт"

# write.csv(s, 'C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2/enctest.csv', fileEncoding= "windows-1251")
# s<- read.table('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2/enctest.csv', sep = ",", encoding = "windows-1251", col.names=FALSE)

msg1 <- mime_part(paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
                        Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
                        <html xmlns="http://www.w3.org/1999/xhtml">
                        <head>
                        <meta http-equiv="Content-Type" content="text/html; charset=windows-1251" />
                        <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                        <style type="text/css">
                        </style>
                        </head>
                        
                        
                        
                        <body> ', print ("<h3>Добрый день! </h3> ", type = 'html') , 
                        #print( xtable(s), type = 'html'),
                        #print ("Привет!", type = 'html'), 
                        #print( xtable(s), type = 'html'), 
                        print("", type = 'html'), ', 
                        
                        <br>
                        <br>
                        
                        
                        
                        <br>
                        
                        
                        
                        <h3> Позиции, по которым возможно возникновение дефицита. В колонке "Плановый остаток через 4 нед" указана прогнозируемая величина дефицита. При необходмости предлагаю на эту величину разместить допзказ.</h3>
                        <br>
                        ', print( xtable(s), type = 'html'), '
                        
                        <br>
                        
                        
                        
                        
                        
                        <br>
                        <br>
                        <h3>Ниже указаны позиции, по которым оборачиваемость более 2 месяцев, т.е. остаток превышает месячные продажи более, чем в 2 раза.Требуется активизировать продажи, чтобы вернуть деньги в оборот.</h3>
                        
                        <br>
                        ', print( xtable(k), type = 'html'), ', 
                        
                        <br>
                        <br>
                        
                        
                        </body>
                        
                        
                        
                       
                        
                        <font color = "red"> 
                        <h3>Это письмо сформировано автоматически, отвечать на него не нужно.</h3>
                        
                        </body>
                        <IMG SRC="http://pastenow.ru/Upload/Paste/LKCU.gif" WIDTH=150 HEIGHT=120/> 
                        
                        
                        </html>
                        
                        
                        
                        
                        
                        </html>  '))




## Override content type.
msg1[["headers"]][["Content-Type"]] <- "text/html"

bodyWithAttachment <- list( msg1)
sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)






















# Новосибирск

to <- c( "m.smirnov@eltis.com", "maximov@eltis.com", "o.nazina@eltis.com", "i.ryabcovskaya@eltis.com",  "m.floren@eltis.com", "t.gan@eltis.com")
body <- iconv("график закупок", to = "KOI8-R")
subject <- iconv("Отчеты Новосибирск", to = "KOI8-R")

mailControl=list(smtpServer="xch01.eltis.intra")






setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')

s<- data.frame(matrix(nrow=2, ncol=2))
s<- read.csv("deficitNSKfilter.csv", stringsAsFactors = FALSE)

k<- data.frame(matrix(nrow=2, ncol=2))
k<- read.csv("surplusCostNSK.csv", stringsAsFactors = FALSE)
k<- k [order(-k[,9]),] 
# colnames(s)<- c("количество", "дата")
# s[,]<- "план и факт"

# write.csv(s, 'C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2/enctest.csv', fileEncoding= "windows-1251")
# s<- read.table('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2/enctest.csv', sep = ",", encoding = "windows-1251", col.names=FALSE)

msg1 <- mime_part(paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
                        Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
                        <html xmlns="http://www.w3.org/1999/xhtml">
                        <head>
                        <meta http-equiv="Content-Type" content="text/html; charset=windows-1251" />
                        <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                        <style type="text/css">
                        </style>
                        </head>
                        
                        
                        
                        <body> ', print ("<h3>Добрый день! </h3> ", type = 'html') , 
                        #print( xtable(s), type = 'html'),
                        #print ("Привет!", type = 'html'), 
                        #print( xtable(s), type = 'html'), 
                        print("", type = 'html'), ', 
                        
                        <br>
                        <br>
                        
                        
                        
                        <br>
                        
                        
                        
                        <h3> Позиции, по которым возможно возникновение дефицита. В колонке "Плановый остаток через 4 нед" указана прогнозируемая величина дефицита. При необходмости предлагаю на эту величину разместить допзказ.</h3>
                        <br>
                        ', print( xtable(s), type = 'html'), '
                        
                        <br>
                        
                        
                        
                        
                        
                        <br>
                        <br>
                        <h3>Ниже указаны позиции, по которым оборачиваемость более 2 месяцев, т.е. остаток превышает месячные продажи более, чем в 2 раза. Требуется активизировать продажи, чтобы вернуть деньги в оборот.</h3>
                        
                        <br>
                        ', print( xtable(k), type = 'html'), ', 
                        
                        <br>
                        <br>
                        
                        
                        </body>
                        
                        
                        
                       
                        
                        <font color = "red"> 
                        <h3>Это письмо сформировано автоматически, отвечать на него не нужно.</h3>
                        
                        </body>
                        <IMG SRC="http://pastenow.ru/Upload/Paste/LKCU.gif" WIDTH=150 HEIGHT=120/> 
                        
                        
                        </html>
                        
                        
                        
                        
                        
                        </html>  '))




## Override content type.
msg1[["headers"]][["Content-Type"]] <- "text/html"

bodyWithAttachment <- list( msg1)
sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)

#развернуть систему на сервере
library(shiny)
runExample("ELTIS dashboard2",host="192.168.20.17",port=5050)




