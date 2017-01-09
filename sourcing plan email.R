library(sendmailR)
library(xtable)
#####send plain email




# скопировать все полученные после расчетов файлы
flist <- list.files("Z:/Отчеты для R", full.names = TRUE)
file.copy(flist, "C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2", overwrite = TRUE)




setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')
reportTotalsub<- read.csv("reportTotalsub.csv", stringsAsFactors = FALSE)

# reportTotalsub<- subset(reportTotal, reportTotal [,9]>0)
# reportTotalsub<- subset(reportTotalsub, reportTotalsub[,3] =="Р·Р°РєР°Р·Р°С‚СЊ") [,c(1,2,6,8,9)] # фильтр по заказам на текущей неделе
# write.csv(reportTotalsub, "reportTotalsubFilter.csv")
# 
# reportTotalsub<-read.csv("reportTotalsubFilter.csv", stringsAsFactors = FALSE)

#source ('C:/Documents and Settings/smirnov/Мои документы/Максим/R план/charts.R')





# преобразование  файла из формата csv в xls
setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')
reportTotal<- read.csv("reportTotal.csv",stringsAsFactors = FALSE)
colnames(reportTotal)[c(1:9)]<- c("code","item", "status", "MOQ", "safety stock", "lead time", "order size", "supplier", "responsible")


reportName <- paste("reportTotal", " ", Sys.Date(), ".", "xls", sep="")
reportNameSub<-  paste("reportTotalsub", " ", Sys.Date(), ".", "xls", sep="")

setwd('//Srv20/ПЗУ/Смирнов/Prognozilla/')
library(WriteXLS)
WriteXLS(reportTotal, ExcelFileName = reportName, SheetNames = NULL, perl = "perl",
         verbose = FALSE, Encoding = c("UTF-8" ),
         row.names = FALSE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = TRUE, BoldHeaderRow = TRUE,
         na = "",
         FreezeRow = 1, FreezeCol = 8,
         envir = parent.frame())

colnames(reportTotalsub)[1:5]<- c("code", "item", "lead time", "supplier", "responsible")

library(WriteXLS)
WriteXLS(reportTotalsub, ExcelFileName = reportNameSub, SheetNames = NULL, perl = "perl",
         verbose = FALSE, Encoding = c("UTF-8" ),
         row.names = FALSE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = TRUE, BoldHeaderRow = TRUE,
         na = "",
         FreezeRow = 1, FreezeCol = 8,
         envir = parent.frame())


#, "a.suhovsky@eltis.com", "v.litvinenkov@eltis.com", "a.osipova@eltis.com", "t.linnikova@eltis.com"

to <- c( "m.smirnov@eltis.com", "a.suhovsky@eltis.com", "v.litvinenkov@eltis.com", "a.osipova@eltis.com", "t.linnikova@eltis.com")

from <- "prognozilla@eltis.com"

body <- iconv("график закупок", to = "KOI8-R")
subject <- iconv("График закупок ПКИ", to = "KOI8-R")

mailControl=list(smtpServer="xch01.eltis.intra")






setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')

s<- data.frame(matrix(nrow=2, ncol=2))

s<- read.csv("reportTotalsubMail.csv", stringsAsFactors = FALSE)
s<-s[order(s$ответственный),] 

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
                        
                        
                        
                        <body>
                        <h4>
                        <font color = "000000"> 
                        Уважаемые коллеги!<br>
                        <br>
                        
                        <p><a href="file://Srv20/ПЗУ/Смирнов/Prognozilla/">график закупок ПКИ</a></p>
                        
                        
                        Пройдя по ссылке, Вы сможете открыть файл с планом закупок и все текущие параметры по товарным позициям. План можно посмотреть на текущую дату, также как и архивные.<br>
                        
                        <br>
                        
                        
                        
                        <h4> Ниже указаны заказы, которые необходимо разместиь в ближайшие 4 недели у поставщиков, либо отправить на доработку полуфабрикаты разных этапов</h3>
                        <br>
                        ', print( xtable(s), type = 'html'), '
                        
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