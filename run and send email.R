
# скопировать все полученные после расчетов файлы
flist <- list.files("Z:/Отчеты для R", full.names = TRUE)
file.copy(flist, "C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2", overwrite = TRUE)




# преобразование  файла из формата csv в xls
setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')
reportTotal<- read.csv("reportTotal.csv",stringsAsFactors = FALSE)
colnames(reportTotal)[c(1:8)]<- c("code","item", "status", "MOQ", "safety stock", "lead time", "order size", "supplier")

reportName <- paste("reportTotal", " ", Sys.Date(), ".", "xls", sep="")

setwd('//Srv20/ПЗУ/Смирнов/Prognozilla/')
library(WriteXLS)
WriteXLS(reportTotal, ExcelFileName = reportName, SheetNames = NULL, perl = "perl",
         verbose = FALSE, Encoding = c("UTF-8" ),
         row.names = FALSE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = TRUE, BoldHeaderRow = TRUE,
         na = "",
         FreezeRow = 1, FreezeCol = 8,
         envir = parent.frame())



####
library(sendmailR)
#####send plain email

from <- "m.smirnov@eltis.com"
to <- c( "m.smirnov@eltis.com", "a.suhovsky@eltis.com", "v.litvinenkov@eltis.com", "a.osipova@eltis.com")
#"a.suhovsky@eltis.com", "v.litvinenkov@eltis.com", "a.osipova@eltis.com"
body <- iconv("график закупок", to = "KOI8-R")
subject <- iconv("график закупок", to = "KOI8-R")

mailControl=list(smtpServer="xch01.eltis.intra")

#sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)






msg <- mime_part('
                 
                 
                 <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
                 Strict//ru" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
                 
                 
                 <html xmlns="http://www.w3.org/1999/xhtml">
                 <head>
                 <meta http-equiv="Content-Type" content="text/html; charset=windows-1251" />
                 <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                 <title>HTML тест рассылка</title>
                 <style type="text/css">
                 </style>
                 </head>
                 
                 
                 <body>
                 <h2>
                 <font color = "000000"> 
                 Уважаемые коллеги!<br>
                 <br>
                 
                 <p><a href="file://Srv20/ПЗУ/Смирнов/Prognozilla/">график закупок ПКИ</a></p>
                 
                 
                 Пройдя по ссылке, Вы сможете открыть файл с планом закупок и все текущие параметры по товарным позициям. План можно посмотреть на текущую дату, также как и архивные.<br>
                 
                 <br>
                 
                 <font color = "red"> 
                 Это письмо сформировано автоматически, отвечать на него не нужно.
                 </h1>
                 
                 </body>
                 <IMG SRC="http://pastenow.ru/Upload/Paste/LKCU.gif" WIDTH=100 HEIGHT=80/> 
                 
                 ')


## Override content type.
msg[["headers"]][["Content-Type"]] <- "text/html"



bodyWithAttachment <- list(msg)


sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)







#развернуть систему на сервере
library(shiny)
runExample("ELTIS dashboard2",host="192.168.20.17",port=5050)



