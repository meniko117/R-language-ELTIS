library(sendmailR)
library(xtable)
#####send plain email




# ����������� ��� ���������� ����� �������� �����
flist <- list.files("Z:/������ ��� R", full.names = TRUE)
file.copy(flist, "C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2", overwrite = TRUE)




setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')
reportTotalsub<- read.csv("reportTotalsub.csv", stringsAsFactors = FALSE)

# reportTotalsub<- subset(reportTotal, reportTotal [,9]>0)
# reportTotalsub<- subset(reportTotalsub, reportTotalsub[,3] =="заказать") [,c(1,2,6,8,9)] # ������ �� ������� �� ������� ������
# write.csv(reportTotalsub, "reportTotalsubFilter.csv")
# 
# reportTotalsub<-read.csv("reportTotalsubFilter.csv", stringsAsFactors = FALSE)

#source ('C:/Documents and Settings/smirnov/��� ���������/������/R ����/charts.R')





# ��������������  ����� �� ������� csv � xls
setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')
reportTotal<- read.csv("reportTotal.csv",stringsAsFactors = FALSE)
colnames(reportTotal)[c(1:9)]<- c("code","item", "status", "MOQ", "safety stock", "lead time", "order size", "supplier", "responsible")


reportName <- paste("reportTotal", " ", Sys.Date(), ".", "xls", sep="")
reportNameSub<-  paste("reportTotalsub", " ", Sys.Date(), ".", "xls", sep="")

setwd('//Srv20/���/�������/Prognozilla/')
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

body <- iconv("������ �������", to = "KOI8-R")
subject <- iconv("������ ������� ���", to = "KOI8-R")

mailControl=list(smtpServer="xch01.eltis.intra")






setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')

s<- data.frame(matrix(nrow=2, ncol=2))

s<- read.csv("reportTotalsubMail.csv", stringsAsFactors = FALSE)
s<-s[order(s$�������������),] 

# colnames(s)<- c("����������", "����")
# s[,]<- "���� � ����"

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
                        ��������� �������!<br>
                        <br>
                        
                        <p><a href="file://Srv20/���/�������/Prognozilla/">������ ������� ���</a></p>
                        
                        
                        ������ �� ������, �� ������� ������� ���� � ������ ������� � ��� ������� ��������� �� �������� ��������. ���� ����� ���������� �� ������� ����, ����� ��� � ��������.<br>
                        
                        <br>
                        
                        
                        
                        <h4> ���� ������� ������, ������� ���������� ��������� � ��������� 4 ������ � �����������, ���� ��������� �� ��������� ������������� ������ ������</h3>
                        <br>
                        ', print( xtable(s), type = 'html'), '
                        
                        <br>
                        
                        
                        
                        
                        
                        
                        </body>
                        
                        
                        
                        
                        
                        <font color = "red"> 
                        <h3>��� ������ ������������ �������������, �������� �� ���� �� �����.</h3>
                        
                        </body>
                        <IMG SRC="http://pastenow.ru/Upload/Paste/LKCU.gif" WIDTH=150 HEIGHT=120/> 
                        
                        
                        </html>
                        
                        
                        
                        
                        
                        </html>  '))




## Override content type.
msg1[["headers"]][["Content-Type"]] <- "text/html"

bodyWithAttachment <- list( msg1)
sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)