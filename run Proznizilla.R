source('~/������/R ����/send ALL reports.R')









library(readxl) #��������
library(readr) #��������
library(stringr) #��������
library (reshape2) #��������
library (Rcpp) # ��������
library (lubridate) # ��������


# C:\Program Files\R\R-3.1.3\library\shiny\examples\ELTIS dashboard2
# � ��������� ����� ���������� ��� ��������������� R ������
# ������� ����� � ����� C:\Documents and Settings\smirnov\��� ���������\������\R ����\���������� ������� ��� ��������



source('~/������/R ����/send ALL reports.R')

source('~/������/R ����/run and send email.R')

source('~/������/R ����/email with HTML charts 2.R')

# ������ �������� ��������
setwd('C:/Documents and Settings/smirnov/��� ���������/������/R ����')
source('~/������/R ����/send all mails test.R')
source('~/������/R ����/send ALL reports.R')


# ����������� ��� ���������� ����� �������� �����
flist <- list.files("Z:/������ ��� R", full.names = TRUE)
file.copy(flist, "C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2", overwrite = TRUE)


library(shiny)
runExample("ELTIS dashboard2",host="192.168.20.17",port=5050)













setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')
reportTotal<- read.csv("reportTotal.csv",stringsAsFactors = FALSE)
# 
# reportTotal<-rbind(colnames(reportTotal), reportTotal)
# 
# write.csv(reportTotal, 'C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2/reportTotal.csv', fileEncoding= "UTF-8", row.names= FALSE, col.names = FALSE, sep= ",")
# 
# 
# colnames(reportTotal)[c(1:8)]<- c("���","������������", "������", "���. ������", "�����. �����", "���� ��������", "������ ������", "���������")
# reportTotal[1, c(1:8)] <- c("���","������������", "������", "��� ������", "����� �����", "���� ��������", "������ ������", "���������")
# 
# reportTotalNames<- t(as.data.frame(colnames(reportTotal)))
# 
# write.csv(reportTotalNames, 'C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2/reportTotalNames.csv', fileEncoding= "UTF-8", row.names=FALSE)
# 
# reportTotalNames<- read.csv("reportTotalNames.csv",stringsAsFactors = FALSE, encoding = "UTF-8")
# 
# colnames(reportTotalNames)<-reportTotalNames[1,]
# reportTotal<- rbind(reportTotalNames[1,], reportTotal)
# colnames(reportTotal)<- reportTotal[1,]
# colnames(reportTotal)<- colnames(reportTotalNames)
colnames(reportTotal)[c(1:8)]<- c("code","item", "status", "MOQ", "safety stock", "lead time", "order size", "supplier")
# 
# colnames(reportTotal) <-c(paste(c(1:8), reportTotalNames[1,c(1:8)]),
#                               c(9:29))
# reportTotal [1, c(1:8)]<-reportTotalNames[1,c(1:8)]
# reportTotal [1, c(9:29)]<-c(9:29)
# reportTotal [1, 1] <- 1224

# testPerl(perl = "perl", verbose = TRUE)
# 
# dt<- head(mtcars)

reportName <- paste("reportTotal", " ", Sys.Date(), ".", "xls", sep="")

setwd('//Srv20/���/�������/Prognozilla/')
library(WriteXLS)
WriteXLS(reportTotal, ExcelFileName = reportName, SheetNames = NULL, perl = "perl",
         verbose = FALSE, Encoding = c("UTF-8" ),
         row.names = FALSE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = TRUE, BoldHeaderRow = TRUE,
         na = "",
         FreezeRow = 1, FreezeCol = 8,
         envir = parent.frame())



#"latin1", "cp1252"

library(sendmailR)
#####send plain email

from <- "m.smirnov@eltis.com"
to <- c( "m.smirnov@eltis.com")
body <- iconv("������ �������", to = "KOI8-R")
subject <- iconv("�������", to = "KOI8-R")

mailControl=list(smtpServer="xch01.eltis.intra")

#sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)





#####send same email with attachment

#needs full path if not in working directory
attachmentPath <- "C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2/reportTotal.csv"

#same as attachmentPath if using working directory
attachmentName <- "reportTotal.csv"


#key part for attachments, put the body and the mime_part in a list for msg
attachmentObject <- mime_part(x=attachmentPath,name=attachmentName)
bodyWithAttachment <- list(body,attachmentObject)

sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)



#########
msg <- mime_part('
                 
                 
                 <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
                 Strict//ru" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
                 
                 
                 <html xmlns="http://www.w3.org/1999/xhtml">
                 <head>
                 <meta http-equiv="Content-Type" content="text/html; charset=windows-1251" />
                 <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                 <title>HTML ���� ��������</title>
                 <style type="text/css">
                 </style>
                 </head>
                 
                 
                 <body>
                 <h1>
                 <font color = "40E0D0"> 
                 ��������� ������� !!
                 ����������� ���, ��� �������� ���� ����� ����������� ����� 100%-���������� ������ ������. ������ ������������ ���������� �����.
                 
                 <font color = "red"> 
                 � �� �
                 </h1>
                 

                 </body>
                 <IMG SRC="C:/WINDOWS/web/exclam.gif" ALT="some text" WIDTH=120 HEIGHT=120>
                 <IMG SRC="C:/WINDOWS/web//piechart.gif" ALT="some text" WIDTH=480 HEIGHT=480>
                 </html>')

  

## Override content type.
msg[["headers"]][["Content-Type"]] <- "text/html"


body    <- list(msg)
#body <- list(MyTable)
#MyTable[["Content-Type"]] <- "text/html"
sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)



library(xtable)
#�������� �������
s<-reportTotal[1:50,]




msg <- mime_part(paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
</head>

<body>', print(xtable(s), type = 'html'),',</body>

</html> '))





msg[["headers"]][["Content-Type"]] <- "text/html"
from <- "m.smirnov@eltis.com"
to <- "m.smirnov@eltis.com"
subject <- iconv("��� ����� ����", to = "KOI8-R")
body    <- list(msg, msg1)
sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)




# �������� ������� � �������������
# http://stackoverflow.com/questions/31290427/how-to-embed-a-html-file-in-email-body-using-rdcomclient



library(googleVis)
BranchTurnMSK<-renderGvis({ gvisGauge(1, 
                                             options=list(min=0, max=3, greenFrom=0.7,
                                                          greenTo=1.5, yellowFrom=1.5, yellowTo=3,
                                                          redFrom=0, redTo=0.7, width=200, height=200)) 
                                   
                                   
})


cat(BranchTurnMSK, file="tmpSample.html")





###################################
slices <- c(10, 12, 4, 16, 8) 
lbls <- c("400", "B-21", "�������", "Germany", "France")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")


s<- head(mtcars)

s<-pie3D(slices,labels=lbls,explode=0.1,
         main="Pie Chart of Countries ")

png('piechart.png')
plot(s)
# make plot
dev.off()


png('piechart2.png')
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")
# make plot
dev.off()

