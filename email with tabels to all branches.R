library(sendmailR)
library(xtable)
#####send plain email

#source ('C:/Documents and Settings/smirnov/��� ���������/������/R ����/charts.R')

to <- c( "m.smirnov@eltis.com")

from <- "prognozilla@eltis.com"
to <- c( "m.smirnov@eltis.com", "r.antropov@eltis.com", "j.popkova@eltis.com", "e.kochetygova@eltis.com",  "o.grishakina@eltis.com", "m.floren@eltis.com")
body <- iconv("������ �������", to = "KOI8-R")
subject <- iconv("������ ������", to = "KOI8-R")

mailControl=list(smtpServer="xch01.eltis.intra")






setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')

s<- data.frame(matrix(nrow=2, ncol=2))
s<- read.csv("deficitMSKfilter.csv", stringsAsFactors = FALSE)

k<- data.frame(matrix(nrow=2, ncol=2))
k<- read.csv("surplusCostMSK.csv", stringsAsFactors = FALSE)
k<- k [order(-k[,9]),] 
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
                        
                        
                        
                        <body> ', print ("<h3>������ ����! </h3> ", type = 'html') , 
                        #print( xtable(s), type = 'html'),
                        #print ("������!", type = 'html'), 
                        #print( xtable(s), type = 'html'), 
                        print("", type = 'html'), ', 
                        
                        <br>
                        <br>
                        
                         
                        
                        <br>
                        
                        
                        
                        <h3> �������, �� ������� �������� ������������� ��������. � ������� "�������� ������� ����� 4 ���" ������� �������������� �������� ��������. ��� ������������ ��������� �� ��� �������� ���������� �������.</h3>
                        <br>
                        ', print( xtable(s), type = 'html'), '
                        
                        <br>
                        
                        
                        
  
                        
                        <br>
                        <br>
                        <h3>���� ������� �������, �� ������� ��������������� ����� 2 �������, �.�. ������� ��������� �������� ������� �����, ��� � 2 ����. ��������� �������������� �������, ����� ������� ������ � ������.</h3>
                        
                        <br>
                        ', print( xtable(k), type = 'html'), ', 
                        
                        <br>
                        <br>
                        
                        
                        </body>
                        
                        
                        
                        <h1>"����� ��� �������, �������� � �������� ����������, ��������� �����!" (c) �.�. ��������</h1>
                        
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





# �����-���������

to <- c( "m.smirnov@eltis.com", "a.ochagov@eltis.com", "e.ilina@eltis.com", "o.grishakina@eltis.com", "m.floren@eltis.com")
body <- iconv("������ �������", to = "KOI8-R")
subject <- iconv("������ �����-���������", to = "KOI8-R")

mailControl=list(smtpServer="xch01.eltis.intra")






setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')

s<- data.frame(matrix(nrow=2, ncol=2))
s<- read.csv("deficitSPbfilter.csv", stringsAsFactors = FALSE)

k<- data.frame(matrix(nrow=2, ncol=2))
k<- read.csv("surplusCostSPb.csv", stringsAsFactors = FALSE)
k<- k [order(-k[,9]),] 
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
                        
                        
                        
                        <body> ', print ("<h3>������ ����! </h3> ", type = 'html') , 
                        #print( xtable(s), type = 'html'),
                        #print ("������!", type = 'html'), 
                        #print( xtable(s), type = 'html'), 
                        print("", type = 'html'), ', 
                        
                        <br>
                        <br>
                        
                        
                        
                        <br>
                        
                        
                        
                        <h3> �������, �� ������� �������� ������������� ��������. � ������� "�������� ������� ����� 4 ���" ������� �������������� �������� ��������. ��� ������������ ��������� �� ��� �������� ���������� �������.</h3>
                        <br>
                        ', print( xtable(s), type = 'html'), '
                        
                        <br>
                        
                        
                        
                        
                        
                        <br>
                        <br>
                        <h3>���� ������� �������, �� ������� ��������������� ����� 2 �������, �.�. ������� ��������� �������� ������� �����, ��� � 2 ����.��������� �������������� �������, ����� ������� ������ � ������.</h3>
                        
                        <br>
                        ', print( xtable(k), type = 'html'), ', 
                        
                        <br>
                        <br>
                        
                        
                        </body>
                        
                        
                        
                        <h1>"����� ��� �������, �������� � �������� ����������, ��������� �����!" (c) �.�. ��������</h1>
                        
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






















# �����������

to <- c( "m.smirnov@eltis.com", "maximov@eltis.com", "o.nazina@eltis.com", "i.ryabcovskaya@eltis.com", "o.grishakina@eltis.com", "m.floren@eltis.com")
body <- iconv("������ �������", to = "KOI8-R")
subject <- iconv("������ �����������", to = "KOI8-R")

mailControl=list(smtpServer="xch01.eltis.intra")






setwd('C:/Program Files/R/R-3.1.3/library/shiny/examples/ELTIS dashboard2')

s<- data.frame(matrix(nrow=2, ncol=2))
s<- read.csv("deficitNSKfilter.csv", stringsAsFactors = FALSE)

k<- data.frame(matrix(nrow=2, ncol=2))
k<- read.csv("surplusCostNSK.csv", stringsAsFactors = FALSE)
k<- k [order(-k[,9]),] 
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
                        
                        
                        
                        <body> ', print ("<h3>������ ����! </h3> ", type = 'html') , 
                        #print( xtable(s), type = 'html'),
                        #print ("������!", type = 'html'), 
                        #print( xtable(s), type = 'html'), 
                        print("", type = 'html'), ', 
                        
                        <br>
                        <br>
                        
                        
                        
                        <br>
                        
                        
                        
                        <h3> �������, �� ������� �������� ������������� ��������. � ������� "�������� ������� ����� 4 ���" ������� �������������� �������� ��������. ��� ������������ ��������� �� ��� �������� ���������� �������.</h3>
                        <br>
                        ', print( xtable(s), type = 'html'), '
                        
                        <br>
                        
                        
                        
                        
                        
                        <br>
                        <br>
                        <h3>���� ������� �������, �� ������� ��������������� ����� 2 �������, �.�. ������� ��������� �������� ������� �����, ��� � 2 ����. ��������� �������������� �������, ����� ������� ������ � ������.</h3>
                        
                        <br>
                        ', print( xtable(k), type = 'html'), ', 
                        
                        <br>
                        <br>
                        
                        
                        </body>
                        
                        
                        
                        <h1>"����� ��� �������, �������� � �������� ����������, ��������� �����!" (c) �.�. ��������</h1>
                        
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

