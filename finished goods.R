##�������� ������� ��� ����������� ������
##�������� ����� ������� ��������� ������
##
library(readxl)
library(readr)
sat2[is.na(sat2)]<-0

warehouseStockMoscow<-match("������ ������� (�������)", colnames(sat2))
stockComponentsMoscow<-sat2[,c(4,6,warehouseStockMoscow)]
names(stockComponentsMoscow)<-c("code", "item")

stockComponentsMoscow[is.na(stockComponentsMoscow)]<-0


warehouseStockMoscowBranch<-match("������, ����� �������", colnames(sat2))
warehouseStockMoscowTransit<-match("������ � ���� (������)", colnames(sat2))
# 
# 
# #������� ���� ������ �� ��������
# date<-names(allStock)[1]
# library(stringr)
# dateReport<-str_sub(date, start= -8)
# 
# weekReport<-round(as.numeric(
#   difftime(strptime(dateReport, format = "%d.%m.%y"),
#            strptime("01.01.2015", format = "%d.%m.%Y"), units="weeks") +1), digits=0)

#��������� ����� ������ ������� ��������
names(stockComponentsMoscow)[3]<-weekReport


#������� �� �������� ������� � �������� 
stockComponentsMoscow[ ,3]<- sat2[ ,warehouseStockMoscow]-sat2[ ,warehouseStockMoscow +1]+sat2[ ,warehouseStockMoscowBranch] - sat2[ ,warehouseStockMoscowBranch+1] + sat2[ ,warehouseStockMoscowTransit] - sat2[ ,warehouseStockMoscowTransit +1]


stockComponentsMoscow[is.na(stockComponentsMoscow)]<-0
#write.csv(stockComponentsMoscow, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/stockMoscow.�sv")



# ��������� �������� ������� ���������
finishedGoodsList<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/finishedGoodsList.xls")
colnames(finishedGoodsList)<- c ("code", "item")


#������� ��������� 52-��������� ������
currentFinishedStock<-as.data.frame(matrix(nrow=nrow(finishedGoodsList), ncol=54))
currentFinishedStock[,1:2]<-finishedGoodsList[,1:2]
names(currentFinishedStock)<-c("code", "item", c(1:52))

# �������� ������� ������� ��� ������ � ������� ������
weeklyStockMoscow<-merge(currentFinishedStock, stockComponentsMoscow, by ="code", all.x= TRUE)

weeklyStockMoscow [ ,weekReport+2]<-weeklyStockMoscow [,56]
weeklyStockMoscow <- weeklyStockMoscow [ , 1:54]
weeklyStockMoscow[is.na(weeklyStockMoscow)]<-0
colnames(weeklyStockMoscow)[3:54] <- c (1:52)

#��������� ��������� �������������� (�����. �����, ������ ������ � ����), � ����� ���� ������
salesMoscow<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/Moscow sales and parameters.xls")








##
##
##
##
## �������� ����� ������ ������ � ������� 1�, � �� �� �����


# �������� ����� ������ �� ������� �����

mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", month(Sys.Date()), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 
t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("code", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])

salesMoscow<- merge(salesMoscow, t, by ="code", all.x= TRUE)
salesMoscow[is.na(salesMoscow)]<-0

#� ��������� ������ �������� �������
colnames(salesMoscow) [c(6:57)] <-colnames(alltimeHeader)

#������ �������, ����������� � �������� ������
colMonth<- which (alltimeHeader[1,] %in% month(Sys.Date()) )
# ����������� ������������ ���������� �������������� ������� �������� ������
salesMoscow [ ,colMonth+5 ]<- ceiling(salesMoscow [ ,59] /length(colMonth))

salesMoscow <- salesMoscow [ ,1:57]




# �������� ����� ������ �� ��������� �����

mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", 
           ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 
t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("code", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])

salesMoscow<- merge(salesMoscow, t, by ="code", all.x= TRUE)
salesMoscow[is.na(salesMoscow)]<-0


#������ �������, ����������� � C��������� ������
colMonth<- which (alltimeHeader[1,] %in% ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12)  )
# ����������� ������������ ���������� �������������� ������� ���������� ������
salesMoscow [ ,colMonth+5 ]<- ceiling(salesMoscow [ ,59] /length(colMonth))
salesMoscow <- salesMoscow [ ,1:57]






# �������� ����� ������ ����� 2 ������

mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", 
           ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 
t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("code", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])

salesMoscow<- merge(salesMoscow, t, by ="code", all.x= TRUE)
salesMoscow[is.na(salesMoscow)]<-0


#������ �������, ����������� � C��������� ������
colMonth<- which (alltimeHeader[1,] %in% ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12)  )
# ����������� ������������ ���������� �������������� ������� ���������� ������
salesMoscow [ ,colMonth+5 ]<- ceiling(salesMoscow [ ,59] /length(colMonth))
salesMoscow <- salesMoscow [ ,1:57]

# 
# 
# 
# 
# 
# 
# 
#



paramMoscow<- salesMoscow [ ,c (2, 1, 4, 5, 3)]

colnames(paramMoscow) <- c("item", "code", "safety stock", "order q-ty", "lead time")

salesMoscow<- salesMoscow [ , c(2,1, 6:57) ]
colnames(salesMoscow) [3:54]<- c("1":"52")


# ������� ������� � ���������� ������� �������
paramMoscow<-merge(currentFinishedStock, paramMoscow, by ="code", all.x= TRUE)
salesMoscow<-merge(currentFinishedStock, salesMoscow, by ="code", all.x= TRUE)
salesMoscow[is.na(salesMoscow)]<-0




#������� � ������� year transition








# �������� 4 ������� ��� �������� � RCPP
StockMatrixMSK<-as.matrix(weeklyStockMoscow [,3:54])
OrdersMatrixMSK<-matrix(nrow=nrow(currentFinishedStock), ncol=52)
OrdersMatrixMSK[is.na(OrdersMatrixMSK)]<-0
SalesMatrixMSK<-as.matrix(salesMoscow[ ,56:107])
ParametersMatrixMSK<-as.matrix(paramMoscow[ ,c(56:58)])

#������� ����������� �������� �������, ���������� ����� merge
colnames(SalesMatrixMSK) <-colnames(alltimeHeader)
colnames(StockMatrixMSK) <-c(1:52)
colnames(OrdersMatrixMSK) <-c(1:52)

StockMatrix2 <- StockMatrixMSK
SalesMatrix2 <- SalesMatrixMSK
OrdersMatrix2 <- OrdersMatrixMSK

for (i in 1:ncol(OrdersMatrixMSK) ) {  # 
  
  colnames (StockMatrix2) <- colnames(alltimeHeaderMix)
  StockMatrix2 [ ,match(i, colnames(StockMatrix2))] <- StockMatrixMSK[ , match(i, colnames(StockMatrixMSK))] # 
  
  
#   colnames (SalesMatrix2) <- colnames(alltimeHeaderMix)
#   SalesMatrix2 [ ,match(i, colnames(SalesMatrix2))] <- SalesMatrixMSK[ , match(i, colnames(SalesMatrixMSK))]
#   
  
  colnames (OrdersMatrix2) <- colnames(alltimeHeaderMix)
  OrdersMatrix2 [,match(i, colnames(OrdersMatrix2))] <- OrdersMatrixMSK[ , match(i, colnames(OrdersMatrixMSK))]
  
  
}

StockMatrixMSK <- StockMatrix2
SalesMatrixMSK <- SalesMatrix2
OrdersMatrixMSK <- OrdersMatrix2

# �� 2 ������, ��� ������� � ������� �������
time<- match(weekReport, colnames(alltimeHeader)) -2
#time<- time+1

#������ ������������ ��� ������
flowMSK<-round(stockTurnover(StockMatrixMSK,SalesMatrixMSK,OrdersMatrixMSK, ParametersMatrixMSK, time))



# ����������� �������� � ������ ��� ������ �������
NamesMatrix<-as.data.frame(matrix(nrow=nrow(flowMSK), ncol=2))
NamesMatrix<- weeklyStockMoscow[rep(row.names(weeklyStockMoscow), 3), 1:2]

statusMatrix<-as.data.frame(matrix(nrow=nrow(flowMSK), ncol=1))
statusMatrix[1:nrow(flowMSK)/3, ]<-"�������"
statusMatrix[nrow(flowMSK)/3+2:nrow(flowMSK)/3*2, ]<-"������"
statusMatrix[(nrow(flowMSK)/3*2+1):nrow(flowMSK), ]<- "� ����"



##
##������ ������� � ������� �� ������
##
reportMSK<-cbind(NamesMatrix,statusMatrix, flowMSK)


#��������� �������� ��������
names(reportMSK) [4:ncol(reportMSK)]<-names(alltimeHeaderMix)





# # �������� 4 ������� ��� �������� � RCPP
# 
# 
# ##
# ##������ ������� � ������� �� ������
# ##
# reportMSK<-cbind(NamesMatrix,statusMatrix, flowMSK)
write.csv(reportMSK [, c(1,2,3, which (colnames(reportMSK) %in% (weekReport) ) :ncol(reportMSK))], "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/reportMSK.csv")







# 
# 
# 
# ���������� ��������� ����� �� �������������� ��� �����-����������
# 
# 
# 



warehouseStockSPb<-match("�����-���������,����� �������", colnames(sat2))
stockComponentsSPb<-sat2[,c(4,6,warehouseStockSPb)] # ����� ������ 4,6, warehouseStockMoscow
names(stockComponentsSPb)<-c("code", "item")

stockComponentsSPb[is.na(stockComponentsSPb)]<-0




#����������� ����� ������
names(stockComponentsSPb)[3]<-weekReport


#������� �� �������� ������� � �������� 
stockComponentsSPb[ ,3]<- sat2[ ,warehouseStockSPb]-sat2[ ,warehouseStockSPb +1]


stockComponentsSPb[is.na(stockComponentsSPb)]<-0
#write.csv(stockComponentsMoscow, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/stockMoscow.�sv")



# ��������� �������� ������� ���������
finishedGoodsList<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/finishedGoodsList.xls")
colnames(finishedGoodsList)<- c ("code", "item")


#������� ��������� 52-��������� ������
currentFinishedStock<-as.data.frame(matrix(nrow=nrow(finishedGoodsList), ncol=54))
currentFinishedStock[,1:2]<-finishedGoodsList[,1:2]
names(currentFinishedStock)<-c("code", "item", c(1:52))

# �������� ������� ������� ��� ������ � ������� ������
weeklyStockSPb<-merge(currentFinishedStock, stockComponentsSPb, by ="code", all.x= TRUE)

weeklyStockSPb [ ,weekReport+2]<-weeklyStockSPb [,56]
weeklyStockSPb <- weeklyStockSPb [ , 1:54]
weeklyStockSPb[is.na(weeklyStockSPb)]<-0
colnames(weeklyStockSPb)[3:54] <- c (1:52)




#��������� ��������� �������������� (�����. �����, ������ ������ � ����), � ����� ���� ������
salesSPb<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/SPb sales and parameters.xls")








##
##
##
##
## �������� ����� ������ �����-���������� � ������� 1�, � �� �� �����


# �������� ����� ������ �� ������� �����

mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", month(Sys.Date()), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 
t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("code", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])

salesSPb<- merge(salesSPb, t, by ="code", all.x= TRUE)
salesSPb[is.na(salesSPb)]<-0

#� ��������� ������ �������� �������
colnames(salesSPb) [c(6:57)] <-colnames(alltimeHeader)

#������ �������, ����������� � �������� ������
colMonth<- which (alltimeHeader[1,] %in% month(Sys.Date()) )
# ����������� ������������ ���������� �������������� ������� �������� ������
salesSPb [ ,colMonth+5 ]<- ceiling(salesSPb [ ,59] /length(colMonth))
salesSPb <- salesSPb [ ,1:57]




# �������� ����� ������ �� ��������� �����

mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", 
           ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 
t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("code", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])

salesSPb<- merge(salesSPb, t, by ="code", all.x= TRUE)
salesSPb[is.na(salesSPb)]<-0


#������ �������, ����������� � C��������� ������
colMonth<- which (alltimeHeader[1,] %in% ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12)  )
# ����������� ������������ ���������� �������������� ������� ���������� ������
salesSPb [ ,colMonth+5 ]<- ceiling(salesSPb [ ,59] /length(colMonth))
salesSPb <- salesSPb [ ,1:57]






# �������� ����� ������ ����� 2 ������

mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", 
           ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 
t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("code", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])

salesSPb<- merge(salesSPb, t, by ="code", all.x= TRUE)
salesSPb[is.na(salesSPb)]<-0


#������ �������, ����������� � C��������� ������
colMonth<- which (alltimeHeader[1,] %in% ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12)  )
# ����������� ������������ ���������� �������������� ������� ���������� ������
salesSPb [ ,colMonth+5 ]<- ceiling(salesSPb [ ,59] /length(colMonth))
salesSPb <- salesSPb [ ,1:57]
# 
# 
# 
# 
# 
# 
# 
#

paramSPb<- salesSPb [ ,c (2,1, 4, 5, 3)]

colnames(paramSPb) <- c("item", "code", "safety stock", "order q-ty", "lead time")

salesSPb<- salesSPb [ , c(2,1, 6:57) ]
colnames(salesSPb) [3:54]<- c("1":"52")



# ������� ������� � ���������� ������� �������
paramSPb<-merge(currentFinishedStock, paramSPb, by ="code", all.x= TRUE)
salesSPb<-merge(currentFinishedStock, salesSPb, by ="code", all.x= TRUE)
salesSPb[is.na(salesSPb)]<-0


# 

# 
# 
# ##
# ##������ ������� � ������� �� �����-����������
# ##
# reportSPb<-cbind(NamesMatrix,statusMatrix, flowSPb)



# �������� 4 ������� ��� �������� � RCPP
StockMatrixSPb<-as.matrix(weeklyStockSPb [,3:54])
OrdersMatrixSPb<-matrix(nrow=nrow(currentFinishedStock), ncol=52)
OrdersMatrixSPb[is.na(OrdersMatrixSPb)]<-0
SalesMatrixSPb<-as.matrix(salesSPb[ ,56:107])
ParametersMatrixSPb<-as.matrix(paramSPb[ ,c(56:58)])

#������� ����������� �������� �������, ���������� ����� merge
colnames(SalesMatrixSPb) <-colnames(alltimeHeader)
colnames(StockMatrixSPb) <-c(1:52)
colnames(OrdersMatrixSPb) <-c(1:52)

StockMatrix2 <- StockMatrixSPb
SalesMatrix2 <- SalesMatrixSPb
OrdersMatrix2 <- OrdersMatrixSPb

for (i in 1:ncol(OrdersMatrixSPb) ) {
  
  colnames (StockMatrix2) <- colnames(alltimeHeaderMix)
  StockMatrix2 [ ,match(i, colnames(StockMatrix2))] <- StockMatrixSPb[ , match(i, colnames(StockMatrixSPb))]
  
  
#   colnames (SalesMatrix2) <- colnames(alltimeHeaderMix)
#   SalesMatrix2 [ ,match(i, colnames(SalesMatrix2))] <- SalesMatrixSPb[ , match(i, colnames(SalesMatrixSPb))]
#   
  
  colnames (OrdersMatrix2) <- colnames(alltimeHeaderMix)
  OrdersMatrix2 [,match(i, colnames(OrdersMatrix2))] <- OrdersMatrixSPb[ , match(i, colnames(OrdersMatrixSPb))]
  
  
}

StockMatrixSPb <- StockMatrix2
SalesMatrixSPb <- SalesMatrix2
OrdersMatrixSPb <- OrdersMatrix2

# �� 2 ������, ��� ������� � ������� �������
time<-match(weekReport, colnames(alltimeHeader)) -2


#������ ������������ ��� ������
flowSPb<-round(stockTurnover(StockMatrixSPb,SalesMatrixSPb,OrdersMatrixSPb, ParametersMatrixSPb, time))



# ����������� �������� � ������ ��� ������ �������
NamesMatrix<-as.data.frame(matrix(nrow=nrow(flowSPb), ncol=2))
NamesMatrix<- weeklyStockSPb[rep(row.names(weeklyStockSPb), 3), 1:2]

statusMatrix<-as.data.frame(matrix(nrow=nrow(flowSPb), ncol=1))
statusMatrix[1:nrow(flowSPb)/3, ]<-"�������"
statusMatrix[nrow(flowSPb)/3+2:nrow(flowSPb)/3*2, ]<-"������"
statusMatrix[(nrow(flowSPb)/3*2+1):nrow(flowSPb), ]<- "� ����"



##
##������ ������� � ������� �� ���
##
reportSPb<-cbind(NamesMatrix,statusMatrix, flowSPb)


#��������� �������� ��������
names(reportSPb) [4:ncol(reportSPb)]<-names(alltimeHeaderMix)
write.csv(reportSPb [, c(1,2,3, which (colnames(reportSPb) %in% (weekReport) ) :ncol(reportSPb))], "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/reportSPb.csv")




# 
# 
# 
# ���������� ��������� ����� �� �������������� ��� ������������
# 
# 
# 

warehouseStockNSK<-match("�����������, ����� �������", colnames(sat2))
stockComponentsNSK<-sat2[,c(4,6,warehouseStockNSK)]
names(stockComponentsNSK)<-c("code", "item")

stockComponentsNSK[is.na(stockComponentsNSK)]<-0

warehouseStockNSKBranch<-match("����� ����������  ��� ���", colnames(sat2))
warehouseStockNSKTransit<-match("���������� �����", colnames(sat2))

#����������� ����� ������
names(stockComponentsNSK)[3]<-weekReport


#������� �� �������� ������� � �������� 
stockComponentsNSK[ ,3]<- sat2[ ,warehouseStockNSK ]-sat2[ ,warehouseStockNSK +1]+sat2[ ,warehouseStockNSKBranch] - sat2[ ,warehouseStockNSKBranch+1] + sat2[ ,warehouseStockNSKTransit] - sat2[ ,warehouseStockNSKTransit +1]



stockComponentsNSK[is.na(stockComponentsNSK)]<-0
#write.csv(stockComponentsMoscow, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/stockMoscow.�sv")



# ��������� �������� ������� ���������
finishedGoodsList<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/finishedGoodsList.xls")
colnames(finishedGoodsList)<- c ("code", "item")


#������� ��������� 52-��������� ������
currentFinishedStock<-as.data.frame(matrix(nrow=nrow(finishedGoodsList), ncol=54))
currentFinishedStock[,1:2]<-finishedGoodsList[,1:2]
names(currentFinishedStock)<-c("code", "item", c(1:52))

# �������� ������� ������� ��� ������ � ������� ������
weeklyStockNSK<-merge(currentFinishedStock, stockComponentsNSK, by ="code", all.x= TRUE)

weeklyStockNSK [ ,weekReport+2]<-weeklyStockNSK [,56]
weeklyStockNSK <- weeklyStockNSK [ , 1:54]
weeklyStockNSK[is.na(weeklyStockNSK)]<-0
colnames(weeklyStockNSK)[3:54] <- c (1:52)




#��������� ��������� �������������� (�����. �����, ������ ������ � ����), � ����� ���� ������
salesNSK<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/NSK sales and parameters.xls")


##
##
##
##
## �������� ����� ������ ������������ � ������� 1�, � �� �� �����


# �������� ����� ������ �� ������� �����

mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", month(Sys.Date()), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 

t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("code", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])

salesNSK<- merge(salesNSK, t, by ="code", all.x= TRUE)
salesNSK[is.na(salesNSK)]<-0

#� ��������� ������ �������� �������
colnames(salesNSK) [c(6:57)] <-colnames(alltimeHeader)

#������ �������, ����������� � �������� ������
colMonth<- which (alltimeHeader[1,] %in% month(Sys.Date()) )
# ����������� ������������ ���������� �������������� ������� �������� ������
salesNSK [ ,colMonth+5 ]<- ceiling(salesNSK [ ,59] /length(colMonth))  #60
salesNSK <- salesNSK [ ,1:57]




# �������� ����� ������ �� ��������� �����

mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", 
           ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 
t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("code", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])

salesNSK<- merge(salesNSK, t, by ="code", all.x= TRUE)
salesNSK[is.na(salesNSK)]<-0


#������ �������, ����������� � C��������� ������
colMonth<-which (alltimeHeader[1,] %in% ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12)  )
# ����������� ������������ ���������� �������������� ������� ���������� ������
salesNSK [ ,colMonth+5 ]<- ceiling(salesNSK [ ,59] /length(colMonth))
salesNSK <- salesNSK [ ,1:57]






# �������� ����� ������ ����� 2 ������

mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", 
           ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 
t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("code", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])

salesNSK<- merge(salesNSK, t, by ="code", all.x= TRUE)
salesNSK[is.na(salesNSK)]<-0


#������ �������, ����������� � C��������� ������
colMonth<- which (alltimeHeader[1,] %in% ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12)  )
# ����������� ������������ ���������� �������������� ������� ���������� ������
salesNSK [ ,colMonth+5 ]<- ceiling(salesNSK [ ,59] /length(colMonth))
salesNSK <- salesNSK [ ,1:57]
# 
# 
# 
# 
# 
# 
# 
#

paramNSK<- salesNSK [ ,c (2,1, 4, 5, 3)]

colnames(paramNSK) <- c("item", "code", "safety stock", "order q-ty", "lead time")

salesNSK<- salesNSK [ , c(2,1, 6:57) ]
#colnames(salesNSK) [3:54]<- c("1":"52")



# ������� ������� � ���������� ������� �������
paramNSK<-merge(currentFinishedStock, paramNSK, by ="code", all.x= TRUE)
salesNSK<-merge(currentFinishedStock, salesNSK, by ="code", all.x= TRUE)
salesNSK[is.na(salesNSK)]<-0




# 
# 
# ##
# ##������ ������� � ������� �� ������������
# ##
# reportNSK<-cbind(NamesMatrix,statusMatrix, flowNSK)


# �������� 4 ������� ��� �������� � RCPP
StockMatrixNSK<-as.matrix(weeklyStockNSK [,3:54])
OrdersMatrixNSK<-matrix(nrow=nrow(currentFinishedStock), ncol=52)
OrdersMatrixNSK[is.na(OrdersMatrixNSK)]<-0
SalesMatrixNSK<-as.matrix(salesNSK[ ,56:107])
ParametersMatrixNSK<-as.matrix(paramNSK[ ,c(56:58)])

#������� ����������� �������� �������, ���������� ����� merge
colnames(SalesMatrixNSK) <-colnames(alltimeHeader)
colnames(StockMatrixNSK) <-c(1:52)
colnames(OrdersMatrixNSK) <-c(1:52)

StockMatrix2 <- StockMatrixNSK
SalesMatrix2 <- SalesMatrixNSK
OrdersMatrix2 <- OrdersMatrixNSK

for (i in 1:ncol(OrdersMatrixNSK) ) {
  
  colnames (StockMatrix2) <- colnames(alltimeHeaderMix)
  StockMatrix2 [ ,match(i, colnames(StockMatrix2))] <- StockMatrixNSK[ , match(i, colnames(StockMatrixNSK))]
  
#   
#   colnames (SalesMatrix2) <- colnames(alltimeHeaderMix)
#   SalesMatrix2 [ ,match(i, colnames(SalesMatrix2))] <- SalesMatrixNSK[ , match(i, colnames(SalesMatrixNSK))]
#   
  
  colnames (OrdersMatrix2) <- colnames(alltimeHeaderMix)
  OrdersMatrix2 [,match(i, colnames(OrdersMatrix2))] <- OrdersMatrixNSK[ , match(i, colnames(OrdersMatrixNSK))]
  
  
}

StockMatrixNSK <- StockMatrix2
SalesMatrixNSK <- SalesMatrix2
OrdersMatrixNSK <- OrdersMatrix2

# �� 2 ������, ��� ������� � ������� �������
time<-match(weekReport, colnames(alltimeHeader)) -2


#������ ������������ ��� ������
flowNSK<-round(stockTurnover(StockMatrixNSK,SalesMatrixNSK,OrdersMatrixNSK, ParametersMatrixNSK, time))



# ����������� �������� � ������ ��� ������ �������
NamesMatrix<-as.data.frame(matrix(nrow=nrow(flowNSK), ncol=2))
NamesMatrix<- weeklyStockNSK[rep(row.names(weeklyStockNSK), 3), 1:2]

statusMatrix<-as.data.frame(matrix(nrow=nrow(flowNSK), ncol=1))
statusMatrix[1:nrow(flowNSK)/3, ]<-"�������"
statusMatrix[nrow(flowNSK)/3+2:nrow(flowNSK)/3*2, ]<-"������"
statusMatrix[(nrow(flowNSK)/3*2+1):nrow(flowNSK), ]<- "� ����"



##
##������ ������� � ������� �� ���
##
reportNSK<-cbind(NamesMatrix,statusMatrix, flowNSK)


#��������� �������� ��������
names(reportNSK) [4:ncol(reportNSK)]<-names(alltimeHeaderMix)
write.csv(reportNSK [, c(1,2,3, which (colnames(reportNSK) %in% (weekReport) ) :ncol(reportNSK))], "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/reportNSK.csv")

##
##������� � ���������������� ������� �� ����������� � ������ �� ���� 3-� ��������
##
totalProduction <- reportMSK[(nrow(flowMSK)/3+1):(nrow(flowMSK)/3*2), c(4:55)] + reportSPb[(nrow(flowSPb)/3+1):(nrow(flowSPb)/3*2), c(4:55)]+reportNSK[(nrow(flowNSK)/3+1):(nrow(flowNSK)/3*2), c(4:55)]
totalProduction<- cbind(reportMSK[c(1: (nrow(flowMSK)/3)), c(1,2) ], totalProduction)
write.csv(totalProduction, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/totalProduction.csv")


#��������� ����� ������ ��� ���������� ����������
salesMoscow<- cbind (salesMoscow[ ,1:2], SalesMatrixMSK)
salesMoscow [ ,3:ncol(salesMoscow)] <- round (salesMoscow [ ,3:ncol(salesMoscow)], digits=0)
salesMoscow <- salesMoscow [ , c(1, 2, which (colnames(salesMoscow) %in% (weekReport) ):ncol(salesMoscow))]

salesSPb<- cbind (salesSPb[ ,1:2], SalesMatrixSPb)
salesSPb [ ,3:ncol(salesSPb)] <- round (salesSPb [ ,3:ncol(salesSPb)], digits=0)
salesSPb <- salesSPb [ , c(1, 2, which (colnames(salesSPb) %in% (weekReport) ):ncol(salesSPb))]

salesNSK<- cbind (salesNSK[ ,1:2], SalesMatrixNSK)
salesNSK [ ,3:ncol(salesNSK)] <- round (salesNSK [ ,3:ncol(salesNSK)], digits=0)
salesNSK <- salesNSK [ , c(1, 2, which (colnames(salesNSK) %in% (weekReport) ):ncol(salesNSK))]


write.csv(salesMoscow, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/salesMoscow.csv")
write.csv(salesSPb, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/salesSPb.csv")
write.csv(salesNSK, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/salesNSK.csv")


