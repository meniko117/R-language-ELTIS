# ���������� ����� ������ ��� ������
# �� ������� �����
soldMSK<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������� ���.xls", skip =2)
colnames(soldMSK) <- soldMSK [1, ]
salesPlanMSK<-read_excel(paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", month(Sys.Date()), ".xls", sep=""))
colnames(salesPlanMSK) <- salesPlanMSK [2, ]

soldplanMSK<-merge(salesPlanMSK, soldMSK, by ="���", all=TRUE)

for (i in 3:nrow(soldplanMSK) ) {
ifelse(is.na(soldplanMSK[i,4]), soldplanMSK[i,4]<-soldplanMSK[i,11], soldplanMSK[i,4]<-soldplanMSK[i,4])
}

soldplanMSK<-soldplanMSK[complete.cases(soldplanMSK[,4]),]

soldplanMSK<- soldplanMSK [ ,c(1,4,6, 12, 13, 14)]
colnames(finishedGoodsList)<- c("���", "������������", "�������������")

colnames(soldplanMSK)<- c("���", "������������", "���� ������", "���� ������", "�������", "�����")
soldplanMSK [ ,1]<- as.numeric(soldplanMSK[ , 1])


soldplanMSK<-merge(soldplanMSK, finishedGoodsList, by ="���", all.x= TRUE)
soldplanMSK<- soldplanMSK [ , c(1:6,8)]

soldplanMSK[is.na(soldplanMSK)]<-0
soldplanMSK <-soldplanMSK [ c(1:(nrow(soldplanMSK)-3)), ]


soldplanMSK[soldplanMSK==" "]<- "0"


#��������������� � �������� ������
for (i in 3:7 ) {
  soldplanMSK[ ,i] <- as.numeric(soldplanMSK[ ,i])
}

#���� ������ �� ������������� / ���� ������ �� �������������
MSKsalesprogress<-round(sum(as.numeric(soldplanMSK[ , 4]) *  soldplanMSK [,7])  /  sum(as.numeric(soldplanMSK[ , 3]) *  soldplanMSK [,7]), digits=2)

MSKrealSales<- sum(as.numeric(soldplanMSK[ , 4]) *  soldplanMSK [,7])
MSKplanSales<- sum(as.numeric(soldplanMSK[ , 3]) *  soldplanMSK [,7])

soldplanMSK$���������� <-  round(soldplanMSK [ ,4]/soldplanMSK [ ,3], digits=2)*100
soldplanMSK<- soldplanMSK [order(-soldplanMSK$�������),] 
row.names(soldplanMSK) <- NULL

colnames (soldplanMSK) [8] <- "%, ����������"

write.csv(soldplanMSK, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/soldplanMSK.csv")








# ���������� ����� ������ ��� �����-����������

soldSPb<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������� ���.xls", skip =2)
colnames(soldSPb) <- soldSPb [1, ]
salesPlanSPb<-read_excel(paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", month(Sys.Date()), ".xls", sep=""))
colnames(salesPlanSPb) <- salesPlanSPb [2, ]


soldplanSPb<-merge(salesPlanSPb, soldSPb, by ="���", all= TRUE)

for (i in 3:nrow(soldplanSPb) ) {
  ifelse(is.na(soldplanSPb[i,4]), soldplanSPb[i,4]<-soldplanSPb[i,11], soldplanSPb[i,4]<-soldplanSPb[i,4])
}

soldplanSPb<-soldplanSPb[complete.cases(soldplanSPb[,4]),]
soldplanSPb<- soldplanSPb [ ,c(1,4,6, 12, 13, 14)]
colnames(finishedGoodsList)<- c("���", "������������", "�������������")

colnames(soldplanSPb)<- c("���", "������������", "���� ������", "���� ������", "�������", "�����")
soldplanSPb [ ,1]<- as.numeric(soldplanSPb[ , 1])


soldplanSPb<-merge(soldplanSPb, finishedGoodsList, by ="���", all.x= TRUE)
soldplanSPb<- soldplanSPb [ , c(1:6,8)]

soldplanSPb[is.na(soldplanSPb)]<-0
soldplanSPb <-soldplanSPb [ c(1:(nrow(soldplanSPb)-3)), ]


soldplanSPb[soldplanSPb==" "]<- "0"

#��������������� � �������� ������
for (i in 3:7 ) {
  soldplanSPb[ ,i] <- as.numeric(soldplanSPb[ ,i])
}

#���� ������ �� ������������� / ���� ������ �� �������������
SPbsalesprogress<- round(sum(as.numeric(soldplanSPb[ , 4]) *  soldplanSPb [,7])  /  sum(as.numeric(soldplanSPb[ , 3]) *  soldplanSPb [,7]), digits=2)

SPbrealSales<- sum(as.numeric(soldplanSPb[ , 4]) *  soldplanSPb [,7])
SPbplanSales<- sum(as.numeric(soldplanSPb[ , 3]) *  soldplanSPb [,7])

soldplanSPb$���������� <-  round(soldplanSPb [ ,4]/soldplanSPb [ ,3], digits=2)*100
soldplanSPb<- soldplanSPb [order(-soldplanSPb$�������),] 
row.names(soldplanSPb) <- NULL

colnames (soldplanSPb) [8] <- "%, ����������"

write.csv(soldplanSPb, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/soldplanSPb.csv")










# ���������� ����� ������ �������������

soldNSK<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������� ���.xls", skip =2)
colnames(soldNSK) <- soldNSK [1, ]
salesPlanNSK<-read_excel(paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", month(Sys.Date()), ".xls", sep=""))
colnames(salesPlanNSK) <- salesPlanNSK [2, ]

soldplanNSK<-merge(salesPlanNSK, soldNSK, by ="���", all= TRUE)


for (i in 3:nrow(soldplanNSK) ) {
  ifelse(is.na(soldplanNSK[i,4]), soldplanNSK[i,4]<-soldplanNSK[i,11], soldplanNSK[i,4]<-soldplanNSK[i,4])
}

soldplanNSK<-soldplanNSK[complete.cases(soldplanNSK[,4]),]
soldplanNSK<- soldplanNSK [ ,c(1,4,6, 12, 13, 14)]
colnames(finishedGoodsList)<- c("���", "������������", "�������������")

colnames(soldplanNSK)<- c("���", "������������", "���� ������", "���� ������", "�������", "�����")
soldplanNSK [ ,1]<- as.numeric(soldplanNSK[ , 1])


soldplanNSK<-merge(soldplanNSK, finishedGoodsList, by ="���", all.x= TRUE)
soldplanNSK<- soldplanNSK [ , c(1:6,8)]

soldplanNSK[is.na(soldplanNSK)]<-0
soldplanNSK <-soldplanNSK [ c(1:(nrow(soldplanNSK)-3)), ]


soldplanNSK[soldplanNSK==" "]<- "0"

#��������������� � �������� ������
for (i in 3:7 ) {
  soldplanNSK[ ,i] <- as.numeric(soldplanNSK[ ,i])
}

#���� ������ �� ������������� / ���� ������ �� �������������
NSKsalesprogress<- round(sum(as.numeric(soldplanNSK[ , 4]) *  soldplanNSK [,7])  /  sum(as.numeric(soldplanNSK[ , 3]) *  soldplanNSK [,7]), digits=2)

NSKrealSales<- sum(as.numeric(soldplanNSK[ , 4]) *  soldplanNSK [,7])
NSKplanSales<- sum(as.numeric(soldplanNSK[ , 3]) *  soldplanNSK [,7])

soldplanNSK$���������� <-  round(soldplanNSK [ ,4]/soldplanNSK [ ,3], digits=2)*100
soldplanNSK<- soldplanNSK [order(-soldplanNSK$�������),] 

row.names(soldplanNSK) <- NULL

colnames (soldplanNSK) [8] <- "%, ����������"

write.csv(soldplanNSK, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/soldplanNSK.csv")


# ��������� ������� � ������� ����/���� � ����� % ���������� ��� ������� ������� ��� �������� �� ������
salesAnalysis <- data.frame(
                            c(sum((as.numeric(soldplanMSK[ , 4]) *  soldplanMSK [,7])), 
                              sum(as.numeric(soldplanSPb[ , 4]) *  soldplanSPb [,7]),
                              sum(as.numeric(soldplanNSK[ , 4]) *  soldplanNSK [,7])),
                            
                            c(sum(as.numeric(soldplanMSK[ , 3]) *  soldplanMSK [,7]),
                              sum(as.numeric(soldplanSPb[ , 3]) *  soldplanSPb [,7]),
                              sum(as.numeric(soldplanNSK[ , 3]) *  soldplanNSK [,7])))

salesAnalysis <- as.data.frame(t(salesAnalysis))
colnames(salesAnalysis) <- c("������", "�����-���������", "�����������")
row.names(salesAnalysis) <- c("����", "����")
salesAnalysis <- rbind(salesAnalysis, round(salesAnalysis[1,]/salesAnalysis[2,]*100, digits=1) )
row.names(salesAnalysis) <- c("���� �������", "���� ������", "% ����������")
write.csv(salesAnalysis, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/salesAnalysis.csv")








###

#������ �����/����� ������ 
# ����� ����/���� ������ ��� �����, ���
# ����/���� ������ �� ��������� �����, ��

#���� �� ����� ���, ��� 2,1 ��� ��� ��� 1,8 ��� ���

###
###
# ������������ ������� ������� �� �������� �������� � ���������� �� ������ ����
# ������ ���������� ����, � ������� �������� ����� �� �����������

#�������� ���� � ��������� 
flist <- list.files("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/�������", full.names = TRUE)
file.remove(flist)

#�������� �� ������ ������ �������� ����� ��, ��� ��������� � �������� ������
flist <- list.files("C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/�����/", full.names = TRUE)
flist<-as.data.frame(flist)

flist$month <- as.numeric(str_sub(flist[,1], start= -5, end=-4))
colnames(flist) <- c("path", "month")
flistCurrentMonth<- subset(flist, flist[ ,2]== month(Sys.Date()))


# ��������� ����� � ��������� ��� ��� ���, ����� ��� ���� ������������, � ��������� ����� ��� ����������� ���������
for (i in 1:nrow(flistCurrentMonth)) {
  #ERROR HANDLING
  possibleError <- tryCatch(
    
    u<-read_excel(paste(flistCurrentMonth[i,1], "/", "��� �������", ".xls",  sep="")),
    error=function(e) e
    
    
  )
  
  if(inherits(possibleError, "error")) next
  #print(i) 
  #print(str_sub(flist [i,1], start= -8))
  
  
  #REAL WORK
  #surplusHistory <- read.csv(paste(flist[i,1], "/", "surplusBranch",  ".csv",  sep=""))
  stockDate<- as.numeric(str_sub(paste(flistCurrentMonth[i,1], "/", "��� �������", ".xls",  sep=""), start= -18, end=-17))
  
  write.csv(u, paste("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/�������/", "��� �������", " ", 
                     stockDate, ".csv", sep="" ))
  
  ;
  
}  #end for













#�������� ���������� ������ �������������� ������� (% ���������� ���� � ������, ����� "����������" ������� ������ �� ������ � �� 
# �������� ��������� �����)

#������������� ������ ��� ������

# ���� �� 1 �� 22 (���������� ������� ����) �� ��������  � ���������� �������� � �������

##
##��������� ������ �� �������� �� ���� �������
##
##
##��������� ������ �� �������� �� ���� �������
##
#allStock<-read.csv(paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/��� ������� ", Sys.Date(), ".csv", sep=""), sep= ";", skip = "1")



# �������� ���������� ����, � ������� ���������������� ������� �� �������
flist <- list.files("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/�������", full.names = TRUE)
#
flist<-as.data.frame(flist)

#���� �������
flist$date <- as.numeric(str_sub(flist[,1], start= -6, end=-4))
datesNumber<- flist$date 
datesNumber <- datesNumber[!is.na(datesNumber)] #������� NA
datesNumber<-sort(datesNumber)

for (i in datesNumber ) {
  
  
  # i<-5
  allStock<-read.csv(paste("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/�������/��� ������� ", i, ".csv", sep=""), dec = ".", stringsAsFactors=FALSE)
  
  #�������� NA �� ����
  allStock[is.na(allStock)]<-0
  
  header<-allStock[1,]

names(allStock)<-header
  
  #���������� ���������� ��� ��������� ����
#   write.csv(allStock, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/all stock.�sv")
#   
#   sat<-read.csv("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/all stock.�sv", 
#                 , na.strings=c(" ", NA), stringsAsFactors=FALSE) 
#   
#   write.csv(sat, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/all stock.�sv" )
#   
#   sat2<-read.csv("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/all stock.�sv") 
  
  
  #�������� ��������� �������
#   names(sat2)<-c(0,0,header)
  
  
  warehouseStockMoscow<-match("������ ������� (�������)", colnames(allStock))
  warehouseStockMoscowBranch<-match("������, ����� �������", colnames(allStock))
  warehouseStockMoscowTransit<-match("������ � ���� (������)", colnames(allStock))
  
  stockComponentsMoscow<-allStock[,c(3,5,warehouseStockMoscow)]
  #names(stockComponentsMoscow)<-c("code", "item")
  
allStock[is.na(allStock)]<-0
allStock[allStock==" "]<- 0

  
  
  
  
  
  #������� �� �������� ������� � �������� 
  stockComponentsMoscow[ ,3]<- as.numeric(allStock[  ,warehouseStockMoscow])+as.numeric(allStock[ ,warehouseStockMoscowBranch]) + as.numeric(allStock[ ,warehouseStockMoscowTransit]) 
  
  #��������� ����� ������ ������� ��������
  names(stockComponentsMoscow)[3]<-as.character(i)
  
  stockComponentsMoscow [,1]<- as.numeric(stockComponentsMoscow [,1])

  soldplanMSK<-merge(soldplanMSK, stockComponentsMoscow, by ="���", all.x= TRUE)
  soldplanMSK <- soldplanMSK [ , c(1:(ncol(soldplanMSK)-2),ncol(soldplanMSK))]
}


#���������� �� �������� �������
soldplanMSK<- soldplanMSK [order(-soldplanMSK$�������),] 


for (i in 1:nrow(soldplanMSK) ) {
  # ���������� ����, ����� ������� ��������� ��������� �����, �.�. 25% �� ����� ������
  vec <- soldplanMSK [i, 9:(ncol(soldplanMSK)-1)] > soldplanMSK [i,3]*0.25
  #������ �������� ����, ����� ������� �������� ��������� ����� �� ������ ���������� ����
  soldplanMSK$������������� [i] <- round(length(vec[vec==TRUE]) /length(9:(ncol(soldplanMSK)-1))*100)
  
}


row.names(soldplanMSK) <- NULL

MSKstockArch <- soldplanMSK [ , c(1,2,3, 10:ncol(soldplanMSK)-1)]
write.csv(MSKstockArch, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/MSKstockArch.csv")


soldplanMSK <- soldplanMSK [ , c(1:8, ncol(soldplanMSK))] 

write.csv(soldplanMSK, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/soldplanMSK.csv")
















#�����-���������

# �������� ���������� ����, � ������� ���������������� ������� �� �������
flist <- list.files("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/�������", full.names = TRUE)
#
flist<-as.data.frame(flist)

#���� �������
flist$date <- as.numeric(str_sub(flist[,1], start= -6, end=-4))
datesNumber<- flist$date 
datesNumber <- datesNumber[!is.na(datesNumber)] #������� NA
datesNumber<-sort(datesNumber)

##
##��������� ������ �� �������� �� ���� �������
##


for (i in datesNumber ) {
  

  allStock<-read.csv(paste("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/�������/��� ������� ", i, ".csv", sep=""), dec = ".", stringsAsFactors=FALSE)
  
  #�������� NA �� ����
  allStock[is.na(allStock)]<-0
  
  header<-allStock[1,]
  
  names(allStock)<-header
  
  
  warehouseStockSPb<-match("�����-���������,����� �������", colnames(allStock))
  
  stockComponentsSPb<-allStock[,c(3,5,warehouseStockSPb)]
  #names(stockComponentsMoscow)<-c("code", "item")
  
  allStock[is.na(allStock)]<-0
  allStock[allStock==" "]<- 0
  
  
  
  
  
  #������� �� �������� ������� � �������� 
  stockComponentsSPb[ ,3]<- as.numeric(allStock[ ,warehouseStockSPb])
  
   
  #��������� ����� ������ ������� ��������
  names(stockComponentsSPb)[3]<-as.character(i)
  
  stockComponentsSPb [,1]<- as.numeric(stockComponentsSPb [,1])
  soldplanSPb<-merge(soldplanSPb, stockComponentsSPb, by ="���", all.x= TRUE)
  soldplanSPb <- soldplanSPb [ , c(1:(ncol(soldplanSPb)-2),ncol(soldplanSPb))]
}


#���������� �� �������� �������
soldplanSPb<- soldplanSPb [order(-soldplanSPb$�������),] 


for (i in 1:nrow(soldplanSPb) ) {
  # ���������� ����, ����� ������� ��������� ��������� �����, �.�. 25% �� ����� ������
  vec <- soldplanSPb [i, 9:(ncol(soldplanSPb)-1)] > soldplanSPb [i,3]*0.25
  #������ �������� ����, ����� ������� �������� ��������� ����� �� ������ ���������� ����
  soldplanSPb$������������� [i] <- round(length(vec[vec==TRUE]) /length(9:(ncol(soldplanSPb)-1))*100)
  
}

row.names(soldplanSPb) <- NULL

SPbstockArch <- soldplanSPb [ , c(1,2,3, 10:ncol(soldplanSPb)-1)]
write.csv(SPbstockArch, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/SPbstockArch.csv")


soldplanSPb <- soldplanSPb [ , c(1:8, ncol(soldplanSPb))] 

write.csv(soldplanSPb, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/soldplanSPb.csv")















#�����������

# �������� ���������� ����, � ������� ���������������� ������� �� �������


##
##��������� ������ �� �������� �� ���� �������
##


for (i in datesNumber ) {
  
  
  
  
  allStock<-read.csv(paste("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/�������/��� ������� ", i, ".csv", sep=""), dec = ".", stringsAsFactors=FALSE)
  
  #�������� NA �� ����
  allStock[is.na(allStock)]<-0
  
  header<-allStock[1,]
  
  names(allStock)<-header
  
  
  
  warehouseStockNSK<- match("�����������, ����� �������", colnames(allStock))
  warehouseStockNSKBranch<- match("����� ����������  ��� ���", colnames(allStock))
  warehouseStockNSKTransit<- match("���������� �����", colnames(allStock))
  
  stockComponentsNSK<-allStock[,c(3,5,warehouseStockNSK)]
  #names(stockComponentsNSK)<-c("code", "item")
  
  allStock[is.na(allStock)]<-0
  allStock[allStock==" "]<- 0
  
  
  
  
  
  
  #������� �� �������� ������� � �������� 
  stockComponentsNSK [ ,3]<- as.numeric(allStock[ ,warehouseStockNSK])+as.numeric(allStock[ ,warehouseStockNSKBranch]) + as.numeric(allStock[ ,warehouseStockNSKTransit]) 
  
  #��������� ����� ������ ������� ��������
  names(stockComponentsNSK)[3]<-as.character(i)
  stockComponentsNSK [,1]<- as.numeric(stockComponentsNSK [,1])
  
  soldplanNSK<-merge(soldplanNSK, stockComponentsNSK, by ="���", all.x= TRUE)
  soldplanNSK <- soldplanNSK [ , c(1:(ncol(soldplanNSK)-2),ncol(soldplanNSK))]
}


#���������� �� �������� �������
soldplanNSK<- soldplanNSK [order(-soldplanNSK$�������),] 


for (i in 1:nrow(soldplanNSK) ) {
  # ���������� ����, ����� ������� ��������� ��������� �����, �.�. 25% �� ����� ������
  vec <- soldplanNSK [i, 9:(ncol(soldplanNSK)-1)] > soldplanNSK [i,3]*0.25
  #������ �������� ����, ����� ������� �������� ��������� ����� �� ������ ���������� ����
  soldplanNSK$������������� [i] <- round(length(vec[vec==TRUE]) /length(9:(ncol(soldplanNSK)-1))*100)
  
}

row.names(soldplanNSK) <- NULL

NSKstockArch <- soldplanNSK [ , c(1,2,3, 10:ncol(soldplanNSK)-1)]
vecname <- rep("�", each=ncol(NSKstockArch))
colnames(NSKstockArch) <- vecname
names<- c("���", "������������", "���� ������", c (1:(ncol(NSKstockArch)-3)) )
NSKstockArch <- rbind(names, NSKstockArch)
write.csv(NSKstockArch, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/NSKstockArch.csv")
#write.table( NSKstockArch, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/NSKstockArch.csv", sep=",", col.names=FALSE)

soldplanNSK <- soldplanNSK [ , c(1:8, ncol(soldplanNSK))] 

write.csv(soldplanNSK, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/soldplanNSK.csv")



# # ����� ������� �������������� ������� ��� ������� ������� �� ������



source('~/������/R ����/incomingGoods.R')



# # ����������� ��� ���������� ����� �������� �����
flist <- list.files("C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������", full.names = TRUE)
file.copy(flist, paste("C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/�����/", Sys.Date(), sep = ""), overwrite = TRUE)
file.copy(flist, "Z:/�������/������ ��� R", overwrite = TRUE)

