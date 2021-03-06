#
#��������� ���������� "alltimeHeader" � "mergw tables"
#

# library (lubridate)
# library (stringr)
# 
# weekBegin<- data.frame(matrix(nrow=1, ncol=52))
# 
# 
# #����� ��� ������ (������� �� lubridate) ��� �������� ����
# # � ������ ���� ��� ����� ���� �������� �� ������ ������ ������������ ���� (� 2-� ������)
# 
# firstMonday<- as.Date(strptime("01.01.2015", format = "%d.%m.%Y"))-(wday(as.Date(strptime("01.01.2015", format = "%d.%m.%Y")))-2)
# 
# weekBegin[1]<-firstMonday
# 
# 
# for (i in 1:51 ) {
#   weekBegin[i+1] = weekBegin[i] +days(7)
# }
# 
# 
# #номер месяца для каждой недели
# monthBegin<- data.frame(matrix(nrow=1, ncol=52))
# for (i in 1:52 ) {
#  
#   monthBegin[1,i] <- month(as.POSIXlt(weekBegin[1,i], format="%d.%m.%Y"))
# }
# 
# colnames(weekBegin)<-monthBegin[1,]
# 
# row<-as.data.frame(matrix(nrow=1,ncol=52))
# row[is.na(row)]<-0
# 
# 
# 
# weekBegin3<- rbind(weekBegin [ ,1], row)
# 
# 
# 
# 
# 
# colnames (weekBegin3)<- colnames (weekBegin)
# 
# weekBegin3 [2,1] <-1
# 
# for (i in 2:52 ) {
# ifelse ( colnames(weekBegin3)[i] == colnames(weekBegin3)[i-1], weekBegin3 [2,i]<- weekBegin3 [2,i-1]+1, weekBegin3 [2,i]<-1)
# 
# }
# 
# 
# 
# for (i in 1:52 ) {
#   weekBegin3[1,i] <- as.character(weekBegin[1, i])
# }
# 
# # ��� ���������� ����������� � ������������
# twocolumns<-as.data.frame(matrix(nrow=2,ncol=2))
# 
# alltimeHeader<- cbind(twocolumns, weekBegin3)
# 
# #������ ������� ������������ �� ���� ������������
# monthnumber<-as.data.frame(matrix(nrow=1,ncol=54))
# monthnumber<-colnames(alltimeHeader)
# alltimeHeader<- rbind(monthnumber, alltimeHeader)
# 
# colnames(alltimeHeader)<-c("code", "item.x", c(1:52))


##
## ����� �����, ������� ���� ��������� � ������ ���� "merge tables" ��� ������������� �� ������� �����������, ������������� � 1�
##






threecolumns<-as.data.frame(matrix(nrow=3,ncol=2))
alltimeHeader<- cbind(threecolumns, alltimeHeader)
colnames(alltimeHeader)<-c("code", "item.x", colnames(alltimeHeader)[3:ncol(alltimeHeader)])






#
# ���� ������ ������������ � ��������� (������ ������, ������ ������, ���� ������������, ���������� ������ � ������)
#
totalProductiontime<- rbind(alltimeHeader, totalProduction)


# ��������� ������ ������� ��� ����������� �� ������� 
monthlyProductionPlan<-as.data.frame(matrix(nrow=nrow(finishedGoodsList),ncol=12))
monthlyProductionPlan[is.na(monthlyProductionPlan)]<-0


#�������� �� ���� ������� ����������� ������������ � ���������� �����������, ����������� � ������ ������

for (m in 4:nrow(totalProductiontime) ) {
  
for (i in 3:53 ) {
  
  
         
 monthlyProductionPlan [m-3, as.numeric(totalProductiontime [1,i]) ] <- 
 round(monthlyProductionPlan [m-3, as.numeric(totalProductiontime [1,i]) ] + as.numeric(totalProductiontime [m,i]))#  
}

}

# monthlyProductionPlan - ���������� ���� ������������


itemNames<- totalProductiontime [4:nrow(totalProductiontime), 1:2]
monthlyProductionPlan<- cbind(itemNames, monthlyProductionPlan)

colnames (monthlyProductionPlan) <- c("code", "item", "���", "���", "����", "���", "���", "����", "����", "������", "����", "���", "������", "���")


#
# ��������� ����������� ���� ������������ ��������� ��� ������
#
totalProductiontimeMSK<- rbind(alltimeHeader, reportMSK [(nrow(flowMSK)/3+1):(nrow(flowMSK)/3*2), c(1, 2, 4:55)])



# ��������� ������ ������� ��� ����������� �� ������� 
monthlyProductionPlanMSK<-as.data.frame(matrix(nrow=nrow(finishedGoodsList),ncol=12))
monthlyProductionPlanMSK[is.na(monthlyProductionPlanMSK)]<-0



for (m in 4:nrow(totalProductiontimeMSK) ) {
  
  for (i in 3:53 ) {
    
    
    
    monthlyProductionPlanMSK [m-3, as.numeric(totalProductiontimeMSK [1,i]) ] <- 
      round(monthlyProductionPlanMSK [m-3, as.numeric(totalProductiontimeMSK [1,i]) ] + as.numeric(totalProductiontimeMSK [m,i]))#  
  }
  
}


itemNames<- totalProductiontimeMSK [4:nrow(totalProductiontimeMSK), 1:2]
monthlyProductionPlanMSK <- cbind(itemNames, monthlyProductionPlanMSK)

colnames (monthlyProductionPlanMSK) <- c("code", "item", "���", "���", "����", "���", "���", "����", "����", "������", "����", "���", "������", "���")


#
# ��������� ����������� ���� ������������ ��������� ��� �����-����������
#

totalProductiontimeSPb<- rbind(alltimeHeader, reportSPb [(nrow(flowSPb)/3+1):(nrow(flowSPb)/3*2), c(1, 2, 4:55)])



# ��������� ������ ������� ��� ����������� �� ������� 
monthlyProductionPlanSPb<-as.data.frame(matrix(nrow=nrow(finishedGoodsList),ncol=12))
monthlyProductionPlanSPb[is.na(monthlyProductionPlanSPb)]<-0



for (m in 4:nrow(totalProductiontimeSPb) ) {
  
  for (i in 3:53 ) {
    
    
    
    monthlyProductionPlanSPb [m-3, as.numeric(totalProductiontimeSPb [1,i]) ] <- 
      round(monthlyProductionPlanSPb [m-3, as.numeric(totalProductiontimeSPb [1,i]) ] + as.numeric(totalProductiontimeSPb [m,i]))#  
  }
  
}


itemNames<- totalProductiontimeSPb [4:nrow(totalProductiontimeSPb), 1:2]
monthlyProductionPlanSPb <- cbind(itemNames, monthlyProductionPlanSPb)

colnames (monthlyProductionPlanSPb) <- c("code", "item", "���", "���", "����", "���", "���", "����", "����", "������", "����", "���", "������", "���")


#
# ��������� ����������� ���� ������������ ��������� ��� ������������
#

totalProductiontimeNSK<- rbind(alltimeHeader, reportNSK [(nrow(flowNSK)/3+1):(nrow(flowNSK)/3*2), c(1, 2, 4:55)])



# ��������� ������ ������� ��� ����������� �� ������� 
monthlyProductionPlanNSK<-as.data.frame(matrix(nrow=nrow(finishedGoodsList),ncol=12))
monthlyProductionPlanNSK[is.na(monthlyProductionPlanNSK)]<-0



for (m in 4:nrow(totalProductiontimeNSK) ) {
  
  for (i in 3:53 ) {
    
        
    monthlyProductionPlanNSK [m-3, as.numeric(totalProductiontimeNSK [1,i]) ] <- 
      round(monthlyProductionPlanNSK [m-3, as.numeric(totalProductiontimeNSK [1,i]) ] + as.numeric(totalProductiontimeNSK [m,i]))#  
  }
  
}


itemNames<- totalProductiontimeNSK [4:nrow(totalProductiontimeNSK), 1:2]
monthlyProductionPlanNSK <- cbind(itemNames, monthlyProductionPlanNSK)

colnames (monthlyProductionPlanNSK) <- c("code", "item", "���", "���", "����", "���", "���", "����", "����", "������", "����", "���", "������", "���")

#
# ������������ ���� (� %) ����������� ������� ������� � ����� �����������
#

partMSK <- monthlyProductionPlanMSK[ , 3:14]/ monthlyProductionPlan[ , 3:14]
partSPb <- monthlyProductionPlanSPb[ , 3:14]/ monthlyProductionPlan[ , 3:14]
partNSK <- monthlyProductionPlanNSK[ , 3:14]/ monthlyProductionPlan[ , 3:14]

# ����������� ��������� ������� ������ �������
colnames (partMSK) <- c(1:12)
colnames (partSPb) <- c(1:12)
colnames (partNSK) <- c(1:12)



#�������� �������� NaN �� ���� � ������� � ������
partMSK <- replace(partMSK, is.na(partMSK), 0)
partSPb <- replace(partSPb, is.na(partSPb), 0)
partNSK <- replace(partNSK, is.na(partNSK), 0)





##
# ������������ ������� �������� � ������ ������ �� ������ ������������� ����� ������������ �� 1�,
# ���������� � ������������ ����� ������������ ������� ������� � ����� ����� ������������


# ��������� �������� ������� ��������� library readr � readxl
#productionPlan1C<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������� �������/production plan 1�_3.xls")

productionPlan1C<-read.table("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/������ �������� ��.csv", skip = 2, 
                             sep = ";", header = TRUE)

productionPlan1C<- productionPlan1C[ , c(2, 4: ncol(productionPlan1C))]


# productionPlan1C<- productionPlan1C [complete.cases(productionPlan1C),]
# write.table(productionPlan1C , "C:/Documents and Settings/smirnov/��� ���������/������/R ����/������� �������/production plan 1�_4.csv", row.names = FALSE)
# productionPlan1C<-read.table("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������� �������/production plan 1�_4.csv", blank.lines.skip = TRUE)


# colnames(productionPlan1C)<- c ("code", "item", c (3:ncol(productionPlan1C)))
 colnames(productionPlan1C)[1:2]<- c ("code", "item")

fullRangeProdPlan<-merge(monthlyProductionPlanMSK [,c(1,2)], productionPlan1C, by ="code", all.x= TRUE)

#��������������� ���� � �������� ������
fullRangeProdPlan [,1]<-   as.numeric(fullRangeProdPlan[,1])

#��������� � ��������� �������
fullRangeProdPlan<-fullRangeProdPlan[order(fullRangeProdPlan[,1]),] 
fullRangeProdPlan<- fullRangeProdPlan [,c (1,2, 4:ncol(fullRangeProdPlan))]

fullRangeProdPlan[is.na(fullRangeProdPlan)]<-0

#������� ��� �� ���������� ��������
namesDates<- names(fullRangeProdPlan)[3:ncol(fullRangeProdPlan)-1]

colnames(fullRangeProdPlan)[3:ncol(fullRangeProdPlan)-1]<- str_sub(namesDates, start= -10)

# 
# 
# ������� "���������" �� ��������� ������� ������ � ������ �� ��������
# ��� ������

arrivalPlanMSK<- totalProductiontime
arrivalPlanMSK [4:nrow(arrivalPlanMSK), 3:ncol(arrivalPlanMSK)]<- 0

#����������� "������" ������� �� �������� ����� ����� ������ �� ����������
# ������ 5 �������� �������� ������ - ������, ����� 5 �������� - ���, ����� 5 �������� ���
as.numeric(str_sub(colnames(fullRangeProdPlan) [3], end =2))

#��������� ������� ����� ������ "������" ������
weekNumberMonth<- as.numeric(arrivalPlanMSK [  3, match (weekReport, colnames(arrivalPlanMSK) )])

# �������� ������ ������, ������� � ������� � ����������� �������� ����������
# ������� � ������ ������� ������ ������ ������

#����������� ����� �������, � ������� ���������� ������� �����
startCurrentMonth<-match(month(Sys.Date()), arrivalPlanMSK [1,])

# ����������� ��������� �� ��������������� ������
for (i in weekNumberMonth :5 ) {
  for (d in 4:nrow(arrivalPlanMSK)) {
    
  arrivalPlanMSK[d, i+startCurrentMonth-1] <- fullRangeProdPlan[d-3, i+2]
  
  }
}
  

# 
# �� ������ ����� ������������ �� �����, ����� ������������� �� ��������, ��������� ���� �������� � ������ ������
# 
# 



# ����� ������� � ����� ����� ������������, ��� ���������� ���� �� ������� �����
monthDigit<- ifelse(month(Sys.Date())<10, paste(0, month(Sys.Date()), sep = "" ), month(Sys.Date())  )
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))
#colNumberProduction<- match("X28.12.16", colnames(fullRangeProdPlan))


#������ �������, ����������� � �������� ������
colMonth<- which (arrivalPlanMSK[1,] %in% month(Sys.Date()))

#������ ������� �������� ������, ������� � ������� ������ � �� ����� ������ !!
#colMonth<- c((weekReport+2):max(colMonth))
colMonth<- c(which (colnames(arrivalPlanMSK) %in% (weekReport) ) : max(colMonth))


# �������� �������� ����������� ������ �� ������� ����� � ������ ���������� (������ ���������); 
# ��������������� � �������� ������ 
arrivalPlanMSK [4:nrow(arrivalPlanMSK), c(colMonth)]<- round(as.numeric(data.matrix(arrivalPlanMSK [4:nrow(arrivalPlanMSK), c(colMonth)], rownames.force = NA))+  
# �������� ���� �� ������� ����� �� ���� � ����� �� ���������� ������ � ������
(fullRangeProdPlan[ , colNumberProduction] * partMSK [ , month(Sys.Date())])/ length(colMonth))



# ����� ������� � ����� ����� ������������, ��� ���������� ���� �� ����� ��������� �� �������
monthDigit<-ifelse(month(Sys.Date())+1<10 | month(Sys.Date())+1>12, 
                   paste(0, month(Sys.Date())+1-ifelse(month(Sys.Date())+1>12, 12, 0), sep = "" ), month(Sys.Date())+1)
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())+1<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))

#������ �������, ����������� � �������� ������ ��������� �� �������
colMonth<- which (arrivalPlanMSK[1,] %in% ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12) )


arrivalPlanMSK [4:nrow(arrivalPlanMSK), c(colMonth)]<- round(data.matrix(arrivalPlanMSK [4:nrow(arrivalPlanMSK), c(colMonth)], rownames.force = NA)+  
                                                               # �������� ���� �� ������� ����� �� ���� � ����� �� ���������� ������ � ������
                                                               (fullRangeProdPlan[ , colNumberProduction] * partMSK [ , ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12)])/ length(colMonth))

# 
# �������� ������ ��� ������ ������ ��� ��������� ����� �� 3 ������
# 


# ����� ������� � ����� ����� ������������ ����� 2 ���
monthDigit<-ifelse(month(Sys.Date())+2<10 | month(Sys.Date())+2>12, 
                   paste(0, month(Sys.Date())+2-ifelse(month(Sys.Date())+2>12, 12, 0), sep = "" ), month(Sys.Date())+2)
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())+2<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))

#������ �������, ����������� � ������ ����� 2 ������
colMonth<- which (arrivalPlanMSK[1,] %in% ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12) )


arrivalPlanMSK [4:nrow(arrivalPlanMSK), c(colMonth)]<- round(data.matrix(arrivalPlanMSK [4:nrow(arrivalPlanMSK), c(colMonth)], rownames.force = NA)+  
                                                               # �������� ���� �� ������� ����� �� ���� � ����� �� ���������� ������ � ������
                                                               (fullRangeProdPlan[ , colNumberProduction] * partMSK [ , ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12)])/ length(colMonth))

#������ ����� �� ������� ������ � �� ����� ����
arrivalPlanMSK<- arrivalPlanMSK [ , c(1,2, c(match(weekReport, colnames(arrivalPlanMSK)):ncol(arrivalPlanMSK)))]
write.csv(arrivalPlanMSK, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/arrivalPlanMSK.csv")
















# ������� "���������" �� ��������� ������� ������ � ������ �� ��������
# ��� �����-����������

arrivalPlanSPb<- totalProductiontime
arrivalPlanSPb [4:nrow(arrivalPlanSPb), 3:ncol(arrivalPlanSPb)]<- 0

#����������� "������" ������� �� �������� ����� ����� ������ �� ����������
# ������ 5 �������� �������� ������ - ������, ����� 5 �������� - ���, ����� 5 �������� ���
as.numeric(str_sub(colnames(fullRangeProdPlan) [3], end =2))

#��������� ������� ����� ������ "������" ������
weekNumberMonth<- as.numeric(arrivalPlanSPb [  3, match (weekReport, colnames(arrivalPlanSPb) )])

# �������� ������ ������, ������� � ������� � ����������� �������� ����������
# ������� � ������ ������� ������ ������ ������

#����������� ����� �������, � ������� ���������� ������� �����
startCurrentMonth<-match(month(Sys.Date()), arrivalPlanSPb [1,])

# ����������� ��������� �� ��������������� ������
for (i in weekNumberMonth :5 ) {
  for (d in 4:nrow(arrivalPlanSPb)) {
    
    arrivalPlanSPb[d, i+startCurrentMonth-1] <- fullRangeProdPlan[d-3, i+2+5] #"5" ���������� ������� ��� ������������� ��������� ���
    
  }
}


# 
# �� ������ ����� ������������ �� �����, ����� ������������� �� ��������, ��������� ���� �������� � ������ ������
# 
# 



# ����� ������� � ����� ����� ������������, ��� ���������� ���� �� ������� �����
monthDigit<- ifelse(month(Sys.Date())<10, paste(0, month(Sys.Date()), sep = "" ), month(Sys.Date())  )
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))



#������ �������, ����������� � �������� ������
colMonth<- which (arrivalPlanSPb[1,] %in% month(Sys.Date()))

#������ ������� �������� ������, ������� � ������� ������ � �� ����� ������ !!
#colMonth<- c((weekReport+2):max(colMonth))
colMonth<- c(which (colnames(arrivalPlanSPb) %in% (weekReport) ) : max(colMonth))

# ������� �������� ����������� ������ �� ������� ����� � ������ ���������� (������ ���������); 
# ��������������� � �������� ������ 
arrivalPlanSPb [4:nrow(arrivalPlanSPb), c(colMonth)]<- round(as.numeric(data.matrix(arrivalPlanSPb [4:nrow(arrivalPlanSPb), c(colMonth)], rownames.force = NA))+  
                                                               # �������� ���� �� ������� ����� �� ���� � ����� �� ���������� ������ � ������
                                                               (fullRangeProdPlan[ , colNumberProduction] * partSPb [ , month(Sys.Date())])/ length(colMonth))



# ����� ������� � ����� ����� ������������, ��� ���������� ���� �� ����� ��������� �� �������
monthDigit<-ifelse(month(Sys.Date())+1<10 | month(Sys.Date())+1>12, 
                   paste(0, month(Sys.Date())+1-ifelse(month(Sys.Date())+1>12, 12, 0), sep = "" ), month(Sys.Date())+1)
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())+1<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))

#������ �������, ����������� � �������� ������ ��������� �� �������
colMonth<- which (arrivalPlanSPb[1,] %in% ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12) )


arrivalPlanSPb [4:nrow(arrivalPlanSPb), c(colMonth)]<- round(data.matrix(arrivalPlanSPb [4:nrow(arrivalPlanSPb), c(colMonth)], rownames.force = NA)+  
                                                               # �������� ���� �� ������� ����� �� ���� � ����� �� ���������� ������ � ������
                                                               (fullRangeProdPlan[ , colNumberProduction] * partSPb [ , ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12)])/ length(colMonth))

# 
# �������� ������ ��� ������ ������ ��� ��������� ����� �� 3 ������
# 


# ����� ������� � ����� ����� ������������, ��� ���������� ���� �� ����� ��������� �� �������
monthDigit<- ifelse(month(Sys.Date())+2<10 | month(Sys.Date())+2>12, 
                    paste(0, month(Sys.Date())+2-ifelse(month(Sys.Date())+2>12, 12, 0), sep = "" ), month(Sys.Date())+2)
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())+2<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))

#������ �������, ����������� � �������� ������ ��������� �� �������
colMonth<- which (arrivalPlanSPb[1,] %in% ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12) )


arrivalPlanSPb [4:nrow(arrivalPlanSPb), c(colMonth)]<- round(data.matrix(arrivalPlanSPb [4:nrow(arrivalPlanSPb), c(colMonth)], rownames.force = NA)+  
                                                               # �������� ���� �� ������� ����� �� ���� � ����� �� ���������� ������ � ������
                                                               (fullRangeProdPlan[ , colNumberProduction] * partSPb [ , ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12)])/ length(colMonth))
#������ ����� �� ������� ������ � �� ����� ����
arrivalPlanSPb<- arrivalPlanSPb [ , c(1,2, c(match(weekReport, colnames(arrivalPlanSPb)):ncol(arrivalPlanSPb)))]
write.csv(arrivalPlanSPb, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/arrivalPlanSPb.csv")











# ������� "���������" �� ��������� ������� ������ � ������ �� ��������
# ��� ������������

arrivalPlanNSK<- totalProductiontime
arrivalPlanNSK [4:nrow(arrivalPlanNSK), 3:ncol(arrivalPlanNSK)]<- 0

#����������� "������" ������� �� �������� ����� ����� ������ �� ����������
# ������ 5 �������� �������� ������ - ������, ����� 5 �������� - ���, ����� 5 �������� ���
as.numeric(str_sub(colnames(fullRangeProdPlan) [3], end =2))

#��������� ������� ����� ������ "������" ������
weekNumberMonth<- as.numeric(arrivalPlanNSK [  3, match (weekReport, colnames(arrivalPlanNSK) )])

# �������� ������ ������, ������� � ������� � ����������� �������� ����������
# ������� � ������ ������� ������ ������ ������

#����������� ����� �������, � ������� ���������� ������� �����
startCurrentMonth<-match(month(Sys.Date()), arrivalPlanNSK [1,])

# ����������� ��������� �� ��������������� ������
for (i in weekNumberMonth :5 ) {
  for (d in 4:nrow(arrivalPlanNSK)) {
    
    arrivalPlanNSK[d, i+startCurrentMonth-1] <- fullRangeProdPlan[d-3, i+2+10]
    
  }
}


# 
# �� ������ ����� ������������ �� �����, ����� ������������� �� ��������, ��������� ���� �������� � ������ ������
# 
# �����������



# ����� ������� � ����� ����� ������������, ��� ���������� ���� �� ������� �����
monthDigit<- ifelse(month(Sys.Date())<10, paste(0, month(Sys.Date()), sep = "" ), month(Sys.Date())  )
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))



#������ �������, ����������� � �������� ������
colMonth<- which (arrivalPlanNSK[1,] %in% month(Sys.Date()))

#������ ������� �������� ������, ������� � ������� ������ � �� ����� ������ !!
#colMonth<- c((weekReport+2):max(colMonth))
colMonth<- c(which (colnames(arrivalPlanNSK) %in% (weekReport) ) : max(colMonth))
# ������� �������� ����������� ������ �� ������� ����� � ������ ���������� (������ ���������); 
# ��������������� � �������� ������ 
arrivalPlanNSK [4:nrow(arrivalPlanNSK), c(colMonth)]<- round(as.numeric(data.matrix(arrivalPlanNSK [4:nrow(arrivalPlanNSK), c(colMonth)], rownames.force = NA))+  
                                                               # �������� ���� �� ������� ����� �� ���� � ����� �� ���������� ������ � ������
                                                               (fullRangeProdPlan[ , colNumberProduction] * partNSK [ , month(Sys.Date())])/ length(colMonth))



# ����� ������� � ����� ����� ������������, ��� ���������� ���� �� ����� ��������� �� �������
monthDigit<- ifelse(month(Sys.Date())+1<10 | month(Sys.Date())+1>12, 
                    paste(0, month(Sys.Date())+1-ifelse(month(Sys.Date())+1>12, 12, 0), sep = "" ), month(Sys.Date())+1)
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())+1<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))

#������ �������, ����������� � �������� ������ ��������� �� �������
colMonth<- which (arrivalPlanNSK[1,] %in% ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12) )


arrivalPlanNSK [4:nrow(arrivalPlanNSK), c(colMonth)]<- round(data.matrix(arrivalPlanNSK [4:nrow(arrivalPlanNSK), c(colMonth)], rownames.force = NA)+  
                                                               # �������� ���� �� ������� ����� �� ���� � ����� �� ���������� ������ � ������
                                                               (fullRangeProdPlan[ , colNumberProduction] * partNSK [ , ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12)])/ length(colMonth))

# 
# �������� ������ ��� ������ ������ ��� ��������� ����� �� 3 ������
# 


# ����� ������� � ����� ����� ������������, ��� ���������� ���� �� ����� ��������� �� �������
monthDigit<- ifelse(month(Sys.Date())+2<10 | month(Sys.Date())+2>12, 
                    paste(0, month(Sys.Date())+2-ifelse(month(Sys.Date())+2>12, 12, 0), sep = "" ), month(Sys.Date())+2)
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())+2<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))

#������ �������, ����������� � �������� ������ ��������� �� �������
colMonth<-which (arrivalPlanNSK[1,] %in% ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12) )


arrivalPlanNSK [4:nrow(arrivalPlanNSK), c(colMonth)]<- round(data.matrix(arrivalPlanNSK [4:nrow(arrivalPlanNSK), c(colMonth)], rownames.force = NA)+  
                                                               # �������� ���� �� ������� ����� �� ���� � ����� �� ���������� ������ � ������
                                                               (fullRangeProdPlan[ , colNumberProduction] * partNSK [ , ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12)])/ length(colMonth))

#������ ����� �� ������� ������ � �� ����� ����
arrivalPlanNSK<- arrivalPlanNSK [ , c(1,2, c(match(weekReport, colnames(arrivalPlanNSK)):ncol(arrivalPlanNSK)))]
write.csv(arrivalPlanNSK, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/arrivalPlanNSK.csv")

# ���������� ��� ������� � ����������� �������� ������������ � ������� �� � ������ ������ � ����� �������
write.csv(monthlyProductionPlan, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/monthlyProductionPlan.csv")
write.csv(monthlyProductionPlanMSK, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/monthlyProductionPlanMSK.csv")
write.csv(monthlyProductionPlanSPb, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/monthlyProductionPlanSPb.csv")
write.csv(monthlyProductionPlanNSK, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/monthlyProductionPlanNSK.csv")



# ����
partMSK<- cbind(salesMoscow [ ,c (1:2)], round(partMSK, digits = 2))
partSPb<- cbind(salesSPb [ ,c (1:2)], round(partSPb, digits = 2))
partNSK<- cbind(salesNSK [ ,c (1:2)], round(partNSK, digits = 2))

write.csv(partMSK, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/partMSK.csv")
write.csv(partSPb, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/partSPb.csv")
write.csv(partNSK, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/partNSK.csv")








##
##
##
## ������������ ����������� ����������
##

# ��������������� � �������� ������ 
# ��� ������
for (i in 3:ncol(arrivalPlanMSK) ) {
  arrivalPlanMSK[ ,i] <- as.numeric(arrivalPlanMSK[ ,i])
}
arrivalPlanMSKmonth <- arrivalPlanMSK[ 4: nrow(arrivalPlanMSK) , 1:6]

#����� �������� �������� �� ��������� 4 ������
arrivalPlanMSKmonth$sum <- rowSums (arrivalPlanMSKmonth [ , 3:6])

# ��������� ������ �� ��������� 4 ������
salesMoscowMonth <- salesMoscow [ , 1:6]
salesMoscowMonth$sum <- rowSums (salesMoscow [ , 3:6])
# ����� ������� � ������� �������
currntWeekcolumnNumber<- which(colnames(reportMSK) %in% weekReport)

# ����� ������� �������� � �������� �� ��������� 4 ������
deficitMSK<- cbind(reportMSK[1:(nrow(flowMSK)/3),c(1:2)], (reportMSK[1:(nrow(flowMSK)/3), currntWeekcolumnNumber] + arrivalPlanMSKmonth$sum 
                                                           -salesMoscowMonth$sum)/ salesMoscowMonth$sum)

deficitMSK<- cbind(reportMSK[1:(nrow(flowMSK)/3),c(1:2)], 
                   reportMSK[1:(nrow(flowMSK)/3), currntWeekcolumnNumber],
                   arrivalPlanMSKmonth$sum,
                   salesMoscowMonth$sum,
                   reportMSK[1:(nrow(flowMSK)/3), currntWeekcolumnNumber] + arrivalPlanMSKmonth$sum -salesMoscowMonth$sum, 
                   round(deficitMSK[ ,3], digits = 2))

colnames(deficitMSK)<-  c ("���", "������������", "������� �������", "���� ������� 4 ���", "���� ������ 4 ���", "�������� ������� ����� 4 ���", "������-�� ����� 4 ���, ���")

deficitMSKFilter <- subset(deficitMSK, deficitMSK [ ,6] < -0)
surplusMSKFilter <- subset(deficitMSK, deficitMSK [ ,7] > 2)


#"���������� �������" ��� ����� ��������
stockMSK<- cbind( sat2 [,4], (sat2[ ,warehouseStockMoscow]+sat2[ ,warehouseStockMoscowBranch] + sat2[ ,warehouseStockMoscowTransit]))
colnames(stockMSK) <- c("���", "����������")

deficitMSKFilter<-merge(deficitMSKFilter, stockMSK, by ="���", all.x= TRUE)
deficitMSKFilter<- cbind(deficitMSKFilter[ , c(1:2)], deficitMSKFilter[ ,8], deficitMSKFilter[ , c(3:7)])

colnames(deficitMSKFilter)<-  c ("���", "������������", "������� �������", "������� � ��������", "���� ������� 4 ���", "���� ������ 4 ���", "�������� ������� ����� 4 ���", "������-�� ����� 4 ���, ���")


write.csv(deficitMSKFilter, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/deficitMSKFilter.csv")
write.csv(surplusMSKFilter, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/surplusMSKFilter.csv")






#��� �����-����������


for (i in 3:ncol(arrivalPlanSPb) ) {
  arrivalPlanSPb[ ,i] <- as.numeric(arrivalPlanSPb[ ,i])
}
arrivalPlanSPbmonth <- arrivalPlanSPb[ 4: nrow(arrivalPlanSPb) , 1:6]

#����� �������� �������� �� ��������� 4 ������
arrivalPlanSPbmonth$sum <- rowSums (arrivalPlanSPbmonth [ , 3:6])

# ��������� ������ �� ��������� 4 ������
salesSPbMonth <- salesSPb [ , 1:6]
salesSPbMonth$sum <- rowSums (salesSPb [ , 3:6])

# ����� ������� �������� � �������� �� ��������� 4 ������
deficitSPb<- cbind(reportSPb[1:(nrow(flowSPb)/3),c(1:2)], (reportSPb[1:(nrow(flowSPb)/3), currntWeekcolumnNumber] + arrivalPlanSPbmonth$sum 
                                                           -salesSPbMonth$sum)/ salesSPbMonth$sum)

deficitSPb<- cbind(reportSPb[1:(nrow(flowSPb)/3),c(1:2)], 
                   reportSPb[1:(nrow(flowSPb)/3), currntWeekcolumnNumber],
                   arrivalPlanSPbmonth$sum,
                   salesSPbMonth$sum,
                   reportSPb[1:(nrow(flowSPb)/3), currntWeekcolumnNumber] + arrivalPlanSPbmonth$sum -salesSPbMonth$sum, 
                   round(deficitSPb[ ,3], digits = 2))

colnames(deficitSPb)<-  c ("���", "������������", "������� �������", "���� ������� 4 ���", "���� ������ 4 ���", "�������� ������� ����� 4 ���", "������-�� ����� 4 ���, ���")

deficitSPbFilter <- subset(deficitSPb, deficitSPb [ ,6] < -0)
surplusSPbFilter <- subset(deficitSPb, deficitSPb [ ,7] > 2)


#"���������� �������" ��� ����� ��������
stockSPb<- cbind( sat2 [,4], sat2[ ,warehouseStockSPb])
colnames(stockSPb) <- c("���", "����������")

deficitSPbFilter<-merge(deficitSPbFilter, stockSPb, by ="���", all.x= TRUE)
deficitSPbFilter<- cbind(deficitSPbFilter[ , c(1:2)], deficitSPbFilter[ ,8], deficitSPbFilter[ , c(3:7)])

colnames(deficitSPbFilter)<-  c ("���", "������������", "������� �������", "������� � ��������", "���� ������� 4 ���", "���� ������ 4 ���", "�������� ������� ����� 4 ���", "������-�� ����� 4 ���, ���")



write.csv(deficitSPbFilter, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/deficitSPbFilter.csv")
write.csv(surplusSPbFilter, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/surplusSPbFilter.csv")



# ��� ������������

for (i in 3:ncol(arrivalPlanNSK) ) {
  arrivalPlanNSK[ ,i] <- as.numeric(arrivalPlanNSK[ ,i])
}
arrivalPlanNSKmonth <- arrivalPlanNSK[ 4: nrow(arrivalPlanNSK) , 1:6]

#����� �������� �������� �� ��������� 4 ������
arrivalPlanNSKmonth$sum <- rowSums (arrivalPlanNSKmonth [ , 3:6])

# ��������� ������ �� ��������� 4 ������
salesNSKMonth <- salesNSK [ , 1:6]
salesNSKMonth$sum <- rowSums (salesNSK [ , 3:6])

# ����� ������� �������� � �������� �� ��������� 4 ������
deficitNSK<- cbind(reportNSK[1:(nrow(flowNSK)/3),c(1:2)], (reportNSK[1:(nrow(flowNSK)/3), currntWeekcolumnNumber] + arrivalPlanNSKmonth$sum 
                   -salesNSKMonth$sum)/ salesNSKMonth$sum)

deficitNSK<- cbind(reportNSK[1:(nrow(flowNSK)/3),c(1:2)], 
                   reportNSK[1:(nrow(flowNSK)/3), currntWeekcolumnNumber],
                   arrivalPlanNSKmonth$sum,
                    salesNSKMonth$sum,
                    reportNSK[1:(nrow(flowNSK)/3), currntWeekcolumnNumber] + arrivalPlanNSKmonth$sum -salesNSKMonth$sum, 
                    round(deficitNSK[ ,3], digits = 2))

colnames(deficitNSK)<-  c ("���", "������������", "������� �������", "���� ������� 4 ���", "���� ������ 4 ���", "�������� ������� ����� 4 ���", "������-�� ����� 4 ���, ���")

deficitNSKFilter <- subset(deficitNSK, deficitNSK [ ,6] < 0)
surplusNSKFilter <- subset(deficitNSK, deficitNSK [ ,7] > 2)


#"���������� �������" ��� ����� ��������
stockNSK<- cbind( sat2 [,4], (sat2[ ,warehouseStockNSK ]+sat2[ ,warehouseStockNSKBranch] + sat2[ ,warehouseStockNSKTransit]))
colnames(stockNSK) <- c("���", "����������")

deficitNSKFilter<-merge(deficitNSKFilter, stockNSK, by ="���", all.x= TRUE)
deficitNSKFilter<- cbind(deficitNSKFilter[ , c(1:2)], deficitNSKFilter[ ,8], deficitNSKFilter[ , c(3:7)])

colnames(deficitNSKFilter)<-  c ("���", "������������", "������� �������", "������� � ��������", "���� ������� 4 ���", "���� ������ 4 ���", "�������� ������� ����� 4 ���", "������-�� ����� 4 ���, ���")



write.csv(deficitNSKFilter, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/deficitNSKFilter.csv")
write.csv(surplusNSKFilter, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/surplusNSKFilter.csv")






# ������� ���� �� ������� �����
allParts<-cbind(salesMoscow [ ,c (1:2)], partMSK[ , month(Sys.Date())+2], partSPb[ , month(Sys.Date())+2,], partNSK[ , month(Sys.Date())+2])
colnames(allParts)<- c("code", "item", "MSK", "SPB", "NSK")


allParts <- cbind(allParts[ , 1:2],   
                  weeklyStockMoscow [ , weekReport+2], salesMoscowMonth [ , 7],
                  weeklyStockSPb [ , weekReport+2], salesSPbMonth [ , 7],
                  weeklyStockNSK [ , weekReport+2], salesNSKMonth [ , 7],
                  allParts [ ,3:5])

colnames(allParts) <- c("code", "������������", "������� ���", "������� ���", "������� ���", "������� ���","������� ���", "������� ���","MSK", "SPB", "NSK")
write.csv(allParts, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/allParts.csv")

allParts$���������<- 0

for (i in 1:nrow(allParts)) {

  allParts[i,12]<- ifelse (allParts[i,3]<0 | allParts[i,5]<0 | allParts[i,7]<0, 
                               allParts[i,12]<- "���������", allParts[i,12]<- "����")
  
}

priorityProduction <- subset(allParts, allParts [ ,12] == "���������") [1:8]

write.csv(priorityProduction, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/priorityProduction.csv", row.names=FALSE)



#������ ������� �� ���������� ����� ������, �������������� �������, �������� ��������� ��������� ������� � "��������"
#source('~/������/R ����/sales analytics.R')

#
#����������� �������� � ���������� ������
#


source('~/������/R ����/Surplus cost and turnover.R')

# ����������� ��� �������� �����
flist <- list.files("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������", full.names = TRUE)
file.copy(flist, paste("C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/�����/", Sys.Date(), sep = ""))

# ����������� ��� ���������� ����� �������� �����
flist <- list.files("C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������", full.names = TRUE)
file.copy(flist, paste("C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/�����/", Sys.Date(), sep = ""))


#����������� ��� ����� �� ������� ������
file.copy(flist, "Z:/�������/������ ��� R", overwrite = TRUE)



