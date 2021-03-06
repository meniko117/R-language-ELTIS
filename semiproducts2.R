# ���������� ������������ ��������������

library(readxl)
semiProducts<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/semiproducts 2.xls")
colnames(semiProducts) <- c("��� ���������� �����", "code", "������������", "�����", "�������", "�����������", "��������" )
# �������� ���� ��������� ������ � 1-� �������. ����������� �� ���� ����� ����� �� 1�.
semiProducts$id  <- 1:nrow(semiProducts) # ������� �������� ��� ���������� ������� �����
# ����������� �� ��������� ����� ����� ����������
lastStageCode<-aggregate(semiProducts [ ,4] ~ semiProducts [ ,1], semiProducts, FUN = function(x) length(unique(x)))



#����������� ������� �� ���
semiProducts<-merge(semiProducts, stockComponents, by ="code", all.x= TRUE, row.names = FALSE)
colnames(semiProducts)[10]<- "����������"
semiProducts<-semiProducts[order(semiProducts$id), ]


rownames(semiProducts) <- NULL
semiProducts[is.na(semiProducts)]<-0
# ������������ ������� � ������
semiProducts <- semiProducts [ , c(2,1,3:10)]

semiProducts[ ,10][semiProducts[ ,10]== 0]<- 1 # ������� ���������� �� ����������� ����� ������� � reshape2, ������� ������ �� 1

semiProducts[ ,5] <- semiProducts[ ,10]








#����������� ����������� ��� (������������� ���������� ��� ���� �����������)
# ��� ������� ����� "��������" ����������� �� ������������� ������ ���������� �������������� ����������
mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/����������� ����������� ��� ", 
           ifelse(month(Sys.Date())<10,0,""),
           month(Sys.Date()), ".xls", sep="")


componentsConsumption<-read_excel(mu)
componentsConsumption<- componentsConsumption [, c(2,4,match("��������� � �������", componentsConsumption[2, ]))]
colnames(componentsConsumption) <- c("code", "item", "quantity")
componentsConsumption <- componentsConsumption [3:nrow(componentsConsumption), ]
componentsConsumption [ ,3] <- round(as.numeric(componentsConsumption [ ,3])) 

semiProducts2<-merge(semiProducts, componentsConsumption, by ="code", all.x= TRUE)
semiProducts2 [ ,6] <- semiProducts2 [ ,12] 
semiProducts2 <- semiProducts2[order(semiProducts2$id), ]

semiProducts [ ,6]<- semiProducts2 [ ,6]
semiProducts[is.na(semiProducts)]<-0





# ����������� ������� �� ������ ������ �� �������� �� ������ ������� ��� �������������� 1-� ������
# ������ ������ �� �����
# ������������ "������� - �����������" ��� �������������� 1-� ������ � ����������� ������� � ����� ����� �� ��������
# ��������� � 2-� � ����� ������
# ������ ������� ������� �� ������������ ��� ������� ����

library(reshape2)
code<-dcast(semiProducts, code ~ id, fun.aggregate=sum) #���������� ��� ������� ���� ������ � �������




# ������ ��������� ��� ����, ������� ��������� � ���� ���������� ����� 
# ��� ������� ���� ����� ����������� ���������� ����� � ���������� ����������� � �������������� ������,
# ������ �� �������� ������� ��������������� �����

#nrow(lastStageCode)
for (i in 1: nrow(lastStageCode)) { # ������ ����������� �� ���������� ����� ���������� �����
  #nrow (lastStageCode)
  lastStage<-max(grep(lastStageCode [i,1], semiProducts[ ,1])) # ������ ����������� � ���������� ����� ���� �������������
  firstStage<- min(grep(lastStageCode [i,1], semiProducts[ ,1])) # ������ ����������� � ������� ����� ���� �������������
  
  semiProducts [lastStage, 7]<- ifelse (semiProducts [lastStage, 5]- semiProducts [lastStage, 6]<0, abs(semiProducts [lastStage, 5]- semiProducts [lastStage, 6]), 0)

  
  for (k in 1:(lastStage-firstStage)) { # ��������� ������� ���������� �����, ������� ��������� � ���� ���������� �������������
    
    #     ## ��������� ���� 
    #     # ������������ ����� ���� ��� ����������� ��� ������ ��������������� ��������, �.�. �� ������������ merge
    # semiProducts [lastStage-k, 5] <- semiProducts [lastStage-k, 10]
    #   semiProducts<-merge(stockComponents, semiProducts, by ="code", all.x= TRUE, row.names = FALSE) # ������ ����������� � ����� �����
    #   
    #     ##
    
    
    semiProducts [lastStage-k, 6] <-  semiProducts [lastStage-(k-1), 7] # ���������� ����������� ����������� �����
    semiProducts [lastStage-k, 7] <-  ifelse(semiProducts [lastStage-k, 6] - semiProducts [lastStage-k, 5]>0,  semiProducts [lastStage-k, 6] - semiProducts [lastStage-k, 5], 0)
    # 
   
    # ����������� ����� ������� ����� ������� ��������� ������ �������������
    
    # ���������� ������ ��������� ���� � ����� ������ "semiproducts"
    line<- grep(semiProducts [lastStage-k,2], code[,1]) # ��������� ������ � ��������� ��������� � ������� "code"
    listCodeId<- which (code[ line, ]> 0) -1 # �������� ������ ��������
    listCodeId <- listCodeId [c(2:length(listCodeId))] # ���������� 1-� ��������
    
    #���������� ����� ������� � ��������� � ������ ������ ��������������
    semiProducts [listCodeId,10] <- semiProducts [lastStage-k, 5] -  semiProducts [lastStage-k, 6]
    #���������� �������������� ������� �� ������������ ������ � "�������" ������� "5" ��� ������� �� ��. �������
    semiProducts [ listCodeId,5] <-ifelse(semiProducts [ listCodeId,10]>0, semiProducts [ listCodeId,10], 0) #���������� ����� ������� (����� ������� ������ ��������������)
    
    #ifelse(semiProducts [ listCodeId,10]>0, semiProducts [ listCodeId,10], 0)
    
  }

}



# ����������� ��� ������� ���� ����������� ���������� ������ ��� �������
purchase<-aggregate(semiProducts$��������, by=list(semiProducts$code), FUN=sum)
colnames(purchase) <- c("code", "������")
purchase <- merge(purchase, param, by ="code", all.x= TRUE) #���������� � ����� ��������� ������������
purchase <- merge(purchase, stockComponents, by ="code", all.x= TRUE) #���������� �������
purchase <- merge(purchase, componentsConsumption, by ="code", all.x= TRUE) #���������� ����������� �������� ������

purchase <- purchase [ , c(1,3,10,12,2)]
colnames(purchase) [c(3,4)]<- c("�������", "�����������")
purchase[is.na(purchase)]<-0
write.csv(purchase, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/purchaseSemiProducts 1.csv")

















# ����� ������� �� ������� semiproducts- ��� ������� �� ����� ���������� �������, �� �������� ���� ����� �����������
# ���������� ��� ������� ��� ������� ���������� ������
# �������� ����������� ��� ���������� ������
# ��������� ��� ������ ���������� �� �������
# ������� ����������� ��� ���� ������ ��� ������� �� 3-� �������, ���������� �� � salesMatrix ������� ���� ��� �� ������������ ���������


#��������� ����������� �� �����, ��������� �� �������
mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/����������� ����������� ��� ", 
           ifelse(month(Sys.Date())+1-12<10,0,""),
           ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12) , ".xls", sep="")
componentsConsumption<-read_excel(mu)


componentsConsumption<- componentsConsumption [, c(2,4,match("��������� � �������", componentsConsumption[2, ]))]
colnames(componentsConsumption) <- c("code", "item", "quantity")
componentsConsumption <- componentsConsumption [3:nrow(componentsConsumption), ]
componentsConsumption [ ,3] <- round(as.numeric(componentsConsumption [ ,3]))
componentsConsumption [ ,1] <- round(as.numeric(componentsConsumption [ ,1]))


semiProducts2<-merge(semiProducts, componentsConsumption, by ="code", all.x= TRUE)
semiProducts2 [ ,6] <- semiProducts2 [ ,12] 
semiProducts2 <- semiProducts2[order(semiProducts2$id), ]

semiProducts [ ,6]<- semiProducts2 [ ,6]
semiProducts[is.na(semiProducts)]<-0



#code<-dcast(semiProducts, code ~ id, fun.aggregate=sum) #���������� ��� ������� ���� ������ � �������




# ������ ��������� ��� ����, ������� ��������� � ���� ���������� ����� 
# ��� ������� ���� ����� ����������� ���������� ����� � ���������� ����������� � �������������� ������,
# ������ �� �������� ������� ��������������� �����


for (i in 1:nrow(lastStageCode) ) { # ������ ����������� �� ���������� ����� ���������� �����
  #nrow (lastStageCode)
  lastStage<-max(grep(lastStageCode [i,1], semiProducts[ ,1])) # ������ ����������� � ���������� ����� ���� �������������
  firstStage<- min(grep(lastStageCode [i,1], semiProducts[ ,1])) # ������ ����������� � ������� ����� ���� �������������
  
  semiProducts [lastStage, 7]<- ifelse (semiProducts [lastStage, 5]- semiProducts [lastStage, 6]<0, abs(semiProducts [lastStage, 5]- semiProducts [lastStage, 6]), 0)
  
  
  for (k in 1:(lastStage-firstStage)) { # ��������� ������� ���������� �����, ������� ��������� � ���� ���������� �������������
    
    #     ## ��������� ���� 
    #     # ������������ ����� ���� ��� ����������� ��� ������ ��������������� ��������, �.�. �� ������������ merge
    # semiProducts [lastStage-k, 5] <- semiProducts [lastStage-k, 10]
    #   semiProducts<-merge(stockComponents, semiProducts, by ="code", all.x= TRUE, row.names = FALSE) # ������ ����������� � ����� �����
    #   
    #     ##
    
    
    semiProducts [lastStage-k, 6] <-  semiProducts [lastStage-(k-1), 7] # ���������� ����������� ����������� �����
    semiProducts [lastStage-k, 7] <-  ifelse(semiProducts [lastStage-k, 6] - semiProducts [lastStage-k, 5]>0,  semiProducts [lastStage-k, 6] - semiProducts [lastStage-k, 5], 0)
    # 
    
    # ����������� ����� ������� ����� ������� ��������� ������ �������������
    
    # ���������� ������ ��������� ���� � ����� ������ "semiproducts"
    line<- grep(semiProducts [lastStage-k,2], code[,1]) # ��������� ������ � ��������� ��������� � ������� "code"
    listCodeId<- which (code[ line, ]> 0) -1 # �������� ������ ��������
    listCodeId <- listCodeId [c(2:length(listCodeId))] # ���������� 1-� ��������
    
    #���������� ����� ������� � ��������� � ������ ������ ��������������
    semiProducts [listCodeId,10] <- semiProducts [lastStage-k, 5] -  semiProducts [lastStage-k, 6]
    #���������� �������������� ������� �� ������������ ������ � "�������" ������� "5" ��� ������� �� ��. �������
    semiProducts [ listCodeId,5] <-ifelse(semiProducts [ listCodeId,10]>0, semiProducts [ listCodeId,10], 0) #���������� ����� ������� (����� ������� ������ ��������������)
    
    #ifelse(semiProducts [ listCodeId,10]>0, semiProducts [ listCodeId,10], 0)
    
  }
  
}



# ����������� ��� ������� ���� ����������� ���������� ������ ��� �������
purchase<-aggregate(semiProducts$��������, by=list(semiProducts$code), FUN=sum)
colnames(purchase) <- c("code", "������")
purchase <- merge(purchase, param, by ="code", all.x= TRUE) #���������� � ����� ��������� ������������
purchase <- merge(purchase, stockComponents, by ="code", all.x= TRUE) #���������� �������
purchase <- merge(purchase, componentsConsumption, by ="code", all.x= TRUE) #���������� ����������� �������� ������

purchase <- purchase [ , c(1,3,10,12,2)]
colnames(purchase) [c(3,4)]<- c("�������", "�����������")
purchase[is.na(purchase)]<-0
write.csv(purchase, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/purchaseSemiProducts 2.csv")



















#��������� ����������� ����� 2 ������
mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/����������� ����������� ��� ", 
           ifelse(month(Sys.Date())+2-12<10,0,""),
           ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12), ".xls", sep="")
componentsConsumption<-read_excel(mu)


componentsConsumption<- componentsConsumption [, c(2,4,match("��������� � �������", componentsConsumption[2, ]))]
colnames(componentsConsumption) <- c("code", "item", "quantity")
componentsConsumption <- componentsConsumption [3:nrow(componentsConsumption), ]
componentsConsumption [ ,3] <- round(as.numeric(componentsConsumption [ ,3]))
componentsConsumption [ ,1] <- round(as.numeric(componentsConsumption [ ,1]))

semiProducts2<-merge(semiProducts, componentsConsumption, by ="code", all.x= TRUE)
semiProducts2 [ ,6] <- semiProducts2 [ ,12] 
semiProducts2 <- semiProducts2[order(semiProducts2$id), ]

semiProducts [ ,6]<- semiProducts2 [ ,6]
semiProducts[is.na(semiProducts)]<-0



#code<-dcast(semiProducts, code ~ id, fun.aggregate=sum) #���������� ��� ������� ���� ������ � �������




# ������ ��������� ��� ����, ������� ��������� � ���� ���������� ����� 
# ��� ������� ���� ����� ����������� ���������� ����� � ���������� ����������� � �������������� ������,
# ������ �� �������� ������� ��������������� �����


for (i in 1:nrow(lastStageCode) ) { # ������ ����������� �� ���������� ����� ���������� �����
  #nrow (lastStageCode)
  lastStage<-max(grep(lastStageCode [i,1], semiProducts[ ,1])) # ������ ����������� � ���������� ����� ���� �������������
  firstStage<- min(grep(lastStageCode [i,1], semiProducts[ ,1])) # ������ ����������� � ������� ����� ���� �������������
  
  semiProducts [lastStage, 7]<- ifelse (semiProducts [lastStage, 5]- semiProducts [lastStage, 6]<0, abs(semiProducts [lastStage, 5]- semiProducts [lastStage, 6]), 0)
  
  
  for (k in 1:(lastStage-firstStage)) { # ��������� ������� ���������� �����, ������� ��������� � ���� ���������� �������������
    
    #     ## ��������� ���� 
    #     # ������������ ����� ���� ��� ����������� ��� ������ ��������������� ��������, �.�. �� ������������ merge
    # semiProducts [lastStage-k, 5] <- semiProducts [lastStage-k, 10]
    #   semiProducts<-merge(stockComponents, semiProducts, by ="code", all.x= TRUE, row.names = FALSE) # ������ ����������� � ����� �����
    #   
    #     ##
    
    
    semiProducts [lastStage-k, 6] <-  semiProducts [lastStage-(k-1), 7] # ���������� ����������� ����������� �����
    semiProducts [lastStage-k, 7] <-  ifelse(semiProducts [lastStage-k, 6] - semiProducts [lastStage-k, 5]>0,  semiProducts [lastStage-k, 6] - semiProducts [lastStage-k, 5], 0)
    # 
    
    # ����������� ����� ������� ����� ������� ��������� ������ �������������
    
    # ���������� ������ ��������� ���� � ����� ������ "semiproducts"
    line<- grep(semiProducts [lastStage-k,2], code[,1]) # ��������� ������ � ��������� ��������� � ������� "code"
    listCodeId<- which (code[ line, ]> 0) -1 # �������� ������ ��������
    listCodeId <- listCodeId [c(2:length(listCodeId))] # ���������� 1-� ��������
    
    #���������� ����� ������� � ��������� � ������ ������ ��������������
    semiProducts [listCodeId,10] <- semiProducts [lastStage-k, 5] -  semiProducts [lastStage-k, 6]
    #���������� �������������� ������� �� ������������ ������ � "�������" ������� "5" ��� ������� �� ��. �������
    semiProducts [ listCodeId,5] <-ifelse(semiProducts [ listCodeId,10]>0, semiProducts [ listCodeId,10], 0) #���������� ����� ������� (����� ������� ������ ��������������)
    
    #ifelse(semiProducts [ listCodeId,10]>0, semiProducts [ listCodeId,10], 0)
    
  }
  
}



# ����������� ��� ������� ���� ����������� ���������� ������ ��� �������
purchase<-aggregate(semiProducts$��������, by=list(semiProducts$code), FUN=sum)
colnames(purchase) <- c("code", "������")
purchase <- merge(purchase, param, by ="code", all.x= TRUE) #���������� � ����� ��������� ������������
purchase <- merge(purchase, stockComponents, by ="code", all.x= TRUE) #���������� �������
purchase <- merge(purchase, componentsConsumption, by ="code", all.x= TRUE) #���������� ����������� �������� ������

purchase <- purchase [ , c(1,3,10,12,2)]
colnames(purchase) [c(3,4)]<- c("�������", "�����������")
purchase[is.na(purchase)]<-0
write.csv(purchase, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/purchaseSemiProducts 3.csv")

