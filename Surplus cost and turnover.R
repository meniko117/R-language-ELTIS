
# ����������� ���������
# �� ��������:
# 1.  ����������� �������� �������� � ������ ���������������� (����������)
# 2.  ������� ��������� ��������� (����������) - ������
# 3.	��������� �������������� ������� 
# 4.	������� ���������� ����� ������ �� �������� (����/����/ % ������ / ��� ������)

# ������������� ���������� ���
colnames (finishedGoodsList) [1] <- "���"
surplusCostMSK <-  merge(surplusMSKFilter, finishedGoodsList, by ="���", all.x= TRUE)
surplusCostMSK$������������� <- surplusCostMSK [ ,3] * as.numeric(surplusCostMSK [ ,9])
surplusCostMSK <- surplusCostMSK [ , c(1:7, 10)]
surplusCostMSK[is.na(surplusCostMSK)]<-0
write.csv(surplusCostMSK, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/surplusCostMSK.csv")
MSKsurplus<-round(sum(surplusCostMSK$�������������), digits=0)


# ������������� ���������� ���
surplusCostSPb <-  merge(surplusSPbFilter, finishedGoodsList, by ="���", all.x= TRUE)
surplusCostSPb$������������� <- surplusCostSPb [ ,3] * as.numeric(surplusCostSPb [ ,9])
surplusCostSPb <- surplusCostSPb [ , c(1:7, 10)]
surplusCostSPb[is.na(surplusCostSPb)]<-0
write.csv(surplusCostSPb, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/surplusCostSPb.csv")
SPbsurplus<-round(sum(surplusCostSPb$�������������), digits =0)


# ������������� ���������� ���
surplusCostNSK <-  merge(surplusNSKFilter, finishedGoodsList, by ="���", all.x= TRUE)
surplusCostNSK$������������� <- surplusCostNSK [ ,3] * as.numeric(surplusCostNSK [ ,9])
surplusCostNSK <- surplusCostNSK [ , c(1:7, 10)]
surplusCostNSK[is.na(surplusCostNSK)]<-0
write.csv(surplusCostNSK, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/surplusCostNSK.csv")
NSKsurplus<-round(sum(surplusCostNSK$�������������), digits=0)


#������� ������� �� ���������� ��� ���� ��������
surplusBranch <- data.frame(c(1:3), c(1:3))
branchList<- c("���", "���", "���")
surplusBranch [ ,1 ] <-branchList 
surplusBranch [ ,2 ] <- c(MSKsurplus, SPbsurplus, NSKsurplus)
colnames(surplusBranch)<- c("������", "�����-�� ����������, ���")
#surplusBranch [,2]<- formatC(surplusBranch [,2], format="d", big.mark=',') #�������������� ��������

#surplusBranch <- t(surplusBranch)

write.csv(surplusBranch, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/surplusBranch.csv")

# ������� ����� �����
dir.create(paste("C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/�����/", Sys.Date(), sep = ""))


# # ����������� ��� ���������� ����� �������� �����
flist <- list.files("C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������", full.names = TRUE)
file.copy(flist, paste("C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/�����/", Sys.Date(), sep = ""), overwrite = TRUE)







### 
### 
### ������� ��������

#�������� �������� ����� ��� ����������� ������� �������� ��������� ����������� �������
flist <- list.files("C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/�����/", full.names = TRUE)
flist<-as.data.frame(flist)

setwd("C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/�����/")



###
### i=108
###
# ������������ ������� ������� �� �������� �������� � ���������� �� ������ ����
# ������ ���������� ����, � ������� �������� ����� �� �����������
surplusHistory<- data.frame(c("���",  "���", "���"))

for (i in 1:nrow(flist)) {
  #ERROR HANDLING
  possibleError <- tryCatch(
    
    u<-read.csv(paste(flist[i,1], "/", "surplusBranch",  ".csv",  sep="")),
    error=function(e) e
    
    
  )
  
  if(inherits(possibleError, "error")) next
  #print(i) 
  print(str_sub(flist [i,1], start= -8))
  
  
  #REAL WORK
  #surplusHistory <- read.csv(paste(flist[i,1], "/", "surplusBranch",  ".csv",  sep=""))
  
  
  colnames (u) [3]<- print(str_sub(flist [i,1], start= -8)) # �������� ���� � �������� �������� ������� � ����������� �� ��� ����
  r<-as.data.frame( u [ ,3])
  names(r) <- names (u) [3]
  surplusHistory<- cbind(surplusHistory, r);
  # surplusHistory <- surplusHistory [ , c(1:ncol(surplusHistory)-2,ncol(surplusHistory))]
}  #end for



surplusHistory <- t(surplusHistory)
colnames(surplusHistory) <- surplusHistory [1 ,]
surplusHistory <- surplusHistory [-1 ,]


surplusHistory <- as.matrix(surplusHistory)
mode(surplusHistory) <- "numeric"
surplusHistory <- as.data.frame(surplusHistory)



write.csv(surplusHistory, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/surplusHistory.csv", row.names=TRUE)




# ������ ��������������� ��� ������� ������� ��� ������ ����������� ��������� ���� gauge
# ��������� ������� ��������/ ������� ���� ������ � ����������� ���������


# ����� ��������������� ��� ������
mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", month(Sys.Date()), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 
t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("���", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])
finishedGoodsList[,3]<-as.numeric(finishedGoodsList[,3])
MSKturn <- merge(finishedGoodsList, stockMSK, by ="���", all.x= TRUE)
MSKturn <- merge(MSKturn, t, by ="���", all.x= TRUE)
MSKturn[is.na(MSKturn)]<-0
MSKturn[is.null(MSKturn)]<-0
MSKturn <- round(sum(as.numeric(MSKturn[ , 3]) *  as.numeric(MSKturn[ , 4]))  /  sum(as.numeric(MSKturn[ , 3]) *  as.numeric(MSKturn[ , 6])), digits=2)



# ����� ��������������� ��� ���
mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", month(Sys.Date()), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 
t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("���", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])

SPbturn <- merge(finishedGoodsList, stockSPb, by ="���", all.x= TRUE)
SPbturn <- merge(SPbturn, t, by ="���", all.x= TRUE)
SPbturn[is.na(SPbturn)]<-0
SPbturn[is.null(SPbturn)]<-0
SPbturn <- round(sum(as.numeric(SPbturn[ , 3]) *  as.numeric(SPbturn[ , 4]))  /  sum(as.numeric(SPbturn[ , 3]) *  as.numeric(SPbturn[ , 6])), digits=2)


# ����� ��������������� ��� ���
mu<-paste ("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/���� ������ ���-", month(Sys.Date()), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 
t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("���", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])

NSKturn <- merge(finishedGoodsList, stockNSK, by ="���", all.x= TRUE)
NSKturn <- merge(NSKturn, t, by ="���", all.x= TRUE)
NSKturn[is.na(NSKturn)]<-0
NSKturn[is.null(NSKturn)]<-0
NSKturn <- round(sum(as.numeric(NSKturn[ , 3]) *  as.numeric(NSKturn[ , 4]))  /  sum(as.numeric(NSKturn[ , 3]) *  as.numeric(NSKturn[ , 6])), digits=2)


#������� ������� �� ���������������
BranchTurn <- data.frame(c(1:3), c(1:3))
branchList<- c("���", "���", "�������")
BranchTurn [ ,1 ] <-branchList 
BranchTurn [ ,2 ] <- c(MSKturn, SPbturn, NSKturn)
colnames(BranchTurn)<- c("������", "���������������")

write.csv(BranchTurn, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/BranchTurn.csv", row.names=FALSE, quote=FALSE)






# ����������� ��� ���������� ����� �������� �����
flist <- list.files("C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������", full.names = TRUE)
file.copy(flist, paste("C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/�����/", Sys.Date(), sep = ""))
file.copy(flist, "Z:/�������/������ ��� R", overwrite = TRUE)
file.copy("C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/reportTotal.csv", "Z:/�������/������ ��� R", overwrite = TRUE)

