#������ �������������� �������, �.�. ����� �������� �� 1-� ����� + ����� ������ �� ������� ���� 
# ��� ���
incomingGoodsSPb<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/������ ������ ���.xls", col_names=TRUE, skip=1)

code2<-regexpr(' ',incomingGoodsSPb[,1])
incomingGoodsSPb$���<-substr(incomingGoodsSPb[,1], 1, code2-1)

incomingGoodsSPb<-merge(incomingGoodsSPb, finishedGoodsList, by ="���", all.x= TRUE)

incomingGoodsSPb[incomingGoodsSPb==" "]<- "0"
incomingGoodsSPb$������������<-as.numeric(incomingGoodsSPb[,3])+as.numeric(incomingGoodsSPb[,4])


soldplanSPb2<- merge(soldplanSPb, incomingGoodsSPb, by ="���", all.x= TRUE)[,-c(10:16)]

soldplanSPb2<-soldplanSPb2[order(-soldplanSPb2$�������),] 
soldplanSPb2<- soldplanSPb2[,c(1:4, 10, 9 ,8,5:7)]


write.csv(soldplanSPb2, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/soldplanSPb.csv")








# ������ �������������� �������, �.�. ����� �������� �� 1-� ����� + ����� ������ �� ������� ���� 
# ��� ���
# ��������� �������� 2-� ������� "������ ������� (�������)", "������ ����� �������"
## ���������� ����������� ������ 1 ����� "������ ������� (�������)", ���� ��� �������� ��� ������� �� ����� �������
incomingGoodsMSK<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/������ ������ ���.xls", col_names=TRUE, skip=1)

code2<-regexpr(' ',incomingGoodsMSK[,1])
incomingGoodsMSK$���<-substr(incomingGoodsMSK[,1], 1, code2-1)

incomingGoodsMSK<-merge(incomingGoodsMSK, finishedGoodsList, by ="���", all.x= TRUE)

incomingGoodsMSK[incomingGoodsMSK==" "]<- "0"
incomingGoodsMSK[is.na(incomingGoodsMSK)]<-0



incomingGoodsMSK$������������<-as.numeric(incomingGoodsMSK[,3]) #�� ������ "������ �������" ����������� ������ ��������� �������
#������ ����������� ������ ��� ������ "������ ������� (�������)", �.�. ����� ���� ����� ��������� ������ ��������� �� "������ �������"

# colnames(incomingGoodsMSK)<- c("���", 2:8, "������������")
# incomingGoodsMSK<- incomingGoodsMSK[2:516,c(1,9)]

incomingGoodsMSKcodesum<-aggregate(incomingGoodsMSK$������������, by=list(incomingGoodsMSK$���), FUN=sum)
colnames(incomingGoodsMSKcodesum)<-c("���", "������������")

soldplanMSK2<- merge(soldplanMSK, incomingGoodsMSKcodesum, by ="���", all.x= TRUE)
soldplanMSK2[is.na(soldplanMSK2)]<-0






incomingGoodsMSK<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/������ ������ ��� �������.xls", col_names=TRUE, skip=1)

code2<-regexpr(' ',incomingGoodsMSK[,1])
incomingGoodsMSK$���<-substr(incomingGoodsMSK[,1], 1, code2-1)

incomingGoodsMSK<-merge(incomingGoodsMSK, finishedGoodsList, by ="���", all.x= TRUE)

incomingGoodsMSK[incomingGoodsMSK==" "]<- "0"
incomingGoodsMSK[is.na(incomingGoodsMSK)]<-0



incomingGoodsMSK$������������<-as.numeric(incomingGoodsMSK[,3])+as.numeric(incomingGoodsMSK[,4])
# colnames(incomingGoodsMSK)<- c("���", 2:8, "������������")
# incomingGoodsMSK<- incomingGoodsMSK[2:516,c(1,9)]

incomingGoodsMSKcodesum<-aggregate(incomingGoodsMSK$������������, by=list(incomingGoodsMSK$���), FUN=sum)
colnames(incomingGoodsMSKcodesum)<-c("���", "������������")

soldplanMSK2<- merge(soldplanMSK2, incomingGoodsMSKcodesum, by ="���", all.x= TRUE)
soldplanMSK2[is.na(soldplanMSK2)]<-0

soldplanMSK2[,10]<-soldplanMSK2[,10]+soldplanMSK2[,11] 
soldplanMSK2<- soldplanMSK2[,c(1:10)]
colnames(soldplanMSK2)[10]<-c("������������")

soldplanMSK2<-soldplanMSK2[order(-soldplanMSK2$�������),] 
soldplanMSK2<- soldplanMSK2[,c(1:4, 10, 9 ,8,5:7)]

write.csv(soldplanMSK2, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/soldplanMSK.csv")






# ������ �������������� �������, �.�. ����� �������� �� 1-� ����� + ����� ������ �� ������� ���� 
# ��� ���
incomingGoodsNSK<-read_excel("C:/Documents and Settings/smirnov/��� ���������/������/R ����/������ ��� ��������/������ ������ ���.xls", col_names=TRUE, skip=1)

code2<-regexpr(' ',incomingGoodsNSK[,1])
incomingGoodsNSK$���<-substr(incomingGoodsNSK[,1], 1, code2-1)

incomingGoodsNSK<-merge(incomingGoodsNSK, finishedGoodsList, by ="���", all.x= TRUE)

incomingGoodsNSK[incomingGoodsNSK==" "]<- "0"
incomingGoodsNSK[is.na(incomingGoodsNSK)]<-0



incomingGoodsNSK$������������<-as.numeric(incomingGoodsNSK[,3])+as.numeric(incomingGoodsNSK[,4])
#colnames(incomingGoodsNSK)<- c("���", 2:8, "������������")
# incomingGoodsMSK<- incomingGoodsMSK[2:516,c(1,9)]

incomingGoodsNSKcodesum<-aggregate(incomingGoodsNSK$������������, by=list(incomingGoodsNSK$���), FUN=sum)
colnames(incomingGoodsNSKcodesum)<-c("���", "������������")

soldplanNSK2<- merge(soldplanNSK, incomingGoodsNSKcodesum, by ="���", all.x= TRUE)

soldplanNSK2<-soldplanNSK2[order(-soldplanNSK2$�������),] 
soldplanNSK2<- soldplanNSK2[,c(1:4, 10, 9 ,8,5:7)]
write.csv(soldplanNSK2, "C:/Documents and Settings/smirnov/��� ���������/������/R ����/���������� ������� ��� ��������/soldplanNSK.csv")



