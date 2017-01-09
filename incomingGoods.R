#расчет обеспеченности товаром, т.е. сумма остатков на 1-е число + общий приход на текущую дату 
# для СПб
incomingGoodsSPb<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/приход товара СПб.xls", col_names=TRUE, skip=1)

code2<-regexpr(' ',incomingGoodsSPb[,1])
incomingGoodsSPb$Код<-substr(incomingGoodsSPb[,1], 1, code2-1)

incomingGoodsSPb<-merge(incomingGoodsSPb, finishedGoodsList, by ="Код", all.x= TRUE)

incomingGoodsSPb[incomingGoodsSPb==" "]<- "0"
incomingGoodsSPb$ОбеспечПлана<-as.numeric(incomingGoodsSPb[,3])+as.numeric(incomingGoodsSPb[,4])


soldplanSPb2<- merge(soldplanSPb, incomingGoodsSPb, by ="Код", all.x= TRUE)[,-c(10:16)]

soldplanSPb2<-soldplanSPb2[order(-soldplanSPb2$выручка),] 
soldplanSPb2<- soldplanSPb2[,c(1:4, 10, 9 ,8,5:7)]


write.csv(soldplanSPb2, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/soldplanSPb.csv")








# расчет обеспеченности товаром, т.е. сумма остатков на 1-е число + общий приход на текущую дату 
# для МСК
# требуется контроль 2-х складов "Москва продажи (регионы)", "Москва склад продажи"
## ИСПРАВЛЕНО учитывается только 1 склад "Москва продажи (регионы)", хотя код оставлен для расчета по обоим складам
incomingGoodsMSK<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/приход товара МСК.xls", col_names=TRUE, skip=1)

code2<-regexpr(' ',incomingGoodsMSK[,1])
incomingGoodsMSK$Код<-substr(incomingGoodsMSK[,1], 1, code2-1)

incomingGoodsMSK<-merge(incomingGoodsMSK, finishedGoodsList, by ="Код", all.x= TRUE)

incomingGoodsMSK[incomingGoodsMSK==" "]<- "0"
incomingGoodsMSK[is.na(incomingGoodsMSK)]<-0



incomingGoodsMSK$ОбеспечПлана<-as.numeric(incomingGoodsMSK[,3]) #на складе "Москва продажи" учитывается только начальный остаток
#приход учитывается только для склада "Москва продажи (регионы)", т.к. через этот склад транзитом товаро поступает на "Москва продажи"

# colnames(incomingGoodsMSK)<- c("Код", 2:8, "ОбеспечПлана")
# incomingGoodsMSK<- incomingGoodsMSK[2:516,c(1,9)]

incomingGoodsMSKcodesum<-aggregate(incomingGoodsMSK$ОбеспечПлана, by=list(incomingGoodsMSK$Код), FUN=sum)
colnames(incomingGoodsMSKcodesum)<-c("Код", "ОбеспечПлана")

soldplanMSK2<- merge(soldplanMSK, incomingGoodsMSKcodesum, by ="Код", all.x= TRUE)
soldplanMSK2[is.na(soldplanMSK2)]<-0






incomingGoodsMSK<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/приход товара МСК регионы.xls", col_names=TRUE, skip=1)

code2<-regexpr(' ',incomingGoodsMSK[,1])
incomingGoodsMSK$Код<-substr(incomingGoodsMSK[,1], 1, code2-1)

incomingGoodsMSK<-merge(incomingGoodsMSK, finishedGoodsList, by ="Код", all.x= TRUE)

incomingGoodsMSK[incomingGoodsMSK==" "]<- "0"
incomingGoodsMSK[is.na(incomingGoodsMSK)]<-0



incomingGoodsMSK$ОбеспечПлана<-as.numeric(incomingGoodsMSK[,3])+as.numeric(incomingGoodsMSK[,4])
# colnames(incomingGoodsMSK)<- c("Код", 2:8, "ОбеспечПлана")
# incomingGoodsMSK<- incomingGoodsMSK[2:516,c(1,9)]

incomingGoodsMSKcodesum<-aggregate(incomingGoodsMSK$ОбеспечПлана, by=list(incomingGoodsMSK$Код), FUN=sum)
colnames(incomingGoodsMSKcodesum)<-c("Код", "ОбеспечПлана")

soldplanMSK2<- merge(soldplanMSK2, incomingGoodsMSKcodesum, by ="Код", all.x= TRUE)
soldplanMSK2[is.na(soldplanMSK2)]<-0

soldplanMSK2[,10]<-soldplanMSK2[,10]+soldplanMSK2[,11] 
soldplanMSK2<- soldplanMSK2[,c(1:10)]
colnames(soldplanMSK2)[10]<-c("ОбеспечПлана")

soldplanMSK2<-soldplanMSK2[order(-soldplanMSK2$выручка),] 
soldplanMSK2<- soldplanMSK2[,c(1:4, 10, 9 ,8,5:7)]

write.csv(soldplanMSK2, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/soldplanMSK.csv")






# расчет обеспеченности товаром, т.е. сумма остатков на 1-е число + общий приход на текущую дату 
# для НСК
incomingGoodsNSK<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/приход товара НСК.xls", col_names=TRUE, skip=1)

code2<-regexpr(' ',incomingGoodsNSK[,1])
incomingGoodsNSK$Код<-substr(incomingGoodsNSK[,1], 1, code2-1)

incomingGoodsNSK<-merge(incomingGoodsNSK, finishedGoodsList, by ="Код", all.x= TRUE)

incomingGoodsNSK[incomingGoodsNSK==" "]<- "0"
incomingGoodsNSK[is.na(incomingGoodsNSK)]<-0



incomingGoodsNSK$ОбеспечПлана<-as.numeric(incomingGoodsNSK[,3])+as.numeric(incomingGoodsNSK[,4])
#colnames(incomingGoodsNSK)<- c("Код", 2:8, "ОбеспечПлана")
# incomingGoodsMSK<- incomingGoodsMSK[2:516,c(1,9)]

incomingGoodsNSKcodesum<-aggregate(incomingGoodsNSK$ОбеспечПлана, by=list(incomingGoodsNSK$Код), FUN=sum)
colnames(incomingGoodsNSKcodesum)<-c("Код", "ОбеспечПлана")

soldplanNSK2<- merge(soldplanNSK, incomingGoodsNSKcodesum, by ="Код", all.x= TRUE)

soldplanNSK2<-soldplanNSK2[order(-soldplanNSK2$выручка),] 
soldplanNSK2<- soldplanNSK2[,c(1:4, 10, 9 ,8,5:7)]
write.csv(soldplanNSK2, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/soldplanNSK.csv")



