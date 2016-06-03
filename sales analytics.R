# Выполнение плана продаж для Москвы
# на текущий месяц
soldMSK<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/факт продажи МСК.xls", skip =2)
colnames(soldMSK) <- soldMSK [1, ]
salesPlanMSK<-read_excel(paste ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/план продаж МСК-", month(Sys.Date())-1, ".xls", sep=""))
colnames(salesPlanMSK) <- salesPlanMSK [2, ]

soldplanMSK<-merge(salesPlanMSK, soldMSK, by ="Код", all.x= TRUE)
soldplanMSK<- soldplanMSK [ ,c(1,4,6, 12, 13, 14)]
colnames(finishedGoodsList)<- c("Код", "наименование", "себестоимость")

colnames(soldplanMSK)<- c("Код", "наименование", "план продаж", "факт продаж", "выручка", "маржа")
soldplanMSK [ ,1]<- as.numeric(soldplanMSK[ , 1])


soldplanMSK<-merge(soldplanMSK, finishedGoodsList, by ="Код", all.x= TRUE)
soldplanMSK<- soldplanMSK [ , c(1:6,8)]

soldplanMSK[is.na(soldplanMSK)]<-0
soldplanMSK <-soldplanMSK [ c(1:(nrow(salesPlanMSK)-2)), ]


soldplanMSK[soldplanMSK==" "]<- "0"

#преобразовываем в числовой формат
for (i in 3:7 ) {
  soldplanMSK[ ,i] <- as.numeric(soldplanMSK[ ,i])
}

#факт продаж по себестоимости / план продаж по себестоимости
MSKsalesprogress<-round(sum(as.numeric(soldplanMSK[ , 4]) *  soldplanMSK [,7])  /  sum(as.numeric(soldplanMSK[ , 3]) *  soldplanMSK [,7]), digits=2)

MSKrealSales<- sum(as.numeric(soldplanMSK[ , 4]) *  soldplanMSK [,7])
MSKplanSales<- sum(as.numeric(soldplanMSK[ , 3]) *  soldplanMSK [,7])

soldplanMSK$выполнение <-  round(soldplanMSK [ ,4]/soldplanMSK [ ,3], digits=2)*100
soldplanMSK<- soldplanMSK [order(-soldplanMSK$выручка),] 
row.names(soldplanMSK) <- NULL

colnames (soldplanMSK) [8] <- "%, выполнения"

write.csv(soldplanMSK, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/soldplanMSK.csv")








# Выполнение плана продаж для Санкт-Петербурга

soldSPb<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/факт продажи СПб.xls", skip =2)
colnames(soldSPb) <- soldSPb [1, ]
salesPlanSPb<-read_excel(paste ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/план продаж СПб-", month(Sys.Date())-1, ".xls", sep=""))
colnames(salesPlanSPb) <- salesPlanSPb [2, ]


soldplanSPb<-merge(salesPlanSPb, soldSPb, by ="Код", all.x= TRUE)
soldplanSPb<- soldplanSPb [ ,c(1,4,6, 12, 13, 14)]
colnames(finishedGoodsList)<- c("Код", "наименование", "себестоимость")

colnames(soldplanSPb)<- c("Код", "наименование", "план продаж", "факт продаж", "выручка", "маржа")
soldplanSPb [ ,1]<- as.numeric(soldplanSPb[ , 1])


soldplanSPb<-merge(soldplanSPb, finishedGoodsList, by ="Код", all.x= TRUE)
soldplanSPb<- soldplanSPb [ , c(1:6,8)]

soldplanSPb[is.na(soldplanSPb)]<-0
soldplanSPb <-soldplanSPb [ c(1:(nrow(salesPlanSPb)-2)), ]


soldplanSPb[soldplanSPb==" "]<- "0"

#преобразовываем в числовой формат
for (i in 3:7 ) {
  soldplanSPb[ ,i] <- as.numeric(soldplanSPb[ ,i])
}

#факт продаж по себестоимости / план продаж по себестоимости
SPbsalesprogress<- round(sum(as.numeric(soldplanSPb[ , 4]) *  soldplanSPb [,7])  /  sum(as.numeric(soldplanSPb[ , 3]) *  soldplanSPb [,7]), digits=2)

SPbrealSales<- sum(as.numeric(soldplanSPb[ , 4]) *  soldplanSPb [,7])
SPbplanSales<- sum(as.numeric(soldplanSPb[ , 3]) *  soldplanSPb [,7])

soldplanSPb$выполнение <-  round(soldplanSPb [ ,4]/soldplanSPb [ ,3], digits=2)*100
soldplanSPb<- soldplanSPb [order(-soldplanSPb$выручка),] 
row.names(soldplanSPb) <- NULL

colnames (soldplanSPb) [8] <- "%, выполнения"

write.csv(soldplanSPb, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/soldplanSPb.csv")










# выполнение плана продаж Новосибирском

soldNSK<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/факт продажи НСК.xls", skip =2)
colnames(soldNSK) <- soldNSK [1, ]
salesPlanNSK<-read_excel(paste ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/план продаж НСК-", month(Sys.Date())-1, ".xls", sep=""))
colnames(salesPlanNSK) <- salesPlanNSK [2, ]

soldplanNSK<-merge(salesPlanNSK, soldNSK, by ="Код", all.x= TRUE)
soldplanNSK<- soldplanNSK [ ,c(1,4,6, 12, 13, 14)]
colnames(finishedGoodsList)<- c("Код", "наименование", "себестоимость")

colnames(soldplanNSK)<- c("Код", "наименование", "план продаж", "факт продаж", "выручка", "маржа")
soldplanNSK [ ,1]<- as.numeric(soldplanNSK[ , 1])


soldplanNSK<-merge(soldplanNSK, finishedGoodsList, by ="Код", all.x= TRUE)
soldplanNSK<- soldplanNSK [ , c(1:6,8)]

soldplanNSK[is.na(soldplanNSK)]<-0
soldplanNSK <-soldplanNSK [ c(1:(nrow(salesPlanNSK)-2)), ]


soldplanNSK[soldplanNSK==" "]<- "0"

#преобразовываем в числовой формат
for (i in 3:7 ) {
  soldplanNSK[ ,i] <- as.numeric(soldplanNSK[ ,i])
}

#факт продаж по себестоимости / план продаж по себестоимости
NSKsalesprogress<- round(sum(as.numeric(soldplanNSK[ , 4]) *  soldplanNSK [,7])  /  sum(as.numeric(soldplanNSK[ , 3]) *  soldplanNSK [,7]), digits=2)

NSKrealSales<- sum(as.numeric(soldplanNSK[ , 4]) *  soldplanNSK [,7])
NSKplanSales<- sum(as.numeric(soldplanNSK[ , 3]) *  soldplanNSK [,7])

soldplanNSK$выполнение <-  round(soldplanNSK [ ,4]/soldplanNSK [ ,3], digits=2)*100
soldplanNSK<- soldplanNSK [order(-soldplanNSK$выручка),] 

row.names(soldplanNSK) <- NULL

colnames (soldplanNSK) [8] <- "%, выполнения"

write.csv(soldplanNSK, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/soldplanNSK.csv")


# формируем таблицу с данными план/факт и общий % выполнения для каждого филиала для передачи на сервер
salesAnalysis <- data.frame(
                            c(sum((as.numeric(soldplanMSK[ , 4]) *  soldplanMSK [,7])), 
                              sum(as.numeric(soldplanSPb[ , 4]) *  soldplanSPb [,7]),
                              sum(as.numeric(soldplanNSK[ , 4]) *  soldplanNSK [,7])),
                            
                            c(sum(as.numeric(soldplanMSK[ , 3]) *  soldplanMSK [,7]),
                              sum(as.numeric(soldplanSPb[ , 3]) *  soldplanSPb [,7]),
                              sum(as.numeric(soldplanNSK[ , 3]) *  soldplanNSK [,7])))

salesAnalysis <- as.data.frame(t(salesAnalysis))
colnames(salesAnalysis) <- c("Москва", "Санкт-Петербург", "Новосибирск")
row.names(salesAnalysis) <- c("факт", "план")
salesAnalysis <- rbind(salesAnalysis, round(salesAnalysis[1,]/salesAnalysis[2,]*100, digits=1) )
write.csv(salesAnalysis, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/salesAnalysis.csv")








###

#анализ плана/факта продаж 
# Общий план/факт продаж “по марже”, руб
# План/факт продаж по продукции ЭЛТИС, шт

#План по марже МСК, СПб 2,1 млн руб НСК 1,8 млн руб

###
###
# Формирование сводной таблицы по денежным остаткам в неликвидах на каждую дату
# скрипт пропускает даты, в которых исходный отчет не сформирован

#очистить файл с остатками 
flist <- list.files("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/Остатки", full.names = TRUE)
file.remove(flist)

#отобрать из общего списка архивных папок те, что относятся к текущему месяцу
flist <- list.files("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/Архив/", full.names = TRUE)
flist<-as.data.frame(flist)

flist$month <- as.numeric(str_sub(flist[,1], start= -5, end=-4))
colnames(flist) <- c("path", "month")
flistCurrentMonth<- subset(flist, flist[ ,2]== month(Sys.Date()))


# сохраняем файлы с остатками для тех дат, когда они были софрмированы, в отдельную папку для последующей обработки
for (i in 1:nrow(flistCurrentMonth)) {
  #ERROR HANDLING
  possibleError <- tryCatch(
    
    u<-read_excel(paste(flistCurrentMonth[i,1], "/", "все остатки", ".xls",  sep="")),
    error=function(e) e
    
    
  )
  
  if(inherits(possibleError, "error")) next
  #print(i) 
  #print(str_sub(flist [i,1], start= -8))
  
  
  #REAL WORK
  #surplusHistory <- read.csv(paste(flist[i,1], "/", "surplusBranch",  ".csv",  sep=""))
  stockDate<- as.numeric(str_sub(paste(flistCurrentMonth[i,1], "/", "все остатки", ".xls",  sep=""), start= -18, end=-17))
  
  write.csv(u, paste("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/Остатки/", "все остатки", " ", 
                     stockDate, ".csv", sep="" ))
  
  ;
  
}  #end for













#добавить показатель уровня логистического сервиса (% количества дней в месяце, когда "физический" остаток товара на складе в шт 
# превышал страховой запас)

#логистический сервис для Москвы

# цикл от 1 до 22 (количество рабочих дней) по загрузке  и добавлению остатков к таблице

##
##загружаем данные по остаткам на всех складах
##
##
##загружаем данные по остаткам на всех складах
##
#allStock<-read.csv(paste ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/все остатки ", Sys.Date(), ".csv", sep=""), sep= ";", skip = "1")



# получаем количество дней, в которых регистрировались остатки на складах
flist <- list.files("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/Остатки", full.names = TRUE)
#
flist<-as.data.frame(flist)

#даты отчетов
flist$date <- as.numeric(str_sub(flist[,1], start= -6, end=-4))
datesNumber<- flist$date 
datesNumber <- datesNumber[!is.na(datesNumber)] #убираем NA
datesNumber<-sort(datesNumber)

for (i in datesNumber ) {
  
  
  # i<-5
  allStock<-read.csv(paste("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/Остатки/все остатки ", i, ".csv", sep=""), dec = ".", stringsAsFactors=FALSE)
  
  #заменить NA на ноль
  allStock[is.na(allStock)]<-0
  
  header<-allStock[1,]

names(allStock)<-header
  
  #обработать содержимое для получения цифр
#   write.csv(allStock, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/all stock.сsv")
#   
#   sat<-read.csv("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/all stock.сsv", 
#                 , na.strings=c(" ", NA), stringsAsFactors=FALSE) 
#   
#   write.csv(sat, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/all stock.сsv" )
#   
#   sat2<-read.csv("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/all stock.сsv") 
  
  
  #добавить заголовки стобцов
#   names(sat2)<-c(0,0,header)
  
  
  warehouseStockMoscow<-match("Москва продажи (регионы)", colnames(allStock))
  warehouseStockMoscowBranch<-match("Москва, Склад продажи", colnames(allStock))
  warehouseStockMoscowTransit<-match("Москва в Пути (филиал)", colnames(allStock))
  
  stockComponentsMoscow<-allStock[,c(3,5,warehouseStockMoscow)]
  #names(stockComponentsMoscow)<-c("code", "item")
  
allStock[is.na(allStock)]<-0
allStock[allStock==" "]<- 0

  
  
  
  
  
  #вычесть из остатков резервы и добавить 
  stockComponentsMoscow[ ,3]<- as.numeric(allStock[  ,warehouseStockMoscow])+as.numeric(allStock[ ,warehouseStockMoscowBranch]) + as.numeric(allStock[ ,warehouseStockMoscowTransit]) 
  
  #присвоили номер недели текущим остаткам
  names(stockComponentsMoscow)[3]<-as.character(i)
  
  stockComponentsMoscow [,1]<- as.numeric(stockComponentsMoscow [,1])

  soldplanMSK<-merge(soldplanMSK, stockComponentsMoscow, by ="Код", all.x= TRUE)
  soldplanMSK <- soldplanMSK [ , c(1:(ncol(soldplanMSK)-2),ncol(soldplanMSK))]
}


#сортировка по убыванию выручки
soldplanMSK<- soldplanMSK [order(-soldplanMSK$выручка),] 


for (i in 1:nrow(soldplanMSK) ) {
  # количество дней, когда остаток превышает страховой запас, т.е. 25% от плана продаж
  vec <- soldplanMSK [i, 9:(ncol(soldplanMSK)-1)] > soldplanMSK [i,3]*0.25
  #расчет процента дней, когда остаток превышал страховой запас от общего количества дней
  soldplanMSK$ЛогСервисПроц [i] <- round(length(vec[vec==TRUE]) /length(9:(ncol(soldplanMSK)-1))*100)
  
}


row.names(soldplanMSK) <- NULL

MSKstockArch <- soldplanMSK [ , c(1,2,3, 10:ncol(soldplanMSK)-1)]
write.csv(MSKstockArch, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/MSKstockArch.csv")


soldplanMSK <- soldplanMSK [ , c(1:8, ncol(soldplanMSK))] 

write.csv(soldplanMSK, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/soldplanMSK.csv")
















#Санкт-Петербург

# получаем количество дней, в которых регистрировались остатки на складах
flist <- list.files("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/Остатки", full.names = TRUE)
#
flist<-as.data.frame(flist)

#даты отчетов
flist$date <- as.numeric(str_sub(flist[,1], start= -6, end=-4))
datesNumber<- flist$date 
datesNumber <- datesNumber[!is.na(datesNumber)] #убираем NA
datesNumber<-sort(datesNumber)

##
##загружаем данные по остаткам на всех складах
##


for (i in datesNumber ) {
  

  allStock<-read.csv(paste("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/Остатки/все остатки ", i, ".csv", sep=""), dec = ".", stringsAsFactors=FALSE)
  
  #заменить NA на ноль
  allStock[is.na(allStock)]<-0
  
  header<-allStock[1,]
  
  names(allStock)<-header
  
  
  warehouseStockSPb<-match("Санкт-Петербург,Склад продажи", colnames(allStock))
  
  stockComponentsSPb<-allStock[,c(3,5,warehouseStockSPb)]
  #names(stockComponentsMoscow)<-c("code", "item")
  
  allStock[is.na(allStock)]<-0
  allStock[allStock==" "]<- 0
  
  
  
  
  
  #вычесть из остатков резервы и добавить 
  stockComponentsSPb[ ,3]<- as.numeric(allStock[ ,warehouseStockSPb])
  
   
  #присвоили номер недели текущим остаткам
  names(stockComponentsSPb)[3]<-as.character(i)
  
  stockComponentsSPb [,1]<- as.numeric(stockComponentsSPb [,1])
  soldplanSPb<-merge(soldplanSPb, stockComponentsSPb, by ="Код", all.x= TRUE)
  soldplanSPb <- soldplanSPb [ , c(1:(ncol(soldplanSPb)-2),ncol(soldplanSPb))]
}


#сортировка по убыванию выручки
soldplanSPb<- soldplanSPb [order(-soldplanSPb$выручка),] 


for (i in 1:nrow(soldplanSPb) ) {
  # количество дней, когда остаток превышает страховой запас, т.е. 25% от плана продаж
  vec <- soldplanSPb [i, 9:(ncol(soldplanSPb)-1)] > soldplanSPb [i,3]*0.25
  #расчет процента дней, когда остаток превышал страховой запас от общего количества дней
  soldplanSPb$ЛогСервисПроц [i] <- round(length(vec[vec==TRUE]) /length(9:(ncol(soldplanSPb)-1))*100)
  
}

row.names(soldplanSPb) <- NULL

SPbstockArch <- soldplanSPb [ , c(1,2,3, 10:ncol(soldplanSPb)-1)]
write.csv(SPbstockArch, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/SPbstockArch.csv")


soldplanSPb <- soldplanSPb [ , c(1:8, ncol(soldplanSPb))] 

write.csv(soldplanSPb, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/soldplanSPb.csv")















#Новосибирск

# получаем количество дней, в которых регистрировались остатки на складах


##
##загружаем данные по остаткам на всех складах
##


for (i in datesNumber ) {
  
  
  
  
  allStock<-read.csv(paste("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/Остатки/все остатки ", i, ".csv", sep=""), dec = ".", stringsAsFactors=FALSE)
  
  #заменить NA на ноль
  allStock[is.na(allStock)]<-0
  
  header<-allStock[1,]
  
  names(allStock)<-header
  
  
  
  warehouseStockNSK<- match("Новосибирск, Склад продажи", colnames(allStock))
  warehouseStockNSKBranch<- match("Склад Транзитный  для НВС", colnames(allStock))
  warehouseStockNSKTransit<- match("ТРАНЗИТНЫЙ СКЛАД", colnames(allStock))
  
  stockComponentsNSK<-allStock[,c(3,5,warehouseStockNSK)]
  #names(stockComponentsNSK)<-c("code", "item")
  
  allStock[is.na(allStock)]<-0
  allStock[allStock==" "]<- 0
  
  
  
  
  
  
  #вычесть из остатков резервы и добавить 
  stockComponentsNSK [ ,3]<- as.numeric(allStock[ ,warehouseStockNSK])+as.numeric(allStock[ ,warehouseStockNSKBranch]) + as.numeric(allStock[ ,warehouseStockNSKTransit]) 
  
  #присвоили номер недели текущим остаткам
  names(stockComponentsNSK)[3]<-as.character(i)
  stockComponentsNSK [,1]<- as.numeric(stockComponentsNSK [,1])
  
  soldplanNSK<-merge(soldplanNSK, stockComponentsNSK, by ="Код", all.x= TRUE)
  soldplanNSK <- soldplanNSK [ , c(1:(ncol(soldplanNSK)-2),ncol(soldplanNSK))]
}


#сортировка по убыванию выручки
soldplanNSK<- soldplanNSK [order(-soldplanNSK$выручка),] 


for (i in 1:nrow(soldplanNSK) ) {
  # количество дней, когда остаток превышает страховой запас, т.е. 25% от плана продаж
  vec <- soldplanNSK [i, 9:(ncol(soldplanNSK)-1)] > soldplanNSK [i,3]*0.25
  #расчет процента дней, когда остаток превышал страховой запас от общего количества дней
  soldplanNSK$ЛогСервисПроц [i] <- round(length(vec[vec==TRUE]) /length(9:(ncol(soldplanNSK)-1))*100)
  
}

row.names(soldplanNSK) <- NULL

NSKstockArch <- soldplanNSK [ , c(1,2,3, 10:ncol(soldplanNSK)-1)]
vecname <- rep("д", each=ncol(NSKstockArch))
colnames(NSKstockArch) <- vecname
names<- c("Код", "наименование", "план продаж", c (1:(ncol(NSKstockArch)-3)) )
NSKstockArch <- rbind(names, NSKstockArch)
write.csv(NSKstockArch, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/NSKstockArch.csv")
#write.table( NSKstockArch, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/NSKstockArch.csv", sep=",", col.names=FALSE)

soldplanNSK <- soldplanNSK [ , c(1:8, ncol(soldplanNSK))] 

write.csv(soldplanNSK, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/soldplanNSK.csv")



# # конец расчета логистического сервиса для каждого филиала за период







# # скопировать все полученные после расчетов файлы
flist <- list.files("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов", full.names = TRUE)
file.copy(flist, paste("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/Архив/", Sys.Date(), sep = ""), overwrite = TRUE)





