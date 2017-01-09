
# ЗАПУСКАЕТСЯ ЕЖЕДНЕВНО
# По филиалам:
# 1.  Стоимостная величина остатков с низкой оборачиваемостью (неликвидов)
# 2.  История изменения стоимости (неликвидов) - График
# 3.	Стоимость необеспеченных заказов 
# 4.	История выполнения плана продаж по позициям (план/факт/ % выполн / лог сервис)

# себестоимость нелеквидов МСК
colnames (finishedGoodsList) [1] <- "код"
surplusCostMSK <-  merge(surplusMSKFilter, finishedGoodsList, by ="код", all.x= TRUE)
surplusCostMSK$СтоимНелеквид <- surplusCostMSK [ ,3] * as.numeric(surplusCostMSK [ ,9])
surplusCostMSK <- surplusCostMSK [ , c(1:7, 10)]
surplusCostMSK[is.na(surplusCostMSK)]<-0
write.csv(surplusCostMSK, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/surplusCostMSK.csv")
MSKsurplus<-round(sum(surplusCostMSK$СтоимНелеквид), digits=0)


# себестоимость нелеквидов СПб
surplusCostSPb <-  merge(surplusSPbFilter, finishedGoodsList, by ="код", all.x= TRUE)
surplusCostSPb$СтоимНелеквид <- surplusCostSPb [ ,3] * as.numeric(surplusCostSPb [ ,9])
surplusCostSPb <- surplusCostSPb [ , c(1:7, 10)]
surplusCostSPb[is.na(surplusCostSPb)]<-0
write.csv(surplusCostSPb, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/surplusCostSPb.csv")
SPbsurplus<-round(sum(surplusCostSPb$СтоимНелеквид), digits =0)


# себестоимость нелеквидов НСК
surplusCostNSK <-  merge(surplusNSKFilter, finishedGoodsList, by ="код", all.x= TRUE)
surplusCostNSK$СтоимНелеквид <- surplusCostNSK [ ,3] * as.numeric(surplusCostNSK [ ,9])
surplusCostNSK <- surplusCostNSK [ , c(1:7, 10)]
surplusCostNSK[is.na(surplusCostNSK)]<-0
write.csv(surplusCostNSK, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/surplusCostNSK.csv")
NSKsurplus<-round(sum(surplusCostNSK$СтоимНелеквид), digits=0)


#сводная таблица по неликвидам для всех филиалов
surplusBranch <- data.frame(c(1:3), c(1:3))
branchList<- c("МСК", "СПб", "НСК")
surplusBranch [ ,1 ] <-branchList 
surplusBranch [ ,2 ] <- c(MSKsurplus, SPbsurplus, NSKsurplus)
colnames(surplusBranch)<- c("Филиал", "Стоим-ть нелеквидов, руб")
#surplusBranch [,2]<- formatC(surplusBranch [,2], format="d", big.mark=',') #форматирование разрядов

#surplusBranch <- t(surplusBranch)

write.csv(surplusBranch, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/surplusBranch.csv")

# создать новую папку
dir.create(paste("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/Архив/", Sys.Date(), sep = ""))


# # скопировать все полученные после расчетов файлы
flist <- list.files("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов", full.names = TRUE)
file.copy(flist, paste("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/Архив/", Sys.Date(), sep = ""), overwrite = TRUE)







### 
### 
### сводная динамика

#перечень архивных папок для составления графика динамики изменения неликвидных позиций
flist <- list.files("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/Архив/", full.names = TRUE)
flist<-as.data.frame(flist)

setwd("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/Архив/")



###
### i=108
###
# Формирование сводной таблицы по денежным остаткам в неликвидах на каждую дату
# скрипт пропускает даты, в которых исходный отчет не сформирован
surplusHistory<- data.frame(c("МСК",  "СПб", "НСК"))

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
  
  
  colnames (u) [3]<- print(str_sub(flist [i,1], start= -8)) # присваем дату в качестве названия колонки с количеством за эту дату
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



write.csv(surplusHistory, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/surplusHistory.csv", row.names=TRUE)




# расчет оборачиваемости для каждого филиала для вывода графической диаграммы типа gauge
# стоимость текущих остатков/ текущий план продаж в стоимостном выражении


# общая оборачиваемость для Москвы
mu<-paste ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/план продаж МСК-", month(Sys.Date()), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 
t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("код", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])
finishedGoodsList[,3]<-as.numeric(finishedGoodsList[,3])
MSKturn <- merge(finishedGoodsList, stockMSK, by ="код", all.x= TRUE)
MSKturn <- merge(MSKturn, t, by ="код", all.x= TRUE)
MSKturn[is.na(MSKturn)]<-0
MSKturn[is.null(MSKturn)]<-0
MSKturn <- round(sum(as.numeric(MSKturn[ , 3]) *  as.numeric(MSKturn[ , 4]))  /  sum(as.numeric(MSKturn[ , 3]) *  as.numeric(MSKturn[ , 6])), digits=2)



# общая оборачиваемость для СПб
mu<-paste ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/план продаж СПб-", month(Sys.Date()), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 
t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("код", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])

SPbturn <- merge(finishedGoodsList, stockSPb, by ="код", all.x= TRUE)
SPbturn <- merge(SPbturn, t, by ="код", all.x= TRUE)
SPbturn[is.na(SPbturn)]<-0
SPbturn[is.null(SPbturn)]<-0
SPbturn <- round(sum(as.numeric(SPbturn[ , 3]) *  as.numeric(SPbturn[ , 4]))  /  sum(as.numeric(SPbturn[ , 3]) *  as.numeric(SPbturn[ , 6])), digits=2)


# общая оборачиваемость для НСК
mu<-paste ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/план продаж НСК-", month(Sys.Date()), ".xls", sep="")
t<-read_excel(mu)
colnames(t)<- t [2,] 
t<- t [3:nrow(t), c(2,4,6)]
colnames(t)<- c("код", "name", "quantity")
t[, 1]<- as.numeric(t[ ,1])
t[, 3]<- as.numeric(t[ ,3])

NSKturn <- merge(finishedGoodsList, stockNSK, by ="код", all.x= TRUE)
NSKturn <- merge(NSKturn, t, by ="код", all.x= TRUE)
NSKturn[is.na(NSKturn)]<-0
NSKturn[is.null(NSKturn)]<-0
NSKturn <- round(sum(as.numeric(NSKturn[ , 3]) *  as.numeric(NSKturn[ , 4]))  /  sum(as.numeric(NSKturn[ , 3]) *  as.numeric(NSKturn[ , 6])), digits=2)


#сводная таблица по оборачиваемости
BranchTurn <- data.frame(c(1:3), c(1:3))
branchList<- c("МСК", "СПб", "Новосиб")
BranchTurn [ ,1 ] <-branchList 
BranchTurn [ ,2 ] <- c(MSKturn, SPbturn, NSKturn)
colnames(BranchTurn)<- c("Филиал", "Оборачиваемость")

write.csv(BranchTurn, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/BranchTurn.csv", row.names=FALSE, quote=FALSE)






# скопировать все полученные после расчетов файлы
flist <- list.files("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов", full.names = TRUE)
file.copy(flist, paste("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/Архив/", Sys.Date(), sep = ""))
file.copy(flist, "Z:/Смирнов/Отчеты для R", overwrite = TRUE)
file.copy("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/reportTotal.csv", "Z:/Смирнов/Отчеты для R", overwrite = TRUE)

