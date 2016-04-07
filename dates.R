#
#перенести получаемый "alltimeHeader" в "mergw tables"
#

# library (lubridate)
# library (stringr)
# 
# weekBegin<- data.frame(matrix(nrow=1, ncol=52))
# 
# 
# #номер дня недели (функция из lubridate) для текущего года
# # в строке ниже при смене года заменить на первое января наступающего года (в 2-х местах)
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
# #РЅРѕРјРµСЂ РјРµСЃСЏС†Р° РґР»СЏ РєР°Р¶РґРѕР№ РЅРµРґРµР»Рё
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
# # для приведения размерности в соответствие
# twocolumns<-as.data.frame(matrix(nrow=2,ncol=2))
# 
# alltimeHeader<- cbind(twocolumns, weekBegin3)
# 
# #номера месяцев рассчитанных по дате понедельника
# monthnumber<-as.data.frame(matrix(nrow=1,ncol=54))
# monthnumber<-colnames(alltimeHeader)
# alltimeHeader<- rbind(monthnumber, alltimeHeader)
# 
# colnames(alltimeHeader)<-c("code", "item.x", c(1:52))


##
## конец блока, который надо перенести в начало кода "merge tables" для распределения по неделям потребления, рассчитанного в 1С
##






threecolumns<-as.data.frame(matrix(nrow=3,ncol=2))
alltimeHeader<- cbind(threecolumns, alltimeHeader)
colnames(alltimeHeader)<-c("code", "item.x", colnames(alltimeHeader)[3:ncol(alltimeHeader)])






#
# весь график производства с указанием (номера недели, номера месяца, даты понедельника, количества недель в месяце)
#
totalProductiontime<- rbind(alltimeHeader, totalProduction)


# формируем пустую таблицу для потребности по месяцам 
monthlyProductionPlan<-as.data.frame(matrix(nrow=nrow(finishedGoodsList),ncol=12))
monthlyProductionPlan[is.na(monthlyProductionPlan)]<-0


#проходим по всей таблице понедельных потребностей и складываем потребности, относящиеся к одному месяцу

for (m in 4:nrow(totalProductiontime) ) {
  
for (i in 3:53 ) {
  
  
         
 monthlyProductionPlan [m-3, as.numeric(totalProductiontime [1,i]) ] <- 
 round(monthlyProductionPlan [m-3, as.numeric(totalProductiontime [1,i]) ] + as.numeric(totalProductiontime [m,i]))#  
}

}

# monthlyProductionPlan - помесячный план производства


itemNames<- totalProductiontime [4:nrow(totalProductiontime), 1:2]
monthlyProductionPlan<- cbind(itemNames, monthlyProductionPlan)

colnames (monthlyProductionPlan) <- c("code", "item", "янв", "фев", "март", "апр", "май", "июнь", "июль", "август", "сент", "окт", "ноябрь", "дек")


#
# формируем аналогичный план производства помесячно для Москвы
#
totalProductiontimeMSK<- rbind(alltimeHeader, reportMSK [(nrow(flowMSK)/3+1):(nrow(flowMSK)/3*2), c(1, 2, 4:55)])



# формируем пустую таблицу для потребности по месяцам 
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

colnames (monthlyProductionPlanMSK) <- c("code", "item", "янв", "фев", "март", "апр", "май", "июнь", "июль", "август", "сент", "окт", "ноябрь", "дек")


#
# формируем аналогичный план производства помесячно для Санкт-Петербурга
#

totalProductiontimeSPb<- rbind(alltimeHeader, reportSPb [(nrow(flowSPb)/3+1):(nrow(flowSPb)/3*2), c(1, 2, 4:55)])



# формируем пустую таблицу для потребности по месяцам 
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

colnames (monthlyProductionPlanSPb) <- c("code", "item", "янв", "фев", "март", "апр", "май", "июнь", "июль", "август", "сент", "окт", "ноябрь", "дек")


#
# формируем аналогичный план производства помесячно для Новосибирска
#

totalProductiontimeNSK<- rbind(alltimeHeader, reportNSK [(nrow(flowNSK)/3+1):(nrow(flowNSK)/3*2), c(1, 2, 4:55)])



# формируем пустую таблицу для потребности по месяцам 
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

colnames (monthlyProductionPlanNSK) <- c("code", "item", "янв", "фев", "март", "апр", "май", "июнь", "июль", "август", "сент", "окт", "ноябрь", "дек")

#
# рассчитываем доли (в %) потребности каждого филиала в общей потребности
#

partMSK <- monthlyProductionPlanMSK[ , 3:14]/ monthlyProductionPlan[ , 3:14]
partSPb <- monthlyProductionPlanSPb[ , 3:14]/ monthlyProductionPlan[ , 3:14]
partNSK <- monthlyProductionPlanNSK[ , 3:14]/ monthlyProductionPlan[ , 3:14]

# присваиваем названиям колонок номера месяцев
colnames (partMSK) <- c(1:12)
colnames (partSPb) <- c(1:12)
colnames (partNSK) <- c(1:12)



#заменяем значения NaN на ноль в таблице в долями
partMSK <- replace(partMSK, is.na(partMSK), 0)
partSPb <- replace(partSPb, is.na(partSPb), 0)
partNSK <- replace(partNSK, is.na(partNSK), 0)





##
# формирование графика отгурзки в каждый филиал на основе утвержденного плана производства из 1С,
# допзаказов и рассчитанных долей потребностей каждого филиала в общем плане производства


# Загружаем перечень готовой продукции library readr И readxl
#productionPlan1C<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Образцы отчетов/production plan 1С_3.xls")

productionPlan1C<-read.table("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/график отгрузки ГП.csv", skip = 2, 
                             sep = ";", header = TRUE)

productionPlan1C<- productionPlan1C[ , c(2, 4: ncol(productionPlan1C))]


# productionPlan1C<- productionPlan1C [complete.cases(productionPlan1C),]
# write.table(productionPlan1C , "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Образцы отчетов/production plan 1С_4.csv", row.names = FALSE)
# productionPlan1C<-read.table("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Образцы отчетов/production plan 1С_4.csv", blank.lines.skip = TRUE)


# colnames(productionPlan1C)<- c ("code", "item", c (3:ncol(productionPlan1C)))
 colnames(productionPlan1C)[1:2]<- c ("code", "item")

fullRangeProdPlan<-merge(monthlyProductionPlanMSK [,c(1,2)], productionPlan1C, by ="code", all.x= TRUE)

#преобразовываем коды в цифровой формат
fullRangeProdPlan [,1]<-   as.numeric(fullRangeProdPlan[,1])

#ранжируем в убывающем порядке
fullRangeProdPlan<-fullRangeProdPlan[order(fullRangeProdPlan[,1]),] 
fullRangeProdPlan<- fullRangeProdPlan [,c (1,2, 4:ncol(fullRangeProdPlan))]

fullRangeProdPlan[is.na(fullRangeProdPlan)]<-0

#парсинг дат из заголовков стоблцов
namesDates<- names(fullRangeProdPlan)[3:ncol(fullRangeProdPlan)-1]

colnames(fullRangeProdPlan)[3:ncol(fullRangeProdPlan)-1]<- str_sub(namesDates, start= -10)

# 
# 
# создать "заготовку" по плановому приходу товара в каждый из филиалов
# для Москвы

arrivalPlanMSK<- totalProductiontime
arrivalPlanMSK [4:nrow(arrivalPlanMSK), 3:ncol(arrivalPlanMSK)]<- 0

#определение "номера" столбца из которого будут взяты данные по допзаказам
# первые 5 столбцов текущего месяца - Москва, затем 5 стоблцов - СПб, затем 5 столбцов НСК
as.numeric(str_sub(colnames(fullRangeProdPlan) [3], end =2))

#опрделяем текущий номер недели "внутри" месяца
weekNumberMonth<- as.numeric(arrivalPlanMSK [  3, match (weekReport, colnames(arrivalPlanMSK) )])

# проходим номера недель, начиная с текущей и присваиваем величину допзаказов
# начиная с номера текущей недели внутри месяца

#определеяем номер колонки, с которой начинается текущий месяц
startCurrentMonth<-match(month(Sys.Date()), arrivalPlanMSK [1,])

# проставляем допзаказы на соответствующие недели
for (i in weekNumberMonth :5 ) {
  for (d in 4:nrow(arrivalPlanMSK)) {
    
  arrivalPlanMSK[d, i+startCurrentMonth-1] <- fullRangeProdPlan[d-3, i+2]
  
  }
}
  

# 
# на основе плана производства на месяц, долей распределения по филиалам, формируем план поставок в каждый филиал
# 
# 



# поиск колонки в общем плане производства, где содержится план на текущий месяц
monthDigit<- ifelse(month(Sys.Date())<10, paste(0, month(Sys.Date()), sep = "" ), month(Sys.Date())  )
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))
#colNumberProduction<- match("X28.10.15", colnames(fullRangeProdPlan))


#номера колонок, относящихся к текущему месяцу
colMonth<- which (arrivalPlanMSK[1,] %in% month(Sys.Date()))

#номера колонок текущего месяца, начиная с текущей недели и до конца месяца !!
#colMonth<- c((weekReport+2):max(colMonth))
colMonth<- c(which (colnames(arrivalPlanMSK) %in% (weekReport) ) : max(colMonth))


# получаем плановый понедельный приход на текущий месяц с учетом допзаказов (первое слагаемое); 
# преобразовываем в цифровой формат 
arrivalPlanMSK [4:nrow(arrivalPlanMSK), c(colMonth)]<- round(as.numeric(data.matrix(arrivalPlanMSK [4:nrow(arrivalPlanMSK), c(colMonth)], rownames.force = NA))+  
# умножаем план на текущий месяц на доли и делим на количество недель в месяце
(fullRangeProdPlan[ , colNumberProduction] * partMSK [ , month(Sys.Date())])/ length(colMonth))



# поиск колонки в общем плане производства, где содержится план на месяц СЛЕДУЮЩИЙ за текущим
monthDigit<- ifelse(month(Sys.Date())+1<10, paste(0, month(Sys.Date())+1, sep = "" ), month(Sys.Date())+1  )
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())+1<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))

#номера колонок, относящихся к текущему месяцу слеюущему за текущим
colMonth<- which (arrivalPlanMSK[1,] %in% ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12) )


arrivalPlanMSK [4:nrow(arrivalPlanMSK), c(colMonth)]<- round(data.matrix(arrivalPlanMSK [4:nrow(arrivalPlanMSK), c(colMonth)], rownames.force = NA)+  
                                                               # умножаем план на текущий месяц на доли и делим на количество недель в месяце
                                                               (fullRangeProdPlan[ , colNumberProduction] * partMSK [ , ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12)])/ length(colMonth))

# 
# добавить расчет еще одного месяца для получения плана на 3 месяца
# 


# поиск колонки в общем плане производства через 2 мес
monthDigit<- ifelse(month(Sys.Date())+2<10, paste(0, month(Sys.Date())+2, sep = "" ), month(Sys.Date())+2  )
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())+2<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))

#номера колонок, относящихся к месяцу через 2 месяца
colMonth<- which (arrivalPlanMSK[1,] %in% ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12) )


arrivalPlanMSK [4:nrow(arrivalPlanMSK), c(colMonth)]<- round(data.matrix(arrivalPlanMSK [4:nrow(arrivalPlanMSK), c(colMonth)], rownames.force = NA)+  
                                                               # умножаем план на текущий месяц на доли и делим на количество недель в месяце
                                                               (fullRangeProdPlan[ , colNumberProduction] * partMSK [ , ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12)])/ length(colMonth))

#делаем отбор от текущей недели и до конца года
arrivalPlanMSK<- arrivalPlanMSK [ , c(1,2, c(match(weekReport, colnames(arrivalPlanMSK)):ncol(arrivalPlanMSK)))]
write.csv(arrivalPlanMSK, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/arrivalPlanMSK.csv")
















# создать "заготовку" по плановому приходу товара в каждый из филиалов
# для Санкт-Петербурга

arrivalPlanSPb<- totalProductiontime
arrivalPlanSPb [4:nrow(arrivalPlanSPb), 3:ncol(arrivalPlanSPb)]<- 0

#определение "номера" столбца из которого будут взяты данные по допзаказам
# первые 5 столбцов текущего месяца - Москва, затем 5 стоблцов - СПб, затем 5 столбцов НСК
as.numeric(str_sub(colnames(fullRangeProdPlan) [3], end =2))

#опрделяем текущий номер недели "внутри" месяца
weekNumberMonth<- as.numeric(arrivalPlanSPb [  3, match (weekReport, colnames(arrivalPlanSPb) )])

# проходим номера недель, начиная с текущей и присваиваем величину допзаказов
# начиная с номера текущей недели внутри месяца

#определеяем номер колонки, с которой начинается текущий месяц
startCurrentMonth<-match(month(Sys.Date()), arrivalPlanSPb [1,])

# проставляем допзаказы на соответствующие недели
for (i in weekNumberMonth :5 ) {
  for (d in 4:nrow(arrivalPlanSPb)) {
    
    arrivalPlanSPb[d, i+startCurrentMonth-1] <- fullRangeProdPlan[d-3, i+2+5] #"5" индексация колонок для идентификации допзаказа СПб
    
  }
}


# 
# на основе плана производства на месяц, долей распределения по филиалам, формируем план поставок в каждый филиал
# 
# 



# поиск колонки в общем плане производства, где содержится план на текущий месяц
monthDigit<- ifelse(month(Sys.Date())<10, paste(0, month(Sys.Date()), sep = "" ), month(Sys.Date())  )
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))



#номера колонок, относящихся к текущему месяцу
colMonth<- which (arrivalPlanSPb[1,] %in% month(Sys.Date()))

#номера колонок текущего месяца, начиная с текущей недели и до конца месяца !!
#colMonth<- c((weekReport+2):max(colMonth))
colMonth<- c(which (colnames(arrivalPlanSPb) %in% (weekReport) ) : max(colMonth))

# получем плановый понедлеьный приход на текущий месяц с учетом допзаказов (первое слагаемое); 
# преобразовываем в цифровой формат 
arrivalPlanSPb [4:nrow(arrivalPlanSPb), c(colMonth)]<- round(as.numeric(data.matrix(arrivalPlanSPb [4:nrow(arrivalPlanSPb), c(colMonth)], rownames.force = NA))+  
                                                               # умножаем план на текущий месяц на доли и делим на количество недель в месяце
                                                               (fullRangeProdPlan[ , colNumberProduction] * partSPb [ , month(Sys.Date())])/ length(colMonth))



# поиск колонки в общем плане производства, где содержится план на месяц СЛЕДУЮЩИЙ за текущим
monthDigit<- ifelse(month(Sys.Date())+1<10, paste(0, month(Sys.Date())+1, sep = "" ), month(Sys.Date())+1  )
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())+1<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))

#номера колонок, относящихся к текущему месяцу слеюущему за текущим
colMonth<- which (arrivalPlanSPb[1,] %in% ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12) )


arrivalPlanSPb [4:nrow(arrivalPlanSPb), c(colMonth)]<- round(data.matrix(arrivalPlanSPb [4:nrow(arrivalPlanSPb), c(colMonth)], rownames.force = NA)+  
                                                               # умножаем план на текущий месяц на доли и делим на количество недель в месяце
                                                               (fullRangeProdPlan[ , colNumberProduction] * partSPb [ , ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12)])/ length(colMonth))

# 
# добавить расчет еще одного месяца для получения плана на 3 месяца
# 


# поиск колонки в общем плане производства, где содержится план на месяц СЛЕДУЮЩИЙ за текущим
monthDigit<- ifelse(month(Sys.Date())+2<10, paste(0, month(Sys.Date())+2, sep = "" ), month(Sys.Date()) +2 )
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())+2<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))

#номера колонок, относящихся к текущему месяцу слеюущему за текущим
colMonth<- which (arrivalPlanSPb[1,] %in% ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12) )


arrivalPlanSPb [4:nrow(arrivalPlanSPb), c(colMonth)]<- round(data.matrix(arrivalPlanSPb [4:nrow(arrivalPlanSPb), c(colMonth)], rownames.force = NA)+  
                                                               # умножаем план на текущий месяц на доли и делим на количество недель в месяце
                                                               (fullRangeProdPlan[ , colNumberProduction] * partSPb [ , ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12)])/ length(colMonth))
#делаем отбор от текущей недели и до конца года
arrivalPlanSPb<- arrivalPlanSPb [ , c(1,2, c(match(weekReport, colnames(arrivalPlanSPb)):ncol(arrivalPlanSPb)))]
write.csv(arrivalPlanSPb, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/arrivalPlanSPb.csv")











# создать "заготовку" по плановому приходу товара в каждый из филиалов
# для Новосибирска

arrivalPlanNSK<- totalProductiontime
arrivalPlanNSK [4:nrow(arrivalPlanNSK), 3:ncol(arrivalPlanNSK)]<- 0

#определение "номера" столбца из которого будут взяты данные по допзаказам
# первые 5 столбцов текущего месяца - Москва, затем 5 стоблцов - СПб, затем 5 столбцов НСК
as.numeric(str_sub(colnames(fullRangeProdPlan) [3], end =2))

#опрделяем текущий номер недели "внутри" месяца
weekNumberMonth<- as.numeric(arrivalPlanNSK [  3, match (weekReport, colnames(arrivalPlanNSK) )])

# проходим номера недель, начиная с текущей и присваиваем величину допзаказов
# начиная с номера текущей недели внутри месяца

#определеяем номер колонки, с которой начинается текущий месяц
startCurrentMonth<-match(month(Sys.Date()), arrivalPlanNSK [1,])

# проставляем допзаказы на соответствующие недели
for (i in weekNumberMonth :5 ) {
  for (d in 4:nrow(arrivalPlanNSK)) {
    
    arrivalPlanNSK[d, i+startCurrentMonth-1] <- fullRangeProdPlan[d-3, i+2+10]
    
  }
}


# 
# на основе плана производства на месяц, долей распределения по филиалам, формируем план поставок в каждый филиал
# 
# Новосибирск



# поиск колонки в общем плане производства, где содержится план на текущий месяц
monthDigit<- ifelse(month(Sys.Date())<10, paste(0, month(Sys.Date()), sep = "" ), month(Sys.Date())  )
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))



#номера колонок, относящихся к текущему месяцу
colMonth<- which (arrivalPlanNSK[1,] %in% month(Sys.Date()))

#номера колонок текущего месяца, начиная с текущей недели и до конца месяца !!
#colMonth<- c((weekReport+2):max(colMonth))
colMonth<- c(which (colnames(arrivalPlanNSK) %in% (weekReport) ) : max(colMonth))
# получем плановый понедлеьный приход на текущий месяц с учетом допзаказов (первое слагаемое); 
# преобразовываем в цифровой формат 
arrivalPlanNSK [4:nrow(arrivalPlanNSK), c(colMonth)]<- round(as.numeric(data.matrix(arrivalPlanNSK [4:nrow(arrivalPlanNSK), c(colMonth)], rownames.force = NA))+  
                                                               # умножаем план на текущий месяц на доли и делим на количество недель в месяце
                                                               (fullRangeProdPlan[ , colNumberProduction] * partNSK [ , month(Sys.Date())])/ length(colMonth))



# поиск колонки в общем плане производства, где содержится план на месяц СЛЕДУЮЩИЙ за текущим
monthDigit<- ifelse(month(Sys.Date())+1<10, paste(0, month(Sys.Date())+1, sep = "" ), month(Sys.Date())+1  )
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())+1<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))

#номера колонок, относящихся к текущему месяцу слеюущему за текущим
colMonth<- which (arrivalPlanNSK[1,] %in% ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12) )


arrivalPlanNSK [4:nrow(arrivalPlanNSK), c(colMonth)]<- round(data.matrix(arrivalPlanNSK [4:nrow(arrivalPlanNSK), c(colMonth)], rownames.force = NA)+  
                                                               # умножаем план на текущий месяц на доли и делим на количество недель в месяце
                                                               (fullRangeProdPlan[ , colNumberProduction] * partNSK [ , ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12)])/ length(colMonth))

# 
# добавить расчет еще одного месяца для получения плана на 3 месяца
# 


# поиск колонки в общем плане производства, где содержится план на месяц СЛЕДУЮЩИЙ за текущим
monthDigit<- ifelse(month(Sys.Date())+2<10, paste(0, month(Sys.Date())+2, sep = "" ), month(Sys.Date())+2  )
# monthDigit <- ifelse (monthDigit<=12, monthDigit, monthDigit-12)
# monthDigit<-ifelse(monthDigit<10, paste(0, monthDigit, sep=""), monthDigit)
dateHeader<- paste(28,monthDigit, ifelse(month(Sys.Date())+2<=12,year(Sys.Date()), year(Sys.Date())+1), sep =".")
colNumberProduction<- match(dateHeader, colnames(fullRangeProdPlan))

#номера колонок, относящихся к текущему месяцу слеюущему за текущим
colMonth<-which (arrivalPlanNSK[1,] %in% ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+1, month(Sys.Date())+2-12) )


arrivalPlanNSK [4:nrow(arrivalPlanNSK), c(colMonth)]<- round(data.matrix(arrivalPlanNSK [4:nrow(arrivalPlanNSK), c(colMonth)], rownames.force = NA)+  
                                                               # умножаем план на текущий месяц на доли и делим на количество недель в месяце
                                                               (fullRangeProdPlan[ , colNumberProduction] * partNSK [ , ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12)])/ length(colMonth))

#делаем отбор от текущей недели и до конца года
arrivalPlanNSK<- arrivalPlanNSK [ , c(1,2, c(match(weekReport, colnames(arrivalPlanNSK)):ncol(arrivalPlanNSK)))]
write.csv(arrivalPlanNSK, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/arrivalPlanNSK.csv")

# записываем все таблицы с ежемесячной ПЛАНОВОЙ потребностью в приходе ГП в каждый филиал и общем приходе
write.csv(monthlyProductionPlan, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/monthlyProductionPlan.csv")
write.csv(monthlyProductionPlanMSK, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/monthlyProductionPlanMSK.csv")
write.csv(monthlyProductionPlanSPb, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/monthlyProductionPlanSPb.csv")
write.csv(monthlyProductionPlanNSK, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/monthlyProductionPlanNSK.csv")



# доли
partMSK<- cbind(salesMoscow [ ,c (1:2)], round(partMSK, digits = 2))
partSPb<- cbind(salesSPb [ ,c (1:2)], round(partSPb, digits = 2))
partNSK<- cbind(salesNSK [ ,c (1:2)], round(partNSK, digits = 2))

write.csv(partMSK, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/partMSK.csv")
write.csv(partSPb, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/partSPb.csv")
write.csv(partNSK, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/partNSK.csv")








##
##
##
## Рекомендация необходимых допзаказов
##

# преобразовываем в числовой формат 
# для Москвы
for (i in 3:ncol(arrivalPlanMSK) ) {
  arrivalPlanMSK[ ,i] <- as.numeric(arrivalPlanMSK[ ,i])
}
arrivalPlanMSKmonth <- arrivalPlanMSK[ 4: nrow(arrivalPlanMSK) , 1:6]

#сумма плановых приходов за ближайшие 4 недели
arrivalPlanMSKmonth$sum <- rowSums (arrivalPlanMSKmonth [ , 3:6])

# суммарный расход за ближайшие 4 недели
salesMoscowMonth <- salesMoscow [ , 1:6]
salesMoscowMonth$sum <- rowSums (salesMoscow [ , 3:6])
# номер колонки с текущей неделей
currntWeekcolumnNumber<- which(colnames(reportMSK) %in% weekReport)

# сумма текущих остатков и приходов за ближайшие 4 недели
deficitMSK<- cbind(reportMSK[1:(nrow(flowMSK)/3),c(1:2)], (reportMSK[1:(nrow(flowMSK)/3), currntWeekcolumnNumber] + arrivalPlanMSKmonth$sum 
                                                           -salesMoscowMonth$sum)/ salesMoscowMonth$sum)

deficitMSK<- cbind(reportMSK[1:(nrow(flowMSK)/3),c(1:2)], 
                   reportMSK[1:(nrow(flowMSK)/3), currntWeekcolumnNumber],
                   arrivalPlanMSKmonth$sum,
                   salesMoscowMonth$sum,
                   reportMSK[1:(nrow(flowMSK)/3), currntWeekcolumnNumber] + arrivalPlanMSKmonth$sum -salesMoscowMonth$sum, 
                   round(deficitMSK[ ,3], digits = 2))

colnames(deficitMSK)<-  c ("код", "наименование", "текущий остаток", "план прихода 4 нед", "план продаж 4 нед", "плановый остаток через 4 нед", "оборач-ть через 4 нед, мес")

deficitMSKFilter <- subset(deficitMSK, deficitMSK [ ,6] < -0)
surplusMSKFilter <- subset(deficitMSK, deficitMSK [ ,7] > 2)


#"физический остаток" без учета резервов
stockMSK<- cbind( sat2 [,4], (sat2[ ,warehouseStockMoscow]+sat2[ ,warehouseStockMoscowBranch] + sat2[ ,warehouseStockMoscowTransit]))
colnames(stockMSK) <- c("код", "количество")

deficitMSKFilter<-merge(deficitMSKFilter, stockMSK, by ="код", all.x= TRUE)
deficitMSKFilter<- cbind(deficitMSKFilter[ , c(1:2)], deficitMSKFilter[ ,8], deficitMSKFilter[ , c(3:7)])

colnames(deficitMSKFilter)<-  c ("код", "наименование", "текущий остаток", "остаток с резервом", "план прихода 4 нед", "план продаж 4 нед", "плановый остаток через 4 нед", "оборач-ть через 4 нед, мес")


write.csv(deficitMSKFilter, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/deficitMSKFilter.csv")
write.csv(surplusMSKFilter, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/surplusMSKFilter.csv")






#для Санкт-Петербурга


for (i in 3:ncol(arrivalPlanSPb) ) {
  arrivalPlanSPb[ ,i] <- as.numeric(arrivalPlanSPb[ ,i])
}
arrivalPlanSPbmonth <- arrivalPlanSPb[ 4: nrow(arrivalPlanSPb) , 1:6]

#сумма плановых приходов за ближайшие 4 недели
arrivalPlanSPbmonth$sum <- rowSums (arrivalPlanSPbmonth [ , 3:6])

# суммарный расход за ближайшие 4 недели
salesSPbMonth <- salesSPb [ , 1:6]
salesSPbMonth$sum <- rowSums (salesSPb [ , 3:6])

# сумма текущих остатков и приходов за ближайшие 4 недели
deficitSPb<- cbind(reportSPb[1:(nrow(flowSPb)/3),c(1:2)], (reportSPb[1:(nrow(flowSPb)/3), currntWeekcolumnNumber] + arrivalPlanSPbmonth$sum 
                                                           -salesSPbMonth$sum)/ salesSPbMonth$sum)

deficitSPb<- cbind(reportSPb[1:(nrow(flowSPb)/3),c(1:2)], 
                   reportSPb[1:(nrow(flowSPb)/3), currntWeekcolumnNumber],
                   arrivalPlanSPbmonth$sum,
                   salesSPbMonth$sum,
                   reportSPb[1:(nrow(flowSPb)/3), currntWeekcolumnNumber] + arrivalPlanSPbmonth$sum -salesSPbMonth$sum, 
                   round(deficitSPb[ ,3], digits = 2))

colnames(deficitSPb)<-  c ("код", "наименование", "текущий остаток", "план прихода 4 нед", "план продаж 4 нед", "плановый остаток через 4 нед", "оборач-ть через 4 нед, мес")

deficitSPbFilter <- subset(deficitSPb, deficitSPb [ ,6] < -0)
surplusSPbFilter <- subset(deficitSPb, deficitSPb [ ,7] > 2)


#"физический остаток" без учета резервов
stockSPb<- cbind( sat2 [,4], sat2[ ,warehouseStockSPb])
colnames(stockSPb) <- c("код", "количество")

deficitSPbFilter<-merge(deficitSPbFilter, stockSPb, by ="код", all.x= TRUE)
deficitSPbFilter<- cbind(deficitSPbFilter[ , c(1:2)], deficitSPbFilter[ ,8], deficitSPbFilter[ , c(3:7)])

colnames(deficitSPbFilter)<-  c ("код", "наименование", "текущий остаток", "остаток с резервом", "план прихода 4 нед", "план продаж 4 нед", "плановый остаток через 4 нед", "оборач-ть через 4 нед, мес")



write.csv(deficitSPbFilter, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/deficitSPbFilter.csv")
write.csv(surplusSPbFilter, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/surplusSPbFilter.csv")



# для Новосибирска

for (i in 3:ncol(arrivalPlanNSK) ) {
  arrivalPlanNSK[ ,i] <- as.numeric(arrivalPlanNSK[ ,i])
}
arrivalPlanNSKmonth <- arrivalPlanNSK[ 4: nrow(arrivalPlanNSK) , 1:6]

#сумма плановых приходов за ближайшие 4 недели
arrivalPlanNSKmonth$sum <- rowSums (arrivalPlanNSKmonth [ , 3:6])

# суммарный расход за ближайшие 4 недели
salesNSKMonth <- salesNSK [ , 1:6]
salesNSKMonth$sum <- rowSums (salesNSK [ , 3:6])

# сумма текущих остатков и приходов за ближайшие 4 недели
deficitNSK<- cbind(reportNSK[1:(nrow(flowNSK)/3),c(1:2)], (reportNSK[1:(nrow(flowNSK)/3), currntWeekcolumnNumber] + arrivalPlanNSKmonth$sum 
                   -salesNSKMonth$sum)/ salesNSKMonth$sum)

deficitNSK<- cbind(reportNSK[1:(nrow(flowNSK)/3),c(1:2)], 
                   reportNSK[1:(nrow(flowNSK)/3), currntWeekcolumnNumber],
                   arrivalPlanNSKmonth$sum,
                    salesNSKMonth$sum,
                    reportNSK[1:(nrow(flowNSK)/3), currntWeekcolumnNumber] + arrivalPlanNSKmonth$sum -salesNSKMonth$sum, 
                    round(deficitNSK[ ,3], digits = 2))

colnames(deficitNSK)<-  c ("код", "наименование", "текущий остаток", "план прихода 4 нед", "план продаж 4 нед", "плановый остаток через 4 нед", "оборач-ть через 4 нед, мес")

deficitNSKFilter <- subset(deficitNSK, deficitNSK [ ,6] < 0)
surplusNSKFilter <- subset(deficitNSK, deficitNSK [ ,7] > 2)


#"физический остаток" без учета резервов
stockNSK<- cbind( sat2 [,4], (sat2[ ,warehouseStockNSK ]+sat2[ ,warehouseStockNSKBranch] + sat2[ ,warehouseStockNSKTransit]))
colnames(stockNSK) <- c("код", "количество")

deficitNSKFilter<-merge(deficitNSKFilter, stockNSK, by ="код", all.x= TRUE)
deficitNSKFilter<- cbind(deficitNSKFilter[ , c(1:2)], deficitNSKFilter[ ,8], deficitNSKFilter[ , c(3:7)])

colnames(deficitNSKFilter)<-  c ("код", "наименование", "текущий остаток", "остаток с резервом", "план прихода 4 нед", "план продаж 4 нед", "плановый остаток через 4 нед", "оборач-ть через 4 нед, мес")



write.csv(deficitNSKFilter, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/deficitNSKFilter.csv")
write.csv(surplusNSKFilter, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/surplusNSKFilter.csv")






# сводные доли на текущий месяц
allParts<-cbind(salesMoscow [ ,c (1:2)], partMSK[ , month(Sys.Date())+2], partSPb[ , month(Sys.Date())+2,], partNSK[ , month(Sys.Date())+2])
colnames(allParts)<- c("code", "item", "MSK", "SPB", "NSK")


allParts <- cbind(allParts[ , 1:2],   
                  weeklyStockMoscow [ , weekReport+2], salesMoscowMonth [ , 7],
                  weeklyStockSPb [ , weekReport+2], salesSPbMonth [ , 7],
                  weeklyStockNSK [ , weekReport+2], salesNSKMonth [ , 7],
                  allParts [ ,3:5])

colnames(allParts) <- c("code", "наименование", "остатки МСК", "продажи МСК", "остатки СПб", "продажи СПб","остатки НСК", "продажи НСК","MSK", "SPB", "NSK")
write.csv(allParts, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/allParts.csv")

allParts$приоритет<- 0

for (i in 1:nrow(allParts)) {

  allParts[i,12]<- ifelse (allParts[i,3]<0 | allParts[i,5]<0 | allParts[i,7]<0, 
                               allParts[i,12]<- "приоритет", allParts[i,12]<- "норм")
  
}

priorityProduction <- subset(allParts, allParts [ ,12] == "приоритет") [1:8]

write.csv(priorityProduction, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/priorityProduction.csv", row.names=FALSE)



#запуск расчета по выполнению плана продаж, логистическому сервису, динамики колебания стоимости запасов в "излишках"
#source('~/Максим/R план/sales analytics.R')

#
#копирование исходных и полученных файлов
#


source('~/Максим/R план/Surplus cost and turnover.R')

# скопировать все исходные файлы
flist <- list.files("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов", full.names = TRUE)
file.copy(flist, paste("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/Архив/", Sys.Date(), sep = ""))

# скопировать все полученные после расчетов файлы
flist <- list.files("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов", full.names = TRUE)
file.copy(flist, paste("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/Архив/", Sys.Date(), sep = ""))


#скопировать все файлы на сетевой сервер
file.copy(flist, "Z:/Смирнов/Отчеты для R", overwrite = TRUE)



