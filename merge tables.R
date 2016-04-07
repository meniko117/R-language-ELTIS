library(readxl)
library(readr)
library(stringr)
library (reshape2)
library (Rcpp)
library (lubridate)

#options(warn=0)   выключить "warnings" options(warn=-1) 

# скопировать все исходные файлы
flist <- list.files("Z:/Analytical system", full.names = TRUE)
file.copy(flist, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов", overwrite = TRUE)



weekBegin<- data.frame(matrix(nrow=1, ncol=52))


#номер дня недели (функция из lubridate) для текущего года
# в строке ниже при смене года заменить на первое января наступающего года (в 2-х местах)
# +7 добавлено для 2016 года
firstMonday<- as.Date(strptime("01.01.2016", format = "%d.%m.%Y"))-(wday(as.Date(strptime("01.01.2016", format = "%d.%m.%Y")))-2)

weekBegin[1]<-firstMonday+7


for (i in 1:51 ) {
  weekBegin[i+1] = weekBegin[i] +days(7)
}


#РЅРѕРјРµСЂ РјРµСЃСЏС†Р° РґР»СЏ РєР°Р¶РґРѕР№ РЅРµРґРµР»Рё
monthBegin<- data.frame(matrix(nrow=1, ncol=52))
for (i in 1:52 ) {
  
  monthBegin[1,i] <- month(as.POSIXlt(weekBegin[1,i], format="%d.%m.%Y"))
}

colnames(weekBegin)<-monthBegin[1,]

row<-as.data.frame(matrix(nrow=1,ncol=52))
row[is.na(row)]<-0



weekBegin3<- rbind(weekBegin [ ,1], row)





colnames (weekBegin3)<- colnames (weekBegin)

weekBegin3 [2,1] <-1

for (i in 2:52 ) {
  ifelse ( colnames(weekBegin3)[i] == colnames(weekBegin3)[i-1], weekBegin3 [2,i]<- weekBegin3 [2,i-1]+1, weekBegin3 [2,i]<-1)
  
}



for (i in 1:52 ) {
  weekBegin3[1,i] <- as.character(weekBegin[1, i])
}

# для приведения размерности в соответствие
twocolumns<-as.data.frame(matrix(nrow=2,ncol=2))

alltimeHeader<- cbind(twocolumns, weekBegin3)

#номера месяцев рассчитанных по дате понедельника
monthnumber<-as.data.frame(matrix(nrow=1,ncol=54))
monthnumber<-colnames(alltimeHeader)
alltimeHeader<- rbind(monthnumber, alltimeHeader)

colnames(alltimeHeader)<-c("code", "item.x", c(1:52))

# 
# получили "alltimeHeader" с указанием номера недели, номера месяца, даты понедельника, количества недель в месяце
# 
# 


#alltimeHeader <- alltimeHeaderMix # ТЕСТ



##
##загружаем данные по остаткам на всех складах
##
allStock<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/все остатки.xls")
#заменить NA на ноль
allStock[is.na(allStock)]<-0

header<-allStock[1,]


#обработать содержимое для получения цифр
write.csv(allStock, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/all stock.сsv")

sat<-read.csv("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/all stock.сsv", 
              , skip=1, na.strings=c(" ", NA), stringsAsFactors=FALSE) 

write.csv(sat, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/all stock.сsv", )

sat2<-read.csv("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/all stock.сsv", 
               skip=2) 


#добавить заголовки стобцов
names(sat2)<-c(0,0,header)
sat2[is.na(sat2)]<-0

#получить номер колонки заданного склада
warehouseStock<-match("Склад ПКИ и материалы", colnames(sat2))
warehouseStock1<-match("Склад готовой продукции (Произ-во)", colnames(sat2))
warehouseStock2<-match("Основной цех", colnames(sat2))
warehouseStock3<-match("Механический участок", colnames(sat2))
stockComponents<-sat2[,c(4,6,warehouseStock)] 
stockComponents [ ,3]<- stockComponents [ ,3] + sat2[ ,warehouseStock1] + sat2[ ,warehouseStock2] + sat2[ ,warehouseStock3]

names(stockComponents)<-c("code", "item")

stockComponents[is.na(stockComponents)]<-0


#загружаем данные из отчетов
# "материалы в работе" и
# "материалы у подрядчиков"


materialAtWork<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/материалы в работе.xls", skip=2)
bbb<-as.numeric(gregexpr(pattern =']',materialAtWork [,2]))
codeMaterial<-as.numeric(str_sub(materialAtWork [,2], start= 2, end=bbb-1))
materialAtWork [, 1]<-codeMaterial
materialAtWork [, 3] <- round(as.numeric(materialAtWork [, 3]))
materialAtWork <- materialAtWork [c(2:nrow(materialAtWork)), c(1:3)]
colnames(materialAtWork)<- c("code", "item", "quantity")


materialAtSubcontr<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/материалы у подрядчиков.xls", skip=2)
bbb<-as.numeric(gregexpr(pattern =']',materialAtSubcontr [,2]))
codeMaterial<-as.numeric(str_sub(materialAtSubcontr [,2], start= 2, end=bbb-1))
materialAtSubcontr [, 1]<-codeMaterial
materialAtSubcontr [, 3] <- round(as.numeric(materialAtSubcontr [, 3]))
materialAtSubcontr <- materialAtSubcontr [c(2:nrow(materialAtSubcontr)), c(1:3)]
colnames(materialAtSubcontr)<- c("code", "item", "quantity")

stockComponents <- merge(stockComponents, materialAtWork, by ="code", all.x= TRUE)
stockComponents[is.na(stockComponents)]<-0
stockComponents [,3]<-stockComponents [,3] +stockComponents [,5]
stockComponents <- stockComponents [ ,c(1:3)]


stockComponents <- merge(stockComponents, materialAtSubcontr, by ="code", all.x= TRUE)
stockComponents[is.na(stockComponents)]<-0
stockComponents [,3]<-stockComponents [,3] +stockComponents [,5]
stockComponents <- stockComponents [ ,c(1:3)]






##
## конец блока загрузки данных по остаткам



#парсинг даты отчета по остаткам
date<-names(allStock)[1]
library(stringr)
dateReport<-str_sub(date, start= -8)


# номер текущей недели
weekReport<-ceiling(as.numeric(
  difftime(strptime(dateReport, format = "%d.%m.%y"),
           strptime("01.01.2016", format = "%d.%m.%Y"), units="weeks") ))

#присвоили номер недели текущим остаткам
names(stockComponents)[3]<-weekReport




# загружаем таблицу с полным ассортиментом, который держим на контроле и параметрами (срок доставки, страховой запас и проч.)
param<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/stockFlowParameters.xls")

names(param)<-c("code", "item", "MOQ", "safety stock", "lead time", "order quantity")

# names(weeklySales)<-c("code","items", c("1":"52"))

#создать таблицу с полным ассортиментом и 52-неделями пустых значений
#сделать сводную таблицу, включив остатки по текущей неделе 
#(ежнедельно догружать остатки в имеющуюся таблицу)
currentStock<-as.data.frame(matrix(nrow=nrow(param), ncol=54))
currentStock[,1:2]<-param[,1:2]
names(currentStock)<-c("code", "item", c(1:52))

#получаем таблицу с еженедельными остатками и для текущей недели обновляем данные
weeklyStock<-merge(currentStock, stockComponents, by ="code", all.x= TRUE)

# вставить блок с парсингом типа "17.y"
# затем подать получившуюся матрицу на вход Rcpp блока



nam<-colnames(stockComponents) # номера недель, где есть заказы
nam2<-colnames(weeklyStock) # все идентификаторы недель, после объединения таблиц
difnames<-paste(nam, ".x", sep ="") [3]
difnames2<-paste(nam, ".y", sep ="") [3]

len<-length(weeklyStock) # 

colindex<-match(difnames,nam2) # поиск номеров колонок в 52-недельной матрице, совмещенной с матрцие заказов, которые совпали с матрицей заказов
colindex2<-match(difnames2,nam2) # поиск номеров колонок в матрцие заказов, которые совпали неделями заказов



#присваиваем значения размещенных заказов общей таблице, включающей понедельный план
weeklyStock[,colindex]<-weeklyStock[,colindex2]

names(weeklyStock)<-c("code","item", c("1":"52"))
weeklyStock[is.na(weeklyStock)]<-0 # получили таблицу с остатком на текущей неделе в 52-недельной матрице
stockAtHand<-as.matrix(weeklyStock[,3:54]) # матрица с остатками для передачи в RCPP




















##
## получаем 52-недельную матрицу с размещенными заказами на каждой неделе (ТОВАРЫ В ПУТИ)
##

#rat<-read.csv("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/товары в пути.xls",skip=2, col_names = FALSE)
rat<-read.csv("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/товары в пути.csv",skip=2, sep=",", dec=",")
#rat<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/товары в пути.xls", skip=2)
nr<-nrow(rat)

for (m in 2:nr)
{rat[m,14]<-as.data.frame(rat[m,6])
}


code<-regexpr(']',rat[,5])
rat$code<-substr(rat[,5], 2, code-1)
rat$orderDate<-rat[,2]
# "%Y" или "%y" зависит от тображения года, например, "18.08.15" или "18.08.2015"
rat$orderWeek<-round(as.numeric(
  difftime(strptime(rat$orderDate, format = "%d.%m.%y"),  #ФОРМАТ !!! %Y или y%
           strptime("01.01.2016", format = "%d.%m.%Y"), units="weeks") +1), digits=0)
#rat[is.na(rat)]<-0

wTable<-subset(rat, select=c( 15, 5,  17, 14))

#преобразования номера недели >52 при переходе на другой год
# количество недель в текущем году
yearWeeksNumber<- round(as.numeric(
  difftime(strptime(paste("31",".","12",".", year(dmy(date)), sep=""), format = "%d.%m.%y"),  #ФОРМАТ !!! %Y или y%
           strptime(paste("01",".","01",".", year(dmy(date)), sep=""), format = "%d.%m.%y"), units="weeks") +1), digits=0)




for (i in 1:nrow (wTable) ) {
  
  wTable [ i,3] <- ifelse(wTable[i,3]>yearWeeksNumber, wTable[i,3]-yearWeeksNumber, wTable[i,3])
}

names(wTable)<-c("code", "item", "orderWeek", "quantity")

wTable[,4]


# получаем таблицу план-график размещенных заказов
resTable<-dcast(wTable, item ~ orderWeek, fun.aggregate=sum)
# добавляем колонку с кодом
code<-regexpr(']',resTable[,1])
resTable$code<-substr(resTable[,1], 2, code-1)



# # создаем пустую таблицу
# calendf<-as.data.frame(matrix(nrow=154, ncol=52))
# 
# 
# names(calendf)<-c("1":"52")
# 
# calendf[6:154,1]<-resTable[6:154,1]
# names(calendf)<-c("item", c("1":"51"))
# 
# # сводная таблица для обеспечения размщения заказов в 52-недельной матрице
# all<-merge(calendf, resTable, by = "item", all = TRUE)





# code<-regexpr(']',all[,1])
# all$code<-substr(all[,1], 2, code-1)


weeklyOrders<-merge(currentStock, resTable, by ="code", all.x= TRUE)

nam<-colnames(resTable) # номера недель, где есть заказы
nam2<-colnames(weeklyOrders) # все идентификаторы недель, после объединения таблиц
difnames<-paste(nam, ".x", sep ="") 
difnames2<-paste(nam, ".y", sep ="") 

len<-length(nam) # 

colindex<-match(difnames,nam2)[3:len-2] # поиск номеров колонок в 52-недельной матрице, совмещенной с матрицей заказов, которые совпали с матрицей заказов
colindex2<-match(difnames2,nam2)[3:len-2] # поиск немеров колонок в матрице заказов, которые совпали неделями заказов

#убираем неопределившиеся колонки
colindex<-na.omit(colindex)
colindex2<-na.omit(colindex2)

#присваиваем значения размещенных заказов общей таблице, включающей понедельный план
weeklyOrders[,colindex]<-weeklyOrders[,colindex2]

names(weeklyOrders)<-c("code","items", c("1":"52"))
weeklyOrders[is.na(weeklyOrders)]<-0


##
## получаем 3 таблицы: остатки, заказы, потребление в размерности на 52 недели
##
weeklyStock<-weeklyStock[,1:54]
weeklyOrders<-weeklyOrders[,1:54]














weeklySales<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/weeklySales.xls")
names(weeklySales)<-c("code","items", c("1":"52"))

# загрузка в другом формате
# weeklySales<-read.csv("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/weeklySales.csv", sep =";") 
# names(weeklySales)<-c("code","items", c("1":"52"))

currentStock[,1:2]<-weeklyStock[,1:2]
weeklySales2<-merge(currentStock, weeklySales, by ="code", all.x= TRUE)
colnames(weeklySales2)<-c("code", "items", "1":"52")

weeklySales2[,3:54]<-weeklySales2[,56:107] 
weeklySales2<- weeklySales2[,1:54]


#рассчитываем потребность в полуфабрикатах в зависимости от этапа готовности
source('~/Максим/R план/semiproducts2.R')

# 
# 
# 
# 
# weeklysales формируем из файла из 1С потребление ПКИ на ТЕКУЩИЙ месяц
# 
# 
# 
# 
# 
mu<-paste ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/потребление компонентов мес ", 
           ifelse(month(Sys.Date())<10,0,""),
           month(Sys.Date()), ".xls", sep="")


componentsConsumption<-read_excel(mu)
componentsConsumption<- componentsConsumption [, c(2,4,match("Требуется в изделия", componentsConsumption[2, ]))]
colnames(componentsConsumption) <- c("code", "item", "quantity")
componentsConsumption <- componentsConsumption [3:nrow(componentsConsumption), ]
componentsConsumption [ ,3] <- round(as.numeric(componentsConsumption [ ,3]))
componentsConsumption [ ,1] <- round(as.numeric(componentsConsumption [ ,1]))

#загружаем данные по потреблению полуфабрикатов на текущий месяц
semi1<- read.csv ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/purchaseSemiProducts 1.csv" )
# объединяем данные по потреблению из общего файла и специално рассчитанные данные по потреблению полуфабрикатов
# из скрипта semiproducts2.R
componentsConsumption <- merge(componentsConsumption, semi1, by ="code", all.x= TRUE) 
# заменяем данные по потреблению только для полуфабрикатов
for (i in 1:nrow(componentsConsumption) ) {
componentsConsumption [ i,3]<-ifelse(is.na(componentsConsumption [i ,8]), componentsConsumption [ i,3], componentsConsumption [i ,8])
}                                  
componentsConsumption <- componentsConsumption [ ,c(1:3)]

# сохранить оригинал потребления на текущий месяц
write.csv(componentsConsumption, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/потребление ПКИ 1.csv", row.names= FALSE)


componentsConsumption<- merge (weeklySales2[,1:2], componentsConsumption, by ="code", all.x= TRUE) [ , c(1,2,4)]
componentsConsumption[is.na(componentsConsumption)]<-0


#номера колонок, относящихся к текущему месяцу
colMonth<- grep (month(Sys.Date()), alltimeHeader[1,])

#номера колонок текущего месяца, начиная с текущей недели и до конца месяца !!
colMonth<- c((weekReport+2):max(colMonth))

# присваиваем потребляемые количества соответствущим неделям ТЕКУЩЕГО месяца
weeklySales2 [ ,colMonth ]<- round(componentsConsumption [ ,3] /length(colMonth))




#
# weeklysales формируем из файла из 1С потребление ПКИ на СЛЕДУЮЩИЙ месяц
#

mu<-paste ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/потребление компонентов мес ", 
           ifelse(month(Sys.Date())+1<10,0,""),
           ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12) , ".xls", sep="")
componentsConsumption<-read_excel(mu)


componentsConsumption<- componentsConsumption [, c(2,4,match("Требуется в изделия", componentsConsumption[2, ]))]
colnames(componentsConsumption) <- c("code", "item", "quantity")
componentsConsumption <- componentsConsumption [3:nrow(componentsConsumption), ]
componentsConsumption [ ,3] <- round(as.numeric(componentsConsumption [ ,3]))
componentsConsumption [ ,1] <- round(as.numeric(componentsConsumption [ ,1]))

#загружаем данные по потреблению полуфабрикатов на текущий месяц
semi1<- read.csv ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/purchaseSemiProducts 2.csv" )
# объединяем данные по потреблению из общего файла и специално рассчитанные данные по потреблению полуфабрикатов
# из скрипта semiproducts2.R
componentsConsumption <- merge(componentsConsumption, semi1, by ="code", all.x= TRUE) 
# заменяем данные по потреблению только для полуфабрикатов
for (i in 1:nrow(componentsConsumption) ) {
  componentsConsumption [ i,3]<-ifelse(is.na(componentsConsumption [i ,8]), componentsConsumption [ i,3], componentsConsumption [i ,8])
}                
componentsConsumption <- componentsConsumption [ ,c(1:3)]

# сохранить оригинал потребления на следующий месяц
write.csv(componentsConsumption, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/потребление ПКИ 2.csv" )


componentsConsumption<- merge (weeklySales2[,1:2], componentsConsumption, by ="code", all.x= TRUE) [ , c(1,2,4)]
componentsConsumption[is.na(componentsConsumption)]<-0


#номера колонок, относящихся к CЛЕДУЮЩЕМУ месяцу
colMonth<- which (alltimeHeader[1,] %in% ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12) )
# присваиваем потребляемые количества соответствущим неделям ТЕКУЩЕГО месяца
weeklySales2 [ ,colMonth ]<- round(componentsConsumption [ ,3] /length(colMonth))




#
# weeklysales формируем из файла из 1С потребление ПКИ ЧЕРЕЗ 2 месяца
#

mu<-paste ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/потребление компонентов мес ", 
           ifelse(month(Sys.Date())+2<10,0,""),
           ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12), ".xls", sep="")
componentsConsumption<-read_excel(mu)


componentsConsumption<- componentsConsumption [, c(2,4,match("Требуется в изделия", componentsConsumption[2, ]))]
colnames(componentsConsumption) <- c("code", "item", "quantity")
componentsConsumption <- componentsConsumption [3:nrow(componentsConsumption), ]
componentsConsumption [ ,3] <- round(as.numeric(componentsConsumption [ ,3]))
componentsConsumption [ ,1] <- round(as.numeric(componentsConsumption [ ,1]))

#загружаем данные по потреблению полуфабрикатов на текущий месяц
semi1<- read.csv ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/purchaseSemiProducts 3.csv" )
# объединяем данные по потреблению из общего файла и специално рассчитанные данные по потреблению полуфабрикатов
# из скрипта semiproducts2.R
componentsConsumption <- merge(componentsConsumption, semi1, by ="code", all.x= TRUE) 
# заменяем данные по потреблению только для полуфабрикатов
for (i in 1:nrow(componentsConsumption) ) {
  componentsConsumption [ i,3]<-ifelse(is.na(componentsConsumption [i ,8]), componentsConsumption [ i,3], componentsConsumption [i ,8])
}                
componentsConsumption <- componentsConsumption [ ,c(1:3)]

# сохранить оригинал потребления через 2 месяца
write.csv(componentsConsumption, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/потребление ПКИ 3.csv" )


componentsConsumption<- merge (weeklySales2[,1:2], componentsConsumption, by ="code", all.x= TRUE) [ , c(1,2,4)]
componentsConsumption[is.na(componentsConsumption)]<-0


#номера колонок, относящихся потреблению ЧЕРЕЗ 2 месяца
colMonth<- which (alltimeHeader[1,] %in% ifelse(month(Sys.Date())+2<= 12, month(Sys.Date())+2, month(Sys.Date())+2-12) )
# присваиваем потребляемые количества соответствущим неделям ТЕКУЩЕГО месяца
weeklySales2 [ ,colMonth ]<- round(componentsConsumption [ ,3] /length(colMonth))


# убираем нулевые значения
weeklySales2[is.na(weeklySales2)]<-0.001
weeklyOrders[is.na(weeklyOrders)]<-0.001
weeklyStock[is.na(weeklyStock)]<-0.001










#сортируем параметры товародвижения в установленном порядке
parameters<-merge(currentStock, param, by ="code", all.x= TRUE)

# получили 4 матрицы для передачи в RCPP
StockMatrix<-as.matrix(weeklyStock[,3:54])
OrdersMatrix<-as.matrix(weeklyOrders[,3:54])
SalesMatrix<-as.matrix(weeklySales2[,3:54])
ParametersMatrix<-as.matrix(parameters[,c(57,59,58)])




# write.csv(weeklyStock, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/weeklyStock.csv")
# 
# write.csv(weeklyOrders, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/weeklyOrders.csv")
# 
# write.csv(weeklySales2, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/weeklySales.csv")
# 

# 4 таблица
# param - data frame  со всеми параметрами (срок доставки, страховой запас, количество в заказе)









# RCPP


# функция на C++, которой аргументами передаются
# матрицы, каждая из которых включает весь перечень позиций в одинаковом порядке
# матрица x - остатки по неделям (прошлые фактические и расчетные)
# матрица y - потребление по неделям
# матрица z - товары в пути по неделям (из отчета по счетам, по которым не получен товар)
# матрица p - параметры (в неделях): срок доставки, страховой запас, размер заказа



k <- matrix(50, 10,52)
m <- matrix(30, 10,52)

m [,28:35]<-c(20,30,50,100,10,60,30,44,67, 57)
m [,28:35]<-90

placedOrders<-matrix(10, 10,50)
placedOrders[,c(5:10, 12:18, 25:28)]<-0
placedOrders[,36]<-100

# нумерация стоблцов при передаче в RCPP начинается с нуля !!!
# 0-я колонка страховой запас
# 1-я колонка размер заказа
# 2-я колонка время в пути

parameters<-matrix(4, 10,3)
parameters[,1]<-4
parameters[,2]<-2
parameters[,3]<-6

#расчет номера текущей недели
date()
u<-Sys.Date()

#вычитаем "2", т.к. нумерация колонок с "0" в RCPP
time<-round(as.numeric(
  difftime(strptime(u, format = "%Y-%m-%d"),
           strptime("01.01.2016", format = "%d.%m.%Y"), units="weeks") +1), digits=0)-2

# РґР°РЅРЅС‹Рµ РЅР° 20-СЋ РЅРµРґРµР»СЋ
#k[,20]<-c(30,50,10,200,40,150,0,80,20,114)
m [,20:22] <- c(150,10,10,0,40,100,0,80,20,114)

source('~/Максим/R план/RCPPcorrect 2.R')
#Sys.sleep(5)
# cppFunction('NumericMatrix stockTurnover(NumericMatrix x, NumericMatrix y, NumericMatrix z, NumericMatrix p, int t) {
#             int nrow = x.nrow(), ncol = x.ncol();
#             
#             NumericMatrix out(nrow*3, ncol);
#             
#             
#             
#             for (int i = 0; i < nrow; i++) {         // проход по рядам
#             
#             for (int j = t; j < 50; j++) {           // "j=t" проход по колонкам в ряду (начинаем с недели, следующей за текущей)
#             out (i,t+1)=x (i,t+1);                            // создали начальные остатки по рядам
#             
#             out (i+2*nrow, j) = z(i,j);             // включаем блок с товарами в пути (нижняя треть таблицы)
#             
#             out (i,j) = out(i, j-1)-y(i,j)+z(i,j)+out(i+nrow,j-p(i,2)); // результирующее значение по остаткам на начало недели
#             //вычитаем расход за предыдущую неделю и добавляем сгенерированный заказ на lead time недель ранее
#             // также добавляем товары в пути, которые поступают на текущей неделе
#             //p(i,2) 5-я колонка в таблице параметров срок доставки
#             
#             
#             if (out(i,j) < p(i,0)*y(i,j) && j> t) {       //триггер "заказ"-"нет заказа"; p(i,3)  4-я колонка страховой запас в неделях (нумерация колонок с нуля)
#             out (i+nrow,j)= p(i,1)*y(i,j);                //генерирование величины заказа в зависимости от плана продаж; p(i,4) 5-я колонка размер заказа
#             }
#             else {
#             out (i+nrow,j) =0;                // значение "0", если остаток не меньше "страхового" запаса
#             
#             }
#             }                                       // конец обхода колонок
#             
#             }                                       // конец обхода рядов
#             
#             
#             
#             for (int i = 0; i < nrow; i++) {         // проход по рядам НОВЫй ЦИКЛ
#             
#             for (int j = t+1; j < 50; j++) {           // проход по колонкам в ряду для смещения сгенерированного заказа (t+1) !!
#             //на более ранний срок равный "сроку доставки"
#             
#             out (i+nrow,j) = out(i+nrow, j+(p(i,2)-1)); //  "p(i,2)" уменьшить еще на ЕДИНИЦУ?
#             out (i,j+1) = out(i, j-1)-y(i,j)+z(i,j)+out(i+nrow,j-p(i,2));  // добавить в левой части уравнения  out (...j+1), чтобы сохранить превоначальные остатки
#             
#             }
#             
#             }
#             
#             for (int r = t+2; r < ncol; r++) {         // ДОБАВЛЕННЫЙ ЦИКЛ проход по КОЛИЧЕСТВУ КОЛОНОК !!
#             
#             for (int i = 0; i < nrow; i++) {
#             
#             for (int j = t+2; j < 50; j++) {          // !!
#             
#             if (out(i,j+p(i,2)-1) < p(i,0)*y(i,j) && j> t+1 ) {                       //обнуление "лишних заказов"
#             out (i+nrow,j)= p(i,1)*y(i,j+p(i,2));  // в правой части стояло p(i,1)*y(i,j);
#             }
#             else {
#             out (i+nrow,j) =0;
#             
#             }
#             //            out (i+nrow,j) = out(i+nrow, j+(p(i,2)-1)); // !!!! добавлена строка
#             
#             out (i,j) = out(i, j-1)-y(i,j)+z(i,j)+out(i+nrow,j-p(i,2));
#             
#             }                                       // конец обхода колонок
#             
#             
#             }                                       // конец обхода рядов
#             
#             
#             } //ДОБАВЛЕННЫЙ ЦИКЛ
#             
#             
#             return out;                             // возврат вектора значений по всем рядам
#             
#             }')




# #при запуске и "смещении" номеров недель, приблажаясь к 52 неделе (переходить к "rat78"). Не запускать эту строку.
# rat<-stockTurnover(StockMatrix,SalesMatrix,OrdersMatrix, ParametersMatrix, time)







library (lubridate)


#текущий год






weekBegin<- data.frame(matrix(nrow=1, ncol=52))




#номер дня недели (функция из lubridate) для текущего года
# в строке ниже при смене года заменить на первое января наступающего года (в 2-х местах)


firstMonday<- as.Date(strptime("01.01.2016", format = "%d.%m.%Y"))-(wday(as.Date(strptime("01.01.2016", format = "%d.%m.%Y")))-2)


weekBegin[1]<-firstMonday+7




for (i in 1:51 ) {
  weekBegin[i+1] = weekBegin[i] +days(7)
}




#РЅРѕРјРµСЂ РјРµСЃСЏС†Р° РґР»СЏ РєР°Р¶РґРѕР№ РЅРµРґРµР»Рё
monthBegin<- data.frame(matrix(nrow=1, ncol=52))
for (i in 1:52 ) {
  
  monthBegin[1,i] <- month(as.POSIXlt(weekBegin[1,i], format="%d.%m.%Y"))
}


colnames(weekBegin)<-monthBegin[1,]


row<-as.data.frame(matrix(nrow=1,ncol=52))
row[is.na(row)]<-0






weekBegin3<- rbind(weekBegin [ ,1], row)










colnames (weekBegin3)<- colnames (weekBegin)


weekBegin3 [2,1] <-1


for (i in 2:52 ) {
  ifelse ( colnames(weekBegin3)[i] == colnames(weekBegin3)[i-1], weekBegin3 [2,i]<- weekBegin3 [2,i-1]+1, weekBegin3 [2,i]<-1)
  
}






for (i in 1:52 ) {
  weekBegin3[1,i] <- as.character(weekBegin[1, i])
}


# для приведения размерности в соответствие
twocolumns<-as.data.frame(matrix(nrow=2,ncol=2))


alltimeHeader<- cbind(twocolumns, weekBegin3)


#номера месяцев рассчитанных по дате понедельника
monthnumber<-as.data.frame(matrix(nrow=1,ncol=54))
monthnumber<-colnames(alltimeHeader)
alltimeHeader<- rbind(monthnumber, alltimeHeader)


colnames(alltimeHeader)<-c("code", "item.x", c(1:52))


# 
# получили "alltimeHeader" с указанием номера недели, номера месяца, даты понедельника, количества недель в месяце
# 
# 







#следующий год
weekBegin<- data.frame(matrix(nrow=1, ncol=52))




#номер дня недели (функция из lubridate) для текущего года
# в строке ниже при смене года заменить на первое января наступающего года (в 2-х местах)


firstMonday<- as.Date(strptime(paste("01.01.",year(Sys.Date()), sep=""), format = "%d.%m.%Y"))-(wday(as.Date(strptime(paste("01.01.",year(Sys.Date()), sep=""), format = "%d.%m.%Y")))-2)


weekBegin[1]<-firstMonday+7




for (i in 1:51 ) {
  weekBegin[i+1] = weekBegin[i] +days(7)
}




#РЅРѕРјРµСЂ РјРµСЃСЏС†Р° РґР»СЏ РєР°Р¶РґРѕР№ РЅРµРґРµР»Рё
monthBegin<- data.frame(matrix(nrow=1, ncol=52))
for (i in 1:52 ) {
  
  monthBegin[1,i] <- month(as.POSIXlt(weekBegin[1,i], format="%d.%m.%Y"))
}


colnames(weekBegin)<-monthBegin[1,]


row<-as.data.frame(matrix(nrow=1,ncol=52))
row[is.na(row)]<-0






weekBegin3<- rbind(weekBegin [ ,1], row)










colnames (weekBegin3)<- colnames (weekBegin)


weekBegin3 [2,1] <-1


for (i in 2:52 ) {
  ifelse ( colnames(weekBegin3)[i] == colnames(weekBegin3)[i-1], weekBegin3 [2,i]<- weekBegin3 [2,i-1]+1, weekBegin3 [2,i]<-1)
  
}






for (i in 1:52 ) {
  weekBegin3[1,i] <- as.character(weekBegin[1, i])
}


# для приведения размерности в соответствие
twocolumns<-as.data.frame(matrix(nrow=2,ncol=2))


alltimeHeaderNextYear<- cbind(twocolumns, weekBegin3)


#номера месяцев рассчитанных по дате понедельника
monthnumber<-as.data.frame(matrix(nrow=1,ncol=54))
monthnumber<-colnames(alltimeHeaderNextYear)
alltimeHeaderNextYear<- rbind(monthnumber, alltimeHeaderNextYear)


colnames(alltimeHeaderNextYear)<-c("code", "item.x", c(1:52))






# 
# получили "alltimeHeader" с указанием номера недели, номера месяца, даты понедельника, количества недель в месяце
# 
# 






#Sys.sleep(5)

























alltimeHeader <- alltimeHeader[ ,3:ncol(alltimeHeader)]
alltimeHeaderNextYear <- alltimeHeaderNextYear[ ,3:ncol(alltimeHeaderNextYear)]



# БЛОК первоначальный
# time <-45 #присваиваем номер недели для примера
# num<-c (((time+2)-20):52, 1:((time+2)-21))
# in1<-match(1,num)
# #in52<-match(52,num)
# num[1:(in1-1)]<-c((52-(in1-2)):52)
# weeklist<-num[1:52]

time <-weekReport #присваиваем номер недели для примера (45)
num<-c (((time+2)-25):52, 1:((time+2)-26))
in1<-match(1,num)
#in52<-match(52,num)
num[1:(in1-1)]<-c((52-(in1-2)):52)
weeklist<-num[1:52]



#формируем "заготовку" в загловках колонок номера недель
alltimeHeaderMix<- alltimeHeader #общий график на 2 года
colnames(alltimeHeaderMix)<- c(weeklist)
alltimeHeaderMix [ , ]<-0


endYear<- ifelse(match(52,weeklist)>= match(time,weeklist),
                 thisyear<-c(match(time,weeklist):match(52,weeklist)),
                 thisyear<-c(match(time,weeklist):length(weeklist), c(1:match(52,weeklist))))


#alltimeHeaderMix <- alltimeHeaderMix[ ,3:ncol(alltimeHeaderMix)]




# end недели, относящиеся к текущему году
# setdiff(num, end)  недели, относящиеся к следующему году
# alltimeHeaderMix[ ,thisyear+2] <- alltimeHeader [ , names (alltimeHeaderMix) [thisyear+2]]
# alltimeHeaderMix[ ,setdiff(as.numeric(names(alltimeHeaderMix)[3:ncol(alltimeHeaderMix)]), thisyear+2) ] <- alltimeHeaderNextYear [ , match(nextweeknum, colnames(alltimeHeaderNextYear))]


#приравниваем значения к номерам недель ТЕКУЩЕГО года
for (i in thisyear ) {
  #номер недели определяем по номеру колонки
  weeknum<-as.numeric(names (alltimeHeaderMix) [ i]) #+2
  
  alltimeHeaderMix [ ,i]<- alltimeHeader[ ,match(weeknum, names(alltimeHeader))] #
}




#номера колонок, которым надо присвоит значения следующего года
nextcolnum <- setdiff(as.numeric(names(alltimeHeaderMix)), thisyear) #+2


#приравниваем значения к номерам недель СЛЕДУЮЩЕГО года
for (i in nextcolnum ) {
  #номер недели определяем по номеру колонки
  nextweeknum<-as.numeric(names(alltimeHeaderMix)[1:ncol(alltimeHeaderMix)][i])
  
  alltimeHeaderMix [ ,i]<- alltimeHeaderNextYear[ ,match(nextweeknum, names(alltimeHeaderNextYear))] #
}





alltimeHeader <- alltimeHeaderMix






#
#  Корректный вариант для ПКИ
#


StockMatrix2 <- StockMatrix
SalesMatrix2 <- SalesMatrix
OrdersMatrix2 <- OrdersMatrix

for (i in 1:ncol(OrdersMatrix) ) {
  
  colnames (StockMatrix2) <- colnames(alltimeHeaderMix)
  StockMatrix2 [ ,match(i, colnames(StockMatrix2))] <- StockMatrix[ , match(i, colnames(StockMatrix))]
  
  
  colnames (SalesMatrix2) <- colnames(alltimeHeaderMix)
  SalesMatrix2 [ ,match(i, colnames(SalesMatrix2))] <- SalesMatrix[ , match(i, colnames(SalesMatrix))]
  
  
  colnames (OrdersMatrix2) <- colnames(alltimeHeaderMix)
  OrdersMatrix2 [,match(i, colnames(OrdersMatrix2))] <- OrdersMatrix[ , match(i, colnames(OrdersMatrix))]
  
  
}

StockMatrix <- StockMatrix2
SalesMatrix <- SalesMatrix2
OrdersMatrix <- OrdersMatrix2

# StockMatrix [c(1:1423),c(1:52)]<- 1
# SalesMatrix [c(1:1423),c(1:52)]<- 1
# OrdersMatrix [c(1:1423),c(1:52)]<- 1
# dim(StockMatrix)
# dim(SalesMatrix)
# dim(OrdersMatrix)

# write.csv(StockMatrix, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/StockMatrix.csv", row.names= FALSE)
# write.csv(SalesMatrix, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/SalesMatrix.csv", row.names= FALSE)
# write.csv(OrdersMatrix, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/OrdersMatrix.csv", row.names= FALSE)

StockMatrix <- round(StockMatrix, digits = 3)
SalesMatrix <- round(SalesMatrix, digits = 3)
OrdersMatrix <- round(OrdersMatrix, digits = 3)


# на 2 меньше, чем колонка с текущей неделей
time<-match(weekReport, colnames(alltimeHeader)) -2

#rat<-stockTurnover(StockMatrix,SalesMatrix,OrdersMatrix, ParametersMatrix, time)
rat<-stockTurnover(StockMatrix,SalesMatrix,OrdersMatrix, ParametersMatrix, time)



NamesMatrix<-as.data.frame(matrix(nrow=nrow(rat), ncol=2))
NamesMatrix<- weeklyStock[rep(row.names(weeklyStock), 3), 1:2]

statusMatrix<-as.data.frame(matrix(nrow=nrow(rat), ncol=1))
statusMatrix[1:nrow(rat)/3, ]<-"остатки"
statusMatrix[nrow(rat)/3+2:nrow(rat)/3*2, ]<-"заказать"
statusMatrix[(nrow(rat)/3*2+1):nrow(rat), ]<- "в пути"


report<-cbind(NamesMatrix,statusMatrix, rat)

# округляем
report [ 4: ncol(report)] <- round (report [ 4: ncol(report)], digits = 0)


#присвоить значения колонкам
names(report) [4:ncol(report)]<-names(alltimeHeaderMix)
currrentWeek<-which(colnames(report)%in% weekReport)
report<-  report[ , c(1,2, 3, c(currrentWeek:(currrentWeek+20)))]



# 
# конец блока "перехода года" для ПКИ
#















write.csv(report, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/report.csv")

# 
report$sum<- rowSums (report [, 4:8]) 
#report[report=="0"]<- ""
reportOrders<-report  [ (nrow(rat)/3+2) :(nrow(rat)/3*2), ]


# library(dplyr)    
# reportOrdersFilter<- filter(reportOrders, sum > 1)

reportOrdersFilterWeek <- subset(reportOrders, reportOrders [ ,4] > 1) [ , c(1:4)]
reportOrdersFilter <- subset(reportOrders, sum > 1) [ , c(1:8)]

report[report=="0"]<- ""
reportOrdersFilter[reportOrdersFilter=="0"]<- ""

row.names(reportOrdersFilter) <- NULL
row.names(reportOrdersFilterWeek) <- NULL


write.csv(reportOrdersFilter, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/OrdersFilter.csv")
write.csv(reportOrdersFilterWeek, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/reportOrdersFilterWeek.csv")



# отчет по "излишкам" ОС, вложенных в запасы
# загружаем данные по себестоимости
extraStock<- read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/себестоимость.xls")
extraStock<- extraStock [1:nrow(extraStock), c(2,3,ncol(extraStock))]
colnames(extraStock)<- c("code", "name", "price")
extraStock<- extraStock [4:nrow(extraStock), ]

extraStock [,1]<- as.numeric (extraStock [,1])
bg<-  report[1:(nrow(report)/3), c(1,2,4) ]

extraMoney<-merge(bg, extraStock, by ="code", all.x= TRUE)
extraMoney [ ,5]<- as.numeric(extraMoney[ ,5])

extraMoney$Стоимость <- as.numeric(extraMoney[ ,3]) * extraMoney [ ,5]

# 
surplusConsumption <- read.csv ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/потребление ПКИ 1.csv" )

extraMoney<-merge(extraMoney, surplusConsumption, by ="code", all.x= TRUE)

extraMoney<- extraMoney [ , c (1,2,3,5,6,8) ]
extraMoney[is.na(extraMoney)]<-0.001
colnames(extraMoney) <- c("code", "наименование", "остаток", "цена", "стоимость", "потребление" )
extraMoney$остаток <- as.numeric(extraMoney$остаток)
extraMoney$потребление <- as.numeric(extraMoney$потребление)

extraMoney$оборачиваемость <- as.numeric(extraMoney$остаток)/ as.numeric(extraMoney$потребление)

#стоимость запасов превышающих 2 месяца потребления
extraMoney$излишек <- round((extraMoney$остаток- extraMoney$потребление * 2) * extraMoney$цена)


extraMoney<- extraMoney [order(-extraMoney$излишек),] 
extraMoney[is.na(extraMoney)]<-0

# выделяем только "реальные" излишки
extraMoneyFlter <- subset(extraMoney, extraMoney$излишек > 0) 


write.csv(extraMoneyFlter, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/extramoney.csv")

#общая сумма излишков
sum(extraMoneyFlter$излишек)

# бюджет на закупку

price <- extraMoney [ ,c(1,2,4)]

# таблица с планами на закупу и ценами
rr <- merge(report [(nrow(rat)/3+2):(nrow(rat)/3*2+1), ], price, by ="code", all.x= TRUE)
ll<-as.data.frame(rr[ , 4:(ncol(rr)-3)])

# преобразовываем в числовой формат
for (i in 1:ncol(ll) ) {
  ll[ ,i] <- as.numeric(ll[ ,i])
}


budget<- ll* round(rr [ , ncol(rr)])

budget <- cbind(rr[,1:2], budget)
budget[is.na(budget)]<-0
totalcolsum<-  c("", colSums(Filter(is.numeric, budget)))
totalcolsum<-formatC(as.numeric(totalcolsum[3:length(totalcolsum)]), format="d", big.mark=',')

totalcolsum<-c("", "Сумма выплат поставщикам за неделю ",totalcolsum)

budget<- rbind (totalcolsum, budget)





write.csv(budget, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/budget.csv")






#
# Подготовить отчеты по закупкам
# остатки, план продаж, товары в пути, страховой запас, время в пути
# полуфабрикаты 
sm<-as.data.frame(matrix(nrow=nrow(report)/3), ncol=1)
sm [,]<- "потребление"
dadada<-cbind(report[1:(nrow(report)/3) , c(1:2)], sm, SalesMatrix[ ,c((time+2):(time+22))])
colnames(dadada)[4:ncol(dadada)]<-colnames(report[4:(ncol(report)-1)])
reportAll <- rbind(report [c(1:(nrow(report)/3)) ,c(1:(ncol(report)-1))],#первая часть сводной таблицы с отатками
                   dadada, #потребление
                   report [c( (nrow(report)/3+1):nrow(report) ) ,c(1:(ncol(report)-1))]) #часть сводной таблицы с заказать и товарами в пути
reportAll$id  <- c(1:nrow(reportAll))
reportTotal<- merge (reportAll, param, by ="code", all.x= TRUE)

reportTotal[reportTotal==""]<- 0
reportTotal[is.na(reportTotal)]<-0

for (i in 4:24 ) {
    reportTotal [ ,i] <- as.numeric(reportTotal [ ,i]) #  все преобразовываем в цифровой формат
}


reportTotal [ ,c(4:24)]<- round (reportTotal [ ,c(4:24)], digits = 0)

colnames(reportTotal) [31] <- "поставщик"

reportTotal<-reportTotal[order(reportTotal$code,reportTotal$id),]
reportTotal<- reportTotal[ ,c(1:3,27:31,4:24)]

colnames(reportTotal) <- c("код", "наименование", "статус", "мин. партия, шт", "страх. запас, нед", "срок доставки, нед", "партия для произ-ва, нед", "поставщик", colnames(reportTotal)[9:29])

write.csv(reportTotal, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/reportTotal.csv", row.names= FALSE, fileEncoding = "UTF-8")

