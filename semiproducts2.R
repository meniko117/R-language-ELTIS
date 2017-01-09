# обработчик потребностей полуфабрикатов

library(readxl)
semiProducts<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/semiproducts 2.xls")
colnames(semiProducts) <- c("код последнего этапа", "code", "наименование", "этапы", "остаток", "потребность", "заказать" )
# получили коды последних этапов в 1-й колонке. Потребление по этим кодам берем из 1С.
semiProducts$id  <- 1:nrow(semiProducts) # колонка вводится для сохранения порядка строк
# Потребление по остальным кодам нужно рассчитать
lastStageCode<-aggregate(semiProducts [ ,4] ~ semiProducts [ ,1], semiProducts, FUN = function(x) length(unique(x)))



#подставляем остатки по ПКИ
semiProducts<-merge(semiProducts, stockComponents, by ="code", all.x= TRUE, row.names = FALSE)
colnames(semiProducts)[10]<- "количество"
semiProducts<-semiProducts[order(semiProducts$id), ]


rownames(semiProducts) <- NULL
semiProducts[is.na(semiProducts)]<-0
# переставляем колонки с кодами
semiProducts <- semiProducts [ , c(2,1,3:10)]

semiProducts[ ,10][semiProducts[ ,10]== 0]<- 1 # нулевое количество не отображаеся после предачи в reshape2, поэтому замена на 1

semiProducts[ ,5] <- semiProducts[ ,10]








#подставляем потребление ПКИ (подставляются количества для всех компонентов)
# при запуске цикла "перебора" потребности по промежуточным этапам затираются промежуточными значениями
mu<-paste ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/потребление компонентов мес ", 
           ifelse(month(Sys.Date())<10,0,""),
           month(Sys.Date()), ".xls", sep="")


componentsConsumption<-read_excel(mu)
componentsConsumption<- componentsConsumption [, c(2,4,match("Требуется в изделия", componentsConsumption[2, ]))]
colnames(componentsConsumption) <- c("code", "item", "quantity")
componentsConsumption <- componentsConsumption [3:nrow(componentsConsumption), ]
componentsConsumption [ ,3] <- round(as.numeric(componentsConsumption [ ,3])) 

semiProducts2<-merge(semiProducts, componentsConsumption, by ="code", all.x= TRUE)
semiProducts2 [ ,6] <- semiProducts2 [ ,12] 
semiProducts2 <- semiProducts2[order(semiProducts2$id), ]

semiProducts [ ,6]<- semiProducts2 [ ,6]
semiProducts[is.na(semiProducts)]<-0





# подставляем остатки из общего отчета по остаткам по каждой позиции для полуфабрикатов 1-й группы
# делаем расчет по циклу
# рассчитываем "остаток - потребность" для полуфабрикатов 1-й группы и подставляем остаток в общий отчет по остаткам
# переходим к 2-й и далее группе
# делаем сводную таблицу по потребностям для каждого кода

library(reshape2)
code<-dcast(semiProducts, code ~ id, fun.aggregate=sum) #определеям для каждого кода индекс в таблице




# циклом перебрать все коды, которые относятся к коду последнего этапа 
# для каждого кода взять потребность последнего этапа и рассчитать потребность в предшествующих этапах,
# исходя из остатков каждого предшествующего этапа

#nrow(lastStageCode)
for (i in 1: nrow(lastStageCode)) { # запуск обработчика по количеству кодов последного этапа
  #nrow (lastStageCode)
  lastStage<-max(grep(lastStageCode [i,1], semiProducts[ ,1])) # строка относящаяся к последнему этапу кода полуфабриката
  firstStage<- min(grep(lastStageCode [i,1], semiProducts[ ,1])) # строка относящаяся к первому этапу кода полуфабриката
  
  semiProducts [lastStage, 7]<- ifelse (semiProducts [lastStage, 5]- semiProducts [lastStage, 6]<0, abs(semiProducts [lastStage, 5]- semiProducts [lastStage, 6]), 0)

  
  for (k in 1:(lastStage-firstStage)) { # обработка нужного количества строк, которые относятся к коду последнего полуфабриката
    
    #     ## доработка кода 
    #     # использовать поиск кода для присаивания ему нового результирующего значения, т.е. НЕ использовать merge
    # semiProducts [lastStage-k, 5] <- semiProducts [lastStage-k, 10]
    #   semiProducts<-merge(stockComponents, semiProducts, by ="code", all.x= TRUE, row.names = FALSE) # строку переместить в конце цикла
    #   
    #     ##
    
    
    semiProducts [lastStage-k, 6] <-  semiProducts [lastStage-(k-1), 7] # присаиваем потребность предыдущему этапу
    semiProducts [lastStage-k, 7] <-  ifelse(semiProducts [lastStage-k, 6] - semiProducts [lastStage-k, 5]>0,  semiProducts [lastStage-k, 6] - semiProducts [lastStage-k, 5], 0)
    # 
   
    # присваиваем новые остатки после расчета очередной группы полуфабркатов
    
    # определяем индекс вхождения кода в общем списке "semiproducts"
    line<- grep(semiProducts [lastStage-k,2], code[,1]) # опрделяем строку с индексами вхождения в таблице "code"
    listCodeId<- which (code[ line, ]> 0) -1 # получаем список индексов
    listCodeId <- listCodeId [c(2:length(listCodeId))] # пропускаем 1-е значение
    
    #записываем новые остатки и переходим к другой группе полуфабрикатов
    semiProducts [listCodeId,10] <- semiProducts [lastStage-k, 5] -  semiProducts [lastStage-k, 6]
    #записываем результирующие остатки по рассчитанной группе в "остатки" колонки "5" для расчета по др. группам
    semiProducts [ listCodeId,5] <-ifelse(semiProducts [ listCodeId,10]>0, semiProducts [ listCodeId,10], 0) #записываем новые остатки (после расчета группы полуфабрикатов)
    
    #ifelse(semiProducts [ listCodeId,10]>0, semiProducts [ listCodeId,10], 0)
    
  }

}



# суммировать для каждого кода необходимое количество товара для закупки
purchase<-aggregate(semiProducts$заказать, by=list(semiProducts$code), FUN=sum)
colnames(purchase) <- c("code", "купить")
purchase <- merge(purchase, param, by ="code", all.x= TRUE) #подставить к кодам текстовые наименования
purchase <- merge(purchase, stockComponents, by ="code", all.x= TRUE) #подставить остатки
purchase <- merge(purchase, componentsConsumption, by ="code", all.x= TRUE) #подставить потребление текущего месяца

purchase <- purchase [ , c(1,3,10,12,2)]
colnames(purchase) [c(3,4)]<- c("остатки", "потребление")
purchase[is.na(purchase)]<-0
write.csv(purchase, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/purchaseSemiProducts 1.csv")

















# взять остатки из таблицы semiproducts- это остатки на конец расчетного периода, по которому было взято потребление
# подставить эти остатки для расчета следующего месяца
# закачать потребность для следующего месяца
# повторить для месяца следующего за текущим
# получив потребность для всех этапов для каждого из 3-х месяцев, подставить ее в salesMatrix расчета всех ПКИ по стандартному алгоритму


#загружаем потребление на месяц, СЛЕДУЮЩИЙ ЗА ТЕКУЩИМ
mu<-paste ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/потребление компонентов мес ", 
           ifelse(month(Sys.Date())+1-12<10,0,""),
           ifelse(month(Sys.Date())+1<=12, month(Sys.Date())+1, month(Sys.Date())+1-12) , ".xls", sep="")
componentsConsumption<-read_excel(mu)


componentsConsumption<- componentsConsumption [, c(2,4,match("Требуется в изделия", componentsConsumption[2, ]))]
colnames(componentsConsumption) <- c("code", "item", "quantity")
componentsConsumption <- componentsConsumption [3:nrow(componentsConsumption), ]
componentsConsumption [ ,3] <- round(as.numeric(componentsConsumption [ ,3]))
componentsConsumption [ ,1] <- round(as.numeric(componentsConsumption [ ,1]))


semiProducts2<-merge(semiProducts, componentsConsumption, by ="code", all.x= TRUE)
semiProducts2 [ ,6] <- semiProducts2 [ ,12] 
semiProducts2 <- semiProducts2[order(semiProducts2$id), ]

semiProducts [ ,6]<- semiProducts2 [ ,6]
semiProducts[is.na(semiProducts)]<-0



#code<-dcast(semiProducts, code ~ id, fun.aggregate=sum) #определеям для каждого кода индекс в таблице




# циклом перебрать все коды, которые относятся к коду последнего этапа 
# для каждого кода взять потребность последнего этапа и рассчитать потребность в предшествующих этапах,
# исходя из остатков каждого предшествующего этапа


for (i in 1:nrow(lastStageCode) ) { # запуск обработчика по количеству кодов последного этапа
  #nrow (lastStageCode)
  lastStage<-max(grep(lastStageCode [i,1], semiProducts[ ,1])) # строка относящаяся к последнему этапу кода полуфабриката
  firstStage<- min(grep(lastStageCode [i,1], semiProducts[ ,1])) # строка относящаяся к первому этапу кода полуфабриката
  
  semiProducts [lastStage, 7]<- ifelse (semiProducts [lastStage, 5]- semiProducts [lastStage, 6]<0, abs(semiProducts [lastStage, 5]- semiProducts [lastStage, 6]), 0)
  
  
  for (k in 1:(lastStage-firstStage)) { # обработка нужного количества строк, которые относятся к коду последнего полуфабриката
    
    #     ## доработка кода 
    #     # использовать поиск кода для присаивания ему нового результирующего значения, т.е. НЕ использовать merge
    # semiProducts [lastStage-k, 5] <- semiProducts [lastStage-k, 10]
    #   semiProducts<-merge(stockComponents, semiProducts, by ="code", all.x= TRUE, row.names = FALSE) # строку переместить в конце цикла
    #   
    #     ##
    
    
    semiProducts [lastStage-k, 6] <-  semiProducts [lastStage-(k-1), 7] # присаиваем потребность предыдущему этапу
    semiProducts [lastStage-k, 7] <-  ifelse(semiProducts [lastStage-k, 6] - semiProducts [lastStage-k, 5]>0,  semiProducts [lastStage-k, 6] - semiProducts [lastStage-k, 5], 0)
    # 
    
    # присваиваем новые остатки после расчета очередной группы полуфабркатов
    
    # определяем индекс вхождения кода в общем списке "semiproducts"
    line<- grep(semiProducts [lastStage-k,2], code[,1]) # опрделяем строку с индексами вхождения в таблице "code"
    listCodeId<- which (code[ line, ]> 0) -1 # получаем список индексов
    listCodeId <- listCodeId [c(2:length(listCodeId))] # пропускаем 1-е значение
    
    #записываем новые остатки и переходим к другой группе полуфабрикатов
    semiProducts [listCodeId,10] <- semiProducts [lastStage-k, 5] -  semiProducts [lastStage-k, 6]
    #записываем результирующие остатки по рассчитанной группе в "остатки" колонки "5" для расчета по др. группам
    semiProducts [ listCodeId,5] <-ifelse(semiProducts [ listCodeId,10]>0, semiProducts [ listCodeId,10], 0) #записываем новые остатки (после расчета группы полуфабрикатов)
    
    #ifelse(semiProducts [ listCodeId,10]>0, semiProducts [ listCodeId,10], 0)
    
  }
  
}



# суммировать для каждого кода необходимое количество товара для закупки
purchase<-aggregate(semiProducts$заказать, by=list(semiProducts$code), FUN=sum)
colnames(purchase) <- c("code", "купить")
purchase <- merge(purchase, param, by ="code", all.x= TRUE) #подставить к кодам текстовые наименования
purchase <- merge(purchase, stockComponents, by ="code", all.x= TRUE) #подставить остатки
purchase <- merge(purchase, componentsConsumption, by ="code", all.x= TRUE) #подставить потребление текущего месяца

purchase <- purchase [ , c(1,3,10,12,2)]
colnames(purchase) [c(3,4)]<- c("остатки", "потребление")
purchase[is.na(purchase)]<-0
write.csv(purchase, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/purchaseSemiProducts 2.csv")



















#загружаем потребление через 2 МЕСЯЦА
mu<-paste ("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Отчеты для расчетов/потребление компонентов мес ", 
           ifelse(month(Sys.Date())+2-12<10,0,""),
           ifelse(month(Sys.Date())+2<=12, month(Sys.Date())+2, month(Sys.Date())+2-12), ".xls", sep="")
componentsConsumption<-read_excel(mu)


componentsConsumption<- componentsConsumption [, c(2,4,match("Требуется в изделия", componentsConsumption[2, ]))]
colnames(componentsConsumption) <- c("code", "item", "quantity")
componentsConsumption <- componentsConsumption [3:nrow(componentsConsumption), ]
componentsConsumption [ ,3] <- round(as.numeric(componentsConsumption [ ,3]))
componentsConsumption [ ,1] <- round(as.numeric(componentsConsumption [ ,1]))

semiProducts2<-merge(semiProducts, componentsConsumption, by ="code", all.x= TRUE)
semiProducts2 [ ,6] <- semiProducts2 [ ,12] 
semiProducts2 <- semiProducts2[order(semiProducts2$id), ]

semiProducts [ ,6]<- semiProducts2 [ ,6]
semiProducts[is.na(semiProducts)]<-0



#code<-dcast(semiProducts, code ~ id, fun.aggregate=sum) #определеям для каждого кода индекс в таблице




# циклом перебрать все коды, которые относятся к коду последнего этапа 
# для каждого кода взять потребность последнего этапа и рассчитать потребность в предшествующих этапах,
# исходя из остатков каждого предшествующего этапа


for (i in 1:nrow(lastStageCode) ) { # запуск обработчика по количеству кодов последного этапа
  #nrow (lastStageCode)
  lastStage<-max(grep(lastStageCode [i,1], semiProducts[ ,1])) # строка относящаяся к последнему этапу кода полуфабриката
  firstStage<- min(grep(lastStageCode [i,1], semiProducts[ ,1])) # строка относящаяся к первому этапу кода полуфабриката
  
  semiProducts [lastStage, 7]<- ifelse (semiProducts [lastStage, 5]- semiProducts [lastStage, 6]<0, abs(semiProducts [lastStage, 5]- semiProducts [lastStage, 6]), 0)
  
  
  for (k in 1:(lastStage-firstStage)) { # обработка нужного количества строк, которые относятся к коду последнего полуфабриката
    
    #     ## доработка кода 
    #     # использовать поиск кода для присаивания ему нового результирующего значения, т.е. НЕ использовать merge
    # semiProducts [lastStage-k, 5] <- semiProducts [lastStage-k, 10]
    #   semiProducts<-merge(stockComponents, semiProducts, by ="code", all.x= TRUE, row.names = FALSE) # строку переместить в конце цикла
    #   
    #     ##
    
    
    semiProducts [lastStage-k, 6] <-  semiProducts [lastStage-(k-1), 7] # присаиваем потребность предыдущему этапу
    semiProducts [lastStage-k, 7] <-  ifelse(semiProducts [lastStage-k, 6] - semiProducts [lastStage-k, 5]>0,  semiProducts [lastStage-k, 6] - semiProducts [lastStage-k, 5], 0)
    # 
    
    # присваиваем новые остатки после расчета очередной группы полуфабркатов
    
    # определяем индекс вхождения кода в общем списке "semiproducts"
    line<- grep(semiProducts [lastStage-k,2], code[,1]) # опрделяем строку с индексами вхождения в таблице "code"
    listCodeId<- which (code[ line, ]> 0) -1 # получаем список индексов
    listCodeId <- listCodeId [c(2:length(listCodeId))] # пропускаем 1-е значение
    
    #записываем новые остатки и переходим к другой группе полуфабрикатов
    semiProducts [listCodeId,10] <- semiProducts [lastStage-k, 5] -  semiProducts [lastStage-k, 6]
    #записываем результирующие остатки по рассчитанной группе в "остатки" колонки "5" для расчета по др. группам
    semiProducts [ listCodeId,5] <-ifelse(semiProducts [ listCodeId,10]>0, semiProducts [ listCodeId,10], 0) #записываем новые остатки (после расчета группы полуфабрикатов)
    
    #ifelse(semiProducts [ listCodeId,10]>0, semiProducts [ listCodeId,10], 0)
    
  }
  
}



# суммировать для каждого кода необходимое количество товара для закупки
purchase<-aggregate(semiProducts$заказать, by=list(semiProducts$code), FUN=sum)
colnames(purchase) <- c("code", "купить")
purchase <- merge(purchase, param, by ="code", all.x= TRUE) #подставить к кодам текстовые наименования
purchase <- merge(purchase, stockComponents, by ="code", all.x= TRUE) #подставить остатки
purchase <- merge(purchase, componentsConsumption, by ="code", all.x= TRUE) #подставить потребление текущего месяца

purchase <- purchase [ , c(1,3,10,12,2)]
colnames(purchase) [c(3,4)]<- c("остатки", "потребление")
purchase[is.na(purchase)]<-0
write.csv(purchase, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/purchaseSemiProducts 3.csv")

