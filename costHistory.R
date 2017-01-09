### 
### 
### сводная динамика

#перечень архивных папок для составления графика динамики изменения неликвидных позиций
library(readxl)
library(stringr)

flist <- list.files("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/Архив/", full.names = TRUE)
flist<-as.data.frame(flist)

setwd("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/Архив/")

costs<-read_excel("C:/Documents and Settings/smirnov/Мои документы/Максим/R план/себестоимость.xls", skip =3)
costs<-costs[,c(2,3,16)]
colnames(costs)<- c("Код", "Наименование", "Себестоимость")

costs[,3]<- as.numeric(costs[,3])


###
### i=181
###
# Формирование сводной таблицы по денежным остаткам в неликвидах на каждую дату
# скрипт пропускает даты, в которых исходный отчет не сформирован
costsHistory<- data.frame(costs[,1])
colnames(costsHistory)<- "Код"

system.time(

for (i in 1:nrow(flist)) {
  #ERROR HANDLING
  possibleError <- tryCatch(
    
    u<-read_excel(paste(flist[i,1], "/", "себестоимость",  ".xls",  sep=""), skip =3),

    error=function(e) e

    
  )

  
  if(inherits(possibleError, "error")) next
  #print(i) 
  #print(str_sub(flist [i,1], start= -8))
  
  
  #REAL WORK
  #surplusHistory <- read.csv(paste(flist[i,1], "/", "surplusBranch",  ".csv",  sep=""))
  u<-u[,c(2,3,16)]
  colnames(u)<- c("Код", "Наименование", "Себестоимость")
  u[,3]<- as.numeric(u[,3])
  
  colnames (u) [3]<- str_sub(flist [i,1], start= -8) # присваем дату в качестве названия колонки с количеством за эту дату
  r<-as.data.frame( u [ ,c(1,3)])
  names(r) <- names (u) [c(1,3)]
  costsHistory<- merge(costsHistory, r, by ="Код", all.x= TRUE);
  # surplusHistory <- surplusHistory [ , c(1:ncol(surplusHistory)-2,ncol(surplusHistory))]
}  #end for

)
costsHistory<- merge(costsHistory, costs, by ="Код", all.x= TRUE)
costsHistory<-costsHistory[,c(1,ncol(costsHistory)-1,c(2:(ncol(costsHistory)-2)))]



costsHistory$ИзменениеДень<-round(((costsHistory[,ncol(costsHistory)]/costsHistory[,(ncol(costsHistory)-1)])-1)*100, digits =2)
costsHistory$ИзменениеНеделя<-round(((costsHistory[,ncol(costsHistory)-1]/costsHistory[,(ncol(costsHistory)-6)])-1)*100, digits =2)
costsHistory$ИзменениеМесяц<-round(((costsHistory[,ncol(costsHistory)-2]/costsHistory[,(ncol(costsHistory)-24)])-1)*100, digits =2)
costsHistory$ИзменениеПолгода<-round(((costsHistory[,ncol(costsHistory)-3]/costsHistory[,(ncol(costsHistory)-102)])-1)*100, digits =2)
costsHistory$ИзменениеГод<-round(((costsHistory[,ncol(costsHistory)-4]/costsHistory[,(ncol(costsHistory)-172)])-1)*100, digits =2)

costsHistory[is.na(costsHistory)]<-0
#costsHistory$отклонениеНормы<-sapply(costsHistory$ИзменениеДень, function(x) length(x[x<(-30)]))
costsHistory$отклонениеНормы<-sapply(costsHistory$ИзменениеДень, function(x) ifelse(x>0.5, 1,0))+
  sapply(costsHistory$ИзменениеНеделя, function(x) ifelse(x>1, 1,0))+
  sapply(costsHistory$ИзменениеМесяц, function(x) ifelse(x>2, 1,0))+
  sapply(costsHistory$ИзменениеПолгода, function(x) ifelse(x>5, 1,0))+
  sapply(costsHistory$ИзменениеГод, function(x) ifelse(x>10, 1,0))

costsHistoryFilter<-subset(costsHistory, costsHistory$отклонениеНормы > 0) [ , c(1,2,180:196)]

write.csv(costsHistory, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/costsHistory.csv", row.names=TRUE)
write.csv(costsHistoryFilter, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/costsHistoryFilter.csv", row.names=TRUE)












costsHistlatest<-costsHistory[,c(1,2,180:191)]

# ----------------------------------------------------------------------

#заполним все папки с наблюдениями одинаковым отчетом для работы скрипта
i<-181

for (i in 1:(nrow(flist)-2)) {
  path<-setwd(paste(as.character(flist[nrow(flist)-2,1]), "/", sep=""   )) 
  setwd('C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/Архив/2016-01-02/')
  
  
  write.csv(costs, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/surplusHistory.csv", row.names=TRUE)
  
  
library(WriteXLS)
WriteXLS(costs, ExcelFileName = costs, SheetNames = NULL, perl = "perl",
         verbose = FALSE, Encoding = c("UTF-8" ),
         row.names = FALSE, col.names = TRUE,
         AdjWidth = FALSE, BoldHeaderRow = TRUE,
        # na = "",
         FreezeRow = 1, FreezeCol = 8,
         envir = parent.frame())

}









## подготовка к графическому отображению
costsHistory <- t(costsHistory)
colnames(costsHistory) <- costsHistory [1 ,]
costsHistory <- costsHistory [-1 ,]


costsHistory <- as.matrix(costsHistory)
mode(costsHistory) <- "numeric"
costsHistory <- as.data.frame(costsHistory)



write.csv(surplusHistory, "C:/Documents and Settings/smirnov/Мои документы/Максим/R план/Полученные таблицы для расчетов/surplusHistory.csv", row.names=TRUE)
