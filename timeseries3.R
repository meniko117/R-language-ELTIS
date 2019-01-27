# условие задачи

#Please download this CVS file. It contains data in the following format:

#Time,PRICE,SIZE,EXCHANGE

#10:00:00.009,135.14,100,V

#The line above should be read as 100 shares were sold for $135.14 at 10:00:00.009 at the exchange "V".

#Please find the one second-window during which the largest number of trades took place. Do the same considering trades for each exchange separately.

#Your code will be evaluated based on its correctness, performance/computational complexity, and readability (feel free to comment your code and/or write a description).

#решение

start.time <- Sys.time() # измерение времени работы скрипта

library(stringr) # библиотека для работы со строками
library (Rcpp) # библиотека для включения кода на C++
library (anytime) # библиотека для преобразования форматов времени

#загружаем файл с данными
dataset<-read.csv("/Users/apple/Documents/trd.csv", header = FALSE, sep= ",", na.strings=c("", NA), stringsAsFactors=FALSE) 

# включаем колонку с цифровым timestamp
dataset$time<- as.numeric(strptime(dataset[,1], format="%H:%M:%OS")) 




# конвертация цифрового timestamp с миллисекундами обратно
# сразу становится видно, что, например,  встроках № 2, 6, 7 обратная конвертация дала timestamp  с ошибкой в 1 мс
# параметр OS3 при выборе форматирования указывает количество знаков при округлении до миллисекунд
# если подставить OS4, то видно, что в строках № 2, 6, 7 на 0.1 мс меньше, что система не учитывает при округлении
# с учетом вышесказанного в строке 111 кода ниже приведено решение, чтобы потерь 1 мс не происходило при отображении резлуьтата
dataset$timemillisec<-format(anytime(as.numeric(strptime(dataset[,1], "%H:%M:%OS"))), format = "%H:%M:%OS4") 







#функция на С++ подсчета количества сделок за 1 сек
cppFunction('NumericVector tradesNumber(NumericVector x) {
            int nrow = x.size(); // количество элементов в функцию передаваемом векторе
            NumericVector out(3); // вектор с 3-мя результирующими значениями 
            int count,  maxTrades, timeStart, timeEnd; // вводим переменные: счетчик, макс. кол-во сделок, начало периода, конец периода
            
            
             
            
            for (int i = 0; i < nrow; i++) {          // проходим циклом по вектору со значениями timestamp
                      count = 0;                      // обнуляем счетчик сделок при переходе к следущему значению timestamp
            for (int j = 1; j < nrow - i; j++) {      //  счетчик расширения границы массива, пока длится 1 сек (999 мс)
           
            
            if (x(i) + 999 >= x(i + j)) {              /* проверяем вышло ли включенное циклом j++ значение timestamp из интервала 1 сек, добавляя 999 мс к 
                                                       * timestamp начала интервала*/
            count = j + 1;                            // счетчик количества сделок
            }
            

            if (maxTrades < count) {         // сравниваем значение количества сделок для каждого нового интервала длительностью 1 сек
            timeStart = x(i);                // фиксируем начало интервала подсчета
            maxTrades = count;               // обновляем значение переменной максимального количества сделок
            timeEnd = x(i) + 999;            // фиксируем конец интервала подсчета, т.е. получаем именно
                                             // timestamp завершения секунды, а не timestamp
                                             // последней сделки в интервале
            }
            
            // присваиваем полученные значения элементам в результирующем векторе
            out(0) = timeStart;   
            out(1) = timeEnd;
            out(2) = maxTrades;
                    }
                }
            
            return out;
            }')






# получаем список символов бирж
exchSymb<-unique(dataset[,4])

# создаем пустой массив для вывода результатов
results<-data.frame(matrix(NA, nrow = length(exchSymb), ncol = 1))

#проходим циклом по всему массиву со всем сделками на всех биржах
for (i in 1:length(exchSymb) ) {
  
  exchData<- subset(dataset, dataset[,4] == exchSymb[i]) #сортируем массив по конкретной бирже
  exchDataTimeStamps<-exchData[,5] # сокращаем массив до вектора со значениями timestamp
  
  # преобразовываем timestamp значения элементов вектора для передачи целых чисел 
  #с учетом максимально допустимых для int в функцию, написанную на C++
  exchDataTimeStamps<-exchDataTimeStamps*1000-1533000000000 
  
  # вычисляем функцию, написанную на C++, получаем вектор с 3 элементами: начало периода, конец периода, количество сделок
  tt<-tradesNumber(exchDataTimeStamps)
  
  #делаем орбратное преобразование для чслового значения timestamp 
  timeStartNumeric<-(tt[1]+1533000000000)/1000
  
  
  # c учетом потери 0.1 мс в некоторых случаях из-за особенностей округления,
  # преобразовываем timestamp конкантинацией целой и дробной части timestamp
  timeStart<-paste(str_sub(format(anytime(timeStartNumeric), format = "%H:%M:%OS3"),start=1, end =-4), 
                  str_sub(timeStartNumeric, start=-3), sep="")
  
  # аналогичное преобразование делаем для конца периода
  timeEndNumeric<-(tt[2]+1533000000000)/1000
  timeEnd<-paste(str_sub(format(anytime(timeEndNumeric), format = "%H:%M:%OS3"),start=1, end =-4), 
                str_sub(timeEndNumeric, start=-3), sep="")
  
  # записываем полученные результаты в массив
  results[i,1]<-paste("Для брижи", exchSymb[i], "максимальное количество сделок в течение одной секунды было между", timeStart,  "и", timeEnd, ". В этот интервал прошло", tt[3], "сделок")
  
}

# выводим результаты на экран
results



#записываем полученные результаты в файл
write.csv(results,"/Users/apple/Documents/results.csv")

end.time <- Sys.time()

# расчет и вывод результатов измерения времени работы скрипта
time.taken <- round(end.time - start.time,2)
time.taken
