library(caret)

#загрузка тренировочного набора данных
dataset<-read.csv("C:/ Documents and Settings/smirnov/Мои документы/Максим/Разное/Data sample/train.csv", header = FALSE, sep= ";", na.strings=c("", NA), stringsAsFactors=FALSE) 

#присваиваем названия колонок
colnames(dataset)<-c("class", "param1", "param2", "param3")



# исключаем из обучения строки, где содержится неполная информация
dataset<-dataset[complete.cases(dataset), ]

# присваиваем тип данных "factor" для соответствующих колонок
dataset$param1<-as.factor( dataset$param1)
dataset$class<-as.factor( dataset$class)



# устанавливаем 10-блочную кросс-валидацию при ресемплинге
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#тренировка 5 моделей разными способами

# LDA
set.seed(7)
fit.lda <- train(class~., data=dataset, method="lda", metric=metric, trControl=control)

# CART
set.seed(7)
fit.cart <- train(class~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(class~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(class~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(class~., data=dataset, method="rf", metric=metric, trControl=control)




results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))

#вывод результатов по полученными моделям
summary(results)



# графическое сравнение точности моделей
dotplot(results)



# print(fit.lda)
# print(fit.svm)
# print(fit.knn)

# загружаем валидационный набор данных
validation<-read.csv("C:/ Documents and Settings/smirnov/Мои документы/Максим/Разное/Data sample/test.csv", header = FALSE, sep= ";", na.strings=c("", NA), stringsAsFactors=FALSE) 

colnames(validation)<-c(" class", "param1", "param2", "param3")

validation<-validation[ complete.cases(validation), ]

validation$param1<-as.factor( validation$param1)
validation$class<-as.factor( validation$class)


predictions <- predict(fit.svm, validation) # лучший показатель точности классификации

# вывод результатов по модели SVM, имеющей самую высокую точность
confusionMatrix(predictions, validation$class)

#
# Определение количества неверно классифицированных объектов в валидационной выборке и их индексов  
# 
#сопоставление предсказанных значений и фактических
testValidation<-as.data.frame( cbind(predictions,validation[, 1]))
#в колонке "dif" значения отличные от нуля указывают на ошибку классификации
testValidation$dif<-as.numeric(testValidation[,1]) - as.numeric(testValidation[,2])

#подсчет длины вектора с ошибками. Получилось 23 ошибки из 473 объектов , что составляет 95,14% точность, которую продемонстрировала модель
length(testValidation$dif[ testValidation$dif != 0])