source('~/Максим/R план/merge tables.R')
source('~/Максим/R план/finished goods.R')
source('~/Максим/R план/dates.R')
source('~/Максим/R план/sales analytics.R')












source('~/Максим/R план/Surplus cost and turnover.R')
#source('~/Максим/R план/sales analytics.R')
source('~/Максим/R план/semiproducts2.R')
source('~/Максим/R план/YearTransition.R')



library(shiny)
runExample("01_hello")


library(shiny)
runExample("Analytics")

library(shiny)
runExample("ELTIS build")

library(shiny)
runExample("ELTIS dashboard2") #основной

library(shiny)
runExample("ELTIS dashboard2/версия")



# запуск системы 
library(shiny)
runExample("ELTIS dashboard2",host="192.168.23.110",port=5050)


source('~/Максим/R план/merge tables.R')
source('~/Максим/R план/finished goods.R')
source('~/Максим/R план/dates.R')
source('~/Максим/R план/Surplus cost and turnover.R')






runApp(host="0.0.0.0",port=5050)

runApp("ELTIS dashboard2",host="192.168.23.110")

runApp("../microplate",host=ip)

devtools::install_github('rstudio/shinyapps')

library(shiny)
runExample("01_hello")
b<- (3:7)


library(shiny)
runExample("01_hello")


devtools::install_github('rstudio/shinyapps')

if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rstudio/shinyapps")

devtools::install_github("rstudio/shinydashboard")

devtools::install_github('rstudio/DT')
