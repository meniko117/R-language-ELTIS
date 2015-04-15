#ELTIS build

library(shiny)
library(googleVis)

shinyServer(function(input, output) {
  diamonds<- read.csv ("components5.csv")
  MSKdeliver<-read.csv ("MSK deliver.csv")
  topstock<-read.csv ("top stock.csv")
  
#  components<-read.csv("components5.csv")
  
  # a large table, reative to input$show_vars
  output$mytable1 <- renderDataTable({
    #library(ggplot2)
    diamonds [, input$show_vars, drop = FALSE]
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- renderDataTable({
    MSKdeliver
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- renderDataTable({
    MSKdeliver
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
  
output$mytable4 <- renderDataTable({
  MSKdeliver
}, options = list(lengthMenu = c(5, 30, 50), pageLength = 50))

  

output$mytable5 <- renderDataTable({
  topstock
}, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE ))
                                      
                                      
#                                       initComplete = JS(
#                                         "function(settings, json) {",
#                                         "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
#                                         "}")
                                      
                                      
                                      
                                      



















  
  
  output$plot1 <- renderPlot({
    slices <- c(10, 12, 4, 16, 8) 
    lbls <- c("US", "UK", "Australia", "Germany", "France")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(slices,labels = lbls, col=rainbow(length(lbls)),
        main="Pie Chart of Countries")
  })
  
  output$plot2 <- renderPlot({
    slices <- c(20, 22, 4, 36, 8) 
    lbls <- c("US", "UK", "Australia", "Germany", "France")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(slices,labels = lbls, col=rainbow(length(lbls)),
        main="Pie Chart of Countries")
  })
  
  output$plot3 <- renderPlot({
    slices <- c(50, 22, 4, 36, 18) 
    lbls <- c("US", "UK", "Australia", "Germany", "France")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(slices,labels = lbls, col=rainbow(length(lbls)),
        main="Pie Chart of Countries")
  })
  
  CityPopularity<- read.csv("payments.csv")
  output$google<-renderGvis({ gvisGauge(CityPopularity, 
                                        options=list(min=0, max=800, greenFrom=500,
                                                     greenTo=800, yellowFrom=300, yellowTo=500,
                                                     redFrom=0, redTo=300, width=150, height=150))
                                                            
  })
  
  goods<-read.csv("sales.csv")
  output$googleGoods<-renderGvis({ gvisGauge(goods, 
                                        options=list(min=0, max=800, greenFrom=500,
                                                     greenTo=800, yellowFrom=300, yellowTo=500,
                                                     redFrom=0, redTo=300, width=150, height=150))
                              
  })


# оборачиваемость
turnComp<-read.csv("turnover components.csv")
output$googleturnComp<-renderGvis({ gvisGauge(turnComp, 
                                           options=list(min=0, max=8, greenFrom=0,
                                                        greenTo=2, yellowFrom=2, yellowTo=4,
                                                        redFrom=4, redTo=8, width=150, height=150))
                                 
})





  
df=data.frame(country=c("Москва", "Санкт-Петербург", "Новосибирск"), 
              факт =c(40,45,60), 
              план =c(45,35,45))

output$googleSales <- renderGvis(gvisBarChart(df))
output$vertSales <- renderGvis(gvisColumnChart(df))
output$lineSales <- renderGvis(gvisLineChart(df))




topstockbars<-read.csv ("top stock bars.csv")
topstocknames<-read.csv ("top stock names.csv")
topstockqty<-read.csv ("top stock qty.csv")

# df2<-data.frame(country=topstocknames, 
#                  qty =topstockqty, 
#                  plan =topstockqty)
#   
  
df2<- read.csv ("top stock bars.csv")
df2<-as.data.frame(df2) 
  
output$topstockbars <- renderGvis(gvisColumnChart(df2, xvar="Name", yvar="Сумма_руб",
                                                  options=list(seriesType="bars", legend="top",
                                                               bar="{groupWidth:'60%'}",
                                                               width=450, height=450,
                                                               
                                                               colors="['green']",
                                                  chartid="thincolumns")))




Bubble <- gvisBubbleChart(df, idvar="Fruit", 
                          xvar="Sales", yvar="Expenses",
                          colorvar="Year", sizevar="Profit",
                          options=list(
                            hAxis='{minValue:75, maxValue:125}'))


output$googleItems <- renderGvis(gvisBubbleChart(df,options=list(
  hAxis='{minValue:0, maxValue:70}', vAxis='{minValue:0, maxValue:50}')))




dat <- data.frame(Room=c("DP400-RDC24","DP5000-KFDC42","VM500-CL", "B-21", "KM100-7.1", 
                         "SC5000-D1", "ELTIS VS1/4-4","DP400-TDC22","DP5000-KRDC42","VM500-CLM", "B-23", "KM100-7.3", 
                         "CRT-72", "PS2-DKV3"),
                  #Language=c("picking", "assembly", "packing"),
                  start=as.POSIXct(c("2014-03-14 14:00", 
                                     "2014-03-14 15:00",
                                     "2014-03-14 14:30",
                                     "2014-03-14 16:30",
                                     "2014-03-14 14:20",
                                     "2014-03-14 16:00",
                                     "2014-03-14 16:30",
                                     "2014-03-14 14:00", 
                                     "2014-03-14 15:00",
                                     "2014-03-14 14:30",
                                     "2014-03-14 16:05",
                                     "2014-03-14 14:20",
                                     "2014-03-14 16:00",
                                     "2014-03-14 15:45")),
                  end=as.POSIXct(c("2014-03-14 15:00", 
                                   "2014-03-14 16:00",
                                   "2014-03-14 15:30",
                                   "2014-03-14 17:10",
                                   "2014-03-14 15:10",
                                   "2014-03-14 16:45",
                                   "2014-03-14 18:45",
                                   "2014-03-14 15:00", 
                                   "2014-03-14 16:00",
                                   "2014-03-14 15:30",
                                   "2014-03-14 17:00",
                                   "2014-03-14 15:10",
                                   "2014-03-14 16:45",
                                   "2014-03-14 18:00")))
output$googleGantt <- renderGvis(
  gvisTimeline(data=dat, 
               rowlabel="Room", #barlabel="Language", 
               start="start", end="end", options=list(width=1200, height=1500, fontSize=9
               )))





mat <- data.frame(party=c("400 видео", "300 аудио", "трубки", "мониторы",
                          "5000", "mifare"),
                  members.of.parliament=c(193, 93, 44, 
                                          146, 76, 68))
library(googleVis)
## Doughnut chart - a pie with a hole
output$doughnut <- renderGvis(
  gvisPieChart(mat, 
               options=list(
                 width=450,
                 height=400,
                 slices="{0: {offset: 0.2},
                          1: {offset: 0.2},
                          2: {offset: 0.2}}",
#                 title='Доли ассортимента в обороте 1-й квартал 2015',
                 legend='none',
                 colors="['black','orange', 'blue', 
                        'red', 'purple', 'green']",
                 pieSliceText='label',
                 pieHole=0.5, is3D= 'true'),
               chartid="doughnut"))



#infobox dynamics
output$progressBox <- renderValueBox({
  valueBox(
    paste0(25 + input$count, "%"), "Прогресс", icon = icon("list"),
    color = "purple"
  )
})

output$approvalBox <- renderValueBox({
  valueBox(
    "80%", "Запуск в Технику", icon = icon("thumbs-up", lib = "glyphicon"),
    color = "yellow"
  )
})


df3 <- data.frame(Year=2013:2014, Sales=c(120, 130), 
                 Sales.interval.1=c(100,110), 
                 Sales.interval.2=c(140, 150),
                 Sales.interval.3=c(90, 100),
                 Sales.interval.4=c(150, 170),
                 Sales.style=c('red', 'gold'),
                 Sales.annotation=c("VM500-CL", "M500-CL"),
                 check.names=FALSE)

output$coolbars<-renderGvis(
  gvisBarChart(df3, xvar='Year', 
               yvar=c('Sales',                       
                      'Sales.interval.1', 
                      'Sales.interval.2',
                      'Sales.style',
                      'Sales.annotation')
  )
)








  
})


