#ELTIS build

library(shiny)
library(googleVis)


shinyServer(function(input, output) {
  diamonds<- read.csv ("report.csv")
  MSKdeliver<-read.csv ("arrivalPlanMSK.csv")
  SPbdeliver<-read.csv ("arrivalPlanSPb.csv")
  NSKdeliver<-read.csv ("arrivalPlanNSK.csv")
  OrdersFilter<- read.csv ("OrdersFilter.csv")
  OrdersFilterWeek<- read.csv ("reportOrdersFilterWeek.csv")
  ProductionPlan<- read.csv ("monthlyProductionPlan.csv")
  
  ComponentConsumption1 <- read.csv ("потребление ПКИ 1.csv")
  ComponentConsumption2 <- read.csv ("потребление ПКИ 2.csv")
  ComponentConsumption3 <- read.csv ("потребление ПКИ 3.csv")  
  
  stockFlowParameters <- read.csv ("stockFlowParameters.csv")  
  
  MSKreport<-read.csv ("reportMSK.csv")
  SPbreport<-read.csv ("reportSPb.csv")
  NSKreport<-read.csv ("reportNSK.csv")
  
  
  salesMoscow<-read.csv ("salesMoscow.csv")
  salesSPb<-read.csv ("salesSPb.csv")
  salesNSK<-read.csv ("salesNSK.csv")
  
  allParts<-read.csv ("allParts.csv")
  
  extramoney<- read.csv ("extramoney.csv")
  
  
  topstock<-read.csv ("top stock.csv")
  actselling<-read.csv ("actual_selling.csv")
  
  budget<- read.csv ("budget.csv")
  
  deficitMSKFilter <- read.csv ("deficitMSKFilter.csv")
  deficitSPbFilter <- read.csv ("deficitSPbFilter.csv")
  deficitNSKFilter <- read.csv ("deficitNSKFilter.csv")
  
  surplusMSKFilter<-  read.csv ("surplusMSKFilter.csv")
  surplusSPbFilter<-  read.csv ("surplusSPbFilter.csv")
  surplusNSKFilter<-  read.csv ("surplusNSKFilter.csv")
  
  soldplanMSK<- read.csv ("soldplanMSK.csv")
  soldplanSPb<- read.csv ("soldplanSPb.csv")
  soldplanNSK<- read.csv ("soldplanNSK.csv")
  salesAnalysis<- read.csv ("salesAnalysis.csv") [2:4] #убрать первую колонку с навзаниями для последующего корректного отображения
  
  
  
  MSKstockArch <- read.csv("MSKstockArch.csv")
  SPbstockArch <- read.csv("SPbstockArch.csv")
  NSKstockArch <- read.csv("NSKstockArch.csv")
  
  priorityProduction <- read.csv("priorityProduction.csv")
  
  
  #  components<-read.csv("components5.csv")
  
  # a large table, reative to input$show_vars
  output$mytable1 <- renderDataTable({
    diamonds [, input$show_vars, drop = FALSE]
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)) #options = list(orderClasses = TRUE, scrollY = '600px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  #list(lengthMenu = c(5, 30, 50), pageLength = 5)
  #[, input$show_vars, drop = FALSE]
  
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- renderDataTable({
    SPbdeliver
  }, options = list(orderClasses = TRUE, scrollY = '600px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- renderDataTable({
    NSKdeliver
  }, options = list(orderClasses = TRUE, scrollY = '600px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  # либо =list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
  
  output$mytable4 <- renderDataTable({
    MSKdeliver
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)) #добавили высоту таблицы и скролл вверх-вниз
  #list(lengthMenu = c(5, 30, 50), pageLength = 50))
  
  
  
  
  output$mytable5 <- renderDataTable({
    topstock
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE ))
  
  
  output$mytable6 <- renderDataTable({
    actselling
  }, options = list(lengthMenu = c(5, 30, 50), scrollX = '300px', pageLength = 5))
  
  #                                       initComplete = JS(
  #                                         "function(settings, json) {",
  #                                         "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #                                         "}")
  
  
  
  output$mytable7 <- renderDataTable({
    OrdersFilter
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  #list(orderClasses = TRUE, scrollY = '600px', paging = FALSE ))
  
  
  output$mytable8 <- renderDataTable({
    ProductionPlan
  }, options = list(orderClasses = TRUE, scrollY = '600px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  
  output$mytable9 <- renderDataTable({
    MSKreport
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  output$mytable10 <- renderDataTable({
    SPbreport
  }, options = list(orderClasses = TRUE, scrollY = '600px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  output$mytable11 <- renderDataTable({
    NSKreport
  }, options = list(orderClasses = TRUE, scrollY = '600px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  output$mytable12 <- renderDataTable({
    OrdersFilterWeek
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  
  output$mytable13 <- renderDataTable({
    ComponentConsumption1
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  
  output$mytable14 <- renderDataTable({
    ComponentConsumption2
  },  options = list(lengthMenu = c(5, 30, 50), pageLength = 5)) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  output$mytable15 <- renderDataTable({
    ComponentConsumption3
  },  options = list(lengthMenu = c(5, 30, 50), pageLength = 5)) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  output$mytable16 <- renderDataTable({
    stockFlowParameters
  },  options = list(lengthMenu = c(5, 30, 50), pageLength = 5)) #добавили высоту таблицы и скролл вверх-вниз
  
  
  output$mytable17 <- renderDataTable({
    salesMoscow
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)) #добавили высоту таблицы и скролл вверх-вниз
  
  
  output$mytable18 <- renderDataTable({
    salesSPb
  }, options = list(orderClasses = TRUE, scrollY = '600px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  output$mytable19 <- renderDataTable({
    salesNSK
  }, options = list(orderClasses = TRUE, scrollY = '600px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  output$mytable20 <- renderDataTable({
    allParts
  }, options = list(orderClasses = TRUE, scrollY = '600px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  
  
  output$mytable21 <- renderDataTable({
    extramoney
  }, options = list(orderClasses = TRUE, scrollY = '600px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  output$mytable22 <- renderDataTable({
    budget
  }, options = list(orderClasses = TRUE, scrollY = '600px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  output$mytable23 <- renderDataTable({
    deficitMSKFilter
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  output$mytable24 <- renderDataTable({
    deficitSPbFilter
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  output$mytable25 <- renderDataTable({
    deficitNSKFilter
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  output$mytable26 <- renderDataTable({
    surplusMSKFilter
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  output$mytable27 <- renderDataTable({
    surplusSPbFilter
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  output$mytable28 <- renderDataTable({
    surplusNSKFilter
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  
  output$mytable29 <- renderDataTable({
    soldplanMSK
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  output$mytable30 <- renderDataTable({
    soldplanSPb
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  output$mytable31 <- renderDataTable({
    soldplanNSK
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  
  output$mytable32 <- renderDataTable({
    MSKstockArch
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE, header= FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  output$mytable33 <- renderDataTable({
    SPbstockArch
  }, options = list(orderClasses = TRUE, scrollY = '300px', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  output$mytable34 <- renderDataTable({
    NSKstockArch
  }, options = list(orderClasses = TRUE, scrollY = '300px', extensions = 'FixedColumns', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  output$mytable35 <- renderDataTable({
    priorityProduction
  }, options = list(orderClasses = TRUE, scrollY = '300px', extensions = 'FixedColumns', paging = FALSE )) #добавили высоту таблицы и скролл вверх-вниз
  
  
  
  # reactive programming
  v<-reactive({input$freq})
  #k<-as.integer(v)
  
  output$plot1 <- renderPlot({
    slices <- c(10+v(), 12, 4, 16, 8) 
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
  
  
  #   df=data.frame(country=c("Москва", "Санкт-Петербург", "Новосибирск"), 
  #                 план =c(MSKplanSales,SPbplanSales,NSKplanSales),
  #                 факт =c(MSKrealSales,SPbrealSales,NSKrealSales))
  #   
  
  
  df1=data.frame(country=c(colnames(salesAnalysis)), 
                план =  as.numeric(salesAnalysis [2,]),
                факт =as.numeric(salesAnalysis [1,]))
  
  
  #df= read.csv(salesAnalysis.csv)
  
  
  output$googleSales <- renderGvis(gvisBarChart(
    
    
    #вместо df
    data.frame(country=c("Москва", "Санкт-Петербург", "Новосибирск"), 
               факт =c(40+v(),45,60), 
               план =c(45,35,45))
    
    
    
    
  ))
  
  
  df<- read.csv ("surplusHistory.csv")
  
  row.names(df)<- df [ ,1]
  df<- df [ ,-1]
  
  df=data.frame(country=row.names(df), 
                МСК=df[ ,1], 
                СПб=df[ ,2],
                НСК=df[ ,3]
  )
  
  output$lineSales <- renderGvis(gvisLineChart(df))
  
  
  output$vertSales <- renderGvis(gvisColumnChart(df1))
  #output$lineSales <- renderGvis(gvisLineChart(df))
  
  
#   # оборачиваемость
BranchTurn<-read.csv ("BranchTurn.csv")
BranchTurnMSK <- as.data.frame(BranchTurn [1,])
output$BranchTurnMSK<-renderGvis({ gvisGauge(BranchTurnMSK, 
                                      options=list(min=0, max=3, greenFrom=0.7,
                                                   greenTo=1.5, yellowFrom=1.5, yellowTo=3,
                                                   redFrom=0, redTo=0.7, width=200, height=200)) 


})

BranchTurnSPb <- as.data.frame(BranchTurn [2,])
output$BranchTurnSPb<-renderGvis({ gvisGauge(BranchTurnSPb, 
                                             options=list(min=0, max=3, greenFrom=0.7,
                                                          greenTo=1.5, yellowFrom=1.5, yellowTo=3,
                                                          redFrom=0, redTo=0.7, width=200, height=200)) 
                                
                                
})

BranchTurnNSK <- as.data.frame(BranchTurn [3,])
output$BranchTurnNSK<-renderGvis({ gvisGauge(BranchTurnNSK, 
                                             options=list(min=0, max=3, greenFrom=0.7,
                                                          greenTo=1.5, yellowFrom=1.5, yellowTo=3,
                                                          redFrom=0, redTo=0.7, width=200, height=200)) 
                                   
                                   
})











  
  
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
                                                                 width=555, height=450,
                                                                 
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
    gvisPieChart(
      
      
      #вместо mat    
      
      data.frame(party=c("400 видео", "300 аудио", "трубки", "мониторы",
                         "5000", "mifare"),
                 members.of.parliament=c(193+15*v(), 93, 44, 
                                         146, 76, 68))
      
      
      , 
      options=list(
        width=450,
        height=400,
        slices="{0: {offset: 0.2},
        1: {offset: 0.2},
        2: {offset: 0.2},
        3: {offset: 0.5}}",
        
        
        #                 title='Доли ассортимента в обороте 1-й квартал 2015',
        legend='none',
        colors="['#6b38d2','orange', 'blue', 
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
