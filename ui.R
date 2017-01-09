# ELTIS dashboard2

library(shiny)
library(ggplot2)  # for the diamonds dataset
library(shinydashboard)



diamonds<- read.csv ("report.csv")
#components<-read.csv("components5.csv")
colnames <- names(diamonds) #вектор названий колонок
salesAnalysis<- read.csv ("salesAnalysis.csv") [2:4] #убрать первую колонку с навзаниями для последующего корректного отображения



ui <- dashboardPage( skin = "blue",
                     dashboardHeader(title = "ELITS",
                                     
                                     
                                     
                                     
                                     
                                     dropdownMenu(type = "messages",
                                                  messageItem(
                                                    from = "Отдел продаж",
                                                    message = "В этом мес продажи хороши."
                                                  ),
                                                  messageItem(
                                                    from = "Эльвира",
                                                    message = "А где кабель от Паритета?",
                                                    icon = icon("question"),
                                                    time = "13:45"
                                                  ),
                                                  messageItem(
                                                    from = "Логистика",
                                                    message = "Дайте денег на закупку.",
                                                    icon = icon("life-ring"),
                                                    time = "2014-12-01"
                                                  )),
                                     
                                     
                                     dropdownMenu(type = "tasks", badgeStatus = "success",
                                                  taskItem(value = 90, color = "green",
                                                           "Готовность заказа Израиль"
                                                  ),
                                                  taskItem(value = 17, color = "aqua",
                                                           "Продажа неликвидов"
                                                  ),
                                                  taskItem(value = 75, color = "yellow",
                                                           "Общее понимание ситуации"
                                                  ),
                                                  taskItem(value = 80, color = "red",
                                                           "Индекс удовлетворенности"
                                                  )),
                                     
                                     
                                     dropdownMenu(type = "notifications",
                                                  notificationItem(
                                                    text = "у нас 5 новых клиентов с утра",
                                                    icon("users")
                                                  ),
                                                  notificationItem(
                                                    text = "На 16.30 отгружено 40 заказов",
                                                    icon("truck"),
                                                    status = "success"
                                                  ),
                                                  notificationItem(
                                                    text = "Нагрузка на мозг 86%",
                                                    icon = icon("exclamation-triangle"),
                                                    status = "warning"
                                                  )
                                     ) 
                                     
                                     
                                     
                     ),
                     
                     
                     
                     
                     
                     
                     
                     dashboardSidebar(
                       
                       
                       sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                         label = "Поиск..."),
                       
                       sliderInput("freq",
                                   "Периодов в выборке, мес:",
                                   min = 1,  max = 50, value = 6) ,
                       
                       conditionalPanel(
                         'input.dataset === "diamonds"', 
                         checkboxGroupInput('show_vars', 'Показывать колонки:',
                                            names(diamonds), selected = colnames[2:15])
                       ),
                       conditionalPanel(        
                         'input.dataset === "diamonds"',
                         helpText('Click the column header to sort a column.')
                         
                       ),
                       
                       conditionalPanel(
                         'input.dataset === "diamonds"',
                         helpText('Display 5 records by default.')
                       )
                     ),
                     
                     dashboardBody(
                       
                       (navbarPage(                 # меню
                         title = 'Группы отчетов',
                         
                         tabPanel('Основные',  
                                  
                                  # tags$iframe(src = "http://widgetsmonster.com/w1428250696-classic_grey__pro&283&283/",frameBorder="0",width = "300px", height = "300px"), 
                                  
                                  
                                  
                                  
                                  h3(paste("Положение дел на текущую дату", Sys.time())),
                                  
                                  
                                  fluidRow(
                                    column(6,
                                           box(title = "Динамика стоимости излишков в запасах, руб", width=12,  status = "primary", htmlOutput("lineSales"),  solidHeader = TRUE,
                                               collapsible = TRUE)),
                                    #htmlOutput("lineSales", width = "600px", height = "600px"), 
                                    
                                    
                                    
                                    
                                    #                                     column(6,
                                    #                                            box(title = "Потенциал упущенной выгоды из-за товарного дефицита, руб", width=12,  status = "warning", htmlOutput("coolbars"),  solidHeader = TRUE,
                                    #                                                collapsible = TRUE))
                                    
                                    
                                    column(2, 
                                           
                                           box(title = "Оборачиваемость, мес", width=12, status = "primary",htmlOutput("BranchTurnMSK", height = 200), solidHeader = TRUE)
                                           
                                    ),
                                    
                                    column(2, 
                                           
                                           box(title = "Оборачиваемость, мес", width=12, status = "primary",htmlOutput("BranchTurnSPb", height = 200), solidHeader = TRUE)
                                           
                                    ),
                                    
                                    
                                    column(2, 
                                           
                                           box(title = "Оборачиваемость, мес", width=12, status = "primary",htmlOutput("BranchTurnNSK", height = 200), solidHeader = TRUE)
                                           
                                    )
                                    
                                    
                                  ),
                                  
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  h3(paste("Отчеты по продажам за текущий месяц")),
                                  
                                  #                                   fluidRow(
                                  #                                     column(6, 
                                  #                                            
                                  #                                            
                                  #                                            box(title = "Структура выручки, руб", width=12,  status = "warning", htmlOutput("doughnut"), height= 500,  solidHeader = TRUE,
                                  #                                                collapsible = FALSE)
                                  #                                            
                                  #                                            
                                  #                                     ), 
                                  #                                     
                                  #                                     column(3, 
                                  #                                            box(title = "Оборач-ть, мес", width=12, status = "primary", htmlOutput("googleturnComp", height = 250), solidHeader = TRUE,
                                  #                                                collapsible = TRUE) 
                                  #                                            
                                  #                                     ),
                                  #                                     column(3, 
                                  #                                            
                                  #                                            box(title = "Продажи, тыс руб", width=12, status = "primary",htmlOutput("googleGoods", height = 250), solidHeader = TRUE)
                                  #                                            
                                  #                                     ),
                                  #                                     
                                  #                                     column(6, 
                                  #                                            box(title = "Выполнение плана продаж по филиалам", width=12, status = "primary",htmlOutput("googleSales", width = "600px", height = "600px"), 
                                  #                                                solidHeader = TRUE)       
                                  #                                            
                                  #                                     )),  
                                  #                                   
                                  #                                   br(),
                                  #                                   br(),
                                  #                                   br(),
                                  #                                   br(),
                                  #                                   
                                  #                                   
                                  
                                  fluidRow(
                                    
                                    #                                     column(6, 
                                    #                                            
                                    #                                            box(title = "Звезды продаж", width=12, status = "primary",  htmlOutput("googleItems", width = "600px", height = "600px"), solidHeader = TRUE,
                                    #                                                collapsible = TRUE) 
                                    #                                            
                                    #                                     ),
                                    column(12,
                                           
                                           box(title = "План/факт продаж по себестоимости за текущий месяц, руб", width=12, status = "primary",   htmlOutput("vertSales", height = 2400), solidHeader = TRUE,
                                               collapsible = TRUE)
                                           
                                    )),
                                  
                                  h4(paste("Выполнение плана продаж Москва", salesAnalysis [3,1], "%")),
                                  h4(paste("Выполнение плана продаж Санкт-Петербург", salesAnalysis [3,2], "%")),
                                  h4(paste("Выполнение плана продаж Новосибирск", salesAnalysis [3,3], "%")),
                                  
                                  br(),
                                  br(),
                                  
                                  
                                  
                                  
                                  
                                  #actual selling
                                  column(12,  
                                         box(title = "Выполнение плана продаж МОСКВА за текущий месяц", width=12,  status = "warning", dataTableOutput('mytable29'), solidHeader = TRUE,
                                             collapsible = TRUE)),
                                  
                                  
                                  br(),
                                  br(),
                                  
                                  #actual selling
                                  column(12,  
                                         box(title = "Выполнение плана продаж  Санкт-Петербург за текущий месяц", width=12,  status = "success", dataTableOutput('mytable30'), solidHeader = TRUE,
                                             collapsible = TRUE)),
                                  
                                  
                                  br(),
                                  br(),
                                  
                                  #actual selling
                                  column(12,  
                                         box(title = "Выполнение плана продаж  Новосибирск за текущий месяц", width=12,  status = "info", dataTableOutput('mytable31'), solidHeader = TRUE,
                                             collapsible = TRUE)),
                                  
                                  
                                  
                                  br(),
                                  br()
                                  
#                                   #actual selling
#                                   column(12,  
#                                          box(title = "Статистика остатков МСК", width=12,  status = "warning", dataTableOutput('mytable32'), solidHeader = TRUE,
#                                              collapsible = TRUE)),
#                                   
#                                   br(),
#                                   br(),
#                                   
#                                   #actual selling
#                                   column(12,  
#                                          box(title = "Статистика остатков СПб", width=12,  status = "warning", dataTableOutput('mytable33'), solidHeader = TRUE,
#                                              collapsible = TRUE)),
#                                   
#                                   
#                                   br(),
#                                   br(),
#                                   
#                                   #actual selling
#                                   column(12,  
#                                          box(title = "Статистика остатков НСК", width=12,  status = "warning", dataTableOutput('mytable34'), solidHeader = TRUE,
#                                              collapsible = TRUE))
                                  
                                  
                                  





                                  
                                  #                                   
                                  #                                   fluidRow(
                                  #                                     
                                  #                                     column(6,  
                                  #                                            box(title = "Список запасов ГП", width=12,  status = "warning", dataTableOutput('mytable5'), solidHeader = TRUE,
                                  #                                                collapsible = TRUE)),
                                  #                                     
                                  #                                     column(6,
                                  #                                            box(title = "Структура ОС в запасах, руб", width=12,  status = "warning", htmlOutput("topstockbars"),  solidHeader = TRUE,
                                  #                                                collapsible = TRUE))
                                  #                                   ),
                                  #                                   
                                  #                                   
                                  #                                   
                                  #                                   
                                  #                                   
                                  #                                   
                                  #                                   br(),
                                  #                                   br(),
                                  #                                   br(),
                                  #                                   br(),
                                  #                                   
                                  #                                   
                                  #                                   fluidRow(
                                  #                                     column(6,
                                  #                                            box(title = "Структура ОС в запасах, руб", width=12,  status = "warning", htmlOutput("lineSales"),  solidHeader = TRUE,
                                  #                                                collapsible = TRUE)),
                                  #                                     #htmlOutput("lineSales", width = "600px", height = "600px"), 
                                  #                                     
                                  #                                     
                                  #                                     
                                  #                                     
                                  #                                     column(6,
                                  #                                            box(title = "Потенциал упущенной выгоды из-за товарного дефицита, руб", width=12,  status = "warning", htmlOutput("coolbars"),  solidHeader = TRUE,
                                  #                                                collapsible = TRUE))
                                  #                                   ),
                                  #                                   #htmlOutput("coolbars", width = "600px", height = "600px"), 
                                  #                                   
                                  #                                   br(),
                                  #                                   br(),
                                  #                                   br(),
                                  #                                   br(),
                                  #                                   
                                  #                                   fluidRow(
                                  #                                     # A static valueBox
                                  #                                     valueBox(10 * 2, "Заказы", icon = icon("credit-card")),
                                  #                                     
                                  #                                     # Dynamic valueBoxes
                                  #                                     valueBoxOutput("progressBox"),
                                  #                                     
                                  #                                     valueBoxOutput("approvalBox")
                                  #                                   ),
                                  #                                   
                                  #                                   
                                  #                                   
                                  #                                   fluidRow(column(4, offset = 5,
                                  #                                                   # Clicking this will increment the progress amount
                                  #                                                   actionButton("count", "Увеличить продажи вручную")
                                  #                                                   
                                  #                                                   #box(width = 4, actionButton("count", "Увеличить продажи"))
                                  #                                   ))
                                  #                                   
                                  
                                  
                                  
                                  
                                  #                  fluidRow(
                                  #                    column(4, 
                                  #                   tags$iframe(src = "https://plot.ly/~bronsolo/46/relative-daily-index-closings/", 
                                  #                              frameBorder="0", width = "600px", height = "600px") 
                                  #                   ),
                                  #                   column(4, offset = 4,
                                  #                  tags$iframe(src = "https://plot.ly/~alekswis/26/house-price-to-income-ratio-latest-to-historical-average/",  
                                  #                              frameBorder="0", width ="400px", height = "600px")  
                                  #                  
                                  #                    )     
                                  #               
                                  #            )
                                  
                         ),
                         
                         
                         
                         
                         # первый уровень меню
                         tabPanel("Комплектующие",
                                  
                                  tabsetPanel( 
                                    
                                    
                                    id = 'dataset',
                                    
                                    
                                    
                                    tabPanel("График закупок ПКИ",
                                             
                                             
                                             value = 'diamonds', 
                                             
                                             h4("План закупок ПКИ на текущую неделю"),
                                             dataTableOutput('mytable12'),
                                             br(),
                                             
                                             h4("План закупок ПКИ на ближайшие 4 недели"),
                                             dataTableOutput('mytable7'),
                                             br(),
                                             
                                             
                                             h4("Расчетный план закупок ПКИ до конца года, включая информацию по размещенным допзаказам (в пути)" ),
                                             br(),
                                             dataTableOutput('mytable1')
                                             
                                    ),
                                    
                                    
                                    
                                    tabPanel("Плановое потребление ПКИ  и нормативы закупок",
                                             
                                             h4("Потребление ПКИ в текущем месяце" ),
                                             br(),
                                             dataTableOutput('mytable13'),
                                             
                                             
                                             h4("Потребление ПКИ в следующем месяце" ),
                                             br(),
                                             dataTableOutput('mytable14'),
                                             
                                             h4("Потребление ПКИ через 2 месяца" ),
                                             br(),
                                             dataTableOutput('mytable15'),
                                             
                                             h4("Нормативы закупок ПКИ" ),
                                             br(),
                                             dataTableOutput('mytable16')
                                             
                                             #                                              tags$iframe(src = "https://plot.ly/~bronsolo/46/relative-daily-index-closings/", 
                                             #                                                                            frameBorder="0", width = "800px", height = "800px"
                                             #                                                                              )
                                    ),
                                    
                                    tabPanel("Анализ",  
                                             
                                             br(),
                                             
                                             h4("Планируемый бюджет на закупку" ),
                                             br(),
                                             dataTableOutput('mytable22'),
                                             
                                             
                                             
                                             h4("Замороженные средства более чем на 2 мес. Излишек указан в рублях." ),
                                             br(),
                                             dataTableOutput('mytable21'),
                                             
                                             
                                             br(),
                                             br(),
                                             br(),
                                             h4("Временные затраты на комплектацию, сборку и упаковку с учетом  комплектующих и оплаченных заказов"),
                                             br(),
                                             
                                             htmlOutput("googleGantt", width = "600px", height = "600px"))
                                    
                                  ) 
                                  
                                  
                                  
                         ),  
                         
                         
                         
                         tabPanel("Выпуск ГП",      
                                  tabsetPanel(          # вложенное меню (2-й уровень)
                                    
                                    
                                    id = 'dataset',
                                    tabPanel('Москва', 
                                             
                                             
                                             h4("Позиции в дефиците или по которым может возникнуть дефицит в течение 4 недель"),
                                             br(),
                                             
                                             box(title = "Позиции в дефиците или по которым может возникнуть дефицит в течение 4 недель", width=12,  status = "success", dataTableOutput('mytable23'), solidHeader = TRUE,
                                                 collapsible = TRUE),
                                             
                                            
                                             br(),
                                             br(),
                                             
                                             
                                             h4("Излишки, которые образуются в течение 4 ближайших недель. Текущие остатки + приход- расход за 4 недели."),
                                             h4("В колонке ОБОРАЧИВАЕМОСТЬ указано количество мес на сколько хватит товара."),
                                             br(),
                                             
                                             box(title = "Излишки, которые образуются в течение 4 ближайших недель. Текущие остатки + приход- расход за 4 недели.", width=12,  status = "success", dataTableOutput('mytable26'), solidHeader = TRUE,
                                                 collapsible = TRUE),
                                         
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             
                                             h4("Справочные данные"),
                                             
                                             h4("Плановые приходы в ближайшие недели, начиная с текущей (понедельно)"),
                                             h4("Информация из 1С на основе подписанных планов и зарегистированных допзаказов"),
                                             
                                             box(title = "Плановые приходы в ближайшие недели, начиная с текущей (понедельно)", width=12,  status = "success", dataTableOutput('mytable4'), solidHeader = TRUE,
                                                 collapsible = TRUE),
                                             
                                             br(),
                                             br(),
                                             
                                             h4("Плановые продажи"),
                                             
                                             box(title = "Плановые продажи", width=12,  status = "success", dataTableOutput('mytable17'), solidHeader = TRUE,
                                                 collapsible = TRUE),
                                             
                                                                                         
                                             h4("Расчетные приходы в ближайшие недели, начиная с текущей (понедельно)"),
                                             h4("Информация из 1С на основе остатков, резервов и планов продаж"),
                                             
                                             
                                             box(title = "Плановые приходы в ближайшие недели, начиная с текущей (понедельно)", width=12,  status = "success", dataTableOutput('mytable9'), solidHeader = TRUE,
                                                 collapsible = TRUE)
                                             ),
                                    
                                    
                                    
                                    tabPanel('Санкт-Петербург', 
                                             
                                             
                                             h4("Позиции в дефиците или по которым может возникнуть дефицит в течение 4 недель"),
                                             br(),
                                          
                                             
                                             box(title = "Позиции в дефиците или по которым может возникнуть дефицит в течение 4 недель", width=12,  status = "warning", dataTableOutput('mytable24'), solidHeader = TRUE,
                                                 collapsible = TRUE),
                                             
                                             
                                             br(),
                                             br(),
                                             
                                             
                                             h4("Излишки, которые образуются в течение 4 ближайших недель. Текущие остатки + приход- расход за 4 недели."),
                                             h4("В колонке ОБОРАЧИВАЕМОСТЬ указано количество мес на сколько хватит товара."),
                                             br(),
                                                                            
                                             box(title = "Излишки, которые образуются в течение 4 ближайших недель. Текущие остатки + приход- расход за 4 недели.", width=12,  status = "warning", dataTableOutput('mytable27'), solidHeader = TRUE,
                                                 collapsible = TRUE),
                                             
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             
                                             h4("Справочные данные"),
                                             
                                             h4("Плановые приходы в ближайшие недели, начиная с текущей (понедельно)"),
                                             h4("Информация из 1С на основе подписанных планов и зарегистированных допзаказов"),
                                             
                                             
                                             box(title = "Плановые приходы в ближайшие недели, начиная с текущей (понедельно)", width=12,  status = "warning", dataTableOutput('mytable2'), solidHeader = TRUE,
                                                 collapsible = TRUE),
                                             
                                             
                                             br(),
                                             br(),
                                             
                                             h4("Плановые продажи"),
                                                                                          
                                             box(title = "Плановые продажи", width=12,  status = "warning", dataTableOutput('mytable18'), solidHeader = TRUE,
                                                 collapsible = TRUE),
                                             
                                             h4("Расчетные приходы в ближайшие недели, начиная с текущей (понедельно)"),
                                             h4("Информация из 1С на основе остатков, резервов и планов продаж"),
                                             
                                    box(title = "Информация из 1С на основе остатков, резервов и планов продаж", width=12,  status = "warning", dataTableOutput('mytable10'), solidHeader = TRUE,
                                        collapsible = TRUE)),
                                    
                                    
                                    
                                    tabPanel('Новосибирск', 
                                             
                                             
                                             
                                             h4("Позиции в дефиците или по которым может возникнуть дефицит в течение 4 недель"),
                                             br(),
                                             
                                             
                                             box(title = "Позиции в дефиците или по которым может возникнуть дефицит в течение 4 недель", width=12,  status = "primary", dataTableOutput('mytable25'), solidHeader = TRUE,
                                                 collapsible = TRUE),
                                             
                                             br(),
                                             br(),
                                             
                                             
                                             h4("Излишки, которые образуются в течение 4 ближайших недель. Текущие остатки + приход- расход за 4 недели."),
                                             h4("В колонке ОБОРАЧИВАЕМОСТЬ указано количество мес на сколько хватит товара."),
                                             br(),
                                                                                        
                                             box(title = "Излишки, которые образуются в течение 4 ближайших недель. Текущие остатки + приход- расход за 4 недели.", width=12,  status = "primary", dataTableOutput('mytable28'), solidHeader = TRUE,
                                                 collapsible = TRUE),
                                             
                                             
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             
                                             h4("Справочные данные"),
                                             br(),
                                             h4("Плановые приходы в ближайшие недели, начиная с текущей (понедельно)"),
                                             h4("Информация из 1С на основе подписанных планов и зарегистированных допзаказов"),
                                                                          
                                             
                                             box(title = "Плановые приходы в ближайшие недели, начиная с текущей (понедельно)", width=12,  status = "primary", dataTableOutput('mytable3'), solidHeader = TRUE,
                                                 collapsible = TRUE),
                                             
                                             
                                             br(),
                                             br(),
                                             
                                             h4("Плановые продажи"),
                                                                                        
                                             
                                             box(title = "Плановые продажи", width=12,  status = "primary", dataTableOutput('mytable19'), solidHeader = TRUE,
                                                 collapsible = TRUE),
                                             
                                             h4("Расчетные приходы в ближайшие недели, начиная с текущей (понедельно)"),
                                             h4("Информация из 1С на основе остатков, резервов и планов продаж"),
                                             
                                             box(title = "Расчетные приходы в ближайшие недели, начиная с текущей (понедельно)", width=12,  status = "primary", dataTableOutput('mytable11'), solidHeader = TRUE,
                                                 collapsible = TRUE)
                                             
                                            ),
                                    
                                    tabPanel('Расчетный выпуск ГП', 
                                             
                                             h4("Приоритеты в производстве на текущую дату"),
                                             br(),
                                             br(),
                                             dataTableOutput('mytable35'),
                                             
                                             br(),
                                             br(),
                                             h4("Ежедневно изменяющийся план производства ГП в зависимости от темпа продаж"),
                                             br(),
                                             br(),
                                             dataTableOutput('mytable8'),
                                             
                                             br(),
                                             br(),
                                             h4("Доли при распределении ГП между филиалами на текущий месяц"),
                                             dataTableOutput('mytable20')
                                             #                                              plotOutput("plot1"),
                                             #                                              
                                             #                                              fluidRow( column(4,
                                             #                                                               plotOutput("plot2")),
                                             #                                                        
                                             #                                                        column(4, offset = 4,
                                             #                                                               plotOutput("plot3")
                                             #                                                        )      
                                             #                                              )
                                             
                                             
                                    )
                                    
                                    
                                  ))
                         
                       )
                       ))
)