# ELTIS build

library(shiny)
library(ggplot2)  # for the diamonds dataset
library(shinydashboard)



diamonds<- read.csv ("components5.csv")
#components<-read.csv("components5.csv")
colnames <- names(diamonds) #вектор названий колонок



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
                min = 1,  max = 50, value = 15),
    
      conditionalPanel(
        'input.dataset === "diamonds"', 
        checkboxGroupInput('show_vars', 'Показывать колонки:',
                           names(diamonds), selected = colnames[3:15])
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
                 
                
               
                
                   h4(paste("Положение дел на текущую дату", Sys.time())),
                           
                
                
                 fluidRow(
                   column(6, 
                          
                               
                         box(title = "Структура выручки, руб", width=12,  status = "warning", htmlOutput("doughnut"), height= 495, solidHeader = TRUE,
                             collapsible = TRUE)
                           
                          
                   ), 
                   
                   column(3, 
                          box(title = "Оборач-ть, мес", width=12, status = "primary", htmlOutput("googleturnComp", height = 250), solidHeader = TRUE,
                              collapsible = TRUE) 
                          
                   ),
                   column(3, 
                          
                          box(title = "Продажи, млн руб", width=12, status = "primary",htmlOutput("googleGoods", height = 250),solidHeader = TRUE)
                          
                   ),
                   
                   column(6, 
                          box(title = "Выполнение плана продаж по филиалам", width=12, status = "primary",htmlOutput("googleSales", width = "600px", height = "600px"), 
                              solidHeader = TRUE)       
                   
                   )),  
                 
                br(),
                br(),
                br(),
                br(),
                
            
                 
                 fluidRow(
                   
                   column(6, 
                        
                          box(title = "Звезды продаж", width=12, status = "primary",  htmlOutput("googleItems", width = "600px", height = "600px"), solidHeader = TRUE,
                              collapsible = TRUE) 
                          
                   ),
                   column(6,
                        
                          box(title = "План/факт по марже", width=12, status = "primary",   htmlOutput("vertSales", width = "600px", height = "600px"), solidHeader = TRUE,
                              collapsible = TRUE)
                   
                   )),
                
                br(),
                br(),
                     
                
                fluidRow(
                  
                  column(6,  
                box(title = "Список запасов ГП", width=12,  status = "warning", dataTableOutput('mytable5'), solidHeader = TRUE,
                    collapsible = TRUE)),
                
                column(6,
                box(title = "Структура ОС в запасах, руб", width=12,  status = "warning", htmlOutput("topstockbars"),  solidHeader = TRUE,
                    collapsible = TRUE))
                ),
                
                fluidRow(
                  # A static valueBox
                  valueBox(10 * 2, "Заказы", icon = icon("credit-card")),
                  
                  # Dynamic valueBoxes
                  valueBoxOutput("progressBox"),
                  
                  valueBoxOutput("approvalBox")
                ),
                
                
                
                fluidRow(column(4, offset = 5,
                                # Clicking this will increment the progress amount
                                actionButton("count", "Увеличить продажи вручную")
                                
                                #box(width = 4, actionButton("count", "Увеличить продажи"))
                )),
                
                
                
                
                br(),
                br(),
                br(),
                br(),
                
                 
                
                br(),
                br(),
                br(),
                br(),
                htmlOutput("lineSales", width = "600px", height = "600px"), 
                
                br(),
                br(),
                br(),
                br(),
                
                 
                
                br(),
                br(),
                br(),
                br(),
                
                
                htmlOutput("coolbars", width = "600px", height = "600px"), 
                
                br(),
                br(),
                br(),
                br()
                
                
               
                
             
                
                
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
                   
                   
                   tabPanel("График сборки",  
                            br(),
                            br(),
                            h4("Временные затраты на комплектацию, сборку и упаковку с учетом  комплектующих и оплаченных заказов"),
                                             br(),
                                             br(),
                                             br(),
                            
                                                                                
                            htmlOutput("googleGantt", width = "600px", height = "600px")),
                   
                   tabPanel("График закупок ПКИ",
                            
                            
                            value = 'diamonds', 
                                     
                                     h4("Распределение средств в запасах ПКИ"),
                                     br(),
                                     dataTableOutput('mytable1')
                                                        
                            ),
                   
                   
                   
                   tabPanel("График платежей",tags$iframe(src = "https://plot.ly/~bronsolo/46/relative-daily-index-closings/", 
                                                   frameBorder="0", width = "800px", height = "800px"))
                 ) 
                 
                 
                 
        ),  
        
        
        
        tabPanel("Выпуск ГП",      
                 tabsetPanel(          # вложенное меню (2-й уровень)
                   
                   
                   id = 'dataset',
                   tabPanel('Москва', 
                            
                            h4("Распределение средств в запасах ПКИ"),
                            br(),
                            br(),
                            br(),
                            dataTableOutput('mytable4')),
                   
                   tabPanel('Санкт-Петербург', dataTableOutput('mytable2')),
                   tabPanel('Новосибирск', dataTableOutput('mytable3')), 
                   tabPanel('Диаграммы', plotOutput("plot1"),
                            
                            fluidRow( column(4,
                                             plotOutput("plot2")),
                                      
                                      column(4, offset = 4,
                                             plotOutput("plot3")
                                      )      
                            ))
                   
                   
                 ))
        
        )
      ))
  )
