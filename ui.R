library(shiny)
library(ggplot2)  # for the diamonds dataset

diamonds<- read.csv ("SalesPlan2.csv")

shinyUI(fluidPage(
  title = 'Examples of DataTables',
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "Остатки"', #название должно совпадать с tabPanel
        checkboxGroupInput('show_vars', 'Отображение колонок:',
                           names(diamonds), selected = names(diamonds))
      ),
      conditionalPanel(        
        'input.dataset === "Продажи"',
        helpText('Click the column header to sort a column.')
                
      ),
      conditionalPanel(
        'input.dataset === "Планы"',
        helpText('Display 5 records by default.')
      ),
     width = 2),
    
    mainPanel(
      
      (navbarPage(                 # меню
        title = 'Опции',
        
        tabPanel("Аналитика",      
                 tabsetPanel(          # вложенное меню (2-й уровень)
      
      
        id = 'dataset',
        tabPanel('Остатки', dataTableOutput('mytable1')),
        tabPanel('Продажи', dataTableOutput('mytable2')),
        tabPanel('Планы', dataTableOutput('mytable3')), 
        tabPanel('Диаграммы', plotOutput("plot1"),
                
                 fluidRow( column(4,
                                  plotOutput("plot2")),
          
          column(4, offset = 4,
                 plotOutput("plot3")
          )      
        ))

        
        )),
# первый уровень меню
tabPanel("Отчеты",
         
tabsetPanel( 
  
tabPanel("Распределение",  tags$iframe(src = "https://plot.ly/~geomando/0/",  frameBorder="0", width =
                                         "800px", height = "800px")),
 
tabPanel("Резюме",tags$iframe(src = "https://plot.ly/~meniko117/7/", frameBorder="0", width = 
                                "800px", height = "800px")),

tabPanel("Динамика",tags$iframe(src = "https://plot.ly/~bronsolo/46/relative-daily-index-closings/", 
                              frameBorder="0", width = "800px", height = "800px"))
) 



  ))
))

)))