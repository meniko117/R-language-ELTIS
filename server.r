library(shiny)

shinyServer(function(input, output) {
 diamonds<- read.csv ("SalesPlan2.csv")
 components<-read.csv("C:/Users/Hp/Dropbox/Мои документы/Программирование/R/Project/ELTIS/components.csv")
  
  # a large table, reative to input$show_vars
  output$mytable1 <- renderDataTable({
    #library(ggplot2)
    diamonds [, input$show_vars, drop = FALSE]
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- renderDataTable({
    components
  }, options = list(orderClasses = TRUE))
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- renderDataTable({
    iris
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
  

  
  
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
  
})