library(plotly)
library(fpc)
library(cluster) 
library(pvclust)
library(mclust)
library(plotly)
library(rpart)
library(rpart.plot)
library(PerformanceAnalytics)
library('rgl')
library('RColorBrewer')
library("scatterplot3d")
packageVersion('plotly')

options(shiny.maxRequestSize=100*1024^2)

function(input, output, session) {
  # uploading the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    ranges <- reactiveValues(x = NULL, y = NULL)
    try(
    read.csv(file=file1$datapath, sep=input$sep, header = input$header, quote = input$quote))
  })
  # Update select input
  observe({
    #try(df <- data())
    try(updateSelectInput(session,"xcol","xcol",choices = names(data())))
    try(updateSelectInput(session,"ycol","ycol",choices = c(names(data()),"Frequency"),selected = "Frequency"))
    try(updateSelectInput(session,"xcol1","xcol1",choices = names(data())))
    try(updateSelectInput(session,"ycol1","ycol1",choices = names(data())))
    try(updateSelectInput(session,"zcol1","zcol1",choices = names(data())))
    try(updateSelectInput(session,"xcol2","xcol2",choices = names(data())))
    try(updateSelectInput(session,"ycol2","ycol2",choices = names(data())))
    try(updateSelectInput(session,"zcol2","zcol2",choices = names(data())))
    try(updateSelectInput(session,"xcol3","Column 1 (x)",choices = names(data())))
    try(updateSelectInput(session,"ycol3","Column 2 (y)",choices = names(data())))
    try(updateSelectInput(session,"zcol3","Column 3 (z)",choices = names(data())))
    try(updateSelectInput(session,'xcol5', 'X Variable', choices=names(data())))
    try(updateSelectInput(session,'ycol5', 'Y Variable', choices=names(data())))
    try(updateSelectInput(session,'xcol6', 'Nodes (prediction variable)', choices=names(data())))
    try(updateSelectInput(session,'ycol6', 'Variable 2', choices=names(data())))
    try(updateSelectInput(session,'zcol6', 'Variable 3', choices=names(data())))
    try(updateSelectInput(session,"acol3","Column 4 (each value has a unique color)",choices = names(data())) )
  })
    
  
  # plot 2D
  output$plot <- renderPlotly({
    validate(need(input$file,"Upload a file"))
    df <- data()
    if(input$ycol== "Frequency"){
      plot_ly(x = ~df[[input$xcol]],type=input$plotType) 
    }
    else{
        plot_ly(data = df, x = ~df[[input$xcol]], y = ~df[[input$ycol]],type = input$plotType)
    }
    
    
  })
  
  # table of uploaded data
  output$table <- DT::renderDataTable({
    
    if(is.null(data())){return ()}
    if(input$disp == "head") {
      return(head(data()))
    }
    else {
      return(data())
    }
    DT::datatable(data())
  })
  
  # summary of uploaded data 
  output$summary <- renderPrint({
    
    validate(need(input$file,"Upload a file"))
    summary(data())
  })

  selectedData <- reactive({
    #begining of data cleaning
    clustering_data <- na.omit(data())
    clustering_data <- scale(data.matrix(clustering_data))
    #end data cleaning
    clustering_data[, c(input$xcol5, input$ycol5)]
  })
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  output$kmeans <- renderPlot(height = 400, {
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })

  #3D Plot
  output$Scatterplot<- renderPlotly({
    
    validate(need(input$file,"Upload a file"))
    df <- data()
    plot_ly(df, x = ~df[[input$xcol2]], y = ~df[[input$ycol2]], z = ~df[[input$zcol2]],  colorscale = c('#BF382A', '#0C4B8E'),showscale= TRUE) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = df[[input$xcol2]]),
                          yaxis = list(title = df[[input$ycol2]]),
                          zaxis = list(title = df[[input$zcol2]])))


    
  })
  ranges <- reactiveValues(x = NULL, y = NULL)
  #MultipleVarPlot
  output$MultipleVarPlot<- renderPlotly({
    
    validate(need(input$file,"Upload a file"))
    df <- data()
    # if((ncol(df))>=5){
    #   colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')
    #   plot_ly(df, x = ~df[[input$xcol3]], y = ~df[[input$ycol3]], z = ~df[[input$zcol3]],
    #           color = ~df[[input$acol3]],
    #           size=~as.numeric(df[[input$bcol3]]),colors = colors,marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150),
    #           text = ~paste(input$bcol3,':', df[[input$bcol3]],input$acol3,':', df[[input$acol3]]
    #                         )) %>%
    #     layout(scene = list(xaxis = list(title = df[[input$xcol3]]),
    #                         yaxis = list(title = df[[input$ycol3]]),
    #                         zaxis = list(title = df[[input$zcol3]])),
    #            paper_bgcolor = 'rgb(243, 243, 243)',
    #            plot_bgcolor = 'rgb(243, 243, 243)')
    # 
    # }
    # else{
      plot_ly(df, x = ~df[[input$xcol3]], y = ~df[[input$ycol3]], z = ~df[[input$zcol3]],
              marker = list(color = ~as.numeric(df[[input$acol3]]), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE),
              text=~paste(paste('<br>',input$xcol3,':'),paste(df[[input$xcol3]],'<br>'),paste(input$ycol3,':'),paste(df[[input$ycol3]],'<br>'),paste(input$zcol3,':'),paste(df[[input$zcol3]],'<br>'),paste(input$acol3,':'),paste(df[[input$acol3]],'<br>'))) %>%
        add_markers() %>%
        layout(scene = list(xaxis = list(title = df[[input$xcol3]]),
                            yaxis = list(title = df[[input$ycol3]]),
                            zaxis = list(title = df[[input$zcol3]])),
               annotations = list(
                 x = 1.13,
                 y = 1.05,
                 text = input$acol3,
                 showarrow = FALSE
               ))
    # }
    
    
    
    
  })
  

  
  
  #ScatterPlot matrix
  output$pair<-renderPlot({
    validate(need(input$file,"Upload a file"))
    pairs(data(),gap = 0.2 ,main="Scatterplot Matrix")
  })
  
  #Covariance
  output$cov<-renderPrint({
    validate(need(input$file,"Upload a file"))
    df <- data()
    if(input$ycol== "Frequency"){
      print("Cannot calculate covariance when frequency is selected")
    }
    else{
      c("Covariance",cov(as.numeric(df[[input$xcol]]),as.numeric(df[[input$ycol]]))) 
    }
    
  })
  
  #Correlation
  output$cor<-renderPrint({
    validate(need(input$file,"Upload a file"))
    df <- data()
    if(input$ycol== "Frequency"){
      print("Cannot calculate correlation when frequency is selected")
    }
    else{
      c("Correlation",cor(as.numeric(df[[input$xcol]]),as.numeric(df[[input$ycol]])))  
    }
    
  })
  
  #Correlation matrix analytics
  output$analytics<-renderPlot({
    validate(need(input$file,"Upload a file"))
    df <- data()
    chart.Correlation(as.data.frame(lapply(df, as.numeric)), histogram=TRUE, pch=19,main="Scatterplot Matrix Analytic")

  })
  
  #Correlation for scatterplot matrix
  output$corre<-renderPrint({
    validate(need(input$file,"Upload a file"))
    df <- data()
    cor(as.data.frame(lapply(df, as.numeric)))
  })
  
  #Decision tree
  output$decisionTree<-renderPlot({
    validate(need(input$file,"Upload a file"))
    df<-data()
    df <- na.omit(df)
    variable1 <- df[[input$xcol6]]
    variable2 <- df[[input$ycol6]]
    variable3 <- df[[input$zcol6]]
    fit <- rpart(variable1  ~ variable2 + variable3, 
                 method="anova", data=df)
    rpart.plot(fit)
  })
}

