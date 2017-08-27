#
library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  output$C <- renderText("(c) Bruno Fischer Colonimos")
  output$plot1 <- renderPlot({

    number_of_points <- input$numeric
    m <- input$mean
    sd <- input$sd
    # Data generation
    set.seed(2017)
    dataX <- rnorm(n = number_of_points, mean =  m, sd = sd)
    df <- data.frame(x = dataX)
    
    # recommended nb of bins
    ncst <- nclass.Sturges(dataX)
    ncsc <- nclass.scott(dataX)
    ncfd <- nclass.FD(dataX)
    output$text0 <- renderText("Recommended number of bins:")  
    output$text1 <- renderText(as.character(paste0("Sturges = ", ncst))) 
    output$text2 <- renderText(as.character(paste0("Scott = ", ncsc))) 
    output$text3 <- renderText(as.character(paste0("Friedman-Diaconis = ", ncfd))) 
    
    shownorm <- input$shownorm
    xlab <- ifelse(input$show_xlab, "X Axis", "")
    ylab <- ifelse(input$show_ylab, "Y Axis", "")
    main <- ifelse(input$show_title, "Title", "")
    
    bins <- input$bins

    
    ggh <- ggplot(df, aes(x=x)) + 
      geom_histogram(aes(y=..density..), bins = bins) + 
      geom_rug() + 
      scale_x_continuous(limits = c(-20, 120))
      
      
      
    
    if (shownorm) {
      ggh + stat_function(fun=function(x){dnorm(x, mean = m, sd = sd)}, color = "blue" )
    } else {ggh}
    
      
    # ggplot(data = df, aes(x = x)) + geom_histogram(bins = bins) + 
    #   scale_x_continuous(limits = c(0, 100))
  })
})