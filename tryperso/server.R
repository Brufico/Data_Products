# modified BFC
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #data
  x    <- faithful[, 2]
  
  # recommended nb of bins
  ncst <- nclass.Sturges(x)
  ncsc <- nclass.scott(x)
  ncfd <- nclass.FD(x)
  
  output$text0 <- renderText("Recommended number of bins:")  
  output$text1 <- renderText(as.character(paste0("Sturges:", ncst))) 
  output$text2 <- renderText(as.character(paste0("Scott:", ncsc))) 
  output$text3 <- renderText(as.character(paste0("Friedman-Diaconis:", ncfd))) 
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # 
    # Using ggplot2
    gh <- ggplot(data = faithful, aes(x = waiting)) + 
      geom_histogram(bins = input$bins)
    gh
    
  })
  
})
