#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.

library(shiny)
shinyServer(function(input, output) {
  output$text <- renderText(input$slider1 * 2 + 10)
})