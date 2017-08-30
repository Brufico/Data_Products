#
library(shiny)
shinyUI(fluidPage(

  sidebarLayout(
    sidebarPanel(
      h2("Data generation"),
      radioButtons("dist", "Population distribution type:",
                   c("Normal" = "norm",
                     "Uniform" = "unif",
                     "Exponential" = "exp")),

      numericInput("numeric", "Size of the sample?", 
                   value = 100, min = 10, max = 1000, step = 10),
      sliderInput("mean", "Pick the population mean", 10 , 90 , 30 ),
      sliderInput("sd", "Pick the population standard deviation", min = 0, max =  30, value = 15),
      
      h2("Number of bins"),
      h4( textOutput("text0")),
      textOutput("text1"),
      textOutput("text2"),
      textOutput("text3"),
      textOutput(" "),
      sliderInput("bins", "Pick number of bins", min = 1, max =  50, value = 30),
      
      checkboxInput("showdist", "Show/Hide Population distribution", value = TRUE)
      # checkboxInput("show_ylab", "Show/Hide Y Axis Label", value = TRUE),
      # checkboxInput("show_title", "Show/Hide Title")
    ),
    mainPanel(
      h2("Histogram of a random sample drawn from  population with a known distribution"),     
      h3("Distribution"),
      textOutput("namedist"),
      h3("Plot:"),
      plotOutput("plot1"),
      textOutput("C")
    )
  )
))