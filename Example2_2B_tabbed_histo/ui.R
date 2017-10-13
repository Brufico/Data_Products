#
library(shiny)

shinyUI(fluidPage(

  sidebarLayout(
    sidebarPanel(
      h2("Data generation"),
      radioButtons("dist", "Population distribution type:",
                   choices = c("Normal" = "norm",
                     "Uniform" = "unif",
                     "Exponential" = "exp"),
                   inline = TRUE ),

      numericInput("numeric", "Size of the sample?",
                   value = 50, min = 10, max = 1000, step = 10),
      actionButton("go", "Another sample"),
      sliderInput("mean", "Pick the population mean", 10 , 90 , 30 ),
      sliderInput("sd", "Pick the population standard deviation", min = 0, max =  30, value = 15),

      hr(),
      h2("Number of bins"),
      h4( textOutput("text0")),
      textOutput("text1"),
      # textOutput("text2"),
      # textOutput("text3"),
      br(),
      sliderInput("bins", "Pick number of bins", min = 1, max =  50, value = 30),

      checkboxInput("showdist", "Show Population distribution", value = TRUE)

    ),
    mainPanel(
      h2("Histogram of a random sample drawn from  population with a known distribution"),
      h3("Population Distribution"),
      textOutput("namedist"),
      h3("Sample Histogram:"),
      plotOutput("plot1"),
      em(textOutput("C"))
    )
  )
))
