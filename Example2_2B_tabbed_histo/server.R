#
library(shiny)
library(ggplot2)


shinyServer(function(input, output) {

        # inputs
        # ------
        distrib <- reactive({input$dist})
        number_of_points <- reactive({input$numeric})
        m <- reactive({input$mean})
        sd <- reactive({input$sd})
        showdist <- reactive({input$showdist})
        bins <- reactive({input$bins})

        # intermediate steps
        # ------------------

        # Distribution
        ddist <- reactive({get(paste0("d", distrib()))})
        rdist <- reactive({get( paste0("r", distrib()))})

        # Data generation
        set.seed(2017)
        df <- eventReactive(list(input$go, distrib(), m(), sd()),
                            {data.frame(x = switch(distrib(),
                                                   norm = rnorm(n = number_of_points(),
                                                                mean =  m(), sd = sd()),
                                                   exp = rexp(n = number_of_points(), rate = 1 / m()),
                                                   unif = runif(n = number_of_points(),
                                                                min = min(m(), sd()),
                                                                max = max(m(), sd()))
                            ))})


        # Output generation
        # ------------------

        output$C <- renderText("Shiny app by Bruno Fischer Colonimos")
        output$namedist <- renderText(switch(distrib(),
                                             "norm" = "Normal distribution",
                                             "unif" = "Uniform distribution",
                                             "exp" = "Exponential distribution"
                                             ))

        output$text0 <- renderText("Recommended number of bins:")
        # output$text1 <- renderText(as.character(paste0("Sturges = ", nclass.Sturges(df()$x))))
        # output$text2 <- renderText(as.character(paste0("Scott = ", nclass.scott(df()$x))))
        # output$text3 <- renderText(as.character(paste0("Friedman-Diaconis = ", nclass.FD(df()$x))))
        output$text1 <- renderText(as.character(paste0("Sturges = ", nclass.Sturges(df()$x),
                                                       ", Scott = ", nclass.scott(df()$x),
                                                       ", Friedman-Diaconis = ", nclass.FD(df()$x)
                                                       )))

        output$plot1 <- renderPlot({


                # xlab <- ifelse(input$show_xlab, "X Axis", "")
                # ylab <- ifelse(input$show_ylab, "Y Axis", "")
                # main <- ifelse(input$show_title, "Title", "")

                # Histogram
                ggh <- ggplot(df(), aes(x=x)) +
                        geom_histogram(aes(y=..density..), bins = bins()) +
                        geom_rug() +
                        scale_x_continuous(limits = c(-20, 120))

                # Draw distribution if needed + return
                if (showdist()) {
                        ggh + stat_function(fun=function(x){
                                switch(distrib(),
                                       norm= dnorm(x, mean = m(), sd = sd()),
                                       exp = dexp(x, rate = 1 / m()),
                                       unif = dunif(x,
                                                    min = min(m(), sd()),
                                                    max = max(m(), sd()))
                                )
                        }, color = "blue" )
                } else {ggh}


        })
})
